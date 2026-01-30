use std::{
    cell::RefCell,
    path::{Path, PathBuf},
    rc::Rc,
    sync::OnceLock,
};

use parking_lot::Mutex;

use crate::{
    rvals::{as_underlying_type, Custom, Result},
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn, vm::VmCore},
    SteelVal,
};

// TODO: Move this to be on the VM Context directly?
thread_local! {
    pub static CURRENT_CAPABILITIES: RefCell<CapabilityManager> = RefCell::new(CapabilityManager::new());
    pub static CAPABILITY_TOKEN: std::sync::OnceLock<SteelVal> = OnceLock::new();
}

#[steel_derive::define_module(name = "steel/capabilities")]
pub fn capabilities_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/capabilities");

    module
        .register_fn("#%pop-capability", pop_capability_checked)
        .register_fn("#%push-capability", push_capability_checked)
        .register_native_fn_definition(PUSH_CAPABILITIES_DEFINITION)
        .register_fn("#%pop-n-capabilities", pop_n_capabilities)
        .register_fn("file-system-access", Capability::new_file_system)
        .register_fn("fs/write", || FileSystemAccessKind::Write)
        .register_fn("fs/read", || FileSystemAccessKind::Read)
        .register_native_fn_definition(SET_TOKEN_DEFINITION);

    module
}

#[derive(Clone, Debug)]
pub struct CapabilityManager {
    capabilities: alloc::sync::Arc<Mutex<Vec<Capability>>>,
}

impl CapabilityManager {
    pub fn fetch_installed() -> Self {
        CURRENT_CAPABILITIES.with(|x| x.borrow().clone())
    }

    pub fn clear(&mut self) {
        self.capabilities.lock().clear();
    }

    pub fn new() -> Self {
        Self {
            capabilities: alloc::sync::Arc::new(Mutex::new(Vec::new())),
        }
    }
}

pub fn install_capability(capability_manager: &CapabilityManager) {
    CURRENT_CAPABILITIES.with(|x| *(x.borrow_mut()) = capability_manager.clone());
}

pub fn take_current_capability() -> CapabilityManager {
    CURRENT_CAPABILITIES.with(|x| {
        // Type inference doesn't love this
        let inner: CapabilityManager =
            std::mem::replace(&mut x.borrow_mut(), CapabilityManager::new());

        inner
    })
}

#[steel_derive::context(name = "#%set-capability-token!", arity = "Exact(1)")]
fn set_token(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let value = args[0].clone();

    if ctx.thread.compiler.read().kernel.is_none() {
        return Some(Ok(SteelVal::Void));
    }

    CAPABILITY_TOKEN.with(|x| x.set(value)).ok();

    Some(Ok(SteelVal::Void))
}

fn check_token(token: &SteelVal) -> Result<()> {
    println!("Checking against token: {}", token);
    if CAPABILITY_TOKEN.with(|x| dbg!(x.get()) != Some(token)) {
        stop!(Generic => "illegal capabilities token")
    }

    Ok(())
}

#[steel_derive::native(name = "#%push-capabilities", constant = true, arity = "AtLeast(1)")]
pub fn push_capabilities(args: &[SteelVal]) -> Result<SteelVal> {
    let token = &args[0];

    check_token(token)?;

    for arg in &args[1..] {
        if let SteelVal::Custom(c) = arg {
            if let Some(underlying) = as_underlying_type::<Capability>(c.read().as_ref()) {
                push_capability_unchecked(underlying)
            }
        }
    }

    Ok(SteelVal::Void)
}

pub fn push_capability_unchecked(capability: &Capability) {
    CURRENT_CAPABILITIES.with(|x| x.borrow_mut().capabilities.lock().push(capability.clone()));
}

pub fn push_capability_checked(capability: &Capability, token: SteelVal) -> Result<()> {
    check_token(&token)?;
    CURRENT_CAPABILITIES.with(|x| x.borrow_mut().capabilities.lock().push(capability.clone()));
    Ok(())
}

pub fn pop_capability_checked(token: SteelVal) -> Result<()> {
    check_token(&token)?;
    CURRENT_CAPABILITIES.with(|x| x.borrow_mut().capabilities.lock().pop());
    Ok(())
}

pub fn pop_capability_unchecked() {
    CURRENT_CAPABILITIES.with(|x| x.borrow_mut().capabilities.lock().pop());
}

pub fn pop_n_capabilities(token: SteelVal, n: usize) -> Result<()> {
    check_token(&token)?;
    for _ in 0..n {
        pop_capability_unchecked();
    }
    Ok(())
}

pub struct FileSystemAccessRequest<'a> {
    pub kind: FileSystemAccessKind,
    pub resource: &'a str,
}

pub fn check_read(resource: &str) -> Result<()> {
    FileSystemAccessRequest {
        kind: FileSystemAccessKind::Read,
        resource,
    }
    .check()
}

pub fn check_write(resource: &str) -> Result<()> {
    FileSystemAccessRequest {
        kind: FileSystemAccessKind::Write,
        resource,
    }
    .check()
}

impl<'a> FileSystemAccessRequest<'a> {
    pub fn check(&self) -> Result<()> {
        CURRENT_CAPABILITIES.with(|x| {
            let guard = x.borrow();

            // TODO: Actually check that this works?

            let capabilities = guard.capabilities.lock();

            if capabilities.is_empty() {
                return Ok(());
            }

            // The default, is that with no specification, it has
            // full access. Otherwise, we check each specification in scope
            // for a compatibility - if there aren't any compatibilities,
            // we bail out
            let mut found_incompatibility = false;

            // Check the access to the resource
            for value in capabilities.iter() {
                if let Capability::FileSystem(FileSystemCapability { kind, resource }) = value {
                    let matches_pattern = resource.matches(self.resource);

                    if matches_pattern {
                        // (Asking, Granted)
                        match (self.kind, kind) {
                            (FileSystemAccessKind::Write, FileSystemAccessKind::Read) => {
                                stop!(Generic => "This module has not been given
                            access to this kind of system resource!")
                            }
                            (_, _) => {
                                found_incompatibility = false;

                                continue;
                            }
                        }
                    } else {
                        found_incompatibility = true;
                    }
                }
            }

            if found_incompatibility {
                stop!(Generic => "Access denied (capabilities)")
            }

            Ok(())
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Capability {
    FileSystem(FileSystemCapability),
    Dylib(DylibCapability),
    Process(ProcessCapability),
}

impl Capability {
    pub fn new_file_system(kind: FileSystemAccessKind, resource: String) -> Self {
        Self::FileSystem(FileSystemCapability {
            kind,
            resource: glob::Pattern::new(&resource).unwrap(),
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FileSystemAccessKind {
    Read,
    Write,
}

impl Custom for FileSystemAccessKind {}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FileSystemCapability {
    kind: FileSystemAccessKind,
    resource: glob::Pattern,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct DylibCapability;

impl Custom for Capability {}

#[derive(Clone, PartialEq, Eq, Debug)]
struct ProcessCapability {
    allowed_binary: String,
}

impl Custom for ProcessCapability {}
