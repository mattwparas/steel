// Using ABI Stable types is very important
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

struct MemoryInfo {
    info: sys_info::MemInfo,
}

impl MemoryInfo {
    fn total(&self) -> isize {
        self.info.total as isize
    }

    fn avail(&self) -> isize {
        self.info.avail as isize
    }

    fn free(&self) -> isize {
        self.info.free as isize
    }

    fn buffers(&self) -> isize {
        self.info.buffers as isize
    }

    fn cached(&self) -> isize {
        self.info.cached as isize
    }

    fn swap_total(&self) -> isize {
        self.info.swap_total as isize
    }

    fn swap_free(&self) -> isize {
        self.info.swap_free as isize
    }
}

impl Custom for MemoryInfo {}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/sys-info");

    module.register_fn("mem-info", || MemoryInfo {
        info: sys_info::mem_info().unwrap(),
    });

    module
        .register_fn("MemoryInfo-total", MemoryInfo::total)
        .register_fn("MemoryInfo-avail", MemoryInfo::avail)
        .register_fn("MemoryInfo-free", MemoryInfo::free)
        .register_fn("MemoryInfo-buffers", MemoryInfo::buffers)
        .register_fn("MemoryInfo-cached", MemoryInfo::cached)
        .register_fn("MemoryInfo-swap-total", MemoryInfo::swap_total)
        .register_fn("MemoryInfo-swap-free", MemoryInfo::swap_free);

    module
}
