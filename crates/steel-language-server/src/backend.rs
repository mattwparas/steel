#![allow(unused)]

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    error::Error,
    path::PathBuf,
    sync::{Arc, Mutex, RwLock},
};

use dashmap::{DashMap, DashSet};

use crossbeam_utils::atomic::AtomicCell;
use once_cell::sync::Lazy;
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use steel::{
    compiler::{
        modules::{steel_home, MaybeRenamed, MANGLER_PREFIX, MODULE_PREFIX},
        passes::analysis::{
            find_identifier_at_position, query_top_level_define,
            query_top_level_define_on_condition, require_defitinion_to_original_symbol, Analysis,
            IdentifierStatus, RequiredIdentifierInformation, SemanticAnalysis, SemanticInformation,
            SemanticInformationType,
        },
    },
    parser::{
        ast::ExprKind,
        expander::SteelMacro,
        interner::InternedString,
        parser::{Parser, SourceId},
        span::Span,
        tryfrom_visitor::SyntaxObjectFromExprKindRef,
    },
    rvals::{AsRefSteelVal, FromSteelVal, SteelString},
    steel_vm::{builtin::BuiltInModule, engine::Engine, register_fn::RegisterFn},
};
use steel_parser::{
    ast::{Define, ToDoc},
    parser::SyntaxObjectId,
};
use tower_lsp::jsonrpc::{self, Result};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use tower_lsp::lsp_types::SemanticTokenType;

use crate::diagnostics::{
    create_diagnostic, DiagnosticContext, DiagnosticGenerator, FreeIdentifiersAndUnusedIdentifiers,
    StaticArityChecker,
};

pub fn lsp_home() -> PathBuf {
    if let Ok(home) = std::env::var("STEEL_LSP_HOME") {
        return PathBuf::from(home);
    }

    let mut home_directory =
        PathBuf::from(steel_home().expect("Unable to find steel home location"));
    home_directory.push("lsp");

    if !home_directory.exists() {
        std::fs::create_dir_all(&home_directory).expect("Unable to create the lsp home directory");
    }

    home_directory
}

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
];

#[derive(Debug)]
pub struct FileState {
    pub opened: bool,
}

pub struct Backend {
    pub config: Config,

    pub client: Client,
    pub ast_map: DashMap<String, Vec<ExprKind>>,
    pub raw_ast_map: DashMap<String, Vec<ExprKind>>,
    pub lowered_ast_map: DashMap<String, Vec<ExprKind>>,
    pub document_map: DashMap<String, Rope>,
    pub vfs: DashMap<Url, FileState>,
    // TODO: This needs to hold macros to help with resolving definitions
    pub _macro_map: DashMap<String, HashMap<InternedString, SteelMacro>>,
    pub ignore_set: Arc<DashSet<InternedString>>,
    pub globals_set: Arc<DashSet<InternedString>>,
    pub defined_globals: DashSet<String>,

    pub root: PathBuf,
}

#[derive(Debug, Clone, Copy)]
pub enum OffsetEncoding {
    Utf8,
    Utf16,
    Utf32,
}

pub struct Config {
    pub encoding: AtomicCell<OffsetEncoding>,
}

impl Config {
    pub fn new() -> Self {
        Config {
            encoding: AtomicCell::new(OffsetEncoding::Utf16),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let available_encodings = params
            .capabilities
            .general
            .and_then(|general| general.position_encodings)
            .unwrap_or_default();

        let position_encoding = if available_encodings.contains(&PositionEncodingKind::UTF8) {
            self.config.encoding.store(OffsetEncoding::Utf8);
            Some(PositionEncodingKind::UTF8)
        } else if available_encodings.contains(&PositionEncodingKind::UTF32) {
            // we really don't want utf16
            self.config.encoding.store(OffsetEncoding::Utf32);
            Some(PositionEncodingKind::UTF32)
        } else {
            None
        };

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                position_encoding,
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec!["(".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![],
                    work_done_progress_options: Default::default(),
                }),

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("scheme".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                })),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }
    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    // TODO: Separate out did change and on change
    // so that we can get away with incremental updates.
    //
    // On save should kick off a full build of things.
    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    // TODO: In order to get changes in one file to propagate to another,
    // This _should_ trigger a recompile for the file - this will need to probably
    // read the whole file, and then re run the analysis?
    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(self.hover_impl(params).await)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let symbols = async {
            let uri = params.text_document.uri;
            let mut ast = self.raw_ast_map.get_mut(uri.as_str())?;
            let mut rope = self.document_map.get(uri.as_str())?.clone();

            let analysis = SemanticAnalysis::new(&mut ast);

            let top_level_defs: Vec<SymbolInformation> = {
                let definitions = analysis.find_top_level_definitions();

                definitions
                    .iter()
                    .map(|(name, kind, span)| {
                        let container_name = span.source_id.and_then(|source_id| {
                            let contexts = analysis
                                .find_contexts_with_offset(span.start() as usize, source_id);

                            if contexts.is_empty() {
                                return None;
                            }

                            match &contexts.first().unwrap() {
                                SemanticInformationType::Variable(v) => {
                                    Some("variable".to_string())
                                }
                                SemanticInformationType::Function(f) => match f.aliases_to {
                                    Some(function_name_id) => {
                                        match kind {
                                            ExprKind::LambdaFunction(symbol) => None, // if symbol.syntax_object_id == function_name_id.0 => None,
                                            _ => {
                                                let mut id_to_str = HashMap::new();
                                                id_to_str.insert(function_name_id, None);
                                                analysis.syntax_object_ids_to_identifiers(
                                                    &mut id_to_str,
                                                );

                                                match id_to_str.get(&function_name_id) {
                                                    Some(Some(name)) => Some(name.to_string()),
                                                    _ => Some("lambda function".to_string()),
                                                }
                                            }
                                        }
                                    }
                                    _ => Some("lambda function".to_string()),
                                },
                                SemanticInformationType::CallSite(c) => Some("call".to_string()),
                                SemanticInformationType::Let(l) => Some("let".to_string()),
                            }
                        });

                        SymbolInformation {
                            name: name.resolve().into(),
                            kind: match kind {
                                ExprKind::LambdaFunction(_) => SymbolKind::FUNCTION,
                                _ => SymbolKind::CONSTANT,
                            },
                            tags: None,
                            deprecated: None,
                            location: Location {
                                uri: uri.clone(),
                                range: self.config.span_to_range(span, &rope).unwrap(),
                            },
                            container_name,
                        }
                    })
                    .collect()
            };

            let let_bindings: Vec<SymbolInformation> = {
                let definitions = analysis.find_let_bindings();

                definitions
                    .iter()
                    .map(|(name, span)| {
                        let container_name = span.source_id.and_then(|source_id| {
                            let contexts = analysis
                                .find_contexts_with_offset(span.start() as usize, source_id);

                            if contexts.is_empty() {
                                return None;
                            }

                            match &contexts.first().unwrap() {
                                SemanticInformationType::Variable(v) => {
                                    Some("variable".to_string())
                                }
                                SemanticInformationType::Function(f) => match f.aliases_to {
                                    Some(function_name_id) => {
                                        let mut id_to_str = HashMap::new();
                                        id_to_str.insert(function_name_id, None);
                                        analysis.syntax_object_ids_to_identifiers(&mut id_to_str);

                                        match id_to_str.get(&function_name_id) {
                                            Some(Some(name)) => Some(name.to_string()),
                                            _ => Some("function".to_string()),
                                        }
                                    }
                                    _ => Some("function".to_string()),
                                },
                                SemanticInformationType::CallSite(c) => Some("call".to_string()),
                                SemanticInformationType::Let(l) => Some("let".to_string()),
                            }
                        });

                        SymbolInformation {
                            name: name.to_string(),
                            kind: SymbolKind::VARIABLE,
                            tags: None,
                            deprecated: None,
                            location: Location {
                                uri: uri.clone(),
                                range: self.config.span_to_range(span, &rope).unwrap(),
                            },
                            container_name,
                        }
                    })
                    .collect()
            };

            let result = top_level_defs
                .into_iter()
                .chain(let_bindings.into_iter())
                .collect::<Vec<_>>();

            Some(DocumentSymbolResponse::Flat(result))
        }
        .await;

        Ok(symbols)
    }

    // TODO: For macros (and otherwise for find references to)
    // Simplest way to do this is to get an unexpanded ast, and then find the macro
    // that we're interested in identifying, and then we'll index against that by treating it
    // like a function call that we index against.
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let mut found_offset = None;
        let uri = params.text_document_position_params.text_document.uri;
        // TODO: In order for this to work, we'll have to both
        // expose a span -> URI function, as well as figure out how to
        // decide if a definition refers to an import. I think deciding
        // if something is a module import should be like:
        let definition = async {
            let mut ast = self.ast_map.get_mut(uri.as_str())?;
            let mut rope = self.document_map.get(uri.as_str())?.clone();

            let position = params.text_document_position_params.position;
            let offset = self.config.position_to_offset(position, &rope)?;

            found_offset = Some(offset);

            let analysis = SemanticAnalysis::new(&mut ast);

            let (_syntax_object_id, information) =
                analysis.find_identifier_at_offset(offset, uri_to_source_id(&uri)?)?;

            let refers_to = information.refers_to?;

            let maybe_definition = analysis.get_identifier(refers_to)?;

            let mut resulting_span = maybe_definition.span;

            // log::debug!("Refers to information: {:?}", &maybe_definition);

            let mut resolver = |mut interned: InternedString,
                                name: String,
                                original|
             -> Option<()> {
                let maybe_renamed = interned;

                if let Some(original) = original {
                    if interned != original {
                        interned = original;
                    }
                }

                let mut module_prefix_path_to_check =
                    name.trim_end_matches(if maybe_renamed == interned {
                        interned.resolve()
                    } else {
                        maybe_renamed.resolve()
                    });

                resulting_span = {
                    let guard = ENGINE.read().ok()?;

                    let modules = guard.modules();

                    let module = modules
                        .values()
                        .find(|x| x.prefix() == module_prefix_path_to_check)?;

                    let module_ast = module.get_ast();

                    let top_level_define = query_top_level_define(module_ast, interned.resolve())
                        .or_else(|| {
                        query_top_level_define_on_condition(
                            module_ast,
                            interned.resolve(),
                            |name, target| target.ends_with(name),
                        )
                    })?;

                    top_level_define.name.atom_syntax_object().map(|x| x.span)?
                };

                Some(())
            };

            if maybe_definition.is_required_identifier {
                match analysis.resolve_required_identifier(refers_to)? {
                    RequiredIdentifierInformation::Resolved(
                        resolved,
                        mut interned,
                        name,
                        original,
                    ) => {
                        if let Some(original) = original {
                            if interned != original {
                                // Just call the unresolved
                                // todo!()

                                resolver(interned, name, Some(original))?;
                            } else {
                                resulting_span = resolved.span;
                            }
                        } else {
                            resulting_span = resolved.span;
                        }
                    }

                    RequiredIdentifierInformation::Unresolved(mut interned, name, original) => {
                        resolver(interned, name, original)?;
                    }
                }

                // log::debug!("Found new definition: {:?}", maybe_definition);
            }

            let location = source_id_to_uri(resulting_span.source_id()?)?;

            if location != uri {
                // log::debug!("Jumping to definition that is not yet in the document map!");

                let expression = ENGINE
                    .read()
                    .ok()?
                    .get_source(&resulting_span.source_id()?)?;

                rope = self
                    .document_map
                    .get(location.as_str())
                    .map(|x| x.clone())
                    .unwrap_or_else(|| Rope::from_str(&expression));

                self.document_map.insert(location.to_string(), rope.clone());
            }

            let range = self.config.span_to_range(&resulting_span, &rope)?;
            Some(GotoDefinitionResponse::Scalar(Location::new(
                location, range,
            )))
        }
        .await;

        // Attempt a fallback with the new goto definition stuff!
        if definition.is_none() {
            let raw_ast = self.raw_ast_map.get(uri.as_str());
            if let Some(rope) = self
                .document_map
                .get(uri.as_str())
                .map(|x| x.value().clone())
            {
                if let Some(raw_ast) = raw_ast {
                    if let Some(offset) = found_offset {
                        let ident = find_identifier_at_position(&raw_ast, offset as _);
                        if let Some(ident) = ident {
                            eprintln!("Unable to find a definition for: {}", ident.resolve());
                            // Check if this is a macro invocation:
                            let path = PathBuf::from(uri.path());
                            {
                                // Re run the analysis
                                if let Err(e) = ENGINE
                                    .write()
                                    .unwrap()
                                    .emit_expanded_ast(&format!("(require: {:?})", path), None)
                                {
                                    eprintln!("Unable to load module: {:?}", e);
                                }
                            }

                            // Look at the macros that are in scope, just see what matches,
                            // grab the span for it.

                            let modules = { ENGINE.read().unwrap().modules().clone() };

                            if let Some(found_mod) = modules.get(&path) {
                                let macros = found_mod.get_macros();

                                let found = macros.get(&ident);

                                if let Some(found) = found {
                                    let location = found.span();

                                    if let Some(range) = self.config.span_to_range(&location, &rope)
                                    {
                                        return Ok(Some(GotoDefinitionResponse::Scalar(
                                            Location::new(uri, range),
                                        )));
                                    }
                                } else {
                                    for req in found_mod.get_requires() {
                                        let path = req.path.get_path();

                                        if !path.exists() {
                                            continue;
                                        }

                                        let mut found_ident = Some(ident);

                                        if !req.idents_to_import.is_empty() {
                                            let mut found = false;
                                            for idents_to_import in &req.idents_to_import {
                                                match idents_to_import {
                                                    MaybeRenamed::Normal(expr_kind) => {
                                                        if expr_kind.atom_identifier().copied()
                                                            == Some(ident)
                                                        {
                                                            found = true;
                                                            break;
                                                        }
                                                    }
                                                    MaybeRenamed::Renamed(
                                                        expr_kind,
                                                        expr_kind1,
                                                    ) => {
                                                        if expr_kind.atom_identifier().copied()
                                                            == Some(ident)
                                                        {
                                                            found = true;
                                                            found_ident = expr_kind1
                                                                .atom_identifier()
                                                                .copied();
                                                            break;
                                                        }
                                                    }
                                                }
                                            }

                                            if !found {
                                                continue;
                                            } else {
                                                // Use the found ident
                                                let found_mod = modules.get(path.as_ref()).unwrap();
                                                let macros = found_mod.get_macros();
                                                if let Some(found) =
                                                    macros.get(&found_ident.unwrap())
                                                {
                                                    let location = found.span();

                                                    let mut locations = vec![];

                                                    self.spans_to_locations(
                                                        &ENGINE.read().unwrap(),
                                                        vec![location],
                                                        &mut locations,
                                                    );

                                                    return Ok(locations
                                                        .into_iter()
                                                        .next()
                                                        .map(GotoDefinitionResponse::Scalar));
                                                }
                                            }
                                        } else {
                                            // Just check the macros anyway
                                            let found_mod = modules.get(path.as_ref()).unwrap();
                                            let macros = found_mod.get_macros();
                                            if let Some(found) = macros.get(&ident) {
                                                let location = found.span();

                                                let mut locations = vec![];

                                                self.spans_to_locations(
                                                    &ENGINE.read().unwrap(),
                                                    vec![location],
                                                    &mut locations,
                                                );

                                                return Ok(locations
                                                    .into_iter()
                                                    .next()
                                                    .map(GotoDefinitionResponse::Scalar));
                                            }
                                        }
                                    }
                                }
                            } else {
                                eprintln!("Unable to find module: {:?}", path);
                            }
                        }
                    }
                }
            }
        }

        Ok(definition)
    }

    // Finding references:
    // Go over every file, and then iterate over each module
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let mut found_locations = Vec::new();

        let definition = async {
            let uri = params.text_document_position.text_document.uri.clone();

            let mut ast = self.ast_map.get_mut(uri.as_str())?;
            let mut rope = self.document_map.get(uri.as_str())?.clone();

            let position = params.text_document_position.position;
            let offset = self.config.position_to_offset(position, &rope)?;

            let analysis = SemanticAnalysis::new(&mut ast);

            let (syntax_object_id, information) =
                analysis.find_identifier_at_offset(offset, uri_to_source_id(&uri)?)?;

            // If this is a built in, lets just bail
            if information.builtin {
                if let Some(mut l) = self
                    .find_references_builtin(&analysis, *syntax_object_id, information)
                    .await
                {
                    found_locations.append(&mut l);
                }
            }

            // Either refers to something, or is the existing definition
            let refers_to = information.refers_to.unwrap_or_else(|| {
                eprintln!("Unable to find a refers to for this identifier");
                *syntax_object_id
            });

            {
                let mut spans = Vec::new();
                for (_, info) in analysis.analysis.identifier_info() {
                    if let Some(found_refers_to) = info.refers_to {
                        if found_refers_to == refers_to {
                            spans.push(info.span);
                        }
                    }
                }

                self.spans_to_locations(&ENGINE.read().unwrap(), spans, &mut found_locations)
            }

            // Find all the locations which refer to it

            let maybe_definition = analysis.get_identifier(refers_to)?;

            let mut identifier = None;
            let mut resulting_span = maybe_definition.span;

            let mut module_path = None;

            for expr in analysis.exprs.iter() {
                match expr {
                    ExprKind::Define(d) => {
                        if d.name_id() == Some(refers_to) {
                            identifier = d
                                .name
                                .atom_syntax_object()
                                .and_then(|x| x.ty.identifier().cloned());
                        }
                    }
                    ExprKind::Begin(b) => {
                        for expr in &b.exprs {
                            if let ExprKind::Define(d) = expr {
                                if d.name_id() == Some(refers_to) {
                                    identifier = d
                                        .name
                                        .atom_syntax_object()
                                        .and_then(|x| x.ty.identifier().cloned());
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            let mut resolver = |mut interned: InternedString,
                                name: String,
                                original|
             -> Option<()> {
                let maybe_renamed = interned;

                if let Some(original) = original {
                    if interned != original {
                        interned = original;
                    }
                }

                let mut module_prefix_path_to_check =
                    name.trim_end_matches(if maybe_renamed == interned {
                        interned.resolve()
                    } else {
                        maybe_renamed.resolve()
                    });

                resulting_span = {
                    let guard = ENGINE.read().ok()?;

                    let modules = guard.modules();

                    let module = modules
                        .values()
                        .find(|x| x.prefix() == module_prefix_path_to_check)?;

                    module_path = Some(module.name().to_owned());

                    let module_ast = module.get_ast();

                    let top_level_define = query_top_level_define(module_ast, interned.resolve())
                        .or_else(|| {
                        query_top_level_define_on_condition(
                            module_ast,
                            interned.resolve(),
                            |name, target| target.ends_with(name),
                        )
                    })?;

                    let syntax = top_level_define.name.atom_syntax_object()?;

                    identifier = syntax.ty.identifier().cloned();

                    syntax.span
                };

                Some(())
            };

            if maybe_definition.is_required_identifier {
                match analysis.resolve_required_identifier(refers_to)? {
                    RequiredIdentifierInformation::Resolved(
                        resolved,
                        mut interned,
                        name,
                        original,
                    ) => {
                        if let Some(original) = original {
                            if interned != original {
                                resolver(interned, name, Some(original))?;
                            } else {
                                resulting_span = resolved.span;

                                identifier = Some(original);
                            }
                        } else {
                            resulting_span = resolved.span;

                            identifier = Some(name.into());
                        }
                    }

                    RequiredIdentifierInformation::Unresolved(mut interned, name, original) => {
                        resolver(interned, name, original)?;
                    }
                }
            }

            let location = source_id_to_uri(resulting_span.source_id()?)?;

            if location != uri {
                let expression = ENGINE
                    .read()
                    .ok()?
                    .get_source(&resulting_span.source_id()?)?;

                rope = self
                    .document_map
                    .get(location.as_str())
                    .map(|x| x.clone())
                    .unwrap_or_else(|| Rope::from_str(&expression));

                self.document_map.insert(location.to_string(), rope.clone());
            }

            // Include the declaration and the location isn't
            if params.context.include_declaration {
                let mut definition_span = vec![resulting_span];
                self.spans_to_locations(
                    &ENGINE.read().unwrap(),
                    definition_span,
                    &mut found_locations,
                );
            }

            Some((identifier, module_path))
        }
        .await;

        // Now that we have the definition, we should also check for where this came from.
        // We'll walk through the modules, look at what they require, and find
        // the ASTs that we have to analyze to find references to this identifier.
        if let Some((Some(identifier), module_path)) = definition {
            // If we have an identifier and its from another module,
            // then we should calculate the reverse dependencies. That way we can
            // at least attempt to build a reverse index for find references.
            if let Some(module_path) = module_path {
                let mut external_module_refs =
                    self.find_references_external_module(identifier, module_path);
                found_locations.append(&mut external_module_refs);
            } else {
                let module_path = params
                    .text_document_position
                    .text_document
                    .uri
                    .clone()
                    .to_file_path();
                if let Ok(module_path) = module_path {
                    let mut external_module_refs =
                        self.find_references_external_module(identifier, module_path);
                    found_locations.append(&mut external_module_refs);
                }
            }
        }

        // TODO: Dedupe the found locations
        if !found_locations.is_empty() {
            return Ok(Some(found_locations));
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        _params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        _params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        Ok(None)
    }

    async fn inlay_hint(
        &self,
        _params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let mut filter_character = None;

        let completions = || -> Option<Vec<CompletionItem>> {
            let rope = self.document_map.get(&uri.to_string())?;
            let mut ast = self.ast_map.get_mut(&uri.to_string())?;

            let offset = self.config.position_to_offset(position, &rope)?;

            if offset > 0 {
                let previously_typed = rope.get_char(offset - 1);

                if previously_typed.is_some() && previously_typed != Some('(') {
                    if offset > 2 {
                        let prior = rope.get_char(offset - 2);

                        if prior.is_some() && prior.map(char::is_whitespace)? {
                            filter_character = previously_typed;
                        }
                    } else {
                        filter_character = previously_typed;
                    }
                }
            }

            let filter_interned_string = |interned_string: &InternedString| {
                filter_interned_string_with_char(interned_string, filter_character)
            };

            let analysis = SemanticAnalysis::new(&mut ast);

            // Finds the scoped contexts that we're currently inside of by the span
            let contexts = analysis.find_contexts_with_offset(offset, uri_to_source_id(&uri)?);

            let now = std::time::Instant::now();

            let mut completions: HashSet<String> =
                HashSet::with_capacity(contexts.len() + self.defined_globals.len());

            for context in contexts {
                match context {
                    steel::compiler::passes::analysis::SemanticInformationType::Function(info) => {
                        completions.extend(
                            info.arguments()
                                .iter()
                                .map(|x| &x.0)
                                .filter_map(filter_interned_string)
                                .chain(
                                    info.captured_vars()
                                        .iter()
                                        .map(|x| &x.0)
                                        .filter_map(filter_interned_string),
                                ),
                        );
                    }
                    steel::compiler::passes::analysis::SemanticInformationType::Let(info) => {
                        completions.extend(info.arguments.keys().filter_map(filter_interned_string))
                    }
                    _ => {}
                }
            }

            // A bit sillys
            completions.extend(
                analysis
                    .find_global_defs()
                    .into_iter()
                    .filter_map(|x| filter_interned_string(&x.0)),
            );

            completions.extend(self.defined_globals.iter().filter_map(|x| {
                if let Some(c) = filter_character {
                    if !x.starts_with(c) {
                        return None;
                    }
                }

                Some(x.clone())
            }));

            // TODO: Build completions from macros that have been introduced into this scope
            completions.extend(
                ENGINE
                    .read()
                    .ok()?
                    .in_scope_macros()
                    .keys()
                    .filter_map(|x| {
                        if let Some(c) = filter_character {
                            if !x.resolve().starts_with(c) {
                                return None;
                            }
                        }

                        Some(x.resolve().to_string())
                    })
                    .collect::<Vec<_>>(),
            );

            completions.extend(self.globals_set.iter().map(|x| x.resolve().to_owned()));

            let mut ret = Vec::with_capacity(completions.len());
            for var in completions {
                ret.push(CompletionItem {
                    label: var.clone(),
                    insert_text: Some(var.clone()),
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: Some(var),
                    ..Default::default()
                });
            }

            // log::debug!("Time to calculate completions: {:?}", now.elapsed());

            Some(ret)
        }();

        Ok(completions.map(CompletionResponse::Array))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;

        let Some((identifier, range)) = || -> Option<_> {
            let rope = self.document_map.get(uri.as_str())?;
            let mut ast = self.lowered_ast_map.get_mut(uri.as_str())?;

            let offset = self.config.position_to_offset(position, &rope)?;
            let semantic = SemanticAnalysis::new(&mut ast);
            let (_, identifier) =
                semantic.find_identifier_at_offset(offset, uri_to_source_id(&uri)?)?;

            let range = self.config.span_to_range(&identifier.span, &rope)?;
            Some((identifier.clone(), range))
        }() else {
            return Ok(None);
        };

        if identifier.builtin {
            return Err(jsonrpc::Error::invalid_params("cannot rename builtin"));
        }

        if !matches!(
            identifier.kind,
            IdentifierStatus::Local
                | IdentifierStatus::LetVar
                | IdentifierStatus::LocallyDefinedFunction
        ) {
            return Err(jsonrpc::Error::invalid_params(format!(
                "cannot rename symbol of kind {:?}",
                identifier.kind
            )));
        }

        Ok(Some(PrepareRenameResponse::Range(range)))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let changes = || -> Option<Vec<TextEdit>> {
            let rope = self.document_map.get(uri.as_str())?;
            let mut ast = self.lowered_ast_map.get_mut(uri.as_str())?;

            let offset = self.config.position_to_offset(position, &rope)?;
            let semantic = SemanticAnalysis::new(&mut ast);
            let (syntax_object_id, semantic_information) =
                semantic.find_identifier_at_offset(offset, uri_to_source_id(&uri)?)?;

            // it should probaby not be possible to rename builtins ...
            if semantic_information.builtin {
                return None;
            }

            let syntax_object_id = semantic.analysis.resolve_reference(*syntax_object_id);
            let semantic_information = semantic.get_identifier(syntax_object_id).unwrap();

            // it might make sense to be able to rename other things as well,
            // but i think this is at least good start
            if !matches!(
                semantic_information.kind,
                IdentifierStatus::Local
                    | IdentifierStatus::LetVar
                    | IdentifierStatus::LocallyDefinedFunction,
            ) {
                return None;
            }

            let identifier_info = semantic.analysis.identifier_info();
            let identifiers = identifier_info
                .iter()
                .filter(|(&id, _)| semantic.analysis.resolve_reference(id) == syntax_object_id)
                .filter(|(_, info)| info.kind == semantic_information.kind)
                .map(|(_, information)| (information.span.start, information.span.end))
                .filter_map(|(start, end)| {
                    self.config
                        .span_to_range(&Span::new(start, end, None), &rope)
                })
                .map(|range| TextEdit::new(range, params.new_name.clone()))
                .collect::<Vec<_>>();

            Some(identifiers)
        }();

        let Some(changes) = changes else {
            return Ok(None);
        };

        let changes = HashMap::from_iter([(uri, changes)]);
        Ok(Some(WorkspaceEdit::new(changes)))
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}

fn filter_interned_string_with_char(
    interned: &InternedString,
    filter_char: Option<char>,
) -> Option<String> {
    let resolved = interned.resolve();

    if let Some(c) = filter_char {
        if !resolved.starts_with(c) {
            return None;
        }
    }

    if !resolved.starts_with("#")
        && !resolved.starts_with("%")
        && !resolved.starts_with(MANGLER_PREFIX)
        && !resolved.starts_with("mangler#%")
        && !resolved.starts_with("!!dummy-rest")
        && !resolved.starts_with(MODULE_PREFIX)
        // TODO: This should just be prefixed with # as well, to make it
        // so that it doesn't show up here at all
        && !resolved.ends_with("__doc__")
    {
        Some(resolved.to_string())
    } else {
        None
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    async fn find_references_builtin(
        &self,
        analysis: &SemanticAnalysis<'_>,
        syntax_object_id: SyntaxObjectId,
        info: &SemanticInformation,
    ) -> Option<Vec<Location>> {
        let mut syntax_object_id_to_interned_string = HashMap::new();
        syntax_object_id_to_interned_string.insert(syntax_object_id, None);
        analysis.syntax_object_ids_to_identifiers(&mut syntax_object_id_to_interned_string);

        let identifier = syntax_object_id_to_interned_string
            .get(&syntax_object_id)?
            .clone()?;

        // Go through the modules and see which asts are there?
        // Are built ins renamed?
        // let guard = ENGINE.read().unwrap();
        let mut spans = Vec::new();

        let modules = { ENGINE.read().unwrap().modules().clone() };

        {
            for (key, module) in modules.iter() {
                // Calculate the built ins that we have:
                let ast = module.get_ast();

                eprintln!("Reading module: {:?}", key);

                let top_level_define = query_top_level_define(ast, identifier.resolve());
                if let Some(top_level_define) = top_level_define {
                    let id = top_level_define
                        .name
                        .atom_syntax_object()
                        .unwrap()
                        .syntax_object_id;

                    let now = std::time::Instant::now();

                    // This can be cached - basically all built ins
                    // don't need to result in a re analysis pass
                    // assuming the modules hasn't changed
                    let mut analysis = Analysis::default();
                    analysis.run(ast);

                    eprintln!("Analysis time: {:?}", now.elapsed());

                    for (_, info) in analysis.identifier_info() {
                        if let Some(refers_to) = info.refers_to {
                            if refers_to == id {
                                spans.push(info.span);
                            }
                        }
                    }
                }
            }
        }

        let mut locations: Vec<Location> = Vec::new();

        let guard = ENGINE.read().unwrap();
        self.spans_to_locations(&guard, spans, &mut locations);

        Some(locations)
    }

    fn find_references_external_module(
        &self,
        identifier: InternedString,
        module_path: PathBuf,
    ) -> Vec<Location> {
        let guard = ENGINE.read().unwrap();
        let all_modules = guard.modules();
        let mut should_index = Vec::new();

        for (p, module) in all_modules.iter() {
            if &module_path == p {
                continue;
            }

            let require_objects = module.get_requires();

            for req in require_objects {
                if req.path.get_path().as_path() == module_path {
                    // Note: Once the logging is fixed we can remove the clone here
                    should_index.push((module, req));
                }
            }
        }

        let mut spans = Vec::new();

        for (module, req) in should_index {
            let ast = module.get_ast();
            // Build the identifier based on what we actually want.
            // Could be renamed, or could be prefixed. In that case
            // then we want to find the prefixed reference.

            let mut identifier = identifier;

            // There are only specific idents to import, so we'll want to
            // not index this file if this identifier is not brought in.
            if !req.idents_to_import.is_empty() {
                let mut found = false;
                for idents_to_import in &req.idents_to_import {
                    match idents_to_import {
                        MaybeRenamed::Normal(expr_kind) => {
                            if expr_kind.atom_identifier().copied() == Some(identifier) {
                                found = true;
                                break;
                            }
                        }
                        MaybeRenamed::Renamed(expr_kind, expr_kind1) => {
                            if expr_kind.atom_identifier().copied() == Some(identifier) {
                                found = true;
                                identifier = *expr_kind1.atom_identifier().unwrap();
                                break;
                            }
                        }
                    }
                }

                if !found {
                    continue;
                }
            }

            if let Some(prefix) = &req.prefix {
                identifier = (prefix.to_string() + identifier.resolve()).into();
            }

            let ident = ExprKind::atom(identifier);
            let id = ident.atom_syntax_object().unwrap().syntax_object_id;
            let top_level = ExprKind::Define(Box::new(Define {
                name: ident,
                body: ExprKind::empty(),
                location: steel_parser::parser::SyntaxObject::default(
                    steel_parser::tokens::TokenType::Define,
                ),
            }));

            // TODO: Can we cache this analysis? Probably keep an analysis per identifier?
            // Also, does the module level expose when this was last updated?
            let mut analysis = Analysis::default();
            analysis.run(&[top_level]);
            analysis.run_with_scope(ast, false);

            for (_, info) in analysis.identifier_info() {
                if let Some(refers_to) = info.refers_to {
                    if refers_to == id {
                        spans.push(info.span);
                    }
                }
            }
        }

        let mut locations: Vec<Location> = Vec::new();

        self.spans_to_locations(&guard, spans, &mut locations);

        locations
    }

    fn spans_to_locations(&self, guard: &Engine, spans: Vec<Span>, locations: &mut Vec<Location>) {
        for span in spans {
            if let Some(source) = span.source_id().and_then(|x| guard.get_source(&x)) {
                if let Some(path) = span
                    .source_id()
                    .and_then(|x| guard.get_path_for_source_id(&x))
                {
                    if let Ok(url) = Url::from_file_path(&path) {
                        // if this is present in the VFS, then we'll render it.
                        // Otherwise, we'll check if the file exists from the root of this
                        // directory, and then we'll show that too, in order to avoid
                        // needing a file watcher (for now). Compilation should check
                        // time stamps and update accordingly.
                        // TODO: Lift this up to where we actually index the files instead
                        if self.vfs.get(&url).is_none() && !path.starts_with(&self.root) {
                            continue;
                        }

                        let url_str = url.to_string();

                        let rope = if let Some(rope) = self.document_map.get(&url_str) {
                            rope.clone()
                        } else {
                            let rope = ropey::Rope::from(source.to_string());
                            self.document_map.insert(url_str, rope.clone());
                            rope
                        };

                        if let Some(range) = self.config.span_to_range(&span, &rope) {
                            locations.push(Location::new(url, range));
                        }
                    } else {
                        eprintln!("Unable to convert path to url: {:?}", path);
                    }
                }
            }
        }
    }
}

fn find_associated_define(expr: &Define, ident: InternedString) -> Option<SyntaxObjectId> {
    if let Some(found) = require_defitinion_to_original_symbol(expr) {
        if ident == found {
            return expr.name.atom_syntax_object().map(|x| x.syntax_object_id);
        }
    }

    None
}

fn fetch_stdlib_doc(ident: &str) -> Option<SteelString> {
    let mut guard = ENGINE.write().unwrap();

    // Fetch the function pointer table, call the function pointer table function
    let ptr_to_lookup = guard.extract_value(ident);

    if let Ok(ptr_to_lookup) = ptr_to_lookup {
        let fn_ptr_doc = guard.extract_value("#%function-ptr-table").unwrap();
        let inner = steel::LambdaMetadataTable::as_ref(&fn_ptr_doc).ok()?;
        return steel::LambdaMetadataTable::get(&inner, ptr_to_lookup);
    }

    None
}

impl Backend {
    async fn hover_impl(&self, params: HoverParams) -> Option<Hover> {
        let uri = params.text_document_position_params.text_document.uri;

        let mut ast = self.ast_map.get_mut(uri.as_str())?;
        let rope = self.document_map.get(uri.as_str())?;

        let position = params.text_document_position_params.position;
        let offset = self.config.position_to_offset(position, &rope)?;

        let analysis = SemanticAnalysis::new(&mut ast);

        let (syntax_object_id, information) =
            analysis.find_identifier_at_offset(offset, uri_to_source_id(&uri)?)?;

        let mut syntax_object_id_to_interned_string = HashMap::new();
        syntax_object_id_to_interned_string.insert(*syntax_object_id, None);

        // If this is a builtin, reference the engine's internal documentation
        // Note: This does not handle resolving the identifier if it has been brought into scope
        // with some kind of prefix. This should be relatively easy to resolve - the analysis
        // can most likely identify if this is a builtin by checking what it was bought into scope
        // with. For example, it would be brought into scope with:
        // (define foo.list (%proto-hash-get% ...)
        //
        // The ability to then just check what the original identifier was can help us resolve
        // the binding, by just checking against the interned string stored in the proto hash get.
        if information.builtin && information.refers_to.is_none() {
            analysis.syntax_object_ids_to_identifiers(&mut syntax_object_id_to_interned_string);

            let name = syntax_object_id_to_interned_string.get(syntax_object_id)?;

            let doc = ENGINE
                .read()
                .ok()?
                .builtin_modules()
                .get_doc((*name)?.resolve())?;

            return Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(doc)),
                range: None,
            });
        }

        // If we can't figure out what this refers to, it probably refers
        // to some standard library function
        if information.refers_to.is_none() {
            // Resolve what we've found?
            analysis.syntax_object_ids_to_identifiers(&mut syntax_object_id_to_interned_string);

            // In the event... we have found something that more or less matched the standard
            // library values, we should just include that here? I don't think this is going to
            // work in general?
            let base_identifier = syntax_object_id_to_interned_string.get(syntax_object_id);
            if let Some(Some(base_identifier)) = base_identifier {
                let definition = fetch_stdlib_doc(base_identifier.resolve());
                if let Some(definition) = definition {
                    return Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(
                            definition.as_str().to_owned(),
                        )),
                        range: None,
                    });
                }
            }
        }

        // Refers to something - keep that around as well
        let refers_to = information.refers_to?;

        // See if we can find this as well.
        syntax_object_id_to_interned_string.insert(refers_to, None);

        let maybe_definition = analysis.get_identifier(refers_to)?;

        if maybe_definition.is_required_identifier {
            let resolve_required = analysis.resolve_required_identifier(refers_to);

            match resolve_required? {
                RequiredIdentifierInformation::Resolved(_, mut interned, name, original) => {
                    // Find the doc associated with this span then
                    analysis
                        .syntax_object_ids_to_identifiers(&mut syntax_object_id_to_interned_string);

                    // Guaranteed to be here given that we've resolve it above
                    let definition_name = syntax_object_id_to_interned_string
                        .get(&refers_to)
                        .clone()?
                        .clone()?;

                    // Memoize a lot of these lookups if possible, or at least share the memory;
                    let doc_suffix = definition_name.resolve().to_string() + "__doc__";

                    let define = analysis.query_top_level_define(&doc_suffix).or_else(|| {
                        query_top_level_define_on_condition(
                            &analysis.exprs,
                            doc_suffix,
                            |name, target| target.ends_with(name),
                        )
                    });

                    // If there isn't a doc, lets just snag the function to show?
                    match define {
                        Some(define) => {
                            let definition = define.body.to_string_literal()?;

                            let define_ast = if name.starts_with(MANGLER_PREFIX) {
                                let module_prefix_path_to_check = name.trim_end_matches(
                                    original
                                        .as_ref()
                                        .map(|x| x.resolve())
                                        .unwrap_or(interned.resolve()),
                                );

                                query_for_top_level_define(
                                    &module_prefix_path_to_check,
                                    original.unwrap_or(interned).resolve(),
                                )
                            } else {
                                None
                            };

                            if let Some(mut define_ast) = define_ast {
                                // Set up the pretty printed AST as well
                                let mut ast = Vec::new();

                                if let ExprKind::LambdaFunction(l) = &mut define_ast.body {
                                    // Format the args and body:
                                    l.body = ExprKind::ident("...");
                                    define_ast.to_doc().render(60, &mut ast).unwrap();
                                }

                                // This _should_ be the resolved documentation. And then we just extract the
                                // string from the definition.
                                // top_level_define_ast.to_doc().render(60, &mut ast).unwrap();

                                // Include the top level ast string if it exists
                                let ast_string = format!(
                                    r#"{}
```scheme
{}
```"#,
                                    definition,
                                    String::from_utf8(ast).unwrap()
                                );

                                return Some(Hover {
                                    contents: HoverContents::Scalar(MarkedString::String(
                                        ast_string,
                                    )),
                                    range: None,
                                });
                            } else {
                                return Some(Hover {
                                    contents: HoverContents::Scalar(MarkedString::String(
                                        definition.clone(),
                                    )),
                                    range: None,
                                });
                            }
                        }
                        None => return self.unresolved_hover(interned, original, name).await,
                    }
                }

                RequiredIdentifierInformation::Unresolved(mut interned, name, original) => {
                    return self.unresolved_hover(interned, original, name).await;
                }
            }
        }

        // Resolve what we've found?
        analysis.syntax_object_ids_to_identifiers(&mut syntax_object_id_to_interned_string);

        let definition_name = syntax_object_id_to_interned_string
            .get(&refers_to)
            .clone()?
            .clone()?;

        // Memoize a lot of these lookups if possible, or at least share the memory;
        let doc_suffix = definition_name.resolve().to_string() + "__doc__";

        if let Some(define) = analysis.query_top_level_define(&doc_suffix) {
            // This _should_ be the resolved documentation. And then we just extract the
            // string from the definition.
            let definition = define.body.to_string_literal()?;

            Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(definition.clone())),
                range: None,
            })
        } else {
            // Query modules again
            let doc = ENGINE
                .read()
                .ok()?
                .builtin_modules()
                .get_doc(definition_name.resolve())?;

            return Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(doc)),
                range: None,
            });
        }
    }

    // Just do incremental?
    async fn on_change(&self, params: TextDocumentItem) {
        let now = std::time::Instant::now();

        // Ensure this document is marked as open from the perspective of the LSP
        let rope = ropey::Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());

        self.vfs
            .insert(params.uri.clone(), FileState { opened: true });

        let expression = params.text;

        let diagnostics = {
            let raw_program = Engine::emit_ast(&expression);

            let program = {
                let mut guard = ENGINE.write().unwrap();

                // TODO: Reuse this!a
                let macro_env_before: HashSet<InternedString> =
                    guard.in_scope_macros().keys().copied().collect();

                // TODO: Add span to the macro definition!
                let mut introduced_macros: HashMap<InternedString, SteelMacro> = HashMap::new();

                let now = std::time::Instant::now();
                let expressions = guard.emit_expanded_ast_without_optimizations(
                    &expression,
                    params.uri.to_file_path().ok(),
                );
                eprintln!("on change time: {:?}", now.elapsed());

                guard.in_scope_macros_mut().retain(|key, value| {
                    if macro_env_before.contains(key) {
                        return true;
                    } else {
                        // FIXME: Try to avoid this clone!
                        introduced_macros.insert(*key, value.clone());
                        false
                    }
                });

                expressions
            };

            let mut ast = match program {
                Ok(ast) => ast,
                Err(e) => {
                    // drop(engine_guard);

                    self.client
                        .log_message(MessageType::INFO, e.to_string())
                        .await;

                    if let Some(span) = e.span() {
                        let diagnostics = || {
                            let range = self.config.span_to_range(&span, &rope)?;
                            let diag =
                                create_diagnostic(range, DiagnosticSeverity::ERROR, e.to_string());

                            Some(vec![diag])
                        };

                        if let Some(diagnostics) = diagnostics() {
                            self.client
                                .publish_diagnostics(
                                    params.uri.clone(),
                                    diagnostics,
                                    Some(params.version),
                                )
                                .await;
                        }
                    }

                    return;
                }
            };

            let id = uri_to_source_id(&params.uri);

            let analysis = SemanticAnalysis::new(&mut ast);

            let diagnostics = {
                let mut context = DiagnosticContext {
                    engine: &ENGINE.read().unwrap(),
                    analysis: &analysis,
                    uri: &params.uri,
                    source_id: id,
                    rope: rope.clone(),
                    config: &self.config,
                    globals_set: &self.globals_set,
                    ignore_set: &self.ignore_set,
                };

                let mut free_identifiers_and_unused =
                    FreeIdentifiersAndUnusedIdentifiers.diagnose(&mut context);

                let mut static_arity_checking = StaticArityChecker.diagnose(&mut context);

                free_identifiers_and_unused.append(&mut static_arity_checking);

                let now = std::time::Instant::now();

                // TODO: Enable this once the syntax object let conversion is implemented
                let mut user_defined_lints =
                    LINT_ENGINE
                        .write()
                        .unwrap()
                        .diagnostics(&self.config, &rope, &analysis.exprs);

                // log::debug!("Lints found: {:#?}", user_defined_lints);

                free_identifiers_and_unused.append(&mut user_defined_lints);

                // log::debug!("User defined lints time taken: {:?}", now.elapsed());

                // All the diagnostics total
                free_identifiers_and_unused
            };

            self.ast_map.insert(params.uri.to_string(), ast);

            if let Ok(raw_ast) = raw_program {
                self.raw_ast_map.insert(params.uri.to_string(), raw_ast);
            }

            // the ast that is parsed for the `ast_map` is parsed with the `.without_lowering`
            // argument to the `Parser`. but for things like `rename` (and `prepare_rename`),
            // i need an ast that is parsed without that argument, so instead of having to recalculate it on-demand,
            // just do it here, once.
            if let Ok(lowered_ast) =
                Parser::new(&expression, id).collect::<std::result::Result<Vec<_>, _>>()
            {
                self.lowered_ast_map
                    .insert(params.uri.to_string(), lowered_ast);
            }

            diagnostics
        };

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;

        // log::debug!("On change time taken: {:?}", now.elapsed());
    }
}

impl Backend {
    async fn unresolved_hover(
        &self,
        mut interned: InternedString,
        original: Option<InternedString>,
        name: String,
    ) -> Option<Hover> {
        let maybe_renamed = interned;
        if let Some(original) = original {
            if interned != original {
                interned = original;
            }
        }
        let mut module_prefix_path_to_check = name.trim_end_matches(if maybe_renamed == interned {
            interned.resolve()
        } else {
            maybe_renamed.resolve()
        });

        // TODO: Don't _just_ query the __doc__, also look for the definition itself,
        // and try to render that
        let interned_doc = interned.resolve().to_string() + "__doc__";

        let mut top_level_define_ast =
            query_for_top_level_define(module_prefix_path_to_check, interned.resolve())?;

        let top_level_define =
            query_for_top_level_define(module_prefix_path_to_check, &interned_doc);

        match top_level_define {
            Some(top_level_define) => {
                // Set up the pretty printed AST as well
                let mut ast = Vec::new();

                if let ExprKind::LambdaFunction(l) = &mut top_level_define_ast.body {
                    // Format the args and body:
                    l.body = ExprKind::ident("...");
                    top_level_define_ast.to_doc().render(60, &mut ast).unwrap();
                }

                let definition_body = top_level_define.body.to_string_literal()?;

                let definition = format!(
                    r#"{}
```scheme
{}
```"#,
                    definition_body,
                    String::from_utf8(ast).unwrap()
                );

                return Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(definition.clone())),
                    range: None,
                });
            }
            None => {
                // Set up the pretty printed AST as well
                let mut ast = Vec::new();

                if let ExprKind::LambdaFunction(l) = &mut top_level_define_ast.body {
                    // Format the args and body:
                    l.body = ExprKind::ident("...");
                    top_level_define_ast.to_doc().render(60, &mut ast).unwrap();
                }

                // This _should_ be the resolved documentation. And then we just extract the
                // string from the definition.
                // top_level_define_ast.to_doc().render(60, &mut ast).unwrap();

                // Include the top level ast string if it exists
                let ast_string = format!(
                    r#"```scheme
{}
```"#,
                    String::from_utf8(ast).unwrap()
                );

                // let definition = top_level_define_ast.to_string();
                return Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(ast_string.clone())),
                    range: None,
                });
            }
        }
    }
}
fn query_for_top_level_define(
    module_prefix_path_to_check: &str,
    interned: &str,
) -> Option<steel::parser::ast::Define> {
    let guard = ENGINE.read().ok()?;
    let modules = guard.modules();
    let module = modules
        .values()
        .find(|x| x.prefix() == module_prefix_path_to_check)?;
    let module_ast = module.get_ast();
    let top_level_define = query_top_level_define(module_ast, &interned)
        .or_else(|| {
            query_top_level_define_on_condition(module_ast, &interned, |name, target| {
                target.ends_with(name)
            })
        })
        .cloned();
    top_level_define
}

fn uri_to_source_id(uri: &Url) -> Option<steel::parser::parser::SourceId> {
    let id = ENGINE
        .read()
        .ok()?
        .get_source_id(&uri.to_file_path().unwrap());
    id
}

fn source_id_to_uri(source_id: SourceId) -> Option<Url> {
    let path = ENGINE.read().ok()?.get_path_for_source_id(&source_id)?;

    Some(Url::from_file_path(path).ok()?)
}

pub struct ExternalModuleResolver {
    modules: HashMap<String, BuiltInModule>,
}

impl ExternalModuleResolver {
    pub fn new(
        engine: &mut Engine,
        directory: PathBuf,
    ) -> std::result::Result<Self, Box<dyn Error>> {
        let mut modules = HashMap::new();

        for file in std::fs::read_dir(&directory)? {
            let file = file?;

            if file.path().is_file() {
                let contents = std::fs::read_to_string(file.path())?;

                let result = engine.compile_and_run_raw_program(contents)?;

                for value in result {
                    if let Ok(module) = BuiltInModule::from_steelval(&value) {
                        modules.insert(module.name().to_string(), module);
                    }
                }
            }
        }

        // Check globals now - for anything that isn't returned directly
        for global in engine.globals().iter() {
            if let Ok(module) = engine.extract::<BuiltInModule>(global.resolve()) {
                if !modules.contains_key(module.name().as_ref()) {
                    modules.insert(module.name().to_string(), module);
                }
            }
        }

        Ok(ExternalModuleResolver { modules })
    }
}

impl steel::steel_vm::engine::ModuleResolver for ExternalModuleResolver {
    fn resolve(&self, module: &str) -> Option<BuiltInModule> {
        self.modules.get(module).cloned()
    }
    fn names(&self) -> Vec<String> {
        self.modules.keys().cloned().collect()
    }
}

// TODO: Move these to the backend - we don't need them to be global like this now that
// we're thread safe
pub static ENGINE: Lazy<RwLock<Engine>> = Lazy::new(|| RwLock::new(Engine::new()));
pub static LINT_ENGINE: Lazy<RwLock<UserDefinedLintEngine>> =
    Lazy::new(|| RwLock::new(configure_lints().unwrap()));
pub static DIAGNOSTICS: Lazy<RwLock<Vec<SteelDiagnostic>>> = Lazy::new(|| RwLock::new(Vec::new()));

// At one time, call the lints, collecting the diagnostics each time.
pub struct UserDefinedLintEngine {
    engine: Engine,
    lints: Arc<RwLock<HashSet<String>>>,
}

impl UserDefinedLintEngine {
    pub fn diagnostics(
        &mut self,
        config: &Config,
        rope: &Rope,
        ast: &[ExprKind],
    ) -> Vec<Diagnostic> {
        let lints = self.lints.read().unwrap();

        let syntax_objects: Vec<_> = ast
            .iter()
            .map(SyntaxObjectFromExprKindRef::try_from_expr_kind_ref)
            .collect();

        for lint in lints.iter() {
            for object in &syntax_objects {
                if let Ok(o) = object.clone() {
                    // log::debug!("calling: {} with {}", lint, o);

                    let res = self.engine.call_function_by_name_with_args(lint, vec![o]);

                    // log::debug!("{:?}", res);
                }
            }
        }

        DIAGNOSTICS
            .write()
            .unwrap()
            .drain(..)
            .filter_map(|d| {
                let range = config.span_to_range(&d.span, rope)?;
                let diagnostic = create_diagnostic(
                    range,
                    DiagnosticSeverity::INFORMATION,
                    d.message.to_string(),
                );
                Some(diagnostic)
            })
            .collect()
    }
}

#[derive(Clone)]
pub struct SteelDiagnostic {
    span: Span,
    message: SteelString,
}

fn configure_lints() -> std::result::Result<UserDefinedLintEngine, Box<dyn Error>> {
    let mut engine = Engine::new();

    let mut diagnostics = BuiltInModule::new("lsp/diagnostics");
    let lints = Arc::new(RwLock::new(HashSet::new()));

    diagnostics.register_fn("suggest", move |span: Span, message: SteelString| {
        // log::debug!("Adding suggestion at: {:?} - {:?}", span, message);
        DIAGNOSTICS
            .write()
            .unwrap()
            .push(SteelDiagnostic { span, message });
    });

    let engine_lints = lints.clone();
    diagnostics.register_fn("#%register-lint", move |name: String| {
        engine_lints.write().unwrap().insert(name);
    });

    let mut directory = lsp_home();
    directory.push("lints");

    engine.register_module(diagnostics);

    if let Ok(directory) = std::fs::read_dir(directory) {
        for file in directory {
            let file = file?;

            if file.path().is_file() {
                let contents = std::fs::read_to_string(file.path())?;

                engine.compile_and_run_raw_program(contents)?;
            }
        }
    } else {
        log::info!("Missing lints directory in $STEEL_LSP_HOME");
    }

    Ok(UserDefinedLintEngine { engine, lints })
}

impl Config {
    pub fn position_to_offset(&self, position: Position, rope: &Rope) -> Option<usize> {
        let line_start = rope.try_line_to_byte(position.line as usize).ok()?;

        let character = match self.encoding.load() {
            OffsetEncoding::Utf8 => position.character as usize,
            OffsetEncoding::Utf16 => {
                let line = rope.get_line(position.line as usize)?;
                let line_char_offset = line.utf16_cu_to_char(position.character as usize);
                line.char_to_byte(line_char_offset)
            }
            OffsetEncoding::Utf32 => {
                let line = rope.get_line(position.line as usize)?;
                line.char_to_byte(position.character as usize)
            }
        };

        Some(line_start + character)
    }

    pub fn offset_to_position(&self, offset: usize, rope: &Rope) -> Option<Position> {
        let line_idx = rope.try_byte_to_line(offset).ok()?;

        let line_byte_offset = rope.line_to_byte(line_idx);
        let column_byte_offset = offset - line_byte_offset;

        let column = match self.encoding.load() {
            OffsetEncoding::Utf8 => column_byte_offset,
            OffsetEncoding::Utf16 => {
                let line = rope.line(line_idx);
                let column_char_offset = line.byte_to_char(column_byte_offset);
                line.char_to_utf16_cu(column_char_offset)
            }
            OffsetEncoding::Utf32 => {
                let line = rope.line(line_idx);
                line.byte_to_char(column_byte_offset)
            }
        };

        Some(Position::new(line_idx as u32, column as u32))
    }

    pub fn span_to_range(&self, span: &Span, rope: &Rope) -> Option<Range> {
        let start = self.offset_to_position(span.start as usize, rope)?;
        let end = self.offset_to_position(span.end as usize, rope)?;
        Some(Range::new(start, end))
    }
}
