#![allow(unused)]

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    error::Error,
    path::PathBuf,
    sync::{Arc, Mutex, RwLock},
};

use dashmap::{DashMap, DashSet};

use once_cell::sync::Lazy;
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use steel::{
    compiler::{
        modules::{steel_home, MANGLER_PREFIX, MODULE_PREFIX},
        passes::analysis::{
            query_top_level_define, query_top_level_define_on_condition, IdentifierStatus,
            RequiredIdentifierInformation, SemanticAnalysis,
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
use steel_parser::ast::ToDoc;
use tower_lsp::jsonrpc::{self, Result};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use tower_lsp::lsp_types::SemanticTokenType;

use crate::diagnostics::{
    DiagnosticContext, DiagnosticGenerator, FreeIdentifiersAndUnusedIdentifiers, StaticArityChecker,
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

pub struct Backend {
    pub client: Client,
    pub ast_map: DashMap<String, Vec<ExprKind>>,
    pub lowered_ast_map: DashMap<String, Vec<ExprKind>>,
    pub document_map: DashMap<String, Rope>,
    // TODO: This needs to hold macros to help with resolving definitions
    pub _macro_map: DashMap<String, HashMap<InternedString, SteelMacro>>,
    pub ignore_set: Arc<DashSet<InternedString>>,
    pub globals_set: Arc<DashSet<InternedString>>,
    pub defined_globals: DashSet<String>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
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

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        // TODO: In order for this to work, we'll have to both
        // expose a span -> URI function, as well as figure out how to
        // decide if a definition refers to an import. I think deciding
        // if something is a module import should be like:
        let definition = async {
            let uri = params.text_document_position_params.text_document.uri;
            let mut ast = self.ast_map.get_mut(uri.as_str())?;
            let mut rope = self.document_map.get(uri.as_str())?.clone();

            let position = params.text_document_position_params.position;
            let offset = position_to_offset(position, &rope)?;

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

            // log::debug!("Location: {:?}", location);
            // log::debug!("Rope length: {:?}", rope.len_chars());
            // log::debug!("span: {:?}", maybe_definition.span);

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

            // log::debug!("Location: {:?}", location);
            // log::debug!("Rope length: {:?}", rope.len_chars());
            // log::debug!("span: {:?}", maybe_definition.span);

            let start_position = offset_to_position(resulting_span.start, &rope)?;
            let end_position = offset_to_position(resulting_span.end, &rope)?;

            let range = Range::new(start_position, end_position);

            // log::debug!("{:?}", range);

            Some(GotoDefinitionResponse::Scalar(Location::new(
                location, range,
            )))
        }
        .await;
        Ok(definition)
    }

    async fn references(&self, _params: ReferenceParams) -> Result<Option<Vec<Location>>> {
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

            let offset = position_to_offset(position, &rope)?;

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

            let offset = position_to_offset(position, &rope)?;
            let semantic = SemanticAnalysis::new(&mut ast);
            let (_, identifier) =
                semantic.find_identifier_at_offset(offset, uri_to_source_id(&uri)?)?;

            let range = Range::new(
                offset_to_position(identifier.span.start, &rope)?,
                offset_to_position(identifier.span.end, &rope)?,
            );

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

            let offset = position_to_offset(position, &rope)?;
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
                    Some(Range::new(
                        offset_to_position(start, &rope)?,
                        offset_to_position(end, &rope)?,
                    ))
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
        let offset = position_to_offset(position, &rope)?;

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
        if information.builtin {
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

        let rope = ropey::Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());

        let expression = params.text;

        let diagnostics = {
            let program = {
                let mut guard = ENGINE.write().unwrap();

                // TODO: Reuse this!a
                let macro_env_before: HashSet<InternedString> =
                    guard.in_scope_macros().keys().copied().collect();

                // TODO: Add span to the macro definition!
                let mut introduced_macros: HashMap<InternedString, SteelMacro> = HashMap::new();

                let expressions = guard.emit_expanded_ast_without_optimizations(
                    &expression,
                    params.uri.to_file_path().ok(),
                );

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
                            let start_position = offset_to_position(span.start, &rope)?;
                            let end_position = offset_to_position(span.end, &rope)?;

                            Some(vec![Diagnostic::new_simple(
                                Range::new(start_position, end_position),
                                e.to_string(),
                            )])
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
                    globals_set: &self.globals_set,
                    ignore_set: &self.ignore_set,
                };

                let mut free_identifiers_and_unused =
                    FreeIdentifiersAndUnusedIdentifiers.diagnose(&mut context);

                let mut static_arity_checking = StaticArityChecker.diagnose(&mut context);

                free_identifiers_and_unused.append(&mut static_arity_checking);

                let now = std::time::Instant::now();

                // TODO: Enable this once the syntax object let conversion is implemented
                let mut user_defined_lints = LINT_ENGINE
                    .write()
                    .unwrap()
                    .diagnostics(&rope, &analysis.exprs);

                // log::debug!("Lints found: {:#?}", user_defined_lints);

                free_identifiers_and_unused.append(&mut user_defined_lints);

                // log::debug!("User defined lints time taken: {:?}", now.elapsed());

                // All the diagnostics total
                free_identifiers_and_unused
            };

            self.ast_map.insert(params.uri.to_string(), ast);

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

pub fn make_error(mut diagnostic: Diagnostic) -> Diagnostic {
    diagnostic.severity = Some(DiagnosticSeverity::ERROR);
    diagnostic
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
    pub fn diagnostics(&mut self, rope: &Rope, ast: &[ExprKind]) -> Vec<Diagnostic> {
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
                let start_position = offset_to_position(d.span.start, &rope)?;
                let end_position = offset_to_position(d.span.end, &rope)?;

                Some(Diagnostic::new_simple(
                    Range::new(start_position, end_position),
                    d.message.to_string(),
                ))
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

pub fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_start = rope.try_line_to_byte(position.line as usize).ok()?;
    let line = rope.get_line(position.line as usize)?;

    // lsp positions are in utf16 code units by default
    let line_char_offset = line.utf16_cu_to_char(position.character as usize);
    let line_byte_offset = line.char_to_byte(line_char_offset);

    Some(line_start + line_byte_offset)
}

pub fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line_idx = rope.try_byte_to_line(offset).ok()?;
    let line = rope.line(line_idx);

    let line_byte_offset = rope.line_to_byte(line_idx);
    let column_byte_offset = offset - line_byte_offset;

    // lsp by default expects offsets in utf16 encoding
    let column_char_offset = line.byte_to_char(column_byte_offset);
    let column = line.char_to_utf16_cu(column_char_offset);

    Some(Position::new(line_idx as u32, column as u32))
}
