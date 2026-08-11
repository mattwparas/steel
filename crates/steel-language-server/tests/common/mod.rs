#![allow(dead_code)]

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use dashmap::{DashMap, DashSet};
use futures::StreamExt;
use serde::de::DeserializeOwned;
use steel::compiler::modules::MANGLER_PREFIX;
use steel_language_server::backend::{Backend, Config, ENGINE};
use tower::{Service, ServiceExt};
use tower_lsp::jsonrpc::Request;
use tower_lsp::lsp_types::*;
use tower_lsp::LspService;

static SERIAL: Mutex<()> = Mutex::new(());

fn isolate_lsp_home() {
    static LSP_HOME: OnceLock<PathBuf> = OnceLock::new();

    LSP_HOME.get_or_init(|| {
        let dir = std::env::temp_dir().join("steel-lsp-test-home");
        std::fs::create_dir_all(&dir).expect("unable to create the test lsp home");
        std::env::set_var("STEEL_LSP_HOME", &dir);
        dir
    });
}

pub struct TestServer {
    service: LspService<Backend>,
    root: PathBuf,
    capabilities: ServerCapabilities,
    published: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
    next_id: AtomicI64,
    _workspace: tempfile::TempDir,
    outside: tempfile::TempDir,
}

impl TestServer {
    pub async fn new() -> Self {
        Self::with_encodings(&[]).await
    }

    pub async fn with_encodings(encodings: &[PositionEncodingKind]) -> Self {
        isolate_lsp_home();

        let workspace = tempfile::tempdir().expect("unable to create a temp workspace");

        let root = workspace
            .path()
            .canonicalize()
            .expect("unable to canonicalize the temp workspace");

        let published: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>> =
            Arc::new(Mutex::new(HashMap::new()));

        let (service, socket) = LspService::build(|client| Backend {
            config: Config::new(),
            client,
            vfs: DashMap::new(),
            root: root.clone(),
            ast_map: DashMap::new(),
            raw_ast_map: DashMap::new(),
            lowered_ast_map: DashMap::new(),
            document_map: DashMap::new(),
            _macro_map: DashMap::new(),
            globals_set: Arc::new(DashSet::new()),
            ignore_set: Arc::new(DashSet::new()),
            defined_globals: defined_globals(),
        })
        .finish();

        {
            let published = published.clone();
            tokio::spawn(async move {
                let mut socket = socket;
                while let Some(request) = socket.next().await {
                    if request.method() != "textDocument/publishDiagnostics" {
                        continue;
                    }

                    if let Some(params) = request.params() {
                        if let Ok(params) =
                            serde_json::from_value::<PublishDiagnosticsParams>(params.clone())
                        {
                            published
                                .lock()
                                .unwrap()
                                .insert(params.uri, params.diagnostics);
                        }
                    }
                }
            });
        }

        let mut server = TestServer {
            service,
            root,
            capabilities: ServerCapabilities::default(),
            published,
            next_id: AtomicI64::new(1),
            _workspace: workspace,
            outside: tempfile::tempdir().expect("unable to create the out of workspace directory"),
        };

        server.capabilities = server.initialize(encodings).await;

        server
    }

    async fn initialize(&mut self, encodings: &[PositionEncodingKind]) -> ServerCapabilities {
        let general = if encodings.is_empty() {
            None
        } else {
            Some(GeneralClientCapabilities {
                position_encodings: Some(encodings.to_vec()),
                ..Default::default()
            })
        };

        #[allow(deprecated)]
        let params = InitializeParams {
            root_uri: Some(self.root_uri()),
            capabilities: ClientCapabilities {
                general,
                ..Default::default()
            },
            ..Default::default()
        };

        let result: InitializeResult = self.request("initialize", params).await.unwrap();

        self.notify("initialized", InitializedParams {}).await;

        result.capabilities
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn capabilities(&self) -> &ServerCapabilities {
        &self.capabilities
    }

    pub fn root_uri(&self) -> Url {
        Url::from_file_path(&self.root).unwrap()
    }

    pub fn write(&self, name: &str, contents: &str) -> Url {
        let path = self.root.join(name);

        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }

        std::fs::write(&path, contents).unwrap();

        Url::from_file_path(&path).unwrap()
    }

    pub fn write_outside(&self, name: &str, contents: &str) -> Url {
        let path = self.outside.path().canonicalize().unwrap().join(name);

        std::fs::write(&path, contents).unwrap();

        Url::from_file_path(&path).unwrap()
    }

    pub fn index_path(&self, uri: &Url) {
        let path = uri.to_file_path().unwrap();
        let mut guard = ENGINE.write().unwrap();
        let _ = guard.emit_expanded_ast(&format!(r"(require {:?})", path), None);
    }

    pub fn index_workspace(&self) {
        let mut paths: Vec<PathBuf> = Vec::new();

        for entry in ignore::Walk::new(&self.root).flatten() {
            let path = entry.path();

            if path.extension().and_then(|x| x.to_str()) != Some("scm") {
                continue;
            }

            paths.push(path.to_path_buf());
        }

        paths.sort();

        for path in paths {
            let mut guard = ENGINE.write().unwrap();
            let _ = guard.emit_expanded_ast(&format!(r"(require {:?})", path), None);
        }
    }

    pub async fn open(&mut self, name: &str, contents: &str) -> Url {
        let uri = self.write(name, contents);
        self.did_open(uri.clone(), contents).await;
        uri
    }

    pub async fn did_open(&mut self, uri: Url, contents: &str) {
        self.notify(
            "textDocument/didOpen",
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri,
                    language_id: "scheme".to_string(),
                    version: 1,
                    text: contents.to_string(),
                },
            },
        )
        .await;
    }

    pub async fn did_change(&mut self, uri: Url, contents: &str) {
        self.notify(
            "textDocument/didChange",
            DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier { uri, version: 2 },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: contents.to_string(),
                }],
            },
        )
        .await;
    }

    pub async fn goto_definition(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Option<GotoDefinitionResponse> {
        self.request(
            "textDocument/definition",
            GotoDefinitionParams {
                text_document_position_params: text_document_position(uri, position),
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .await
        .unwrap()
    }

    pub async fn definition_location(&mut self, uri: &Url, position: Position) -> Location {
        match self.goto_definition(uri, position).await {
            Some(GotoDefinitionResponse::Scalar(location)) => location,
            Some(GotoDefinitionResponse::Array(mut locations)) if locations.len() == 1 => {
                locations.pop().unwrap()
            }
            other => panic!("expected a single definition location, got {:?}", other),
        }
    }

    pub async fn references(
        &mut self,
        uri: &Url,
        position: Position,
        include_declaration: bool,
    ) -> Option<Vec<Location>> {
        self.request(
            "textDocument/references",
            ReferenceParams {
                text_document_position: text_document_position(uri, position),
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: ReferenceContext {
                    include_declaration,
                },
            },
        )
        .await
        .unwrap()
    }

    pub async fn hover(&mut self, uri: &Url, position: Position) -> Option<Hover> {
        self.request(
            "textDocument/hover",
            HoverParams {
                text_document_position_params: text_document_position(uri, position),
                work_done_progress_params: Default::default(),
            },
        )
        .await
        .unwrap()
    }

    pub async fn document_symbol(&mut self, uri: &Url) -> Option<DocumentSymbolResponse> {
        self.request(
            "textDocument/documentSymbol",
            DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .await
        .unwrap()
    }

    pub async fn completion(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Option<CompletionResponse> {
        self.request(
            "textDocument/completion",
            CompletionParams {
                text_document_position: text_document_position(uri, position),
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .await
        .unwrap()
    }

    pub async fn prepare_rename(
        &mut self,
        uri: &Url,
        position: Position,
    ) -> Result<Option<PrepareRenameResponse>, tower_lsp::jsonrpc::Error> {
        self.request(
            "textDocument/prepareRename",
            text_document_position(uri, position),
        )
        .await
    }

    pub async fn rename(
        &mut self,
        uri: &Url,
        position: Position,
        new_name: &str,
    ) -> Option<WorkspaceEdit> {
        self.request(
            "textDocument/rename",
            RenameParams {
                text_document_position: text_document_position(uri, position),
                new_name: new_name.to_string(),
                work_done_progress_params: Default::default(),
            },
        )
        .await
        .unwrap()
    }

    pub fn diagnostics(&self, uri: &Url) -> Vec<Diagnostic> {
        self.published
            .lock()
            .unwrap()
            .get(uri)
            .cloned()
            .unwrap_or_default()
    }

    async fn request<P, R>(
        &mut self,
        method: &'static str,
        params: P,
    ) -> Result<R, tower_lsp::jsonrpc::Error>
    where
        P: serde::Serialize,
        R: DeserializeOwned,
    {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);

        let request = Request::build(method)
            .id(id)
            .params(serde_json::to_value(params).unwrap())
            .finish();

        let response = ServiceExt::<Request>::ready(&mut self.service)
            .await
            .expect("the language server has exited")
            .call(request)
            .await
            .expect("the language server has exited")
            .unwrap_or_else(|| panic!("no response for {}", method));

        let (_, result) = response.into_parts();

        result.map(|value| {
            serde_json::from_value(value)
                .unwrap_or_else(|e| panic!("could not decode the {} response: {}", method, e))
        })
    }

    async fn notify<P: serde::Serialize>(&mut self, method: &'static str, params: P) {
        let request = Request::build(method)
            .params(serde_json::to_value(params).unwrap())
            .finish();

        let response = ServiceExt::<Request>::ready(&mut self.service)
            .await
            .expect("the language server has exited")
            .call(request)
            .await
            .expect("the language server has exited");

        assert!(
            response.is_none(),
            "notifications should not produce a response"
        );

        // Just try to make sure all the messages make it through
        for _ in 0..10 {
            tokio::task::yield_now().await;
        }
    }
}

fn text_document_position(uri: &Url, position: Position) -> TextDocumentPositionParams {
    TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
    }
}

fn defined_globals() -> DashSet<String> {
    let defined_globals = DashSet::new();

    for global in ENGINE.read().unwrap().globals().iter() {
        let resolved = global.resolve();

        if !resolved.starts_with('#')
            && !resolved.starts_with('%')
            && !resolved.starts_with("mangler#%")
            && !resolved.starts_with(MANGLER_PREFIX)
            && !resolved.starts_with("__module")
        {
            defined_globals.insert(resolved.to_string());
        }
    }

    defined_globals
}

pub fn position(line: u32, character: u32) -> Position {
    Position::new(line, character)
}

pub fn range(line: u32, start: u32, end: u32) -> Range {
    Range::new(Position::new(line, start), Position::new(line, end))
}

pub fn find_nth(text: &str, needle: &str, n: usize) -> Position {
    let offset = text
        .match_indices(needle)
        .nth(n)
        .unwrap_or_else(|| panic!("{:?} does not occur {} times in the fixture", needle, n + 1))
        .0;

    offset_to_position(text, offset)
}

pub fn find(text: &str, needle: &str) -> Position {
    find_nth(text, needle, 0)
}

pub fn range_of_nth(text: &str, needle: &str, n: usize) -> Range {
    let start = find_nth(text, needle, n);

    Range::new(
        start,
        Position::new(start.line, start.character + needle.chars().count() as u32),
    )
}

pub fn range_of(text: &str, needle: &str) -> Range {
    range_of_nth(text, needle, 0)
}

fn offset_to_position(text: &str, offset: usize) -> Position {
    let line = text[..offset].matches('\n').count() as u32;
    let line_start = text[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);

    Position::new(line, (offset - line_start) as u32)
}

pub fn sorted(mut locations: Vec<Location>) -> Vec<Location> {
    locations.sort_by(|a, b| {
        a.uri
            .as_str()
            .cmp(b.uri.as_str())
            .then(a.range.start.line.cmp(&b.range.start.line))
            .then(a.range.start.character.cmp(&b.range.start.character))
    });
    locations
}

pub fn deduped(locations: Vec<Location>) -> Vec<Location> {
    let mut locations = sorted(locations);
    locations.dedup();
    locations
}

pub fn by_file(locations: Vec<Location>) -> Vec<(String, Range)> {
    let mut named: Vec<(String, Range)> = locations
        .into_iter()
        .map(|location| (file_name(&location.uri), location.range))
        .collect();

    named.sort_by(|a, b| {
        a.0.cmp(&b.0)
            .then(a.1.start.line.cmp(&b.1.start.line))
            .then(a.1.start.character.cmp(&b.1.start.character))
    });
    named.dedup();
    named
}

pub fn file_name(uri: &Url) -> String {
    uri.path_segments()
        .and_then(|mut segments| segments.next_back())
        .unwrap_or_default()
        .to_string()
}
