// Harness for driving the language server backend the way an editor would, minus the
// JSON-RPC layer. Tests call Backend's inherent methods, so what's under test is our
// analysis and span conversion rather than tower-lsp's plumbing.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, OnceLock};

use dashmap::{DashMap, DashSet};
use steel_language_server::backend::{visible_globals, Backend, Config, OffsetEncoding, ENGINE};
use tower_lsp::lsp_types::*;
use tower_lsp::LspService;

// point the lint engine at a scratch directory so we never pick up the developer's real
// lint configuration
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
    // only here to own the Backend - a Client can't be built any other way, and nothing
    // the tests call ever touches it
    service: LspService<Backend>,
    root: PathBuf,
    diagnostics: HashMap<Url, Vec<Diagnostic>>,
    // the engine keeps compiled modules keyed by these paths, which is fine as long as
    // every test gets a directory of its own
    _workspace: tempfile::TempDir,
    // a second directory, deliberately not under the workspace root
    outside: tempfile::TempDir,
}

impl TestServer {
    pub fn new() -> Self {
        Self::with_encoding(OffsetEncoding::Utf16)
    }

    pub fn with_encoding(encoding: OffsetEncoding) -> Self {
        isolate_lsp_home();

        let workspace = tempfile::tempdir().expect("unable to create a temp workspace");

        // canonicalize matters on macos, where the temp dir is a symlink into /private.
        // url round trips have to agree with the paths the engine records or nothing
        // resolves
        let root = workspace
            .path()
            .canonicalize()
            .expect("unable to canonicalize the temp workspace");

        let (service, _socket) = LspService::build(|client| Backend {
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
            defined_globals: visible_globals(&ENGINE.read().unwrap()),
        })
        .finish();

        let server = TestServer {
            service,
            root,
            diagnostics: HashMap::new(),
            _workspace: workspace,
            outside: tempfile::tempdir().expect("unable to create the out of workspace directory"),
        };

        server.backend().config.encoding.store(encoding);

        server
    }

    fn backend(&self) -> &Backend {
        self.service.inner()
    }

    pub fn write(&self, name: &str, contents: &str) -> Url {
        let path = self.root.join(name);

        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }

        std::fs::write(&path, contents).unwrap();

        Url::from_file_path(&path).unwrap()
    }

    // A uri under the workspace root for a file that is never written or opened. The
    // queries bail on the document maps before they reach the filesystem.
    pub fn unopened(&self, name: &str) -> Url {
        Url::from_file_path(self.root.join(name)).unwrap()
    }

    // Writes a fixture file outside the workspace root, the way a library in $STEEL_HOME
    // or a sibling checkout would sit. We treat those differently from files we own.
    pub fn write_outside(&self, name: &str, contents: &str) -> Url {
        let path = self.outside.path().canonicalize().unwrap().join(name);

        std::fs::write(&path, contents).unwrap();

        Url::from_file_path(&path).unwrap()
    }

    // Requires a single file into the engine, without treating it as part of the workspace.
    pub fn index_path(&self, uri: &Url) {
        let path = uri.to_file_path().unwrap();
        let mut guard = ENGINE.write().unwrap();
        let _ = guard.emit_expanded_ast(&format!(r"(require {:?})", path), None);
    }

    // Mirrors the indexing the binary does on startup - every .scm file under the root
    // gets required so the engine knows about its module.
    pub fn index_workspace(&self) {
        let mut paths: Vec<PathBuf> = Vec::new();

        for entry in ignore::Walk::new(&self.root).flatten() {
            let path = entry.path();

            if path.extension().and_then(|x| x.to_str()) != Some("scm") {
                continue;
            }

            paths.push(path.to_path_buf());
        }

        // deterministic order, so a failure is reproducible
        paths.sort();

        for path in paths {
            let mut guard = ENGINE.write().unwrap();
            let _ = guard.emit_expanded_ast(&format!(r"(require {:?})", path), None);
        }
    }

    pub fn open(&mut self, name: &str, contents: &str) -> Url {
        let uri = self.write(name, contents);
        self.did_open(uri.clone(), contents);
        uri
    }

    pub fn did_open(&mut self, uri: Url, contents: &str) {
        let diagnostics = self.backend().analyze(&uri, contents.to_string());
        self.diagnostics.insert(uri, diagnostics);
    }

    pub fn did_change(&mut self, uri: Url, contents: &str) {
        self.did_open(uri, contents);
    }

    // The diagnostics from the last time this document was opened or changed.
    pub fn diagnostics(&self, uri: &Url) -> Vec<Diagnostic> {
        self.diagnostics.get(uri).cloned().unwrap_or_default()
    }

    pub async fn goto_definition(
        &self,
        uri: &Url,
        position: Position,
    ) -> Option<GotoDefinitionResponse> {
        self.backend()
            .goto_definition_impl(uri.clone(), position)
            .await
    }

    // Narrows goto definition down to a single location, failing the test otherwise.
    pub async fn definition_location(&self, uri: &Url, position: Position) -> Location {
        match self.goto_definition(uri, position).await {
            Some(GotoDefinitionResponse::Scalar(location)) => location,
            Some(GotoDefinitionResponse::Array(mut locations)) if locations.len() == 1 => {
                locations.pop().unwrap()
            }
            other => panic!("expected a single definition location, got {:?}", other),
        }
    }

    pub async fn references(
        &self,
        uri: &Url,
        position: Position,
        include_declaration: bool,
    ) -> Option<Vec<Location>> {
        self.backend()
            .references_impl(uri.clone(), position, include_declaration)
            .await
    }

    pub async fn hover(&self, uri: &Url, position: Position) -> Option<Hover> {
        self.backend().hover_impl(uri.clone(), position).await
    }

    pub async fn document_symbol(&self, uri: &Url) -> Option<DocumentSymbolResponse> {
        self.backend().document_symbol_impl(uri.clone()).await
    }

    pub async fn completion(&self, uri: &Url, position: Position) -> Option<CompletionResponse> {
        self.backend().completion_impl(uri.clone(), position).await
    }

    pub async fn prepare_rename(
        &self,
        uri: &Url,
        position: Position,
    ) -> Result<Option<PrepareRenameResponse>, tower_lsp::jsonrpc::Error> {
        self.backend()
            .prepare_rename_impl(uri.clone(), position)
            .await
    }

    pub async fn rename(
        &self,
        uri: &Url,
        position: Position,
        new_name: &str,
    ) -> Option<WorkspaceEdit> {
        self.backend()
            .rename_impl(uri.clone(), position, new_name.to_string())
            .await
    }
}

// Zero based (line, character).
pub fn position(line: u32, character: u32) -> Position {
    Position::new(line, character)
}

// The position of the first character of the nth (zero indexed) occurrence of `needle`.
// Keeps the tests readable, and honest when a fixture gets reformatted.
pub fn find_nth(text: &str, needle: &str, n: usize) -> Position {
    let offset = text
        .match_indices(needle)
        .nth(n)
        .unwrap_or_else(|| panic!("{:?} does not occur {} times in the fixture", needle, n + 1))
        .0;

    offset_to_position(text, offset)
}

// find_nth, for the first occurrence.
pub fn find(text: &str, needle: &str) -> Position {
    find_nth(text, needle, 0)
}

// The range covering the nth (zero indexed) occurrence of `needle`.
pub fn range_of_nth(text: &str, needle: &str, n: usize) -> Range {
    let start = find_nth(text, needle, n);

    Range::new(
        start,
        Position::new(start.line, start.character + needle.chars().count() as u32),
    )
}

// The range covering the first occurrence of `needle`.
pub fn range_of(text: &str, needle: &str) -> Range {
    range_of_nth(text, needle, 0)
}

fn offset_to_position(text: &str, offset: usize) -> Position {
    let line = text[..offset].matches('\n').count() as u32;
    let line_start = text[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);

    // the fixtures using these helpers are ascii, so byte and character offsets agree
    Position::new(line, (offset - line_start) as u32)
}

// Sorts locations so a test can compare them without depending on discovery order.
// Deliberately does not dedupe - emitting the same location twice is a bug we want to
// catch.
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

// sorted with duplicates collapsed. For the cases where we know the same location comes
// back more than once, so the test still pins down what was found. See
// references_does_not_report_duplicate_locations.
pub fn deduped(locations: Vec<Location>) -> Vec<Location> {
    let mut locations = sorted(locations);
    locations.dedup();
    locations
}

// Reduces locations to (file name, range), sorted and deduped. Use this instead of deduped
// when the locations can live in different directories - sorting whole uris would depend on
// the random temp directory names.
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

// The last path segment of a file uri.
pub fn file_name(uri: &Url) -> String {
    uri.path_segments()
        .and_then(|mut segments| segments.next_back())
        .unwrap_or_default()
        .to_string()
}
