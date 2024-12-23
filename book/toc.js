// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="introduction.html">Introduction</a></li><li class="chapter-item expanded "><a href="foundations/overview.html"><strong aria-hidden="true">1.</strong> Foundations</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="foundations/evaluation.html"><strong aria-hidden="true">1.1.</strong> Evaluation</a></li><li class="chapter-item expanded "><a href="foundations/literals.html"><strong aria-hidden="true">1.2.</strong> Literals</a></li><li class="chapter-item expanded "><a href="foundations/fncalls.html"><strong aria-hidden="true">1.3.</strong> Function calls</a></li><li class="chapter-item expanded "><a href="foundations/symbols.html"><strong aria-hidden="true">1.4.</strong> Symbols</a></li><li class="chapter-item expanded "><a href="foundations/bindings_scopes.html"><strong aria-hidden="true">1.5.</strong> Bindings and Scopes</a></li></ol></li><li class="chapter-item expanded "><a href="about/about.html"><strong aria-hidden="true">2.</strong> Steel</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="about/features.html"><strong aria-hidden="true">2.1.</strong> Features</a></li><li class="chapter-item expanded "><a href="about/license.html"><strong aria-hidden="true">2.2.</strong> Licensing</a></li></ol></li><li class="chapter-item expanded "><a href="start/start.html"><strong aria-hidden="true">3.</strong> Getting Started</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="start/playground.html"><strong aria-hidden="true">3.1.</strong> Online Playground</a></li><li class="chapter-item expanded "><a href="start/standalone.html"><strong aria-hidden="true">3.2.</strong> Using Steel on its own</a></li><li class="chapter-item expanded "><a href="start/dylib.html"><strong aria-hidden="true">3.3.</strong> Using Rust in Steel</a></li><li class="chapter-item expanded "><a href="start/embedded.html"><strong aria-hidden="true">3.4.</strong> Embedding Steel in Rust</a></li></ol></li><li class="chapter-item expanded "><a href="engine/engine.html"><strong aria-hidden="true">4.</strong> The Rust Engine API</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="engine/register_function.html"><strong aria-hidden="true">4.1.</strong> Registering functions</a></li><li class="chapter-item expanded "><a href="engine/embedding_values.html"><strong aria-hidden="true">4.2.</strong> Embedding values</a></li></ol></li><li class="chapter-item expanded "><a href="reference/language.html"><strong aria-hidden="true">5.</strong> Language Reference</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="reference/keywords.html"><strong aria-hidden="true">5.1.</strong> Keywords</a></li><li class="chapter-item expanded "><a href="reference/syntax.html"><strong aria-hidden="true">5.2.</strong> Syntax</a></li><li class="chapter-item expanded "><a href="reference/macros.html"><strong aria-hidden="true">5.3.</strong> Macros</a></li><li class="chapter-item expanded "><a href="reference/contracts.html"><strong aria-hidden="true">5.4.</strong> Contracts</a></li><li class="chapter-item expanded "><a href="reference/transducers.html"><strong aria-hidden="true">5.5.</strong> Transducers</a></li><li class="chapter-item expanded "><a href="reference/modules.html"><strong aria-hidden="true">5.6.</strong> Modules</a></li><li class="chapter-item expanded "><a href="reference/functions.html"><strong aria-hidden="true">5.7.</strong> Functions</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="generated/logging.html"><strong aria-hidden="true">5.7.1.</strong> Logging</a></li><li class="chapter-item expanded "><a href="reference/builtins.html"><strong aria-hidden="true">5.7.2.</strong> Built ins</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="builtins/steel_base.html"><strong aria-hidden="true">5.7.2.1.</strong> steel/base</a></li><li class="chapter-item expanded "><a href="builtins/steel_bytevectors.html"><strong aria-hidden="true">5.7.2.2.</strong> steel/bytevectors</a></li><li class="chapter-item expanded "><a href="builtins/steel_constants.html"><strong aria-hidden="true">5.7.2.3.</strong> steel/constants</a></li><li class="chapter-item expanded "><a href="builtins/steel_core_option.html"><strong aria-hidden="true">5.7.2.4.</strong> steel/core/option</a></li><li class="chapter-item expanded "><a href="builtins/steel_core_result.html"><strong aria-hidden="true">5.7.2.5.</strong> steel/core/result</a></li><li class="chapter-item expanded "><a href="builtins/steel_core_types.html"><strong aria-hidden="true">5.7.2.6.</strong> steel/core/types</a></li><li class="chapter-item expanded "><a href="builtins/steel_equality.html"><strong aria-hidden="true">5.7.2.7.</strong> steel/equality</a></li><li class="chapter-item expanded "><a href="builtins/steel_filesystem.html"><strong aria-hidden="true">5.7.2.8.</strong> steel/filesystem</a></li><li class="chapter-item expanded "><a href="builtins/steel_hash.html"><strong aria-hidden="true">5.7.2.9.</strong> steel/hash</a></li><li class="chapter-item expanded "><a href="builtins/steel_identity.html"><strong aria-hidden="true">5.7.2.10.</strong> steel/identity</a></li><li class="chapter-item expanded "><a href="builtins/steel_io.html"><strong aria-hidden="true">5.7.2.11.</strong> steel/io</a></li><li class="chapter-item expanded "><a href="builtins/steel_immutable-vectors.html"><strong aria-hidden="true">5.7.2.12.</strong> steel/immutable-vectors</a></li><li class="chapter-item expanded "><a href="builtins/steel_json.html"><strong aria-hidden="true">5.7.2.13.</strong> steel/json</a></li><li class="chapter-item expanded "><a href="builtins/steel_lists.html"><strong aria-hidden="true">5.7.2.14.</strong> steel/lists</a></li><li class="chapter-item expanded "><a href="builtins/steel_meta.html"><strong aria-hidden="true">5.7.2.15.</strong> steel/meta</a></li><li class="chapter-item expanded "><a href="builtins/steel_numbers.html"><strong aria-hidden="true">5.7.2.16.</strong> steel/numbers</a></li><li class="chapter-item expanded "><a href="builtins/steel_ord.html"><strong aria-hidden="true">5.7.2.17.</strong> steel/ord</a></li><li class="chapter-item expanded "><a href="builtins/steel_ports.html"><strong aria-hidden="true">5.7.2.18.</strong> steel/ports</a></li><li class="chapter-item expanded "><a href="builtins/steel_process.html"><strong aria-hidden="true">5.7.2.19.</strong> steel/process</a></li><li class="chapter-item expanded "><a href="builtins/steel_random.html"><strong aria-hidden="true">5.7.2.20.</strong> steel/random</a></li><li class="chapter-item expanded "><a href="builtins/steel_sets.html"><strong aria-hidden="true">5.7.2.21.</strong> steel/sets</a></li><li class="chapter-item expanded "><a href="builtins/steel_streams.html"><strong aria-hidden="true">5.7.2.22.</strong> steel/streams</a></li><li class="chapter-item expanded "><a href="builtins/steel_strings.html"><strong aria-hidden="true">5.7.2.23.</strong> steel/strings</a></li><li class="chapter-item expanded "><a href="builtins/steel_symbols.html"><strong aria-hidden="true">5.7.2.24.</strong> steel/symbols</a></li><li class="chapter-item expanded "><a href="builtins/steel_syntax.html"><strong aria-hidden="true">5.7.2.25.</strong> steel/syntax</a></li><li class="chapter-item expanded "><a href="builtins/steel_time.html"><strong aria-hidden="true">5.7.2.26.</strong> steel/time</a></li><li class="chapter-item expanded "><a href="builtins/steel_transducers.html"><strong aria-hidden="true">5.7.2.27.</strong> steel/transducers</a></li><li class="chapter-item expanded "><a href="builtins/steel_vectors.html"><strong aria-hidden="true">5.7.2.28.</strong> steel/vectors</a></li></ol></li></ol></li></ol></li><li class="chapter-item expanded "><a href="bytecode/bytecode.html"><strong aria-hidden="true">6.</strong> Bytecode</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="bytecode/optimizations.html"><strong aria-hidden="true">6.1.</strong> Optimizations</a></li></ol></li><li class="chapter-item expanded "><a href="benchmarks/benchmarks.html"><strong aria-hidden="true">7.</strong> Benchmarks</a></li><li class="chapter-item expanded "><a href="patterns/patterns.html"><strong aria-hidden="true">8.</strong> Common Patterns</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="patterns/mutability.html"><strong aria-hidden="true">8.1.</strong> Interior Mutability</a></li><li class="chapter-item expanded "><a href="patterns/async.html"><strong aria-hidden="true">8.2.</strong> Async</a></li></ol></li><li class="chapter-item expanded "><a href="garbage-collection/gc.html"><strong aria-hidden="true">9.</strong> Memory Management</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString();
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
