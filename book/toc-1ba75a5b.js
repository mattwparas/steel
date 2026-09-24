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
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="introduction.html">Introduction</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="about/about.html"><strong aria-hidden="true">1.</strong> Steel</a></span><ol class="section"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="about/features.html"><strong aria-hidden="true">1.1.</strong> Features</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="about/values.html"><strong aria-hidden="true">1.2.</strong> Creating values</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="about/define.html"><strong aria-hidden="true">1.3.</strong> Definitions and expressions</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="about/collections.html"><strong aria-hidden="true">1.4.</strong> Collections</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="about/license.html"><strong aria-hidden="true">1.5.</strong> Licensing</a></span></li></ol><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="start/start.html"><strong aria-hidden="true">2.</strong> Getting Started</a></span><ol class="section"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="start/playground.html"><strong aria-hidden="true">2.1.</strong> Online Playground</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="start/standalone.html"><strong aria-hidden="true">2.2.</strong> Using Steel on its own</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="start/dylib.html"><strong aria-hidden="true">2.3.</strong> Using Rust in Steel</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="start/embedded.html"><strong aria-hidden="true">2.4.</strong> Embedding Steel in Rust</a></span></li></ol><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="engine/engine.html"><strong aria-hidden="true">3.</strong> The Rust Engine API</a></span><ol class="section"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="engine/register_function.html"><strong aria-hidden="true">3.1.</strong> Registering functions</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="engine/embedding_values.html"><strong aria-hidden="true">3.2.</strong> Embedding values</a></span></li></ol><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/language.html"><strong aria-hidden="true">4.</strong> Language Reference</a></span><ol class="section"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/keywords.html"><strong aria-hidden="true">4.1.</strong> Keywords</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/macros.html"><strong aria-hidden="true">4.2.</strong> Macros</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/contracts.html"><strong aria-hidden="true">4.3.</strong> Contracts</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/transducers.html"><strong aria-hidden="true">4.4.</strong> Transducers</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/modules.html"><strong aria-hidden="true">4.5.</strong> Modules</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/functions.html"><strong aria-hidden="true">4.6.</strong> Functions</a></span><ol class="section"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="generated/logging.html"><strong aria-hidden="true">4.6.1.</strong> Logging</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/builtins.html"><strong aria-hidden="true">4.6.2.</strong> Built ins</a></span><ol class="section"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_base.html"><strong aria-hidden="true">4.6.2.1.</strong> steel/base</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_bytevectors.html"><strong aria-hidden="true">4.6.2.2.</strong> steel/bytevectors</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_constants.html"><strong aria-hidden="true">4.6.2.3.</strong> steel/constants</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_core_option.html"><strong aria-hidden="true">4.6.2.4.</strong> steel/core/option</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_core_result.html"><strong aria-hidden="true">4.6.2.5.</strong> steel/core/result</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_core_types.html"><strong aria-hidden="true">4.6.2.6.</strong> steel/core/types</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_equality.html"><strong aria-hidden="true">4.6.2.7.</strong> steel/equality</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_filesystem.html"><strong aria-hidden="true">4.6.2.8.</strong> steel/filesystem</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_hash.html"><strong aria-hidden="true">4.6.2.9.</strong> steel/hash</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_identity.html"><strong aria-hidden="true">4.6.2.10.</strong> steel/identity</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_io.html"><strong aria-hidden="true">4.6.2.11.</strong> steel/io</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_immutable-vectors.html"><strong aria-hidden="true">4.6.2.12.</strong> steel/immutable-vectors</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_json.html"><strong aria-hidden="true">4.6.2.13.</strong> steel/json</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_lists.html"><strong aria-hidden="true">4.6.2.14.</strong> steel/lists</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_meta.html"><strong aria-hidden="true">4.6.2.15.</strong> steel/meta</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_numbers.html"><strong aria-hidden="true">4.6.2.16.</strong> steel/numbers</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_ord.html"><strong aria-hidden="true">4.6.2.17.</strong> steel/ord</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_ports.html"><strong aria-hidden="true">4.6.2.18.</strong> steel/ports</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_process.html"><strong aria-hidden="true">4.6.2.19.</strong> steel/process</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_random.html"><strong aria-hidden="true">4.6.2.20.</strong> steel/random</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_sets.html"><strong aria-hidden="true">4.6.2.21.</strong> steel/sets</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_streams.html"><strong aria-hidden="true">4.6.2.22.</strong> steel/streams</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_strings.html"><strong aria-hidden="true">4.6.2.23.</strong> steel/strings</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_symbols.html"><strong aria-hidden="true">4.6.2.24.</strong> steel/symbols</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_syntax.html"><strong aria-hidden="true">4.6.2.25.</strong> steel/syntax</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_time.html"><strong aria-hidden="true">4.6.2.26.</strong> steel/time</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_transducers.html"><strong aria-hidden="true">4.6.2.27.</strong> steel/transducers</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_vectors.html"><strong aria-hidden="true">4.6.2.28.</strong> steel/vectors</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="builtins/steel_tcp.html"><strong aria-hidden="true">4.6.2.29.</strong> steel/tcp</a></span></li></ol><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="reference/stdlib.html"><strong aria-hidden="true">4.6.3.</strong> Stdlib</a></span><ol class="section"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/steel_async.html"><strong aria-hidden="true">4.6.3.1.</strong> steel/async</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/steel_iterators.html"><strong aria-hidden="true">4.6.3.2.</strong> steel/iterators</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/steel_option.html"><strong aria-hidden="true">4.6.3.3.</strong> steel/option</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/steel_result.html"><strong aria-hidden="true">4.6.3.4.</strong> steel/result</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/steel_sync.html"><strong aria-hidden="true">4.6.3.5.</strong> steel/sync</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/private_steel_contract.html"><strong aria-hidden="true">4.6.3.6.</strong> #%private/steel/contract</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/private_steel_control.html"><strong aria-hidden="true">4.6.3.7.</strong> #%private/steel/control</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/private_steel_match.html"><strong aria-hidden="true">4.6.3.8.</strong> #%private/steel/match</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/private_steel_ports.html"><strong aria-hidden="true">4.6.3.9.</strong> #%private/steel/ports</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/private_steel_print.html"><strong aria-hidden="true">4.6.3.10.</strong> #%private/steel/print</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/private_steel_reader.html"><strong aria-hidden="true">4.6.3.11.</strong> #%private/steel/reader</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="stdlib/private_steel_stdlib.html"><strong aria-hidden="true">4.6.3.12.</strong> #%private/steel/stdlib</a></span></li></ol></li></ol></li></ol><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="patterns/patterns.html"><strong aria-hidden="true">5.</strong> Common Patterns</a></span><ol class="section"><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="patterns/context.html"><strong aria-hidden="true">5.1.</strong> Context variables</a></span></li><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="patterns/async.html"><strong aria-hidden="true">5.2.</strong> Async</a></span></li></ol><li class="chapter-item expanded "><span class="chapter-link-wrapper"><a href="garbage-collection/gc.html"><strong aria-hidden="true">6.</strong> Memory Management</a></span></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split('#')[0].split('?')[0];
        if (current_page.endsWith('/')) {
            current_page += 'index.html';
        }
        const links = Array.prototype.slice.call(this.querySelectorAll('a'));
        const l = links.length;
        for (let i = 0; i < l; ++i) {
            const link = links[i];
            const href = link.getAttribute('href');
            if (href && !href.startsWith('#') && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The 'index' page is supposed to alias the first chapter in the book.
            if (link.href === current_page
                || i === 0
                && path_to_root === ''
                && current_page.endsWith('/index.html')) {
                link.classList.add('active');
                let parent = link.parentElement;
                while (parent) {
                    if (parent.tagName === 'LI' && parent.classList.contains('chapter-item')) {
                        parent.classList.add('expanded');
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', e => {
            if (e.target.tagName === 'A') {
                const clientRect = e.target.getBoundingClientRect();
                const sidebarRect = this.getBoundingClientRect();
                sessionStorage.setItem('sidebar-scroll-offset', clientRect.top - sidebarRect.top);
            }
        }, { passive: true });
        const sidebarScrollOffset = sessionStorage.getItem('sidebar-scroll-offset');
        sessionStorage.removeItem('sidebar-scroll-offset');
        if (sidebarScrollOffset !== null) {
            // preserve sidebar scroll position when navigating via links within sidebar
            const activeSection = this.querySelector('.active');
            if (activeSection) {
                const clientRect = activeSection.getBoundingClientRect();
                const sidebarRect = this.getBoundingClientRect();
                const currentOffset = clientRect.top - sidebarRect.top;
                this.scrollTop += currentOffset - parseFloat(sidebarScrollOffset);
            }
        } else {
            // scroll sidebar to current active section when navigating via
            // 'next/previous chapter' buttons
            const activeSection = document.querySelector('#mdbook-sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        const sidebarAnchorToggles = document.querySelectorAll('.chapter-fold-toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(el => {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define('mdbook-sidebar-scrollbox', MDBookSidebarScrollbox);


// ---------------------------------------------------------------------------
// Support for dynamically adding headers to the sidebar.

(function() {
    // This is used to detect which direction the page has scrolled since the
    // last scroll event.
    let lastKnownScrollPosition = 0;
    // This is the threshold in px from the top of the screen where it will
    // consider a header the "current" header when scrolling down.
    const defaultDownThreshold = 150;
    // Same as defaultDownThreshold, except when scrolling up.
    const defaultUpThreshold = 300;
    // The threshold is a virtual horizontal line on the screen where it
    // considers the "current" header to be above the line. The threshold is
    // modified dynamically to handle headers that are near the bottom of the
    // screen, and to slightly offset the behavior when scrolling up vs down.
    let threshold = defaultDownThreshold;
    // This is used to disable updates while scrolling. This is needed when
    // clicking the header in the sidebar, which triggers a scroll event. It
    // is somewhat finicky to detect when the scroll has finished, so this
    // uses a relatively dumb system of disabling scroll updates for a short
    // time after the click.
    let disableScroll = false;
    // Array of header elements on the page.
    let headers;
    // Array of li elements that are initially collapsed headers in the sidebar.
    // I'm not sure why eslint seems to have a false positive here.
    // eslint-disable-next-line prefer-const
    let headerToggles = [];
    // This is a debugging tool for the threshold which you can enable in the console.
    let thresholdDebug = false;

    // Updates the threshold based on the scroll position.
    function updateThreshold() {
        const scrollTop = window.pageYOffset || document.documentElement.scrollTop;
        const windowHeight = window.innerHeight;
        const documentHeight = document.documentElement.scrollHeight;

        // The number of pixels below the viewport, at most documentHeight.
        // This is used to push the threshold down to the bottom of the page
        // as the user scrolls towards the bottom.
        const pixelsBelow = Math.max(0, documentHeight - (scrollTop + windowHeight));
        // The number of pixels above the viewport, at least defaultDownThreshold.
        // Similar to pixelsBelow, this is used to push the threshold back towards
        // the top when reaching the top of the page.
        const pixelsAbove = Math.max(0, defaultDownThreshold - scrollTop);
        // How much the threshold should be offset once it gets close to the
        // bottom of the page.
        const bottomAdd = Math.max(0, windowHeight - pixelsBelow - defaultDownThreshold);
        let adjustedBottomAdd = bottomAdd;

        // Adjusts bottomAdd for a small document. The calculation above
        // assumes the document is at least twice the windowheight in size. If
        // it is less than that, then bottomAdd needs to be shrunk
        // proportional to the difference in size.
        if (documentHeight < windowHeight * 2) {
            const maxPixelsBelow = documentHeight - windowHeight;
            const t = 1 - pixelsBelow / Math.max(1, maxPixelsBelow);
            const clamp = Math.max(0, Math.min(1, t));
            adjustedBottomAdd *= clamp;
        }

        let scrollingDown = true;
        if (scrollTop < lastKnownScrollPosition) {
            scrollingDown = false;
        }

        if (scrollingDown) {
            // When scrolling down, move the threshold up towards the default
            // downwards threshold position. If near the bottom of the page,
            // adjustedBottomAdd will offset the threshold towards the bottom
            // of the page.
            const amountScrolledDown = scrollTop - lastKnownScrollPosition;
            const adjustedDefault = defaultDownThreshold + adjustedBottomAdd;
            threshold = Math.max(adjustedDefault, threshold - amountScrolledDown);
        } else {
            // When scrolling up, move the threshold down towards the default
            // upwards threshold position. If near the bottom of the page,
            // quickly transition the threshold back up where it normally
            // belongs.
            const amountScrolledUp = lastKnownScrollPosition - scrollTop;
            const adjustedDefault = defaultUpThreshold - pixelsAbove
                + Math.max(0, adjustedBottomAdd - defaultDownThreshold);
            threshold = Math.min(adjustedDefault, threshold + amountScrolledUp);
        }

        if (documentHeight <= windowHeight) {
            threshold = 0;
        }

        if (thresholdDebug) {
            const id = 'mdbook-threshold-debug-data';
            let data = document.getElementById(id);
            if (data === null) {
                data = document.createElement('div');
                data.id = id;
                data.style.cssText = `
                    position: fixed;
                    top: 50px;
                    right: 10px;
                    background-color: 0xeeeeee;
                    z-index: 9999;
                    pointer-events: none;
                `;
                document.body.appendChild(data);
            }
            data.innerHTML = `
                <table>
                  <tr><td>documentHeight</td><td>${documentHeight.toFixed(1)}</td></tr>
                  <tr><td>windowHeight</td><td>${windowHeight.toFixed(1)}</td></tr>
                  <tr><td>scrollTop</td><td>${scrollTop.toFixed(1)}</td></tr>
                  <tr><td>pixelsAbove</td><td>${pixelsAbove.toFixed(1)}</td></tr>
                  <tr><td>pixelsBelow</td><td>${pixelsBelow.toFixed(1)}</td></tr>
                  <tr><td>bottomAdd</td><td>${bottomAdd.toFixed(1)}</td></tr>
                  <tr><td>adjustedBottomAdd</td><td>${adjustedBottomAdd.toFixed(1)}</td></tr>
                  <tr><td>scrollingDown</td><td>${scrollingDown}</td></tr>
                  <tr><td>threshold</td><td>${threshold.toFixed(1)}</td></tr>
                </table>
            `;
            drawDebugLine();
        }

        lastKnownScrollPosition = scrollTop;
    }

    function drawDebugLine() {
        if (!document.body) {
            return;
        }
        const id = 'mdbook-threshold-debug-line';
        const existingLine = document.getElementById(id);
        if (existingLine) {
            existingLine.remove();
        }
        const line = document.createElement('div');
        line.id = id;
        line.style.cssText = `
            position: fixed;
            top: ${threshold}px;
            left: 0;
            width: 100vw;
            height: 2px;
            background-color: red;
            z-index: 9999;
            pointer-events: none;
        `;
        document.body.appendChild(line);
    }

    function mdbookEnableThresholdDebug() {
        thresholdDebug = true;
        updateThreshold();
        drawDebugLine();
    }

    window.mdbookEnableThresholdDebug = mdbookEnableThresholdDebug;

    // Updates which headers in the sidebar should be expanded. If the current
    // header is inside a collapsed group, then it, and all its parents should
    // be expanded.
    function updateHeaderExpanded(currentA) {
        // Add expanded to all header-item li ancestors.
        let current = currentA.parentElement;
        while (current) {
            if (current.tagName === 'LI' && current.classList.contains('header-item')) {
                current.classList.add('expanded');
            }
            current = current.parentElement;
        }
    }

    // Updates which header is marked as the "current" header in the sidebar.
    // This is done with a virtual Y threshold, where headers at or below
    // that line will be considered the current one.
    function updateCurrentHeader() {
        if (!headers || !headers.length) {
            return;
        }

        // Reset the classes, which will be rebuilt below.
        const els = document.getElementsByClassName('current-header');
        for (const el of els) {
            el.classList.remove('current-header');
        }
        for (const toggle of headerToggles) {
            toggle.classList.remove('expanded');
        }

        // Find the last header that is above the threshold.
        let lastHeader = null;
        for (const header of headers) {
            const rect = header.getBoundingClientRect();
            if (rect.top <= threshold) {
                lastHeader = header;
            } else {
                break;
            }
        }
        if (lastHeader === null) {
            lastHeader = headers[0];
            const rect = lastHeader.getBoundingClientRect();
            const windowHeight = window.innerHeight;
            if (rect.top >= windowHeight) {
                return;
            }
        }

        // Get the anchor in the summary.
        const href = '#' + lastHeader.id;
        const a = [...document.querySelectorAll('.header-in-summary')]
            .find(element => element.getAttribute('href') === href);
        if (!a) {
            return;
        }

        a.classList.add('current-header');

        updateHeaderExpanded(a);
    }

    // Updates which header is "current" based on the threshold line.
    function reloadCurrentHeader() {
        if (disableScroll) {
            return;
        }
        updateThreshold();
        updateCurrentHeader();
    }


    // When clicking on a header in the sidebar, this adjusts the threshold so
    // that it is located next to the header. This is so that header becomes
    // "current".
    function headerThresholdClick(event) {
        // See disableScroll description why this is done.
        disableScroll = true;
        setTimeout(() => {
            disableScroll = false;
        }, 100);
        // requestAnimationFrame is used to delay the update of the "current"
        // header until after the scroll is done, and the header is in the new
        // position.
        requestAnimationFrame(() => {
            requestAnimationFrame(() => {
                // Closest is needed because if it has child elements like <code>.
                const a = event.target.closest('a');
                const href = a.getAttribute('href');
                const targetId = href.substring(1);
                const targetElement = document.getElementById(targetId);
                if (targetElement) {
                    threshold = targetElement.getBoundingClientRect().bottom;
                    updateCurrentHeader();
                }
            });
        });
    }

    // Takes the nodes from the given head and copies them over to the
    // destination, along with some filtering.
    function filterHeader(source, dest) {
        const clone = source.cloneNode(true);
        clone.querySelectorAll('mark').forEach(mark => {
            mark.replaceWith(...mark.childNodes);
        });
        dest.append(...clone.childNodes);
    }

    // Scans page for headers and adds them to the sidebar.
    document.addEventListener('DOMContentLoaded', function() {
        const activeSection = document.querySelector('#mdbook-sidebar .active');
        if (activeSection === null) {
            return;
        }

        const main = document.getElementsByTagName('main')[0];
        headers = Array.from(main.querySelectorAll('h2, h3, h4, h5, h6'))
            .filter(h => h.id !== '' && h.children.length && h.children[0].tagName === 'A');

        if (headers.length === 0) {
            return;
        }

        // Build a tree of headers in the sidebar.

        const stack = [];

        const firstLevel = parseInt(headers[0].tagName.charAt(1));
        for (let i = 1; i < firstLevel; i++) {
            const ol = document.createElement('ol');
            ol.classList.add('section');
            if (stack.length > 0) {
                stack[stack.length - 1].ol.appendChild(ol);
            }
            stack.push({level: i + 1, ol: ol});
        }

        // The level where it will start folding deeply nested headers.
        const foldLevel = 3;

        for (let i = 0; i < headers.length; i++) {
            const header = headers[i];
            const level = parseInt(header.tagName.charAt(1));

            const currentLevel = stack[stack.length - 1].level;
            if (level > currentLevel) {
                // Begin nesting to this level.
                for (let nextLevel = currentLevel + 1; nextLevel <= level; nextLevel++) {
                    const ol = document.createElement('ol');
                    ol.classList.add('section');
                    const last = stack[stack.length - 1];
                    const lastChild = last.ol.lastChild;
                    // Handle the case where jumping more than one nesting
                    // level, which doesn't have a list item to place this new
                    // list inside of.
                    if (lastChild) {
                        lastChild.appendChild(ol);
                    } else {
                        last.ol.appendChild(ol);
                    }
                    stack.push({level: nextLevel, ol: ol});
                }
            } else if (level < currentLevel) {
                while (stack.length > 1 && stack[stack.length - 1].level > level) {
                    stack.pop();
                }
            }

            const li = document.createElement('li');
            li.classList.add('header-item');
            li.classList.add('expanded');
            if (level < foldLevel) {
                li.classList.add('expanded');
            }
            const span = document.createElement('span');
            span.classList.add('chapter-link-wrapper');
            const a = document.createElement('a');
            span.appendChild(a);
            a.href = '#' + header.id;
            a.classList.add('header-in-summary');
            filterHeader(header.children[0], a);
            a.addEventListener('click', headerThresholdClick);
            const nextHeader = headers[i + 1];
            if (nextHeader !== undefined) {
                const nextLevel = parseInt(nextHeader.tagName.charAt(1));
                if (nextLevel > level && level >= foldLevel) {
                    const toggle = document.createElement('a');
                    toggle.classList.add('chapter-fold-toggle');
                    toggle.classList.add('header-toggle');
                    toggle.addEventListener('click', () => {
                        li.classList.toggle('expanded');
                    });
                    const toggleDiv = document.createElement('div');
                    toggleDiv.textContent = '❱';
                    toggle.appendChild(toggleDiv);
                    span.appendChild(toggle);
                    headerToggles.push(li);
                }
            }
            li.appendChild(span);

            const currentParent = stack[stack.length - 1];
            currentParent.ol.appendChild(li);
        }

        const onThisPage = document.createElement('div');
        onThisPage.classList.add('on-this-page');
        onThisPage.append(stack[0].ol);
        const activeItemSpan = activeSection.parentElement;
        activeItemSpan.after(onThisPage);
    });

    document.addEventListener('DOMContentLoaded', reloadCurrentHeader);
    document.addEventListener('scroll', reloadCurrentHeader, { passive: true });
})();

