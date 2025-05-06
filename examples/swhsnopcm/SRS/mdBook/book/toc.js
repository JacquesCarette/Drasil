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
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded "><a href="title.html"><strong aria-hidden="true">1.</strong> Software Requirements Specification for Solar Water Heating Systems</a></li><li class="chapter-item expanded "><a href="SecToC.html"><strong aria-hidden="true">2.</strong> Table of Contents</a></li><li class="chapter-item expanded "><a href="SecRefMat.html"><strong aria-hidden="true">3.</strong> Reference Material</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="SecToU.html"><strong aria-hidden="true">3.1.</strong> Table of Units</a></li><li class="chapter-item expanded "><a href="SecToS.html"><strong aria-hidden="true">3.2.</strong> Table of Symbols</a></li><li class="chapter-item expanded "><a href="SecTAbbAcc.html"><strong aria-hidden="true">3.3.</strong> Abbreviations and Acronyms</a></li></ol></li><li class="chapter-item expanded "><a href="SecIntro.html"><strong aria-hidden="true">4.</strong> Introduction</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="SecDocPurpose.html"><strong aria-hidden="true">4.1.</strong> Purpose of Document</a></li><li class="chapter-item expanded "><a href="SecReqsScope.html"><strong aria-hidden="true">4.2.</strong> Scope of Requirements</a></li><li class="chapter-item expanded "><a href="SecReaderChars.html"><strong aria-hidden="true">4.3.</strong> Characteristics of Intended Reader</a></li><li class="chapter-item expanded "><a href="SecDocOrg.html"><strong aria-hidden="true">4.4.</strong> Organization of Document</a></li></ol></li><li class="chapter-item expanded "><a href="SecGenSysDesc.html"><strong aria-hidden="true">5.</strong> General System Description</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="SecSysContext.html"><strong aria-hidden="true">5.1.</strong> System Context</a></li><li class="chapter-item expanded "><a href="SecUserChars.html"><strong aria-hidden="true">5.2.</strong> User Characteristics</a></li><li class="chapter-item expanded "><a href="SecSysConstraints.html"><strong aria-hidden="true">5.3.</strong> System Constraints</a></li></ol></li><li class="chapter-item expanded "><a href="SecSpecSystDesc.html"><strong aria-hidden="true">6.</strong> Specific System Description</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="SecProbDesc.html"><strong aria-hidden="true">6.1.</strong> Problem Description</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="SecTermDefs.html"><strong aria-hidden="true">6.1.1.</strong> Terminology and Definitions</a></li><li class="chapter-item expanded "><a href="SecPhysSyst.html"><strong aria-hidden="true">6.1.2.</strong> Physical System Description</a></li><li class="chapter-item expanded "><a href="SecGoalStmt.html"><strong aria-hidden="true">6.1.3.</strong> Goal Statements</a></li></ol></li><li class="chapter-item expanded "><a href="SecSolCharSpec.html"><strong aria-hidden="true">6.2.</strong> Solution Characteristics Specification</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="SecAssumps.html"><strong aria-hidden="true">6.2.1.</strong> Assumptions</a></li><li class="chapter-item expanded "><a href="SecTMs.html"><strong aria-hidden="true">6.2.2.</strong> Theoretical Models</a></li><li class="chapter-item expanded "><a href="SecGDs.html"><strong aria-hidden="true">6.2.3.</strong> General Definitions</a></li><li class="chapter-item expanded "><a href="SecDDs.html"><strong aria-hidden="true">6.2.4.</strong> Data Definitions</a></li><li class="chapter-item expanded "><a href="SecIMs.html"><strong aria-hidden="true">6.2.5.</strong> Instance Models</a></li><li class="chapter-item expanded "><a href="SecDataConstraints.html"><strong aria-hidden="true">6.2.6.</strong> Data Constraints</a></li><li class="chapter-item expanded "><a href="SecCorSolProps.html"><strong aria-hidden="true">6.2.7.</strong> Properties of a Correct Solution</a></li></ol></li></ol></li><li class="chapter-item expanded "><a href="SecRequirements.html"><strong aria-hidden="true">7.</strong> Requirements</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="SecFRs.html"><strong aria-hidden="true">7.1.</strong> Functional Requirements</a></li><li class="chapter-item expanded "><a href="SecNFRs.html"><strong aria-hidden="true">7.2.</strong> Non-Functional Requirements</a></li></ol></li><li class="chapter-item expanded "><a href="SecLCs.html"><strong aria-hidden="true">8.</strong> Likely Changes</a></li><li class="chapter-item expanded "><a href="SecUCs.html"><strong aria-hidden="true">9.</strong> Unlikely Changes</a></li><li class="chapter-item expanded "><a href="SecTraceMatrices.html"><strong aria-hidden="true">10.</strong> Traceability Matrices and Graphs</a></li><li class="chapter-item expanded "><a href="SecAuxConstants.html"><strong aria-hidden="true">11.</strong> Values of Auxiliary Constants</a></li><li class="chapter-item expanded "><a href="SecReferences.html"><strong aria-hidden="true">12.</strong> References</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0].split("?")[0];
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
