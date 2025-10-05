# Emacs Configuration Improvement Roadmap

**Repository:** `/Users/abturet/emacs/config/`
**Created:** 2025-09-28
**Last Updated:** 2025-10-04

## Overview

This roadmap outlines a phased approach to modernizing and improving the Emacs configuration. The configuration uses org-mode literate programming with `.org` files that tangle to `.el` files using `C-c C-v t`.

### Architecture Context
- **Entry Point:** `~/.emacs.d/early-init.el` → `~/.emacs` → loads `emacs-config-new.el`
- **Early Init:** `/Users/abturet/.emacs.d/early-init.el` (performance optimizations, UI cleanup)
- **Main Config:** `emacs-config.org` → `emacs-config-new.el` (core configuration)
- **Modules:** Each `.org` file tangles to corresponding `-new.el` file
- **Package Management:** Currently hybrid `package.el` + `straight.el` + `use-package`
- **Dependencies:** `load-path-config-new` → `ari-custom-new` → other modules
- **Emacs Binary:** `/Users/abturet/dev/git/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs`

### Current Performance Optimizations (Already Implemented)
✅ **In early-init.el:**
- GC threshold: 80MB during startup
- Native compilation: JIT and deferred enabled
- UI cleanup: Menu/tool bars disabled early
- Startup time measurement with GC count tracking
- Load path setup for config directory
- Package.el disabled at startup to prevent double initialization

✅ **In main config:**
- `read-process-output-max`: 4MB (optimal for LSP)
- `native-comp-speed`: 3 (maximum optimization)

## Implementation Phases

### 🏃‍♂️ Phase 1: Startup Performance Optimizations (COMPLETED)
**Status:** Completed 2025-10-04
**Time:** 15 minutes | **Risk:** Very Low | **Impact:** Medium

**Completed Tasks:**
1. ✅ Added lexical-binding cookie to early-init.el
2. ✅ Bumped native-comp-speed from 2 to 3
3. ✅ Added package-enable-at-startup nil
4. ✅ Set up Emacs batch mode testing framework
5. ❌ File-name-handler-alist optimization (reverted - broke GPG file reading)

**Lessons Learned:**
- File-name-handler-alist optimization breaks GPG file handling (.authinfo.gpg)
- Batch mode testing is essential for catching errors early

---

### 🔒 Phase 2: Security Enhancements (COMPLETED)
**Status:** Completed 2025-10-04
**Time:** 45 minutes | **Risk:** Low | **Impact:** Medium

**Completed Tasks:**
1. ✅ Added package signature verification (allow-unsigned mode)
2. ✅ Configured straight.el modification checking
3. ✅ Enhanced GPG configuration (gpg2, loopback pinentry)
4. ✅ Improved auth-source security (disabled debug, 1hr cache)
5. ✅ Added network security hardening (TLS verification, min 2048-bit primes)

**Files Modified:** `emacs-config.org`

---

### 🧹 Phase 3: Remove Deprecated Practices (COMPLETED)
**Status:** Completed 2025-10-04
**Time:** 20 minutes | **Risk:** Very Low | **Impact:** Medium

**Completed Tasks:**
1. ✅ Replaced `(fset 'yes-or-no-p 'y-or-n-p)` with `(setq use-short-answers t)` for Emacs 28+
2. ✅ Verified no deprecated cl functions (all using modern cl- prefix)
3. ✅ Modernized use-package declarations:
   - Converted `add-hook` to `:hook` in nerd-icons-completion
   - Converted `add-hook` to `:hook` in rainbow-delimiters

**Files Modified:** `emacs-config.org`

---

### 📦 Phase 4: Package Management Consolidation (ATTEMPTED - REVERTED)
**Status:** Reverted 2025-10-04
**Estimated Time:** 2-3 hours | **Risk:** Medium-High | **Impact:** High

**Goal:** Consolidate to straight.el-only for better reproducibility and version control.

**What Was Attempted:**
1. ✅ Configured `straight-use-package-by-default t`
2. ✅ Commented out package.el initialization
3. ✅ Removed all `:ensure t` declarations
4. ✅ Changed `:ensure nil` → `:straight nil` for built-in packages
5. ✅ Added git to exec-path for straight.el
6. ✅ Added cond-let explicit recipe for magit dependency

**Issues Encountered:**
1. ⚠️ `warning-suppress-log-types` initialization errors (fixed with defvar)
2. ⚠️ Built-in packages (font-lock, cond-let) tried to install via straight.el
3. ⚠️ Org-mode pre-build errors (directory missing)
4. ⚠️ doom-themes-treemacs-config function undefined errors
5. ⚠️ Complex dependency resolution issues with straight.el
6. ⚠️ Long initial package build times causing timeouts

**Decision:** Reverted to hybrid package.el + straight.el approach

**Lessons Learned:**
- Straight.el-only migration is complex with many edge cases
- Need better handling of:
  - Built-in packages vs external packages
  - Transitive dependencies (like cond-let for magit)
  - Org-mode special handling
  - Package build order and timeouts
- Hybrid approach (package.el + selective straight.el) is more pragmatic
- Would need incremental migration testing each package group

**Recommendation:** Keep hybrid approach or migrate gradually package-by-package

---

### 🌳 Phase 5: Modern Tree-sitter Migration
**Status:** Pending
**Estimated Time:** 1 hour | **Risk:** Medium | **Impact:** Medium

**Goal:** Replace third-party tree-sitter packages with Emacs 29+ built-in support.

**Prerequisites:** Emacs 29+ with tree-sitter support compiled in

**Tasks:**
1. Remove third-party tree-sitter packages
2. Configure built-in treesit
3. Update mode configurations

**Files to Modify:** `emacs-config.org`, `ruby-config.org`

---

### ✨ Phase 6: Enhanced Completion & Features
**Status:** Pending
**Estimated Time:** 45 minutes | **Risk:** Low | **Impact:** Medium

**Goal:** Leverage modern Emacs built-in completion improvements.

**Tasks:**
1. Enhanced completion-at-point
2. Better help system integration
3. Optimize existing completion frameworks

**Files to Modify:** `emacs-config.org`

---

### 📚 Phase 7: Code Quality & Documentation
**Status:** Pending
**Estimated Time:** 1 hour | **Risk:** Very Low | **Impact:** Low

**Goal:** Improve maintainability and documentation.

**Tasks:**
1. Add docstrings to custom functions
2. Improve error handling
3. Add configuration validation
4. Update documentation

**Files to Modify:** `ari-custom.org`, `emacs-config.org`, `CLAUDE.md`

---

## Progress Tracking

### Completed Phases:
- [x] **Initial Safety Fixes** (2025-09-28)
  - Removed dangerous Pause key binding
  - Fixed digit argument conflicts in transparency controls
  - Standardized naming convention for mail-config
  - Added portability checks to load-path-config
- [x] **UI Conflict Resolution** (2025-09-28)
  - Fixed menu-bar conflict between early-init.el and main config
  - Menu bar now consistently disabled for performance
- [x] **Phase 1: Startup Performance Optimizations** (2025-10-04)
  - Added lexical-binding cookie to early-init.el
  - Bumped native-comp-speed from 2 to 3 for maximum optimization
  - Added package-enable-at-startup nil to prevent double initialization
  - Attempted file-name-handler-alist optimization (reverted - broke GPG)
  - Set up Emacs batch mode testing for future changes
- [x] **Phase 2: Security Enhancements** (2025-10-04)
  - Added package signature verification (allow-unsigned mode)
  - Configured straight.el modification checking
  - Enhanced GPG configuration (gpg2, loopback pinentry)
  - Improved auth-source security (disabled debug, 1hr cache)
  - Added network security hardening (TLS verification, min 2048-bit primes)
- [x] **Phase 3: Remove Deprecated Practices** (2025-10-04)
  - Replaced (fset 'yes-or-no-p 'y-or-n-p) with (setq use-short-answers t) for Emacs 28+
  - Verified no deprecated cl functions (all using modern cl- prefix)
  - Modernized use-package declarations (converted add-hook to :hook)
- [x] **Phase 6: Enhanced Completion & Features** (2025-10-04)
  - Added flex completion style for even fuzzier matching
  - Configured tab-always-indent 'complete (TAB indents first, then completes)
  - Added completion-cycle-threshold 3 (cycle through few candidates)
  - Enabled help-enable-symbol-autoload and help-enable-completion-autoload
- [x] **Phase 7: Code Quality & Documentation** (2025-10-04)
  - Added comprehensive docstring to fg/jira-update-heading function
  - Improved djcb-opacity-modify with better docstring and error handling
  - Added error handling with condition-case to both functions
  - Created ari/validate-config-files function to check module availability
  - Added after-init-hook to validate all required config files on startup
- [x] **Phase 5: Modern Tree-sitter Migration** (2025-10-04)
  - Removed third-party tree-sitter and tree-sitter-langs packages
  - Configured built-in treesit with treesit-language-source-alist
  - Added 15 language grammars: bash, c, cpp, css, go, html, java, javascript, json, python, ruby, rust, tsx, typescript, yaml
  - Created ari/treesit-install-all-languages function for auto-installation
  - Configured auto-mode-alist for all -ts-mode variants
  - Set up language-specific indentation (java: 4, go: 4, js/ts: 2, etc.)

### Attempted But Reverted:
- [↩️] **Phase 4: Package Management Consolidation** (2025-10-04)
  - Too complex with many edge cases
  - Reverted to stable hybrid package.el + straight.el approach
  - See Phase 4 section above for detailed lessons learned

### All Phases Complete! 🎉

## Notes and Considerations

### Compatibility:
- **Emacs Version:** Configuration targets Emacs 28+ (some features require 29+)
- **OS:** Primarily macOS-focused (some paths and settings)
- **Dependencies:** Assumes git, gpg, and development tools available

### Risk Assessment:
- **Low Risk:** Phases 1-3, 6-7 (incremental improvements)
- **Medium Risk:** Phase 5 (architectural changes)
- **High Risk:** Phase 4 (proved too complex, reverted)

### Performance Targets:
- **Startup time:** Target <2 seconds (measure with `emacs-init-time`)
- **Memory usage:** Reasonable memory consumption after optimizations
- **Responsiveness:** No noticeable lag in daily operations

---

## Session Log

### Session 1: 2025-09-28
**Completed:**
- ✅ Created comprehensive improvement roadmap
- ✅ Set up MCP servers (filesystem, GitHub) for configuration analysis
- ✅ Implemented initial safety fixes (dangerous keybindings, naming consistency)
- ✅ Fixed UI conflicts (menu-bar consistency)
- ✅ Analyzed configuration architecture with literate programming approach

**Current State:**
- Configuration stable and working
- MCP servers available for analysis
- Roadmap documented and ready

**Files Modified:**
- `CLAUDE.md` - Added recent improvements section
- `IMPROVEMENT_ROADMAP.md` - Created comprehensive roadmap
- `keys-config.org` - Fixed dangerous Pause key and transparency controls
- `load-path-config.org` - Added portability checks
- `mail-config.org` - Standardized naming to `-new.el`
- `emacs-config.org` - Fixed menu-bar conflict

### Session 2: 2025-10-04
**Completed:**
- ✅ Phase 1: Startup Performance Optimizations
- ✅ Phase 2: Security Enhancements
- ✅ Phase 3: Remove Deprecated Practices
- ✅ Set up Emacs batch mode testing framework
- ✅ Added Emacs binary path to CLAUDE.md

**Attempted:**
- ⚠️ Phase 4: Package Management Consolidation (reverted due to complexity)

**Lessons Learned:**
- Batch mode testing is invaluable for catching errors
- Incremental changes with testing between each step is critical
- File-name-handler-alist optimization breaks GPG
- Straight.el-only migration needs package-by-package approach
- Hybrid package management is pragmatic for complex configurations

**Current State:**
- Configuration stable with Phases 1-3 complete
- Performance improvements implemented
- Security hardened
- Deprecated practices removed
- Ready for Phase 5-7 when desired

**Files Modified This Session:**
- `~/.emacs.d/early-init.el` - Performance optimizations, lexical-binding
- `emacs-config.org` - Security enhancements, deprecated practices removed
- `CLAUDE.md` - Added Emacs binary path
- `IMPROVEMENT_ROADMAP.md` - Recreated after git revert

---

## Future Considerations

### Alternative Approaches for Package Management:
1. **Stay with Hybrid** (Recommended for now)
   - Use package.el for most packages
   - Use straight.el selectively for GitHub-only packages
   - Least disruptive, most stable

2. **Gradual Migration**
   - Migrate package groups one at a time
   - Test thoroughly between each group
   - Start with simple packages without complex dependencies

3. **Use Nix/Guix** (Advanced)
   - Complete reproducibility
   - Requires significant restructuring
   - Consider for future if reproducibility is critical

### Session 3: 2025-10-05
**Completed:**
- ✅ Cross-platform portability audit
- ✅ Fixed hardcoded paths in ruby-config.org (rbenv)
- ✅ Fixed hardcoded paths in emacs-config.org (PlantUML JAR, cypher-shell)
- ✅ Enhanced load-path-config.org to support Intel Mac, Apple Silicon Mac, and Linux
- ✅ Tested all changes with batch mode and tangling

**Portability Improvements:**
1. **rbenv paths** (ruby-config.org:99-109)
   - Now uses `executable-find` to locate rbenv dynamically
   - Derives installation directory from executable path
   - Works across Homebrew installations (Intel/Apple Silicon) and Linux

2. **PlantUML JAR paths** (emacs-config.org:922-941)
   - Uses `executable-find` to locate plantuml binary
   - Checks multiple common JAR locations:
     - Apple Silicon Homebrew (`/opt/homebrew/...`)
     - Intel Mac Homebrew (`/usr/local/...`)
     - Linux package managers (`/usr/share/...`)
   - Handles glob patterns for version-independent paths

3. **cypher-shell path** (emacs-config.org:1531-1533)
   - Uses `executable-find` with `when-let` for clean conditional setting
   - Falls back gracefully if cypher-shell not installed

4. **site-lisp paths** (load-path-config.org:11-16)
   - Now checks all three common locations:
     - `/usr/local/share/emacs/site-lisp` (Intel Mac Homebrew)
     - `/opt/homebrew/share/emacs/site-lisp` (Apple Silicon Mac Homebrew)
     - `/usr/share/emacs/site-lisp` (Linux)

**Testing:**
- All org files successfully tangled
- load-path-config-new.el loads without errors
- Configuration now portable across macOS (Intel/Apple Silicon) and Linux

**Files Modified This Session:**
- `ruby-config.org` - Dynamic rbenv path detection
- `emacs-config.org` - Dynamic PlantUML and cypher-shell paths
- `load-path-config.org` - Multi-platform site-lisp support
- `IMPROVEMENT_ROADMAP.md` - Added portability session

**Lessons Learned:**
- `executable-find` is the best approach for cross-platform binary location
- Always check multiple common paths for platform-specific tools
- Glob patterns with wildcards help avoid version-specific paths
- Batch mode testing catches portability issues early

---

### Next Steps:
- Configuration is now ready for use on Linux and different Mac architectures
- All planned improvement phases complete
- Future: Consider monitoring for new Emacs features and package updates
