# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration repository using org-mode for literate configuration. The configuration is written in org-mode files that tangle to Emacs Lisp files using org-babel.

## Architecture

### Literate Configuration System
- Configuration is written in `.org` files using org-mode's literate programming features
- Each `.org` file contains Emacs Lisp code blocks that tangle to corresponding `.el` files
- The tangling is controlled by `#+PROPERTY: header-args:elisp :tangle` headers

### Main Configuration Files
- `emacs-config.org` → `emacs-config-new.el`: Core Emacs configuration including package management
- `keys-config.org` → `keys-config-new.el`: Global keyboard bindings and shortcuts
- `load-path-config.org` → `load-path-config-new.el`: Load path setup for custom packages
- `ari-custom.org` → `ari-custom-new.el`: Custom functions and utilities
- `ruby-config.org` → `ruby-config-new.el`: Ruby development configuration
- `mail-config.org` → `mail-config-new.el`: Email configuration (mu4e, etc.)
- `erc-config.el` and `gnus-config.el`: Communication configurations

### Package Management (straight.el only, since 2026-09-26)
- **straight.el is the only package manager.** package.el is never initialized
  (`package-enable-at-startup nil` in early-init). `straight-use-package-by-default t`,
  so every `use-package` installs via straight; use `:straight nil` for built-ins
  (eglot, flymake, project, savehist, which-key, font-lock) and for extensions
  shipped inside another package (vertico-directory, corfu-popupinfo).
- **`straight-built-in-pseudo-packages`** lists libraries straight must never
  install (project, flymake, xref, eglot, eldoc, jsonrpc, seq, map, ...). Without
  it, a dependency pulls in duplicate copies and `require-with-check` breaks eglot
  ("Feature 'project' ... is now provided by ..."). When checking load-path
  shadows, do NOT filter out shadows of Emacs.app files — only org, transient and
  compat should shadow built-ins.
- **Never add `:ensure t` or `:vc`** — they'd bring package.el back and cause duplicate,
  shadowing copies (the 2026-09 review found ~20 of those).
- **Local checkouts** build from `~/dev/git/<repo>` via
  `:straight (:local-repo "~/dev/git/foo" :host github :repo "owner/foo")`
  (agent-shell, flyover, ligature.el, org-block-capf, notdeft). straight clones
  them there on a machine that lacks them.
- **org** tracks the `bugfix` branch (stable 9.8.x), not `main` (10.0 dev).
- **Lockfile**: `config/straight-lockfile.el` (tracked); the config symlinks
  `~/.emacs.d/straight/versions/default.el` to it. Update flow: `straight-pull-all`
  → restart/test → `straight-freeze-versions` → commit. Restore/sync:
  `git pull` → `straight-thaw-versions`.
- Never `git pull` inside `~/.emacs.d/straight/repos/*` by hand.

### Tree-sitter Configuration
- Uses `treesit-auto` package for automatic grammar management
- **ABI-aware grammar selection**: Automatically detects tree-sitter ABI version
  - ABI 14 (tree-sitter 0.20.x on Ubuntu): Uses v0.20.x grammar versions
  - ABI 15+ (tree-sitter 0.22+ on macOS): Uses v0.23.3 grammar versions
- Provides seamless cross-platform compatibility without ABI warnings
- Grammars are automatically installed on first startup

### Early Init (tracked)
- `config/early-init.el` is the ONLY copy to edit. Install per machine with
  `ln -sf ~/emacs/config/early-init.el ~/.emacs.d/early-init.el` (or copy).
- It owns GC thresholds (100MB startup / 50MB after), native-comp (JIT, 2 jobs,
  speed 2) and `read-process-output-max`. Don't set these in the main config —
  an untracked drifted copy once silently overrode them (2026-09 review).

### Startup policy
Only org, org-roam and notdeft load eagerly. Everything else uses
`:defer`/`:commands`/`:mode`/`:hook`/`:magic`. Never enable a buffer-local
minor mode from `with-eval-after-load` (it lands in *scratch*); use `:hook`.

### Load Order
The main entry point appears to be `emacs-config-new.el` which:
1. Sets up package repositories (MELPA, ELPA, NonGNU, MELPA Stable)
2. Initializes `use-package` and `straight.el`
3. Loads `load-path-config-new` for custom paths
4. Sets up GPG agent
5. Configures fonts and appearance

## Development Workflow

### Emacs Binary Location
Currently Emacs 32.0.50 on the Mac.
- **macOS (Work)**: `/Applications/Emacs.app/Contents/MacOS/Emacs`
- **macOS (Home)**: `/Users/abturet/dev/git/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs`
- **Linux**: `/home/abturet/dev/emacs/src/emacs`
- Use these paths for batch mode testing and validation

### Making Configuration Changes
1. Edit the appropriate `.org` file (not the `.el` file directly)
2. Use `C-c C-v t` (org-babel-tangle) to regenerate the `.el` files
3. Reload Emacs or use `M-x eval-buffer` on the generated `.el` file

### Key Shortcuts Defined
- `F4`: goto-line
- `F5`: compile (or gud-cont in debug mode)
- `F12`: write-blog functionality
- `C-c o`: occur
- `C-M-9/C-M-8/C-M-0`: transparency controls (moved from C-9/C-8/C-0 to preserve digit arguments)
- `S-C-arrow keys`: window resizing
- Various helpful-mode bindings for documentation

### Custom Load Paths
The configuration loads additional packages from (with portability checks):
- `~/emacs/site/` subdirectories (color-theme, lisp, ruby-block, blog)
- `~/dev/git/flyover/`
- `~/dev/git/org-block-capf`

Note: All paths now include existence checks to ensure portability across different systems.

## Working with This Repository

### When Editing Configuration
- Always edit the `.org` files, never the generated `.el` files directly
- Use org-babel tangling to regenerate the Elisp files
- The configuration uses lexical binding throughout

### When Adding New Features
- Follow the literate programming style used throughout
- Add appropriate use-package declarations in the relevant `.org` file
- Consider whether new functionality belongs in an existing config file or needs a new one

### Understanding the Structure
- Each `.org` file is self-contained but may reference functions from `ari-custom-new.el`
- The configuration is modular - each major feature area has its own file
- Custom functions and utilities are centralized in `ari-custom.org`

## Recent Improvements (2025)

The following safety and architectural improvements have been implemented:

### Safety Fixes
- **Removed dangerous Pause key binding**: The `[pause] 'erase-buffer` binding was removed as it could accidentally wipe buffers
- **Fixed digit argument conflicts**: Transparency controls moved from `C-8/C-9/C-0` to `C-M-8/C-M-9/C-M-0` to preserve Emacs' standard digit argument functionality

### Architectural Improvements
- **Standardized naming convention**: All configuration files now use consistent `-new` suffix
- **Enhanced portability**: Load path configurations now include existence checks to prevent errors on different systems
- **Improved error handling**: Package loading now checks for availability before requiring

### Performance Optimizations (October 2025)
- **Startup time improvement**: 54-60s → ~10s (**84% faster**)
- **Deferred loading**: 17+ synchronous `require` statements now load on-demand
- **Lazy-loaded packages**: Heavy packages (magit, treemacs, org-roam, doom-themes, etc.) load when needed
- **Package-quickstart**: Single autoload file replaces dozens of individual package autoloads
- **Removed blocking operations**: org-agenda no longer loads at startup
- **Configuration modules**: Non-essential configs (ruby, erc, gnus, mail) load after 2s idle
- **Key optimization**: `with-eval-after-load` for org-mode components prevents expensive synchronous loading

**Performance by Machine:**
- Personal Mac: 9.44s (Emacs 31+)
- Linux Ubuntu: 12.06s (Emacs 31.0.50)
- Work Mac: ~30-40s estimated (from 150s)

**New machine:** symlink early-init (see above) and start Emacs; straight clones and builds everything on first start (several minutes), then `M-x straight-thaw-versions` to match the lockfile.

### Emacs 31 Compatibility Fixes (October 2025)
- **Migrated from quelpa to straight.el**: Removed quelpa-use-package dependency
  - `copilot` now uses `:straight` directive with GitHub recipe
  - `gptel-aibo` now uses `:straight` directive with GitHub recipe
  - Removed quelpa-specific error handling for package-quickstart-refresh
  - Cleaner configuration with unified straight.el package management
- **Fixed package initialization order**: Proper sequencing to avoid hybrid package manager conflicts
  - straight.el loads FIRST (before package.el is required)
  - `(require 'package)` and `(package-initialize)` called AFTER straight.el bootstrap
  - package-archives configured AFTER straight.el loads to avoid autoload triggers
  - Suppressed straight.el hybrid warnings via `warning-suppress-log-types`
  - Eliminates all straight.el/package.el conflict warnings
- **Fixed copilot-chat syntax errors**: Corrected use-package declarations for AI tools
  - Fixed chatgpt-shell `:config` block placement
  - Corrected gptel use-package syntax
  - Added proper deferral for copilot-chat
- **Location**: AI tools configuration in `emacs-config.org:1880-1900`
- **Location**: Package initialization in `emacs-config.org:93-134`

### Configuration Audit & Improvements (February 2026)
- **Fixed retired model name**: `copilot-chat-default-model` updated from `"claude-3.7-sonnet-thought"` (retired Oct 2025) to `"claude-sonnet-4-6"`
- **Removed dead ivy references**: Deleted `(diminish 'ivy-mode)` (ivy not loaded) and changed `org-ref-completion-library` from `'org-ref-ivy-cite` to `'org-ref-capf`
- **Raised post-startup GC threshold**: 2MB → 50MB in both `early-init.el` and `emacs-config.org` to reduce GC pressure during normal editing
- **Enabled eglot semantic tokens**: Added `eglot-semantic-tokens-mode` hook on `eglot-managed-mode-hook` (guarded with `fboundp`) for semantic highlighting from LSP servers
- **Added gptel presets**: Three named presets accessible via `@preset-name` in prompts or the transient menu:
  - `coding` — low temperature (0.2), concise code-focused system prompt
  - `writing` — higher temperature (0.7), org/note-friendly system prompt
- **Switched corfu icons**: Replaced `kind-icon` (SVG, GUI-only) with `nerd-icons-corfu` (nerd font glyphs) for visual consistency with doom-modeline and nerd-icons-completion