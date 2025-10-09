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

### Package Management
- Uses both traditional `package.el` with MELPA/ELPA repositories
- Also uses `straight.el` for more advanced package management
- `use-package` for clean package configuration

### Tree-sitter Configuration
- Uses `treesit-auto` package for automatic grammar management
- **ABI-aware grammar selection**: Automatically detects tree-sitter ABI version
  - ABI 14 (tree-sitter 0.20.x on Ubuntu): Uses v0.20.x grammar versions
  - ABI 15+ (tree-sitter 0.22+ on macOS): Uses v0.23.3 grammar versions
- Provides seamless cross-platform compatibility without ABI warnings
- Grammars are automatically installed on first startup

### Load Order
The main entry point appears to be `emacs-config-new.el` which:
1. Sets up package repositories (MELPA, ELPA, NonGNU, MELPA Stable)
2. Initializes `use-package` and `straight.el`
3. Loads `load-path-config-new` for custom paths
4. Sets up GPG agent
5. Configures fonts and appearance

## Development Workflow

### Emacs Binary Location
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
- `~/dev/git/lsp-bridge/`
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

**To regenerate package-quickstart on new machines:**
```bash
# Mac:
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "(require 'package)" --eval "(package-quickstart-refresh)"

# Linux:
/home/abturet/dev/emacs/src/emacs --batch --eval "(require 'package)" --eval "(package-quickstart-refresh)"
```