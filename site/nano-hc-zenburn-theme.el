;;; nano-hc-zenburn-theme.el --- Nano aesthetic with hc-zenburn palette -*- lexical-binding: t -*-
;;
;; A standalone Emacs theme combining nano-emacs visual design with
;; the hc-zenburn color palette. Does NOT require nano-theme or nano-emacs.
;;
;; Load with: (load-theme 'nano-hc-zenburn t)
;;
;;; Code:

(deftheme nano-hc-zenburn "Nano aesthetic with hc-zenburn colors.")

;; ---- hc-zenburn palette -------------------------------------------------
(let* ((fg       "#DCDCCC")   ; hc-zenburn-fg
       (fg+1     "#FFFFEF")   ; hc-zenburn-fg+1
       (fg-1     "#70705E")   ; hc-zenburn-fg-1  (faded text)
       (bg       "#313131")   ; hc-zenburn-bg
       (bg-1     "#202020")   ; hc-zenburn-bg-1
       (bg+05    "#383838")   ; hc-zenburn-bg+05
       (bg+1     "#3E3E3E")   ; hc-zenburn-bg+1  (subtle)
       (bg+2     "#4E4E4E")   ; hc-zenburn-bg+2  (highlight)
       (bg+3     "#5E5E5E")   ; hc-zenburn-bg+3
       (red+1    "#E9B0B0")   ; hc-zenburn-red+1
       (red      "#D9A0A0")   ; hc-zenburn-red
       (red-1    "#C99090")   ; hc-zenburn-red-1
       (orange   "#ECBC9C")   ; hc-zenburn-orange
       (yellow   "#FDECBC")   ; hc-zenburn-yellow
       (yellow-1 "#EDDCAC")   ; hc-zenburn-yellow-1
       (green-1  "#6C8C6C")   ; hc-zenburn-green-1
       (green    "#8CAC8C")   ; hc-zenburn-green
       (green+1  "#9CBF9C")   ; hc-zenburn-green+1
       (cyan     "#A0EDF0")   ; hc-zenburn-cyan
       (blue     "#99DDE0")   ; hc-zenburn-blue
       (blue+1   "#9CC7FB")   ; hc-zenburn-blue+1
       (magenta  "#E090C7")   ; hc-zenburn-magenta

       ;; ---- nano semantic roles ----------------------------------------
       ;; foreground/background
       (n-fg     fg)
       (n-bg     bg)
       ;; highlight: slightly elevated bg for hl-line / selection
       (n-hl     bg+2)
       ;; subtle: barely-there bg for regions / tooltips
       (n-sub    bg+1)
       ;; faded: de-emphasized text (comments, secondary info)
       (n-fad    fg-1)
       ;; salient: important info, links, keywords (#8CAC8C green)
       (n-sal    green)
       ;; strong: structural text, headings (#FDECBC yellow)
       (n-str    yellow)
       ;; popout: attention-grabbing text, strings (#ECBC9C orange)
       (n-pop    orange)
       ;; critical: errors, urgent (#D9A0A0 red)
       (n-cri    red))

  (custom-theme-set-variables
   'nano-hc-zenburn
   '(widget-image-enable nil)
   '(x-underline-at-descent-line t))

  ;; Set nano-color-* variables so nano-modeline / nano-agenda pick them up
  (custom-theme-set-variables
   'nano-hc-zenburn
   `(nano-color-foreground ,n-fg)
   `(nano-color-background ,n-bg)
   `(nano-color-highlight  ,n-hl)
   `(nano-color-critical   ,n-cri)
   `(nano-color-salient    ,n-sal)
   `(nano-color-strong     ,n-str)
   `(nano-color-popout     ,n-pop)
   `(nano-color-subtle     ,n-sub)
   `(nano-color-faded      ,n-fad)
   `(nano-light-foreground ,n-fg)
   `(nano-light-background ,n-bg)
   `(nano-light-popout     ,n-pop)
   `(nano-light-faded      ,n-fad))

  (custom-theme-set-faces
   'nano-hc-zenburn

   ;; ---- Base / default -------------------------------------------------
   `(default          ((t (:background ,n-bg :foreground ,n-fg))))
   `(cursor           ((t (:background ,n-fg :foreground ,n-bg))))
   `(mouse            ((t (:background ,n-bg :foreground ,n-fg))))
   `(fringe           ((t (:background ,n-bg :foreground ,n-fad))))
   `(highlight        ((t (:background ,n-hl))))
   `(hl-line          ((t (:background ,n-hl))))
   `(region           ((t (:background ,n-sub :extend t))))
   `(secondary-selection ((t (:background ,n-sub))))
   `(trailing-whitespace ((t (:background ,n-sub))))

   ;; ---- Nano semantic faces -------------------------------------------
   `(nano-default     ((t (:foreground ,n-fg))))
   `(nano-default-i   ((t (:foreground ,n-bg :background ,n-fg))))
   `(nano-subtle      ((t (:background ,n-sub))))
   `(nano-subtle-i    ((t (:foreground ,n-sub))))
   `(nano-faded       ((t (:foreground ,n-fad))))
   `(nano-faded-i     ((t (:foreground ,n-bg :background ,n-fad))))
   `(nano-salient     ((t (:foreground ,n-sal))))
   `(nano-salient-i   ((t (:foreground ,n-bg :background ,n-sal))))
   `(nano-strong      ((t (:foreground ,n-str :weight bold))))
   `(nano-strong-i    ((t (:foreground ,n-bg :background ,n-str :weight normal))))
   `(nano-popout      ((t (:foreground ,n-pop))))
   `(nano-popout-i    ((t (:foreground ,n-bg :background ,n-pop))))
   `(nano-critical    ((t (:foreground ,n-cri :weight bold))))
   `(nano-critical-i  ((t (:foreground ,n-bg :background ,n-cri :weight normal))))

   ;; ---- Mode / header line --------------------------------------------
   `(mode-line         ((t (:background ,bg+2 :foreground ,n-fg
                             :box (:line-width 3 :color ,bg+2 :style nil)))))
   `(mode-line-inactive ((t (:background ,bg+1 :foreground ,n-fad
                              :box (:line-width 3 :color ,bg+1 :style nil)))))
   `(mode-line-highlight ((t (:inherit nano-popout))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis  ((t (:weight bold))))
   `(header-line         ((t (:background ,n-sub :foreground ,n-fg
                               :inherit nil :box nil))))

   ;; ---- nano-modeline faces -------------------------------------------
   `(nano-modeline-active
     ((t (:background ,bg+2 :foreground ,n-fg :box nil))))
   `(nano-modeline-inactive
     ((t (:background ,bg+1 :foreground ,n-fad :box nil))))
   `(nano-modeline-status
     ((t (:background ,green-1 :foreground ,n-fg :weight bold))))
   '(nano-modeline-active-name
     ((t (:inherit (nano-strong nano-modeline-active)))))
   '(nano-modeline-active-primary
     ((t (:inherit (nano-default nano-modeline-active)))))
   '(nano-modeline-active-secondary
     ((t (:inherit (nano-faded nano-modeline-active)))))
   '(nano-modeline-active-status-RO
     ((t (:inherit (nano-subtle nano-strong)))))
   '(nano-modeline-active-status-RW
     ((t (:inherit (nano-faded-i nano-strong)))))
   '(nano-modeline-active-status-**
     ((t (:inherit (nano-popout-i nano-strong)))))
   '(nano-modeline-inactive-name
     ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-primary
     ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-secondary
     ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-RO
     ((t (:inherit (nano-faded nano-strong nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-RW
     ((t (:inherit (nano-faded nano-strong nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-**
     ((t (:inherit (nano-popout nano-strong nano-modeline-inactive)))))

   ;; ---- Window divider ------------------------------------------------
   `(window-divider             ((t (:foreground ,n-bg))))
   `(window-divider-first-pixel ((t (:foreground ,n-bg))))
   `(window-divider-last-pixel  ((t (:foreground ,n-bg))))
   `(vertical-border            ((t (:foreground ,n-bg))))

   ;; ---- Structural ----------------------------------------------------
   ;; bold/italic are NOT redirected through nano-strong: doing so creates an
   ;; inheritance cycle because nano-strong carries :weight bold directly.
   '(link            ((t (:inherit nano-salient))))
   '(fixed-pitch     ((t (:inherit default))))
   '(fixed-pitch-serif ((t (:inherit default))))

   ;; ---- Semantic ------------------------------------------------------
   '(shadow   ((t (:inherit nano-faded))))
   '(success  ((t (:inherit nano-salient))))
   '(warning  ((t (:inherit nano-popout))))
   '(error    ((t (:inherit nano-critical))))
   '(match    ((t (:inherit nano-popout))))

   ;; ---- Line numbers --------------------------------------------------
   '(line-number               ((t (:inherit nano-faded))))
   '(line-number-current-line  ((t (:inherit (nano-strong hl-line)))))
   '(line-number-major-tick    ((t (:inherit nano-default))))
   '(line-number-minor-tick    ((t (:inherit nano-faded))))

   ;; ---- General UI ----------------------------------------------------
   '(minibuffer-prompt          ((t (:inherit nano-strong))))
   '(isearch                    ((t (:inherit nano-strong))))
   '(isearch-fail               ((t (:inherit nano-faded))))
   '(lazy-highlight             ((t (:inherit nano-subtle))))
   '(show-paren-match           ((t (:inherit nano-strong))))
   '(show-paren-mismatch        ((t (:inherit nano-critical))))
   '(completions-annotations    ((t (:inherit nano-faded))))
   '(completions-common-part    ((t (:inherit nano-strong))))
   '(completions-first-difference ((t (:inherit nano-default))))
   '(tooltip                    ((t (:inherit nano-subtle))))
   '(help-argument-name         ((t (:inherit nano-faded))))
   '(tabulated-list-fake-header ((t (:inherit nano-strong))))
   '(buffer-menu-buffer         ((t (:inherit nano-strong))))

   ;; ---- Tab bar -------------------------------------------------------
   '(tab-bar              ((t (:inherit default))))
   '(tab-bar-tab          ((t (:inherit default))))
   '(tab-bar-tab-inactive ((t (:inherit nano-faded))))
   '(tab-line             ((t (:inherit default))))

   ;; ---- Font lock -----------------------------------------------------
   '(font-lock-comment-face        ((t (:inherit nano-faded))))
   '(font-lock-doc-face            ((t (:inherit nano-faded))))
   '(font-lock-string-face         ((t (:inherit nano-popout))))
   '(font-lock-constant-face       ((t (:inherit nano-salient))))
   '(font-lock-warning-face        ((t (:inherit nano-popout))))
   '(font-lock-function-name-face  ((t (:inherit nano-strong))))
   '(font-lock-variable-name-face  ((t (:inherit nano-default))))
   '(font-lock-builtin-face        ((t (:inherit nano-salient))))
   '(font-lock-type-face           ((t (:inherit nano-salient))))
   '(font-lock-keyword-face        ((t (:inherit nano-salient))))

   ;; ---- Diff-HL -------------------------------------------------------
   '(diff-hl-change  ((t (:inherit nano-faded-i))))
   '(diff-hl-insert  ((t (:inherit nano-salient-i))))
   '(diff-hl-delete  ((t (:inherit nano-critical-i))))

   ;; ---- Org -----------------------------------------------------------
   '(org-level-1                  ((t (:inherit nano-strong))))
   '(org-level-2                  ((t (:inherit nano-strong))))
   '(org-level-3                  ((t (:inherit nano-strong))))
   '(org-level-4                  ((t (:inherit nano-strong))))
   '(org-level-5                  ((t (:inherit nano-strong))))
   '(org-level-6                  ((t (:inherit nano-strong))))
   '(org-level-7                  ((t (:inherit nano-strong))))
   '(org-level-8                  ((t (:inherit nano-strong))))
   '(org-link                     ((t (:inherit nano-salient))))
   '(org-todo                     ((t (:inherit nano-salient))))
   '(org-done                     ((t (:inherit nano-faded))))
   '(org-headline-done            ((t (:inherit nano-faded))))
   '(org-tag                      ((t (:inherit nano-popout))))
   '(org-date                     ((t (:inherit nano-faded))))
   '(org-code                     ((t (:inherit nano-salient))))
   '(org-verbatim                 ((t (:inherit nano-popout))))
   '(org-block                    ((t (:inherit highlight))))
   '(org-block-begin-line         ((t (:inherit nano-faded))))
   '(org-block-end-line           ((t (:inherit nano-faded))))
   '(org-meta-line                ((t (:inherit nano-faded))))
   '(org-document-title           ((t (:inherit nano-strong))))
   '(org-document-info            ((t (:inherit nano-faded))))
   '(org-document-info-keyword    ((t (:inherit nano-faded))))
   '(org-drawer                   ((t (:inherit nano-faded))))
   '(org-property-value           ((t (:inherit nano-faded))))
   '(org-special-keyword          ((t (:inherit nano-faded))))
   '(org-table                    ((t (:inherit nano-faded))))
   '(org-formula                  ((t (:inherit nano-faded))))
   '(org-scheduled                ((t (:inherit nano-faded))))
   '(org-scheduled-today          ((t (:inherit nano-default))))
   '(org-upcoming-deadline        ((t (:inherit nano-popout))))
   '(org-warning                  ((t (:inherit nano-popout))))
   '(org-agenda-structure         ((t (:inherit nano-strong))))
   '(org-agenda-date              ((t (:inherit nano-strong))))
   '(org-agenda-date-today        ((t (:inherit (nano-salient nano-strong)))))
   '(org-agenda-date-weekend      ((t (:inherit nano-faded))))
   '(org-agenda-done              ((t (:inherit nano-faded))))
   '(org-agenda-current-time      ((t (:inherit (nano-strong nano-salient)))))

   ;; ---- Vertico -------------------------------------------------------
   '(vertico-current         ((t (:inherit (nano-strong nano-subtle)))))
   '(vertico-group-separator ((t (:inherit nano-faded))))
   '(vertico-group-title     ((t (:inherit nano-faded))))
   '(vertico-multiline       ((t (:inherit nano-faded))))

   ;; ---- Marginalia ----------------------------------------------------
   '(marginalia-documentation ((t (:inherit nano-faded))))
   '(marginalia-key           ((t (:inherit nano-faded))))
   '(marginalia-type          ((t (:inherit nano-faded))))
   '(marginalia-value         ((t (:inherit nano-faded))))
   '(marginalia-size          ((t (:inherit nano-faded))))
   '(marginalia-date          ((t (:inherit nano-faded))))
   '(marginalia-mode          ((t (:inherit nano-faded))))
   '(marginalia-on            ((t (:inherit nano-salient))))
   '(marginalia-off           ((t (:inherit nano-faded))))

   ;; ---- Corfu ---------------------------------------------------------
   '(corfu-annotations ((t (:inherit nano-faded))))
   '(corfu-bar         ((t (:inherit nano-default-i))))
   '(corfu-border      ((t (:inherit nano-default-i))))
   '(corfu-current     ((t (:inherit highlight))))
   '(corfu-default     ((t (:inherit nano-subtle))))
   '(corfu-deprecated  ((t (:inherit nano-faded))))
   '(corfu-echo        ((t (:inherit nano-faded))))

   ;; ---- Orderless -----------------------------------------------------
   '(orderless-match-face-0 ((t (:inherit (nano-salient nano-strong)))))
   '(orderless-match-face-1 ((t (:inherit nano-strong))))
   '(orderless-match-face-2 ((t (:inherit nano-strong))))
   '(orderless-match-face-3 ((t (:inherit nano-strong))))

   ;; ---- Diff ----------------------------------------------------------
   '(diff-header         ((t (:inherit nano-faded))))
   '(diff-file-header    ((t (:inherit nano-strong))))
   '(diff-context        ((t (:inherit nano-default))))
   '(diff-removed        ((t (:inherit nano-faded))))
   '(diff-changed        ((t (:inherit nano-popout))))
   '(diff-added          ((t (:inherit nano-salient))))
   '(diff-refine-added   ((t (:inherit (nano-salient nano-strong)))))
   '(diff-refine-changed ((t (:inherit nano-popout))))
   '(diff-refine-removed ((t (:inherit nano-faded :strike-through t))))

   ;; ---- Magit ---------------------------------------------------------
   '(magit-section-highlight          ((t (:inherit highlight))))
   '(magit-section-heading            ((t (:inherit (nano-salient nano-strong)))))
   '(magit-diff-file-heading          ((t (:inherit nano-strong))))
   '(magit-diff-hunk-heading          ((t (:inherit (nano-subtle nano-default)))))
   '(magit-diff-hunk-heading-highlight ((t (:inherit nano-default))))
   '(magit-diff-added                 ((t (:inherit (nano-salient nano-strong)))))
   '(magit-diff-added-highlight       ((t (:inherit (highlight nano-salient nano-strong)))))
   '(magit-diff-removed               ((t (:inherit (nano-popout nano-strong)))))
   '(magit-diff-removed-highlight     ((t (:inherit (highlight nano-popout nano-strong)))))
   '(magit-diff-context               ((t (:inherit nano-faded))))
   '(magit-diff-context-highlight     ((t (:inherit (highlight nano-faded)))))
   '(magit-branch-current             ((t (:inherit (nano-strong nano-salient)))))
   '(magit-branch-local               ((t (:inherit nano-salient))))
   '(magit-branch-remote              ((t (:inherit nano-salient))))
   '(magit-hash                       ((t (:inherit nano-faded))))
   '(magit-tag                        ((t (:inherit nano-strong))))
   '(magit-dimmed                     ((t (:inherit nano-faded))))
   '(magit-blame-heading              ((t (:inherit (nano-subtle nano-strong)))))
   '(magit-blame-hash                 ((t (:inherit nano-faded))))

   ;; ---- Mu4e ----------------------------------------------------------
   '(mu4e-unread-face       ((t (:inherit nano-strong))))
   '(mu4e-contact-face      ((t (:inherit nano-salient))))
   '(mu4e-flagged-face      ((t (:inherit nano-salient))))
   '(mu4e-header-key-face   ((t (:inherit nano-strong))))
   '(mu4e-header-title-face ((t (:inherit nano-strong))))
   '(mu4e-highlight-face    ((t (:inherit nano-popout))))
   '(mu4e-link-face         ((t (:inherit nano-salient))))
   '(mu4e-replied-face      ((t (:inherit nano-default))))
   '(mu4e-trashed-face      ((t (:inherit nano-faded))))
   '(mu4e-warning-face      ((t (:inherit nano-popout))))
   '(mu4e-cited-1-face      ((t (:inherit nano-faded))))
   '(mu4e-system-face       ((t (:inherit nano-faded))))

   ;; ---- flyspell / flycheck -------------------------------------------
   '(flyspell-duplicate  ((t (:inherit nano-popout :underline t))))
   '(flyspell-incorrect  ((t (:inherit nano-critical :underline t))))
   '(flycheck-error      ((t (:inherit nano-critical :underline t))))
   '(flycheck-warning    ((t (:inherit nano-popout :underline t))))
   '(flycheck-info       ((t (:inherit nano-salient :underline t))))

   ;; ---- Markdown ------------------------------------------------------
   '(markdown-header-face    ((t (:inherit nano-strong))))
   '(markdown-header-face-1  ((t (:inherit nano-strong))))
   '(markdown-header-face-2  ((t (:inherit nano-strong))))
   '(markdown-header-face-3  ((t (:inherit nano-strong))))
   '(markdown-bold-face      ((t (:inherit nano-strong))))
   '(markdown-italic-face    ((t (:inherit nano-faded))))
   '(markdown-code-face      ((t (:inherit nano-default))))
   '(markdown-inline-code-face ((t (:inherit nano-popout))))
   '(markdown-link-face      ((t (:inherit nano-salient))))
   '(markdown-url-face       ((t (:inherit nano-salient))))
   '(markdown-comment-face   ((t (:inherit nano-faded))))
   '(markdown-markup-face    ((t (:inherit nano-faded))))

   ;; ---- SHR / EWW -----------------------------------------------------
   '(shr-text   ((t (:inherit nano-default))))
   '(shr-link   ((t (:inherit nano-salient))))
   '(shr-h1     ((t (:inherit nano-strong))))
   '(shr-h2     ((t (:inherit nano-strong))))
   '(shr-h3     ((t (:inherit nano-strong))))

   ;; ---- Outline -------------------------------------------------------
   '(outline-1 ((t (:inherit nano-strong))))
   '(outline-2 ((t (:inherit nano-strong))))
   '(outline-3 ((t (:inherit nano-strong))))
   '(outline-4 ((t (:inherit nano-strong))))
   '(outline-5 ((t (:inherit nano-strong))))
   '(outline-6 ((t (:inherit nano-strong))))
   '(outline-7 ((t (:inherit nano-strong))))
   '(outline-8 ((t (:inherit nano-strong))))

   ;; ---- Custom / Widget -----------------------------------------------
   '(widget-field              ((t (:inherit nano-subtle))))
   '(widget-button             ((t (:inherit nano-strong))))
   '(custom-group-tag          ((t (:inherit nano-strong))))
   '(custom-variable-tag       ((t (:inherit nano-strong))))
   '(custom-face-tag           ((t (:inherit nano-strong))))
   '(custom-state              ((t (:inherit nano-salient))))
   '(custom-link               ((t (:inherit nano-salient))))

   ;; ---- EPA -------------------------------------------------------
   '(epa-field-body      ((t (:inherit nano-default))))
   '(epa-field-name      ((t (:inherit nano-strong))))
   '(epa-mark            ((t (:inherit nano-salient))))
   '(epa-string          ((t (:inherit nano-popout))))
   '(epa-validity-high   ((t (:inherit nano-strong))))
   '(epa-validity-low    ((t (:inherit nano-faded))))
   '(epa-validity-disabled ((t (:inherit nano-faded))))

   ;; ---- Message / Compose ---------------------------------------------
   '(message-header-name    ((t (:inherit nano-strong))))
   '(message-header-subject ((t (:inherit nano-salient))))
   '(message-header-to      ((t (:inherit nano-salient))))
   '(message-header-cc      ((t (:inherit nano-default))))
   '(message-cited-text     ((t (:inherit nano-faded))))
   '(message-separator      ((t (:inherit nano-faded))))
   '(message-mml            ((t (:inherit nano-popout))))

   ;; ---- ANSI terminal colors ------------------------------------------
   `(ansi-color-black          ((t (:foreground ,bg    :background ,bg))))
   `(ansi-color-red            ((t (:foreground ,red   :background ,red))))
   `(ansi-color-green          ((t (:foreground ,green :background ,green))))
   `(ansi-color-yellow         ((t (:foreground ,yellow :background ,yellow))))
   `(ansi-color-blue           ((t (:foreground ,blue  :background ,blue))))
   `(ansi-color-magenta        ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-cyan           ((t (:foreground ,cyan  :background ,cyan))))
   `(ansi-color-white          ((t (:foreground ,fg    :background ,fg))))
   `(ansi-color-bright-black   ((t (:foreground ,bg+3  :background ,bg+3))))
   `(ansi-color-bright-red     ((t (:foreground ,red+1 :background ,red+1))))
   `(ansi-color-bright-green   ((t (:foreground ,green+1 :background ,green+1))))
   `(ansi-color-bright-yellow  ((t (:foreground ,yellow-1 :background ,yellow-1))))
   `(ansi-color-bright-blue    ((t (:foreground ,blue+1  :background ,blue+1))))
   `(ansi-color-bright-magenta ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-bright-cyan    ((t (:foreground ,cyan    :background ,cyan))))
   `(ansi-color-bright-white   ((t (:foreground ,fg+1    :background ,fg+1))))
   `(ansi-color-bold           ((t (:inherit nano-strong))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nano-hc-zenburn)
;;; nano-hc-zenburn-theme.el ends here
