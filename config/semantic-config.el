(message "loading semantic-config")
(require 'semantic-ia)
(if window-system
    (progn
      (setq semantic-load-turn-everything-on t)
      (semantic-load-enable-gaudy-code-helpers)))
(provide 'semantic-config)
