(add-hook 'after-init-hook (lambda ()
                             (message "activate-malabar-mode")
                             (activate-malabar-mode)))

(add-hook 'malabar-java-mode-hook 'flycheck-mode)
(add-hook 'malabar-groovy-mode-hook 'flycheck-mode)

(provide 'init-malabar)
