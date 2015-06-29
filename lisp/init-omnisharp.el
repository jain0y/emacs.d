(require 'omnisharp)

(add-hook 'csharp-mode-hook 'omnisharp-mode)
;; (add-hook 'vbnet-mode-hook 'omnisharp-mode)

;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-omnisharp))

(setq omnisharp-server-executable-path "E:/LikeUnix/OmniSharpServer/OmniSharp.exe")

(define-key omnisharp-mode-map "\C-c\C-s" 'omnisharp-start-omnisharp-server)
(define-key omnisharp-mode-map "\M-n" 'omnisharp-auto-complete)
(define-key omnisharp-mode-map "." 'omnisharp-add-dot-and-auto-complete)
(define-key omnisharp-mode-map "\C-c\C-c" 'omnisharp-build-in-emacs)
(define-key omnisharp-mode-map "\C-c\C-N" 'omnisharp-navigate-to-solution-member)
(define-key omnisharp-mode-map "\C-c\C-n" 'omnisharp-navigate-to-current-file-member)
(define-key omnisharp-mode-map "\C-c\C-f" 'omnisharp-navigate-to-solution-file)
(define-key omnisharp-mode-map "\C-c\C-g" 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map "\C-c\C-r" 'omnisharp-rename)
(define-key omnisharp-mode-map "\C-c\C-v" 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map "\C-c\C-o" 'omnisharp-auto-complete-overrides)
(define-key omnisharp-mode-map "\C-c\C-u" 'omnisharp-helm-find-symbols)

(provide 'init-omnisharp)
