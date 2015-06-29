(add-hook 'prog-mode-hook 'global-company-mode)
(add-hook 'cmake-mode-hook 'global-company-mode)

(setq company-require-match nil)
;; press SPACE will accept the highlighted candidate and insert a space
;; `M-x describe-variable company-auto-complete-chars` for details
;; That's BAD idea.
(setq company-auto-complete nil)

(if (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            company-complete-number
            )))

(eval-after-load 'company
  '(progn
     ;; @see https://github.com/company-mode/company-mode/issues/348
     (require 'company-statistics)
     (company-statistics-mode)

     (add-to-list 'company-backends 'company-cmake)
     (add-to-list 'company-backends 'company-c-headers)
     ;; can't work with TRAMP
     (setq company-backends (delete 'company-ropemacs company-backends))
     ;; (setq company-backends (delete 'company-capf company-backends))
     ;; I don't like the downcase word in company-dabbrev
     ;; for languages use camel case naming convention
     (setq company-dabbrev-downcase nil)
     (setq company-show-numbers t)
     (setq company-begin-commands '(self-insert-command))
     (setq company-idle-delay 0.2)
     (setq company-clang-insert-arguments nil)
     ))

;; company should be case sensitive
(setq company-dabbrev-downcase nil)

(setq company-clang-arguments
      '("-IC:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/include"
        "-IC:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/atlmfc/include"
        "-IC:/Program Files (x86)/Microsoft SDKs/Windows/v7.0A/Include"
        ;; "-ID:/LikeUnix/MinGW/include"
        ;; "-ID:/LikeUnix/MinGW/mingw32/include"
        ;; "-ID:/LikeUnix/MinGW/lib/gcc/mingw32/4.7.0/include"
        ;; "-ID:/LikeUnix/MinGW/lib/gcc/mingw32/4.7.0/include/c++"
        ;; "-ID:/LikeUnix/MinGW/lib/gcc/mingw32/4.7.0/include/c++/mingw32"
        ))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))


;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-auto-complete t)

(provide 'init-company)
