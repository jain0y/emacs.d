;; set coding
(set-language-environment "Japanese")
(prefer-coding-system 'iso-2022-jp)
(prefer-coding-system 'shift_jis)
(prefer-coding-system 'euc-jp)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'shift_jis)
;; (set-terminal-coding-system 'shift_jis)
(set-buffer-file-coding-system 'shift_jis)
;; (set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8-unix)
;; (set-keyboard-coding-system 'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)

;; buffer name
(setq frame-title-format "emacs@%b")
(setq uniquify-buffer-name-style 'forward)

(setq user-full-name "Jain")
(setq user-mail-address "Jain_Y@126.com")

(setq shell-file-name "bash.exe")
(setq explicit-shell-file-name shell-file-name)
(setq explicit-sh-args '("-login" "-i"))

;; Setting English Font
;; (set-face-attribute
;;  'default nil :font "Consolas 11")

;; ;; Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "Microsoft Yahei" :size 16)))

(set-default-font "Dejavu Sans Mono 10")
;;前面一串“(if...lambda...(with-select-frame frame ())...)"是个很好的函数框架，意思是frame创建后载入，用这个框架可以解决--daemon启动的问题
;;只有set-fontset-font一句指定修改字符集'unicode的字体为文泉驿等宽微米黑，大小为12
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-fontset-font "fontset-default"
				    'unicode "Microsoft YaHei 12"))))
  (set-fontset-font "fontset-default" 'unicode "Microsoft YaHei 12"))

;; time-locale
(setq system-time-locale "C")

;; coursor
(setq-default cursor-type 'bar)

(setq browse-url-generic-program "D:/Program Files (x86)/Mozilla Firefox/firefox.exe")
;; (setq browse-url-generic-program "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

(require 'init-irony)

(require 'init-omnisharp)

;; (require 'init-malabar)

(require 'init-vbnet-mode)

(provide 'init-myconfig)
