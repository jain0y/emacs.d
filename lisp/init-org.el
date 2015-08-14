(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; NO spell check for embedded snippets
;; Please note flyspell only use ispell-word
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let ((rlt ad-return-value)
        (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
        (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
        old-flag
        b e)
    (when ad-return-value
      (save-excursion
        (setq old-flag case-fold-search)
        (setq case-fold-search t)
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t)))
        (setq case-fold-search old-flag))
      (if (and b e (< (point) e)) (setq rlt nil)))
    (setq ad-return-value rlt)))

;; Org v8 change log:
;; @see http://orgmode.org/worg/org-8.0.html

;; {{ export org-mode in Chinese into PDF
;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
;; and you need install texlive-xetex on different platforms
;; To install texlive-xetex:
;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
(setq org-latex-to-pdf-process ;; org v7
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "rm -fr %b.out %b.log %b.tex auto"))
(setq org-latex-pdf-process org-latex-to-pdf-process) ;; org v8
;; }}

(defun my-setup-odt-org-convert-process ()
  (interactive)
  (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
    (when (and *is-a-mac* (file-exists-p cmd))
      ;; org v7
      (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))
      ;; org v8
      (setq org-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))
    ))

(my-setup-odt-org-convert-process)

;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
(defun narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((equal major-mode 'org-mode) (org-narrow-to-subtree))
        (t (error "Please select a region to narrow to"))))

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-src-content-indentation 0
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      ;; org v7
      org-export-odt-preferred-output-format "doc"
      ;; org v8
      org-odt-preferred-output-format "doc"
      org-tags-column 80
      ;; org-startup-indented t
      ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
      ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
      org-agenda-inhibit-startup t ;; ~50x speedup
      org-agenda-use-tag-inheritance nil ;; 3-4x speedup
      ;; }}
      )

;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(eval-after-load 'org-clock
  '(progn
     (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
     (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

(eval-after-load 'org
   '(progn
      (require 'org-clock)
      ; @see http://irreal.org/blog/?p=671
      (setq org-src-fontify-natively t)
      ;; (require 'org-fstree)
      (defun soft-wrap-lines ()
        "Make lines wrap at window edge and on word boundary,
        in current buffer."
        (interactive)
        ;; (setq truncate-lines nil)
        (setq word-wrap t)
        )
      (add-hook 'org-mode-hook '(lambda ()
                                  (setq evil-auto-indent nil)
                                  (soft-wrap-lines)
                                  ))))

(defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
  (let ((browse-url-browser-function
         (cond ((equal (ad-get-arg 0) '(4))
                'browse-url-generic)
               ((equal (ad-get-arg 0) '(16))
                'choose-browser)
               (t
                (lambda (url &optional new)
                  (w3m-browse-url url t))))))
    ad-do-it))

(defadvice org-publish (around org-publish-advice activate)
  "Stop running major-mode hook when org-publish"
  (let ((old load-user-customized-major-mode-hook))
	(setq load-user-customized-major-mode-hook nil)
    ad-do-it
	(setq load-user-customized-major-mode-hook old)))

;; {{ org2nikola set up
(setq org2nikola-output-root-directory "~/.config/nikola")
(setq org2nikola-use-google-code-prettify t)
(setq org2nikola-prettify-unsupported-language
      '(elisp "lisp"
              emacs-lisp "lisp"))
;; }}

(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)

;; publish
(setq org-publish-project-alist
      '(("note-org"
         :base-directory "E:/Mine//Documents/notes/src"
         :publishing-directory "E:/Mine/Documents/notes/publish"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-export-to-html
         :auto-index t
         :index-filename "index.org"
         :index-title "index"
         :link-home "index.html"
         :section-numbers t
         :style "<link rel=\"stylesheet\" href=\"./wheer.css\" type=\"text/css\"/>")
        ("note-static"
         :base-directory "E:/Mine/Documents/notes/org/src"
         :publishing-directory "E:/Mine/Documents/notes/publish"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
         :publishing-function org-publish-attachment)
        ("note"
         :components ("note-org" "note-static")
         :author "jain_y@126.com"
         )))

(setq org-export-htmlize-output-type 'inline-css)

;; org-capture-templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "E:/Company/TODO/gtd.org" "Tasks")
         "* TODO %?\n  %i" :prepend t)

        ("j" "Journal" entry (file+datetree "E:/Mine/Documents/notes/src/notes/journal.org" "Journal")
         "* %?\nEntered on %U\n %i\n %a" :prepend t :empty-lines 1)

        ("w" "WorkNote" entry (file+headline "E:/Mine/Documents/notes/src/notes/worknotes.org" "WorkNotes")
         "* %U %?\n\n  %i" :prepend t :empty-lines 1)

        ("l" "LifeNote" entry (file+headline "E:/Mine/Documents/notes/src/notes/liftnotes.org" "LiftNotes")
         "* %U %?\n\n  %i" :prepend t :empty-lines 1)

        ("s" "StudyNote" entry (file+headline "E:/Mine/Documents/notes/src/notes/studynotes.org" "StudyNotes")
         "* %U %?\n\n  %i" :prepend t :empty-lines 1)
        ))

(setq org-agenda-files (list  "E:/Company/TODO/gtd.org"
                              "E:/Mine/Documents/notes/src/notes/liftnotes.org"
                              "E:/Mine/Documents/notes/src/notes/journal.org"
                              "E:/Mine/Documents/notes/src/notes/worknotes.org"
                              "E:/Mine/Documents/notes/src/notes/studynotes.org"))

(require 'ox-impress-js)
(require 'ox-html5presentation)
(require 'org-dashboard)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (dot . t)
   (ditaa . t)
   (R . t)
   (python . t)
   (C . t)
   (perl . t)
   (ruby . t)
   (gnuplot . t)
   (clojure . t)
   (sh . t)
   (ledger . t)
   (org . t)
   (plantuml . t)
   (latex . t)
   (sql . t)
   (java . t)
   ))

(setq org-plantuml-jar-path "~/.emacs.d/plugins/plantuml/plantuml.jar")

(setf org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

;; Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(defun org-mode-article-modes ()
  (reftex-mode t)
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all)))
(add-hook 'org-mode-hook
          (lambda ()
            (if (member "REFTEX" org-todo-keywords-1)
                (org-mode-article-modes))))
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-latex-classes
             '("ctexart"
               "\\documentclass[10pt,a4paper]{article}
                \\usepackage{graphicx}
                \\usepackage{xcolor}
                \\usepackage{xeCJK}
                \\usepackage{lmodern}
                \\usepackage{verbatim}
                \\usepackage{fixltx2e}
                \\usepackage{longtable}
                \\usepackage{float}
                \\usepackage{tikz}
                \\usepackage{wrapfig}
                \\usepackage{soul}
                \\usepackage{textcomp}
                \\usepackage{minted}
                \\usepackage{listings}
                \\usepackage{geometry}
                \\usepackage{algorithm}
                \\usepackage{algorithmic}
                \\usepackage{marvosym}
                \\usepackage{wasysym}
                \\usepackage{latexsym}
                \\usepackage{natbib}
                \\usepackage{fancyhdr}
                \\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
                linkcolor=blue,
                urlcolor=blue,
                menucolor=blue]{hyperref}
                \\usepackage{fontspec,xunicode,xltxtra}
                \\setmainfont[BoldFont=SimHei]{SimSun}
                \\setsansfont[BoldFont=SimHei]{SimKai}
                \\setmonofont{Courier}
                \\newcommand\\fontnamemono{SimKai}%等宽字体
                \\newfontinstance\\MONO{\\fontnamemono}
                \\newcommand{\\mono}[1]{{\\MONO #1}}
                \\setCJKmainfont[Scale=0.9]{SimHei}%中文字体
                \\setCJKmonofont[Scale=0.9]{SimHei}
                \\hypersetup{unicode=true}
                \\geometry{a4paper, textwidth=6.5in, textheight=10in,
                marginparsep=7pt, marginparwidth=.6in}
                \\definecolor{foreground}{RGB}{220,220,204}%浅灰
                \\definecolor{background}{RGB}{62,62,62}%浅黑
                \\definecolor{preprocess}{RGB}{250,187,249}%浅紫
                \\definecolor{var}{RGB}{239,224,174}%浅肉色
                \\definecolor{string}{RGB}{154,150,230}%浅紫色
                \\definecolor{type}{RGB}{225,225,116}%浅黄
                \\definecolor{function}{RGB}{140,206,211}%浅天蓝
                \\definecolor{keyword}{RGB}{239,224,174}%浅肉色
                \\definecolor{comment}{RGB}{180,98,4}%深褐色
                \\definecolor{doc}{RGB}{175,215,175}%浅铅绿
                \\definecolor{comdil}{RGB}{111,128,111}%深灰
                \\definecolor{constant}{RGB}{220,162,170}%粉红
                \\definecolor{buildin}{RGB}{127,159,127}%深铅绿
                \\punctstyle{kaiming}
                \\title{}
                \\fancyfoot[C]{\\bfseries\\thepage}
                \\chead{\\MakeUppercase\\sectionmark}
                \\pagestyle{fancy}
                \\tolerance=1000
                [NO-DEFAULT-PACKAGES]
                [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-latex-default-class "ctexart")

;; (setq org-latex-listings 'minted)
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
(setq org-latex-listings t)

;; Options for \lset command（reference to listing Manual)
(setq org-latex-listings-options
      '(
        ("basicstyle" "\\color{foreground}\\small\\UbuntuMono")           ; 源代码字体样式
        ("keywordstyle" "\\color{function}\\bfseries\\small\\UbuntuMono") ; 关键词字体样式
        ("identifierstyle" "\\color{doc}\\small\\UbuntuMono")
        ("commentstyle" "\\color{comment}\\small\\itshape")         ; 批注样式
        ("stringstyle" "\\color{string}\\small")                    ; 字符串样式
        ("showstringspaces" "false")                                ; 字符串空格显示
        ("numbers" "left")                                          ; 行号显示
        ("numberstyle" "\\color{preprocess}")                       ; 行号样式
        ("stepnumber" "1")                                          ; 行号递增
        ("backgroundcolor" "\\color{background}")                   ; 代码框背景色
        ("tabsize" "4")                                             ; TAB等效空格数
        ("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
        ("breaklines" "true")                                       ; 自动断行
        ("breakatwhitespace" "true")                                ; 只在空格分行
        ("showspaces" "false")                                      ; 显示空格
        ("columns" "flexible")                                      ; 列样式
        ("frame" "single")                                          ; 代码框：阴影盒
        ("frameround" "tttt")                                       ; 代码框： 圆角
        ("framesep" "0pt")
        ("framerule" "8pt")
        ("rulecolor" "\\color{background}")
        ("fillcolor" "\\color{white}")
        ("rulesepcolor" "\\color{comdil}")
        ("framexleftmargin" "10mm")
        ))

;; 导出Beamer的设置
;; allow for export=>beamer by placing #+LaTeX_CLASS: beamer in org files
;;-----------------------------------------------------------------------------
(add-to-list 'org-latex-classes
             ;; beamer class, for presentations
             '("beamer"
               "\\documentclass[11pt,professionalfonts]{beamer}
                \\mode
                \\usetheme{{{{Warsaw}}}}
                %\\usecolortheme{{{{beamercolortheme}}}}
                \\beamertemplateballitem
                \\setbeameroption{show notes}
                \\usepackage{graphicx}
                \\usepackage{tikz}
                \\usepackage{xcolor}
                \\usepackage{xeCJK}
                \\usepackage{amsmath}
                \\usepackage{lmodern}
                \\usepackage{fontspec,xunicode,xltxtra}
                \\usepackage{polyglossia}
                \\setmainfont{Times New Roman}
                \\setCJKmainfont{DejaVu Sans YuanTi}
                \\setCJKmonofont{DejaVu Sans YuanTi Mono}
                \\usepackage{verbatim}
                \\usepackage{listings}
                \\institute{{{{beamerinstitute}}}}
                \\subject{{{{beamersubject}}}}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

;; MobileOrg
(setq org-mobile-directory "E:/Mine/Dropbox/Dropbox/org")

(setq org-mobile-files (list  "E:/Company/TODO/gtd.org"
                              "E:/Mine/Documents/notes/src/notes/liftnotes.org"
                              "E:/Mine/Documents/notes/src/notes/studynotes.org"))

(setq org-directory "E:/Mine/Documents/notes/src/notes/")
(setq org-mobile-inbox-for-pull "E:/Mine/Documents/notes/src/notes/inbox.org")

(require 'org-password-manager)
;; org password manager
(add-hook 'org-mode-hook 'org-password-manager-key-bindings)

(provide 'init-org)
