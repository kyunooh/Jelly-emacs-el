;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq starter-kit-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;; about elpy
(package-initialize)
(elpy-enable)
;; end of elpy

;; about jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
;; end of jedi


;; about helm
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
;; end of helm

;; about highlight-symbol
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;;end highlight-symbol


;; about font setting
(set-face-font 'default "Monaco-12")
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                  '("NanumGothicOTF" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                  '("NanumGothicOTF" . "iso10646-1"))
(set-fontset-font "fontset-default" 'kana
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'han
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'japanese-jisx0208
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'katakana-jisx0201
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))

(setq face-font-rescale-alist
      '((".*hiragino.*" . 1.2)
        (".*nanum.*" . 1.3)))
;; end of font setting

;; about auto package update
(require 'auto-package-update)
(auto-package-update-maybe)
;; end of auto package update

;;about linum mode setting
(global-linum-mode 1)
;; end of linum mode setting

;; about pyflake

;; endof pyflake

;;  about python-django
(add-to-list 'load-path "/folder/containing/file")
(require 'python-django)
;; endof python-django

;;; init.el ends here
(put 'upcase-region 'disabled nil)
