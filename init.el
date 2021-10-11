  ;; emacs package management
  (require 'package)

  ;; add the repositories
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  ;; refresh the list
  (when (not package-archive-contents)
    (package-refresh-contents))

  (require 'use-package)

    ;; org-mode
  (require 'org)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)

  ;; org-mode agendas
  (setq org-agenda-files (list "~/documents/pcloud_drive/roku_chiji/tsusu/kanban.org"))

  ;; org-capture
  (setq org-default-notes-file (concat "~/documents/pcloud_drive/roku_chiji/tsusu/" "bugs.org"))
  (define-key global-map "\C-cc" 'org-capture)

  (setq org-capture-templates
        '(("b" "Bug" entry (file+headline "~/documents/pcloud_drive/roku_chiji/tsusu/bugs.org" "Bugs")
                        "* BUG %?\n  %i\n  %a")))

  ;; todo-state names
  (setq org-todo-keywords
        '((sequence "BUG" "DOABLE" "DOING" "|" "DONE")))

  ;; org clean-outlines
  (setq org-hide-leading-stars t)
  (setq org-indent-mode t)

  ;; word-wrap
  (setq org-indent-mode t)
  (global-visual-line-mode 1)


;; make sure org-babel comes before jupyter or any other code-based settings
  ;; org-babel
(require 'ob-js)

  (add-to-list 'org-src-lang-modes '("rst" . "rst"))
  (add-to-list 'org-src-lang-modes '("feature" . "feature"))
  (add-to-list 'org-src-lang-modes '("org" . "org"))
  (add-to-list 'org-src-lang-modes '("css" . "css"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (plantuml . t)
     (shell . t)
     (emacs-lisp . t)
     (latex . t)
     (ditaa . t)
     (js . t)
     (jupyter . t)
     ))

  (setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
  
  ;; Don't treat underscores as sub-script notation
  (setq org-export-with-sub-superscripts nil)

  ;; Don't re-evaluate the source blocks before exporting
  (setq org-export-babel-evaluate nil)

  ;; don't confirm block evaluation
  (setq org-confirm-babel-evaluate nil)

  ;;; display/update images in the buffer after evaluation
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; noweb expansion only when you tangle
  (setq org-babel-default-header-args
        (cons '(:noweb . "tangle")
              (assq-delete-all :noweb org-babel-default-header-args))
        )

  ;; syntax highlighting in org-files
  (setq org-src-fontify-natively t)

  ;; export org to rst
  (require 'ox-rst)

  ;; export org to nikola
  (require 'ox-nikola)

  ;; export to latex/pdf
  (require 'ox-latex)

  ;; syntax-highlighting for pdf's
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; let the user set the indentation so you can insert text between methods in classes.
  (setq org-src-preserve-indentation t)

  ;; pygmentize ipython
  (add-to-list 'org-latex-minted-langs '(ipython "python"))

;; show column-numbers
(column-number-mode)

;; show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; global parentheses matching (`autopair` package needs to be installed)
(electric-pair-mode 1)

;; turn off saving backups
(setq make-backup-files nil)

;; turn off auto-fill mode
(remove-hook 'text-mode-hook #'turn-on-auto-fill)


;; (setq default-input-method "TeX")
(setq default-input-method "TeX")

;;   ;; elpy
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))
;;   (add-to-list 'auto-mode-alist '("\\.py" . python-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(package-selected-packages
   '(keychain-environment htmlize ox-nikola ox-rst web-mode swiper smex paredit magit jedi ido-ubiquitous idle-highlight-mode god-mode fuzzy feature-mode ein-mumamo csv-mode autopair ac-js2)))

      
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rst-level-1 ((t (:background "white" :foreground "royal blue"))))
 '(rst-level-2 ((t (:background "white"))))
 '(rst-level-3 ((t (:background "cyan"))))
 '(rst-level-4 ((t (:background "magenta")))))

      ;; rst minor-mode
      (defun turn-on-rst () (rst-minor-mode 1))
      (add-hook 'python-mode-hook 'turn-on-rst)

      ;; hide-show mode
      (defun turn-on-hideshow () (hs-minor-mode 1))
      (add-hook 'python-mode-hook 'turn-on-hideshow)

      ;; Emacs Ipython Notebook
      (require 'ein)
      (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

      ;; make no-tabs universal
      (setq-default indent-tabs-mode nil)

      ;; ipython shell
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i")

          ;; web-mode
          (require 'web-mode)
          (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
      
          (defun my-web-mode-hook ()
            "Hooks for Web mode."
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-enable-current-column-highlight t)
            (setq web-mode-enable-current-element-highlight t)
            (setq web-mode-engines-alist
                '(("jinja"    . "\\.html\\'"))
                )
          )
          (add-hook 'web-mode-hook  'my-web-mode-hook)

          ;; auto-complete
          ;; (defun turn-on-autocomplete () (auto-complete-mode 1))
          (add-to-list 'load-path "~/.emacs.d/lisp")
          (require 'auto-complete-config)
          (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
          (ac-config-default)
          (defadvice auto-complete-mode (around disable-auto-complete-for-python)
          (unless (eq major-mode 'python-mode) ad-do-it))

      (define-global-minor-mode select-electric-pair-mode electric-pair-mode
        (lambda ()
          (when (not (memq major-mode
                           (list 'web-mode 'js2-mode)))
            (electric-pair-mode))))

      (select-electric-pair-mode 1)

      ;; tramp mode
      (setq tramp-default-method "ssh")

      ;; magit
      (setq global-magit-file-mode 1)

      ;; setup the keybinding to launch magit
      (global-set-key (kbd "C-x g") 'magit-status)

;; god-mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(define-key god-local-mode-map (kbd ".") 'repeat)

(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; window bindings for god-mode
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-B") 'switch-to-buffer)

;; allow using 's' and 'r' for repeated searches
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(define-key god-local-mode-map (kbd ".") 'repeat)
;; set a default virtual environment
(pyvenv-activate "~/.virtualenvs/emacs")

;; hide-show is broken by god mode.
;; this adds universal quick and dirty code-folding that works
(defvar hs-special-modes-alist
  (mapcar 'purecopy
          '((c-mode "{" "}" "/[*/]" nil nil)
            (c++-mode "{" "}" "/[*/]" nil nil)
            (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
            (java-mode "{" "}" "/[*/]" nil nil)
            (js2-mode "{" "}" "/[*/]" nil))))

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))
(load-library "hideshow")
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-|") 'toggle-selective-display)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'js2-mode-hook         'hs-minor-mode)

(org-babel-jupyter-override-src-block "python")

  ;; increase/decrease text size
  (global-set-key (kbd "C-c C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

;; js2
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (add-to-list 'auto-mode-alist '("\\.feature" . feature-mode))



(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; setup files ending in “.vue” to open in vue-mode
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(add-to-list 'auto-mode-alist '("\\.bat\\'" . bats-mode))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

(use-package markdown-mode
 :ensure t
 :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
 :init (setq markdown-command "pandoc")
)


(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; ;; flycheck
;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (global-flycheck-mode t)
;;   ;; note that these bindings are optional
;;   (global-set-key (kbd "C-c n") 'flycheck-next-error)
;;   ;; this might override a default binding for running a python process,
;;   ;; see comments below this answer
;;   (global-set-key (kbd "C-c p") 'flycheck-prev-error)
;;   )
;; ;; flycheck-pycheckers
;; ;; Allows multiple syntax checkers to run in parallel on Python code
;; ;; Ideal use-case: pyflakes for syntax combined with mypy for typing
;; (use-package flycheck-pycheckers
;;   :after flycheck
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
;;     )
;;   (setq flycheck-pycheckers-checkers
;;     '(
;;       mypy3
;;       pyflakes
;;       )
;;     )
;;   )
;; ;; elpy
;; (use-package elpy
;;   :after poetry
;;   :ensure t
;;   :config
;;   (elpy-enable)
;;   (add-hook 'elpy-mode-hook 'poetry-tracking-mode) ;; optional if you're using Poetry
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   (setq elpy-syntax-check-command "~/.virtualenvs/neurotic-networks/bin/pyflakes") ;; or replace with the path to your pyflakes binary
;;   ;; allows Elpy to see virtualenv
;;   (add-hook 'elpy-mode-hook
;;         ;; pyvenv-mode
;;         '(lambda ()
;;            (pyvenv-mode +1)
;;            )
;;         )
;;   ;; use flycheck instead of flymake
;;   (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   )
;; ;; poetry
;; (use-package poetry
;;   :ensure t)
