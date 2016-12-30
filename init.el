;; emacs package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; elpy
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(eval-after-load "python"
 '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))

(elpy-use-ipython)

;; global parentheses matching (`autopair` package needs to be installed)
(electric-pair-mode 1)

;; turn off saving backups
(setq make-backup-files nil)

;; fish functions
(setq auto-mode-alist (cons '("\\.fish$" . shell-script-mode) auto-mode-alist))

;; show column-numbers
(column-number-mode)

;; show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; org-mode agendas
(setq org-agenda-files (list "~/Dropbox/roku_chiji/tsusu/kanban.org"))

;; org-capture
(setq org-default-notes-file (concat "~/Dropbox/roku_chiji/tsusu/" "bugs.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("b" "Bug" entry (file+headline "~/Dropbox/roku_chiji/tsusu/bugs.org" "Bugs")
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

;; pweave
(add-to-list 'auto-mode-alist '("\\.pnw" . python-mode))

;; turn off auto-fill mode
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(package-selected-packages
   (quote
    (htmlize ox-nikola ox-rst ob-ipython web-mode swiper smex paredit org magit jedi ido-ubiquitous idle-highlight-mode god-mode fuzzy feature-mode elpy ein-mumamo deft csv-mode autopair ac-js2))))
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
;;(setq ein:use-autocomplete t)

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
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'"))
)

;; auto-complete
(defun turn-on-autocomplete () (auto-complete-mode 1))
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(add-hook 'js3-mode-hook 'turn-on-autocomplete)

(define-global-minor-mode select-electric-pair-mode electric-pair-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'web-mode 'js3-mode)))
      (electric-pair-mode))))

(select-electric-pair-mode 1)

;; js3
(add-to-list 'auto-mode-alist '("\\.js3\\'" . js3-mode))


;; tramp mode
(setq tramp-default-method "ssh")

;; magit
(setq global-magit-file-mode 1)

;; setup the keybinding to launch magit
(global-set-key (kbd "C-x g") 'magit-status)

(require 'deft)
(setq deft-directory "~/Dropbox/notes/")

;; god-mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(define-key god-local-mode-map (kbd ".") 'repeat)

(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar))
  (set-cursor-color (if (or god-local-mode buffer-read-only)
                        "#691520"
                      "#ffffff")))

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
    (js-mode "{" "}" "/[*/]" nil))))

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
(global-set-key (kbd "C-\\") 'toggle-selective-display)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'js2-mode-hook         'hs-minor-mode)

;; org-babel
(add-to-list 'org-src-lang-modes '("rst" . "rst"))
(add-to-list 'org-src-lang-modes '("feature" . "feature"))
(add-to-list 'org-src-lang-modes '("org" . "org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (plantuml . t)
   (shell . t)
   ;; other languages..
   ))

(setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))

(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;;; noweb expansion only when you tangle
(setq org-babel-default-header-args
      (cons '(:noweb . "tangle")
            (assq-delete-all :noweb org-babel-default-header-args))
      )

;; export org to rst
(require 'ox-rst)

(setq org-src-fontify-natively t)
(require 'ox-nikola)
