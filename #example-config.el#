;;; Example configuration for gemini-emacs from GitHub

;; Option 1: Using straight.el
(straight-use-package
  '(gemini-cli :type git :host github :repo "dawsh2/gemini-emacs" :files ("gemini-cli.el")))

(require 'gemini-cli)

;; Option 2: Using use-package with straight
(use-package gemini-cli
  :straight (gemini-cli :type git :host github :repo "dawsh2/gemini-emacs" :files ("gemini-cli.el"))
  :config
  ;; Customize the path if needed
  ;; (setq gemini-program "/path/to/gemini")
  :bind (("C-c g s" . gemini-send-prompt)
         ("C-c g r" . gemini-send-region)
         ("C-c g b" . gemini-send-buffer)
         ("C-c g x" . gemini-context)
         ("C-c g t" . gemini-toggle)
         ("C-c g k" . gemini-kill)))

;; Option 3: Using quelpa
(quelpa '(gemini-cli :fetcher github :repo "dawsh2/gemini-emacs" :files ("gemini-cli.el")))
(require 'gemini-cli)

;; Option 4: Manual installation
;; 1. Clone the repo: git clone https://github.com/dawsh2/gemini-emacs.git ~/emacs-packages/gemini-emacs
;; 2. Add to your init.el:
(add-to-list 'load-path "~/emacs-packages/gemini-emacs")
(require 'gemini-cli)

;; Keybindings (if not using use-package)
(global-set-key (kbd "C-c g s") 'gemini-send-prompt)
(global-set-key (kbd "C-c g r") 'gemini-send-region)
(global-set-key (kbd "C-c g b") 'gemini-send-buffer)
(global-set-key (kbd "C-c g x") 'gemini-context)
(global-set-key (kbd "C-c g t") 'gemini-toggle)
(global-set-key (kbd "C-c g k") 'gemini-kill)