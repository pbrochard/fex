(define-generic-mode 'fex-mode
    '("%")
  '()
  '(("document(" . font-lock-warning-face)
    (")document" . font-lock-warning-face)
    ("forth(" . font-lock-warning-face)
    (")forth" . font-lock-warning-face)
    ("titre:.*$" . font-lock-function-name-face)
    ("image:.*$" . font-lock-function-name-face)
    ("section:.*$" . font-lock-function-name-face)
    ("=>file" . font-lock-warning-face)
    ("=>stdin" . font-lock-warning-face)
    ("%.*$" . font-lock-comment-face)
    "numerote("     "[=][>]"    ")numerote"
    "(ligne)"
    "(page)"
    "(paragraphe)"
    "[_](" ")[_]"
    "[*](" ")[*]"
    "[/](" ")[/]"
    "vspace=[ ]\\sw+"   "hspace=[ ]\\sw+"
    "centre("   ")centre"
    "droite("   ")droite"
    "gauche("   ")gauche"
    "tableau(.*)("  ")tableau"
    ("[$].*[$]" . font-lock-function-name-face)
    "encadre("  ")encadre"
    "boite(.*)("    ")boite"
    "marge:[ ]\\sw+"
    "Entete-Nom"
    "->reponse:" "(simple-reponse)" "(double-reponse)"
    "(reponse->visible)" "(reponse->invisible)"
    )
  '(".fex\\'")
  '(fex-mode-setup)
  "A fex mode")


(defvar fex-mode-syntax-table nil
  "Syntax table in use in `fex-mode' buffers.")

(defvar fex-mode-map nil
  "Keymap used in Fex mode.")

(if (not fex-mode-map)
    (setq fex-mode-map (make-sparse-keymap)))


(unless fex-mode-syntax-table
  (setq fex-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?~ "w" fex-mode-syntax-table))

(defvar fex-command "fex")

(defun run-fex ()
  (interactive)
  (save-buffer)
  (let ((cmd (concat fex-command " -q " buffer-file-name)))
    (message (concat "Fex: " cmd))
    (run-forth cmd)
    (delete-window)))


(defun run-help-fex ()
  (interactive)
  (let ((cmd (concat fex-command " --aide")))
    (message (concat "Fex: " cmd))
    (run-forth cmd)
    (delete-other-windows)
    (message "Tapez Control-X puis Control-K pour fermer ce buffer")))

(defun run-fex-interactive ()
  (interactive)
  (save-buffer)
  (let ((cmd (concat fex-command " " buffer-file-name)))
    (message (concat "Fex: " cmd))
    (run-forth cmd)
    (delete-other-windows)
    (message "Tapez Control-X puis Control-K pour fermer ce buffer")))



(define-key fex-mode-map (read-kbd-macro "<f8>") 'run-fex)
(define-key fex-mode-map (read-kbd-macro "<f1>") 'run-help-fex)
(define-key fex-mode-map (read-kbd-macro "<f2>") 'run-fex-interactive)



(defun fex-mode-setup ()
  (set-syntax-table fex-mode-syntax-table)
  (setq tab-width 2)
  ;; Make keywords case-insensitive
  ;;(setq font-lock-defaults '(generic-font-lock-keywords nil t))
  (use-local-map fex-mode-map))
