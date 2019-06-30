;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; MOVE TEXT MODE
(move-text-default-bindings)

;;; ANSI TERMINAL
(defun toggle-term ()
  "Toggles between terminal and current buffer (creates terminal, if none exists)"
  (interactive)
  (if (string= (buffer-name) "*ansi-term*")
      (switch-to-buffer (other-buffer (current-buffer)))
    (if (get-buffer "*ansi-term*")
        (switch-to-buffer "*ansi-term*")
      (progn
        (ansi-term (getenv "SHELL"))
        (setq show-trailing-whitespace nil)))))
(global-set-key (kbd "<f9>") 'toggle-term)


(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)


(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

(defun my-term-hook ()
  (goto-address-mode))

(add-hook 'term-mode-hook 'my-term-hook)

(defun my-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))

;;; NEOTREE
(global-set-key [f8] 'neotree-toggle)

;;; FULLSCREEN BY DEFAULT
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

;;; SNIPPETS
;; (eval-after-load 'yasnippet
;;   '(progn
;;      (define-key yas-keymap (kbd "TAB") nil)
;;      (define-key yas-keymap (kbd "C-o") 'yas-next-field-or-maybe-expand)))


(global-set-key (kbd "C-M-(") 'scroll-down-line)
(global-set-key (kbd "C-M-)") 'scroll-up-line)

;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

;;; MULTIPLE CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-S-a") 'mc/mark-all-like-this)

;;; GERMAN UMLAUTS
;; (global-set-key "ö" "[")
;; (global-set-key "ä" "]")
;; (global-set-key (kbd "C-ö") "{")
;; (global-set-key (kbd "C-ä") "}")
;; (global-set-key (kbd "M-ö") "ö")
;; (global-set-key (kbd "M-ä") "ä")

;;; SIMULATE VIS CHANGE INSIDE
(require 'expand-region)

(eval-when-compile (require 'cl))

(defun ci--flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'priority 100)
    (run-with-timer 0.2 nil 'delete-overlay overlay)))

(defun change-inner* (yank? search-forward-char)
  "Works like vim's ci command. Takes a char, like ( or \" and
kills the innards of the first ancestor semantic unit starting with that char."
  (let* ((expand-region-fast-keys-enabled nil)
         (char (or search-forward-char
                   (char-to-string
                    (read-char
                     (if yank?
                         "Yank inner, starting with:"
                       "Change inner, starting with:")))))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (when search-forward-char
      (search-forward char (point-at-eol)))
    (flet ((message (&rest args) nil))
      (er--expand-region-1)
      (er--expand-region-1)
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er--expand-region-1))
      (if (not (looking-at q-char))
          (if search-forward-char
              (error "Couldn't find any expansion starting with %S" char)
            (goto-char starting-point)
            (setq mark-active nil)
            (change-inner* yank? char))
        (er/contract-region 1)
        (if yank?
            (progn
              (copy-region-as-kill (region-beginning) (region-end))
              (ci--flash-region (region-beginning) (region-end))
              (goto-char starting-point))
          (kill-region (region-beginning) (region-end)))))))

;;;###autoload
(defun change-inner (arg)
  (interactive "P")
  (change-inner* arg nil))

;;;###autoload
(defun copy-inner ()
  (interactive)
  (change-inner* t nil))

(defun change-outer* (yank? search-forward-char)
  "Works like vim's ci command. Takes a char, like ( or \" and
kills the first ancestor semantic unit starting with that char."
  (let* ((expand-region-fast-keys-enabled nil)
         (char (or search-forward-char
                   (char-to-string
                    (read-char
                     (if yank?
                         "Yank outer, starting with:"
                       "Change outer, starting with:")))))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (when search-forward-char
      (search-forward char (point-at-eol)))
    (flet ((message (&rest args) nil))
      (when (looking-at q-char)
        (er/expand-region 1))
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er/expand-region 1))
      (if (not (looking-at q-char))
          (if search-forward-char
              (error "Couldn't find any expansion starting with %S" char)
            (goto-char starting-point)
            (setq mark-active nil)
            (change-outer* yank? char))
        (if yank?
            (progn
              (copy-region-as-kill (region-beginning) (region-end))
              (ci--flash-region (region-beginning) (region-end))
              (goto-char starting-point))
          (kill-region (region-beginning) (region-end)))))))

;;;###autoload
(defun change-outer (arg)
  (interactive "P")
  (change-outer* arg nil))

;;;###autoload
(defun copy-outer ()
  (interactive)
  (change-outer* t nil))

(provide 'change-inner)
;;; change-inner.el ends here

(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)


