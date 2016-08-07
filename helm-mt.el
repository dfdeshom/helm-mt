;;; helm-mt.el --- helm multi-term management -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016 Didier Deshommes <dfdeshom@gmail.com>

;; Author: Didier Deshommes <dfdeshom@gmail.com>
;; URL: https://github.com/dfdeshom/helm-mt
;; Version: 0.9
;; Package-Requires: ((emacs "24") (helm "0.0") (multi-term "0.0") (cl-lib "0.5"))
;; Keywords: helm multi-term

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Create and delete multi-term terminals easily with Helm.  A call to
;; `helm-mt' will show a list of terminal sessions managed by
;; multi-term.  From there, you are able to create, delete or switch
;; over to existing terminal buffers.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'helm-source)
(require 'multi-term)

(defvar helm-mt/keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (delq nil map))
  "Keymap for `helm-mt'.")

(defun helm-mt/launch-term (name prefix)
  "Launch a new terminal in a buffer called NAME.
PREFIX is passed on to `multi-term' as a prefix argument."
  (setq current-prefix-arg prefix)
  (call-interactively 'multi-term)
  (rename-buffer (generate-new-buffer-name name)))

(defun helm-mt/delete-marked-terms (ignored)
  "Delete marked terminals.
Argument IGNORED is not used."
  (let* ((bufs (helm-marked-candidates))
         (killed-bufs (cl-count-if 'helm-mt/delete-term bufs)))
    (with-helm-buffer
      (setq helm-marked-candidates nil
            helm-visible-mark-overlays nil))
    (message "Deleted %s terminal(s)" killed-bufs)))

(defun helm-mt/delete-term (name)
  "Delete terminal NAME."
  (if (get-buffer-process name)
      (delete-process name))
  (kill-buffer name))

(defun helm-mt/term-source-terminals ()
  "Helm source with candidates for all terminal buffers managed by `multi-term'."
  (helm-build-sync-source
      "Terminals"
    :candidates (lambda () (or
                            (mapcar 'buffer-name multi-term-buffer-list)
                            (list "")))
    :action (helm-make-actions
             "Switch to terminal"
             (lambda (candidate)
               (switch-to-buffer candidate))
             "Exit marked terminal(s)"
             (lambda (ignored)
               (helm-mt/delete-marked-terms ignored)))))

(defun helm-mt/term-source-terminal-not-found (prefix)
  "Dummy helm source to launch a new terminal.
PREFIX is passed on to `helm-mt/launch-term'."
  (let ((source-header "Launch a new terminal"))
    (helm-build-dummy-source
        source-header
      :action (helm-make-actions
               "Launch new terminal"
               (lambda (candidate)
                 ;; default to current working directory for "empty" terminal names
                 (if (string-equal candidate source-header)
                     (setq candidate default-directory))
                 (helm-mt/launch-term candidate prefix))))))

;;;###autoload
(defun helm-mt (prefix)
  "Custom helm buffer for terminals only.
PREFIX is passed on to `helm-mt/term-source-terminal-not-found'."
  (interactive "P")
  (helm :sources `(,(helm-mt/term-source-terminals)
                   ,(helm-mt/term-source-terminal-not-found prefix))
        :keymap helm-mt/keymap
        :buffer "*helm mt*"))

(provide 'helm-mt)
;;; helm-mt.el ends here
