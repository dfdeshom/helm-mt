;;; helm-mt.el --- helm multi-term management. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Didier Deshommes <dfdeshom@gmail.com>

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

;; Author: Didier Deshommes <dfdeshom@gmail.com>
;; URL: https://github.com/dfdeshom/helm-mt
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (helm "1.5.3"))

;;; Commentary:

;; Create and delete multi-term terminals easily with Helm

;; A call to `helm-mt` will show a list of running terminal sessions
;; by examining buffers with major mode `term-mode`. From there, you 
;; should be able to create new terminal buffers are create new ones

;;; Code:

(require 'cl-lib)
;(require 'helm)

(defvar helm-marked-buffer-name)
(defvar helm-mt-term-source-terminals)
(defvar helm-mt-term-source-terminal-not-found)
(defvar helm-mt-term-source-term-list)

(defun helm-mt-terminal-buffers ()
  "Filter for buffers that are terminals only."
  (cl-loop for buf in (buffer-list)
           if (eq 'term-mode (buffer-local-value 'major-mode buf))
           collect (buffer-name buf)) )

(defun helm-mt-launch-term (name)
  "Create new terminal in a buffer called NAME."
  (multi-term)
  (rename-buffer (format "*%s*" name)))

(defun helm-mt-delete-marked-terms ()
  "Delete marked terminals."
  (let* ((buffers (helm-marked-candidates :with-wildcard t))
         (len (length buffers)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (if (not (y-or-n-p (format "Delete %s Terminal(s)? " len)))
          (message "(No deletions performed)")
        (cl-dolist (b buffers)
          ;; kill the process in the buffer
          ;; then delete buffer, to avoid confirmation questions
          (delete-process b)
          (kill-buffer b))
        (message "%s Terminals deleted" len)))))


(setq helm-mt-term-source-terminals
      (helm-build-sync-source "terminal buffers"
        :candidates (lambda () (helm-mt-terminal-buffers))
        ;:fuzzy-match helm-buffers-fuzzy-matching
        ;:keymap helm-buffers-ido-virtual-map
        :action (helm-make-actions
                 "Switch to terminal buffer"
                 (lambda (candidate)
                   (helm-switch-to-buffer candidate))
        
        "Exit marked terminals"  'helm-mt-delete-marked-terms)))
 
(setq helm-mt-term-source-terminal-not-found
  (helm-build-dummy-source
   "Launch new terminal"
   :action (helm-make-actions
            "Launch new terminal"
            (lambda (candidate) (dfdeshom/launch-term candidate)))))


(setq helm-mt-term-source-term-list
      '( helm-mt-term-source-terminals
         helm-mt-term-source-terminal-not-found))
 
;;;###autoload
(defun helm-mt-helm-terms ()
  "Custom helm buffer for terminals only."
  (interactive)
   (helm-other-buffer
    helm-mt-term-source-term-list "*helm terminal buffers*"))

(provide 'helm-mt)

;;; helm-mt.el ends here
