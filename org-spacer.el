;;; org-spacer.el --- Enforce blank lines for Org-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26") (dash "0"))
;; Keywords: org-mode formatting
;; URL: http://github.com/dustinlacewell/org-spacer.el

;;; Commentary:

;; This package lets enforce blank lines between elements in an org-mode document.

;;; Code:
(require 'map)
(require 'seq)
(require 'org)

(defcustom org-spacer-element-blanks '((0 headline)
                                       (1 paragraph src-block table property-drawer)) "")

(defun org-spacer--mark-elements (data types)
  (let ((markers nil))
    (org-element-map data types
      (lambda (element)
        (goto-char (org-element-property :begin element))
        (setq element (org-element-at-point))
        (let* ((type (car element))
               (start-marker (make-marker))
               (end-marker (make-marker))
               (start-marker-pos (or (org-element-property :begin element)
                                     (org-element-property :begin element)))
               (end-marker-pos (or (org-element-property :end element)
                                   (org-element-property :end element)))
               (pre-blank (org-element-property :pre-blank element))
               (post-blank (org-element-property :post-blank element)))
          (set-marker start-marker start-marker-pos)
          (set-marker end-marker end-marker-pos)
          (let ((record `(,type
                          ((:start-marker . ,start-marker)
                           (:end-marker . ,end-marker)
                           (:pre-blank . ,pre-blank)
                           (:post-blank . ,post-blank)))))
            (setq markers (append markers (list record)))))))
    markers))

(defun org-spacer--trim-headline (record blanks)
  (seq-let (_ props) record
    (map-let ((:start-marker marker)
              (:pre-blank pre-blank))
        props
      (goto-char marker)
      (end-of-line)
      (forward-char)
      (unless (eq pre-blank blanks)
        (when (> blanks pre-blank)
          (open-line (- blanks pre-blank)))
        (when (< blanks pre-blank)
          (kill-line (- pre-blank blanks)))))))

(defun org-spacer--trim-non-headline (record blanks)
  (seq-let (_ props) record
    (map-let ((:end-marker marker)
              (:post-blank post-blank))
        props
      (goto-char marker)
      (unless (eq post-blank blanks)
        (when (> blanks post-blank)
          (open-line (- blanks post-blank)))
        (when (< blanks post-blank)
          (delete-backward-char (- post-blank blanks)))))))

(defun org-spacer--build-blanks-map ()
  (let ((map (make-hash-table)))
    (dolist (ele org-spacer-element-blanks)
      (seq-let (blanks &rest types) ele
          (dolist (type types)
            (map-put! map type blanks))))
    map))

(defun org-spacer-enforce ()
  (interactive)
  (let* ((mark (set-marker (make-marker) (point)))
         (map (org-spacer--build-blanks-map))
         (types (map-keys map)))
    (org-save-outline-visibility t
      (let* ((data (org-element-parse-buffer))
             (targets (org-spacer--mark-elements data types)))
        (org-show-all)
        (--each targets
          (let* ((type (car it))
                 (blanks (map-elt map type)))
            (pcase type
              ('headline (org-spacer--trim-headline it blanks))
              (_ (org-spacer--trim-non-headline it blanks)))))
        (goto-char mark)))))

(provide 'org-spacer)

;;; org-spacer.el ends here
