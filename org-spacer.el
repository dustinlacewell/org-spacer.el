(defcustom org-spacer-headline-blanks 0
  "How many blank lines to place after headlines.")

(defcustom org-spacer-non-headline-blanks 1
  "How many blank links to place after non-headline elements.")

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

(defun org-spacer-enforce ()
  (interactive)
  (let* ((data (org-element-parse-buffer))
         (types '(headline src-block paragraph))
         (targets (org-spacer--mark-elements data types)))
    (--each targets
      (pcase (car it)
        ('headline (org-spacer--trim-headline it org-spacer-headline-blanks))
        (t (org-spacer--trim-non-headline it org-spacer-non-headline-blanks))))))
