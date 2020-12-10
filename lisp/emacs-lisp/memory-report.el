;;; memory-report.el --- Short function summaries  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Keywords: lisp, help

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'seq)

;;;###autoload
(defun memory-report ()
  "Generate a report of how Emacs is using memory."
  (interactive)
  (pop-to-buffer "*Memory-Report*")
  (special-mode)
  (setq truncate-lines t)
  (let ((reports (append (memory-report--garbage-collect)
                         (memory-report--image-cache)
                         (memory-report--buffers)
                         (memory-report--total-variables)
                         (memory-report--largest-variables)))
        (inhibit-read-only t)
        summaries details)
    (erase-buffer)
    (insert "Estimated Emacs Memory Usage\n\n")
    (dolist (report reports)
      (if (listp report)
          (push report summaries)
        (push report details)))
    (dolist (summary (nreverse summaries))
      (insert (format "%s  %s\n"
                      (memory-report--format (cdr summary))
                      (car summary))))
    (insert "\n")
    (dolist (detail (nreverse details))
      (insert detail "\n")))
  (goto-char (point-min)))

(defvar memory-report--type-size (make-hash-table))

(defun memory-report--size (type)
  (or (gethash type memory-report--type-size)
      (gethash 'object memory-report--type-size)))

(defun memory-report--set-size (elems)
  (setf (gethash 'string memory-report--type-size)
        (cadr (assq 'strings elems)))
  (setf (gethash 'cons memory-report--type-size)
        (cadr (assq 'conses elems)))
  (setf (gethash 'symbol memory-report--type-size)
        (cadr (assq 'symbols elems)))
  (setf (gethash 'object memory-report--type-size)
        (cadr (assq 'vectors elems)))
  (setf (gethash 'float memory-report--type-size)
        (cadr (assq 'floats elems)))
  (setf (gethash 'buffer memory-report--type-size)
        (cadr (assq 'buffers elems))))

(defun memory-report--garbage-collect ()
  (let ((elems (garbage-collect)))
    (memory-report--set-size elems)
    (let ((data (list
                 (list 'strings
                       (+ (memory-report--gc-elem elems 'strings)
                          (memory-report--gc-elem elems 'string-bytes)))
                 (list 'vectors
                       (+ (memory-report--gc-elem elems 'vectors)
                          (memory-report--gc-elem elems 'vector-slots)))
                 (list 'floats (memory-report--gc-elem elems 'floats))
                 (list 'conses (memory-report--gc-elem elems 'conses))
                 (list 'symbols (memory-report--gc-elem elems 'symbols))
                 (list 'intervals (memory-report--gc-elem elems 'intervals))
                 (list 'buffer-objects
                       (memory-report--gc-elem elems 'buffers)))))
      (list (cons "Overall Object Memory Usage"
                  (seq-reduce #'+ (mapcar (lambda (elem)
                                            (* (nth 1 elem) (nth 2 elem)))
                                          elems)
                              0))
            (cons "Reserved (But Unused) Object Memory"
                  (seq-reduce #'+ (mapcar (lambda (elem)
                                            (if (nth 3 elem)
                                                (* (nth 1 elem) (nth 3 elem))
                                              0))
                                          elems)
                              0))
            (with-temp-buffer
              (insert (format "Object Storage:\n\n"))
              (dolist (object (seq-sort (lambda (e1 e2)
                                          (> (cadr e1) (cadr e2)))
                                        data))
                (insert (format "%s  %s\n"
                                (memory-report--format (cadr object))
                                (capitalize (symbol-name (car object))))))
              (buffer-string))))))

(defun memory-report--total-variables ()
  (let ((counted (make-hash-table :test #'eq))
        (total 0))
    (mapatoms
     (lambda (symbol)
       (when (boundp symbol)
         (cl-incf total (memory-report--variable-size
                         counted (symbol-value symbol)))))
     obarray)
    (list (cons "Memory Used By Global Variables" total))))

(defun memory-report--largest-variables ()
  (let ((variables nil))
    (mapatoms
     (lambda (symbol)
       (when (boundp symbol)
         (let ((size (memory-report--variable-size
                      (make-hash-table :test #'eq)
                      (symbol-value symbol))))
           (when (> size 1000)
             (push (cons symbol size) variables)))))
     obarray)
    (list
     (with-temp-buffer
       (insert "Largest Variables:\n\n")
       (cl-loop for i from 1 upto 20
                for (symbol . size) in (seq-sort (lambda (e1 e2)
                                                   (> (cdr e1) (cdr e2)))
                                                 variables)
                do (insert (memory-report--format size)
                           "  "
                           (symbol-name symbol)
                           "\n"))
       (buffer-string)))))

(defun memory-report--variable-size (counted value)
  (if (gethash value counted)
      0
    (setf (gethash value counted) t)
    (memory-report--variable-size-1 counted value)))

(cl-defgeneric memory-report--variable-size-1 (_counted _value)
  (memory-report--size 'object))

(cl-defmethod memory-report--variable-size-1 (counted (value string))
  (+ (memory-report--size 'string)
     (string-bytes value)
     (memory-report--variable-size counted (object-intervals value))))

(cl-defmethod memory-report--variable-size-1 (counted (value list))
  (let ((total 0)
        (size (memory-report--size 'cons)))
    (while value
      (cl-incf total size)
      (setf (gethash value counted) t)
      (when (car value)
        (cl-incf total (memory-report--variable-size counted (car value))))
      (if (cdr value)
          (if (consp (cdr value))
              (setq value (cdr value))
            (cl-incf total (memory-report--variable-size counted (cdr value)))
            (setq value nil))
        (setq value nil)))
    total))

(cl-defmethod memory-report--variable-size-1 (counted (value vector))
  (let ((total (+ (memory-report--size 'vector)
                  (* (memory-report--size 'object) (length value)))))
    (cl-loop for elem across value
             do (setf (gethash elem counted) t)
             (cl-incf total (memory-report--variable-size counted elem)))
    total))

(cl-defmethod memory-report--variable-size-1 (counted (value hash-table))
  (let ((total (+ (memory-report--size 'vector)
                  (* (memory-report--size 'object) (hash-table-size value)))))
    (maphash
     (lambda (key elem)
       (setf (gethash key counted) t)
       (setf (gethash elem counted) t)
       (cl-incf total (memory-report--variable-size counted key))
       (cl-incf total (memory-report--variable-size counted elem)))
     value)
    total))

(cl-defmethod memory-report--variable-size-1 (_ (_value float))
  (memory-report--size 'float))

(defun memory-report--format (bytes)
  (setq bytes (/ bytes 1024.0))
  (let ((units '("kB" "MB" "GB" "TB")))
    (while (>= bytes 1024)
      (setq bytes (/ bytes 1024.0))
      (setq units (cdr units)))
    (format "%5.1f%s" bytes (car units))))

(defun memory-report--gc-elem (elems type)
  (* (nth 1 (assq type elems))
     (nth 2 (assq type elems))))

(defun memory-report--buffers ()
  (let ((buffers (mapcar (lambda (buffer)
                           (cons buffer (memory-report--buffer buffer)))
                         (buffer-list))))
    (list (cons "Total Memory Usage In Buffers"
                (seq-reduce #'+ (mapcar #'cdr buffers) 0))
          (with-temp-buffer
            (insert "Largest Buffers:\n\n")
            (cl-loop for i from 1 upto 20
                     for (buffer . size) in (seq-sort (lambda (e1 e2)
                                                        (> (cdr e1) (cdr e2)))
                                                      buffers)
                     do (insert (memory-report--format size)
                                "  "
                                (buffer-name buffer)
                                "\n"))
            (buffer-string)))))

(defun memory-report--buffer (buffer)
  (with-current-buffer buffer
    (+ (save-restriction
         (widen)
         (+ (position-bytes (point-max))
	    (- (position-bytes (point-min)))
	    (gap-size)))
       (seq-reduce #'+ (mapcar (lambda (elem)
                                 (if (cdr elem)
                                     (memory-report--variable-size
                                      (make-hash-table :test #'eq)
                                      (cdr elem))
                                   0))
                               (buffer-local-variables buffer))
                   0)
       (memory-report--variable-size (make-hash-table :test #'eq)
                                     (object-intervals buffer))
       (memory-report--variable-size (make-hash-table :test #'eq)
                                     (overlay-lists)))))

(defun memory-report--image-cache ()
  (list (cons "Total Image Cache Size" (image-cache-size))))

(provide 'memory-report)

;;; memory-report.el ends here
