(defpackage #:lem-sql/interface
  (:use :cl :lem :lem-sql-mode :lem/listener-mode)
  (:import-from #:alexandria 
                #:when-let)
  (:import-from #:ppcre 
                #:regex-replace-all)
  (:import-from #:lem-process 
                #:delete-process
                #:run-process
                #:process-alive-p
                #:process-send-input)
  (:export :run-sqlite))
(in-package :lem-sql/interface)

(defun sqlite-run-command (file)
  (format nil "/usr/bin/sqlite3 ~a" file))

(defvar *process* nil)

(define-major-mode run-sqlite-mode sql-mode
    (:name "sqlite"
     :keymap *run-sqlite-mode-keymap*
     :syntax-table nil)
  (reset-listener-variables (current-buffer))
  (start-listener-mode))

(defun reset-listener-variables (buffer)
  (setf (variable-value 'listener-set-prompt-function :buffer buffer) #'identity
        (variable-value 'listener-check-input-function :buffer buffer) (constantly t)
        (variable-value 'listener-execute-function :buffer buffer) 'execute-input))

(defun execute-input (point string)
  (declare (ignore point))
  (unless (process-alive-p *process*)
    (editor-error "Sqlite3 process doesn't exist."))
  (process-send-input *process* (format nil "~a~%" string)))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer "*sqlite*")))
    (unless (eq (buffer-major-mode buffer) 'run-sqlite-mode)
      (change-buffer-mode buffer 'run-sqlite-mode))
    buffer))

(defun output-callback (raw-string)
  (message "gothere")
  (let* ((already-exists (get-buffer "*sqlite*"))
         (buffer (get-repl-buffer))
         (point (buffer-point buffer))
         (string (regex-replace-all "\\r\\n" raw-string (string #\newline))))
    (buffer-end point)
    (insert-string point string)
    ;(when (ppcre:scan))
    (unless already-exists 
      (switch-to-window (pop-to-buffer buffer)))
    (when-let (window (first (get-buffer-windows buffer)))
      (with-current-window window 
        (buffer-end point)
        (window-see window)))
    (redraw-display)))

(defun run-sqlite-internal (file)
  (unless (and *process* (process-alive-p *process*))
    (when *process* (delete-process *process*))
    (setf *process* (run-process (sqlite-run-command file)
                                 :name "run-sqlite"
                                 :output-callback 'output-callback))))

(define-command run-sqlite (file) ("f")
  (run-sqlite-internal file))
  
  
(add-hook *exit-editor-hook* (lambda () (when *process* (delete-process *process*))))



