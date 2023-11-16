(defpackage #:lem-sql/client 
  (:use :cl))
(in-package :lem-sql/client)

(defvar *db* nil)

(defun connect-sqlite (file)
  (when *db* (sqlite:disconnect *db*))
  (setf *db* (sqlite:connect file)))
