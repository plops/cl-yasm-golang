;(ql:quickload "optima")
;(ql:quickload "alexandria")
(defpackage :cl-yasm-generator
  (:use :cl
	;:optima
	:alexandria)
  (:export
   #:write-source))
