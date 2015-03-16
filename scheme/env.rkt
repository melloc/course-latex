#lang racket

(provide (all-defined-out))

;; Fetch environment variables that affect the build, and create our constants
(define build-lib (or (getenv "BUILD_LIB") "/contrib/projects/course-latex"))
(define course-dir (getenv "COURSE_DIR"))
(define latex-root (format "~a/latex" build-lib))
(void (putenv "DVIPSHEADERS" (format "~a:~a" latex-root (or (getenv "DVIPSHEADERS") ""))))
(define course-eval-string (format "(define (course) (string-append \"@include{~a/\" (get-arg) \".tex.mz}\"))\n" latex-root))
(define cs-eval-string (format "\n(define (cs) (string-append \"@include{~a/latex/\" (get-arg) \".tex.mz}\"))" course-dir))
(define content-dir (format "~a/content" course-dir))
