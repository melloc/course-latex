#lang racket

(require scheme/system
         slatex/slatex-wrapper)

(provide (all-defined-out))

(define build-lib (or (getenv "BUILD_LIB") "/contrib/projects/course-latex"))
(define latex-root (format "~a/latex" build-lib))
(void (putenv "DVIPSHEADERS" (format "~a:~a" latex-root (or (getenv "DVIPSHEADERS") ""))))
(define course-dir (getenv "COURSE_DIR"))
(define course-eval-string (format "(define (course) (string-append \"@include{~a/\" (get-arg) \".tex.mz}\"))\n" latex-root))
(define cs-eval-string (format "\n(define (cs) (string-append \"@include{~a/latex/\" (get-arg) \".tex.mz}\"))" course-dir))

(define bin-dir (format "~a/tabin" course-dir))
(define content-dir (format "~a/content" course-dir))
(define latex-dir (format "~a/latex" course-dir))

(define (normalize-type arg-type)
  (case arg-type
    [(supplements supp sup) "supplements"]
    [(homeworks homework hw) "homeworks"]
    [(work workshops) "workshops"]
    [(labs lab) "labs"]
    [(projects project proj) "projects"]
    [(lectures lecture lec) "lectures"]
    [(documentation doc documents handouts) "docs"]
    [else #f]))

;; get-doc (listof string) path-string? -> 
;; takes the arguments to the build program and the current directory
;; returns the full path to the directory, the doc type, and the name of the document
(define (get-doc arguments cur-dir)
  (case (length arguments)
    [(0) (let*-values (((cont doc-name-prelim-path _) (split-path cur-dir))
                       ((doc-name-prelim) (path->string doc-name-prelim-path))
                       ((__ doc-type-path ___) (split-path cont))
                       ((doc-type) (string->symbol (path->string doc-type-path))))
                      (values 
                        cur-dir
                        doc-type
                        (case doc-type
                          ((homeworks)
                           doc-name-prelim)
                          ((labs)
                           doc-name-prelim)
                          ((lectures)
                           doc-name-prelim)
                          (else doc-name-prelim))))]
    [(1) (let ((arg-type (string->symbol (first arguments))))
           (define (doc-values type name)
             (values (build-path content-dir type name) type name))
           (case arg-type
             [(midterm) (doc-values "exams" "midterm")]
             [(final) (doc-values "exams" "final")]
             [else #f]))]
    [(2)
      (let* ((arg-type (string->symbol (first arguments)))
             (arg-type-normal (normalize-type arg-type))
             (parent-dir-path (build-path content-dir arg-type-normal))
             (arg-name (close-file (second arguments) parent-dir-path))
             (format-string "~a"))
        (values (build-path parent-dir-path arg-name)
                arg-type-normal
                (format format-string arg-name)))]
    [else (exit 1)]))


(define (close-file file-name dir)
  (let ((file-names (filter (lambda (pos-file) (regexp-match? (regexp (format ".*~a.*" file-name))
                                                              pos-file))
                            (map path->string (directory-list dir)))))
    (if (>= (length file-names) 1)
        (first file-names)
        file-name)))
