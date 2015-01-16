#lang racket

(require scheme/system
         ffi/unsafe
         preprocessor/mztext
         preprocessor/pp-run
         slatex/slatex-wrapper)

(require "common.rkt")

(provide (all-defined-out))

(define libc (ffi-lib "libc" '("7" "6" #f)))
(define set-umask! (get-ffi-obj "umask" libc (_cprocedure (list _uint32) _uint32)))

;; Unfortunately, getting current umask requires setting it. Let's set it
;; to be stricter than 000. (Most people recommend 000. It's better IMO to
;; set it to something overly strict than accidentally leak something.)
(define umask (make-parameter 
                (let ([current-umask (set-umask! #o077)]) 
                  (set-umask! current-umask)
                  current-umask)))

(define-syntax with-umask
  (syntax-rules ()
    ((with-umask new-umask e ...)
     (let ([old-umask (umask)])
       (parameterize 
         ([umask new-umask])
         (dynamic-wind (lambda () (set-umask! (umask)))
                       (lambda () e ...)
                       (lambda () (set-umask! old-umask))))))))

(define (argv) (current-command-line-arguments))

(define rubber (find-executable-path "rubber"))
(define rubber-pipe (find-executable-path "rubber-pipe"))
(define r-opts `("--pdf" "--texpath" ,latex-root "--texpath" ,(string-append course-dir "/latex")))
(define (build-tex-file name-with-mode . opts)
  (apply subprocess 
         (append (list (open-output-file (format "~a/~a.pdf" (output-dir) name-with-mode) #:exists 'replace)
                       (open-input-file (format "~a.tex" name-with-mode))
                       (current-error-port)
                       rubber-pipe)
                 r-opts
                 opts)))


(define (run-cmd . args)
  (void (apply system args)))

(define output-dir (make-parameter (current-directory)))
(define file-name (make-parameter #f))
(define web-directory (or (getenv "WEB_DIR") (format "~a/web" course-dir)))
(define output-to-web (make-parameter #f))
(define output-umask (make-parameter #o007))

(define (parse-args name arguments)
  (parse-command-line name arguments
       `([multi
           [("-m" "--mode") ,(lambda (flag m) m) ("Add this mode" "mode")]]
         [once-each
           [("-r" "--recursive") ,(lambda (flag) 'recursive)
                                 ("Process files recursively")]
           [("-n" "--name") ,(lambda (flag fname) (file-name fname))
                                 ("Output PDF as <name>.pdf" "name")]]
         [once-any
           [("-w" "--web") ,(lambda (flag) (output-to-web #t))
                                 (,(format "Output PDF in the web directory (~a/content/<type>/<name>.pdf)" web-directory))]
           [("-o" "--output-dir") ,(lambda (flag dir) (output-dir dir))
                                 ("Output PDF in given directory (defaults to directory of main.tex.mz)" "dir")]])
       (lambda (modes . rest) (values modes rest))
       '("content-type" "name")))



(define-syntax with-cwd
  (syntax-rules ()
    ((with-cwd dir e ...)
     (parameterize ((current-directory dir))
       e ...))))



(define (run-build doc-dir main-name doc-name mode-eval-string name-with-mode)
  (with-cwd doc-dir
    (local ((define name-with-mode.tex (string-append name-with-mode ".tex"))
            (define doc.tex.mz main-name))
      (unless (file-exists? doc.tex.mz)
        (error (format "MZ file (~a) doesn't exist in directory ~a." doc.tex.mz (current-directory))))
      (with-umask #o007
        (parameterize ([read-case-sensitive #t]
                       [current-directory doc-dir])
          (add-eval (read-syntax 'command-line (open-input-string mode-eval-string)))
          (add-eval (read-syntax 'command-line (open-input-string course-eval-string)))
          (add-eval (read-syntax 'command-line (open-input-string cs-eval-string)))
          (add-eval `(define name-with-mode ,name-with-mode))
          (with-output-to-file name-with-mode.tex #:exists 'replace
            (lambda () (preprocess doc.tex.mz)))))
      (slatex/no-latex name-with-mode.tex)
      (with-umask (output-umask)
        (build-tex-file name-with-mode))
      (rename-file-or-directory name-with-mode.tex (string-append "." name-with-mode.tex) #t)
      (void))))



