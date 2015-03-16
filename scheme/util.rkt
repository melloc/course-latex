#lang racket

(require ffi/unsafe)

(provide (all-defined-out))

;; We try to load in several versions of libc If we can't load it in, it's
;; not a big deal, it just means that the umask functionality won't work.
;;
;; In case this is a deal breaker, we'll print out a warning to the user.
(define (libc-fail)
  (displayln "Unable to load libc. set-umask! won't work." (current-error-port)) #f)
(define libc (ffi-lib "libc" '("7" "6" #f) #:fail libc-fail))
(define set-umask!
  (if libc
    (get-ffi-obj "umask" libc (_cprocedure (list _uint32) _uint32))
    identity))

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


;; Modified versions of wait-for-pipes and pipe-between from synx/util.plt (v. 1.9)
;; http://planet.racket-lang.org/package-source/synx/util.plt/1/9/pipe-between.ss

(define (wait-for-pipes pipes)
  (for ((pipe pipes))
    (let ([input (car pipe)]
          [pid (cdr pipe)])
      (close-output-port input)
      (subprocess-wait pid))))

;; conditional-pipe allows us to pipe data along through several different
;; commands, specifiying at each step whether or not this command should
;; be run.
(define-syntax conditional-pipe
  (syntax-rules (input: pipes:)
    [(_ (test process args ...) rest ...)
     (conditional-pipe pipes: null input: (current-output-port) (test process args ...) rest ...)]
    [(_ pipes: pipes input: input (test process args ...))
     (if test (let-values
                 ([(pid stdout stdin error) (subprocess (or input (current-output-port))
                                                        (current-input-port)
                                                        (current-error-port)
                                                        (find-executable-path process)
                                                        args ...)])
                 (subprocess-wait pid)
                 (wait-for-pipes pipes))
       (copy-port (current-input-port) input))]
    [(_ pipes: pipes input: input (test process args ...) rest ...)
     (if test (let-values
                ([(pid nothin stdin error) (subprocess (or input (current-output-port))
                                                       #f
                                                       (current-error-port)
                                                       (find-executable-path process)
                                                       args ...)])
                (conditional-pipe pipes: (cons (cons stdin pid) pipes) input: stdin rest ...))
       (conditional-pipe pipes: pipes input: input rest ...))]))

;; Just a nicer way to change directory
(define-syntax with-cwd
  (syntax-rules ()
    ((with-cwd dir e ...)
     (parameterize ((current-directory dir))
       e ...))))

