#lang racket

(require preprocessor/mztext
         preprocessor/pp-run
         slatex/slatex-wrapper)

(require "env.rkt" "util.rkt")

(provide (all-defined-out))

(define web-directory (or (getenv "WEB_DIR") (format "~a/web" course-dir)))
(define output-dir (make-parameter (current-directory)))
(define file-name (make-parameter #f))
(define output-to-web (make-parameter #f))
(define output-umask (make-parameter #o007))
(define password-protect (make-parameter #f))

;; Compile the tex file file into a PDF
(define (build-tex-file name-with-mode)
  (parameterize ([current-output-port (open-output-file (format "~a/~a.pdf" (output-dir) name-with-mode) #:exists 'replace)]
                 [current-input-port (open-input-file (format "~a.tex" name-with-mode))])
                (conditional-pipe
                  ;; Use pdftk if we need to password protect this PDF
                  [(password-protect) "pdftk" "-" "output" "-" "user_pw" (password-protect)]
                  ;; Run rubber-pipe on our tex file
                  [#t "rubber-pipe" "--pdf" "--texpath" latex-root "--texpath" (string-append course-dir "/latex")])))

(define (run-build doc-dir main-name doc-name mode-eval-string name-with-mode)
  (define build-error (make-parameter #f))
  (define name-with-mode.tex (string-append name-with-mode ".tex"))
  (define doc.tex.mz main-name)
  (with-cwd doc-dir
      (unless (file-exists? doc.tex.mz)
        (error (format "MZ file (~a) doesn't exist in directory ~a." doc.tex.mz (current-directory))))
      (when (file-exists? name-with-mode.tex)
        (error (format "The file ~a already exists; please delete it and run again." name-with-mode.tex)))
      (with-handlers ([exn:fail? build-error])
        ;; Use mztext to process the file.
        (with-umask #o007 ; We restrict the umask first to be safe.
          (parameterize ([read-case-sensitive #t]
                         [current-directory doc-dir])
            (add-eval (read-syntax 'command-line (open-input-string mode-eval-string)))
            (add-eval (read-syntax 'command-line (open-input-string course-eval-string)))
            (add-eval (read-syntax 'command-line (open-input-string cs-eval-string)))
            (add-eval `(define name-with-mode ,name-with-mode))
            (with-output-to-file name-with-mode.tex #:exists 'replace
              (lambda () (preprocess doc.tex.mz)))))
        ;; Now run slatex on the generated tex file
        (slatex/no-latex name-with-mode.tex)
        ;; We've finished preprocessing the tex file. Make the PDF.
        (with-umask (output-umask)
          (build-tex-file name-with-mode)))
      ;; Cleanup: hide the tex file, and delete rubber's temporary files
      (rename-file-or-directory name-with-mode.tex (string-append "." name-with-mode.tex) #t)
      (let ([rubtmp-files (find-files (lambda (pth) (regexp-match "^rubtmp*" pth)))])
        (when (cons? rubtmp-files)
          (make-directory* "build_files")
          (map (lambda (pth) (rename-file-or-directory pth (build-path "build_files/" pth) #t)) rubtmp-files)))
      (when (build-error)
        (raise (build-error)))
      (void)))



