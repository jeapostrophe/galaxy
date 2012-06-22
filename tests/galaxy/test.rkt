#lang racket/base
(require rackunit
         racket/system
         unstable/debug
         racket/match
         (for-syntax racket/base
                     syntax/parse))

;; {{ Shelly
;; This macro is intended to make Eli proud.

;; Wow, RackUnit really sucks that test-begin/case don't work inside
;; each other like this already
(define (wrapping-test-case-around thunk)
  (with-handlers ([exn:test:check?
                   (λ (e)
                     (raise (struct-copy
                             exn:test:check e
                             [stack (list* (make-check-name (current-test-name))
                                           (exn:test:check-stack e))])))])
    (thunk)))
(define-syntax-rule (check-begin e ...)
  (parameterize ([current-test-case-around wrapping-test-case-around])
    (test-begin e ...)))
(define-syntax-rule (check-case m e ...)
  (parameterize ([current-test-case-around wrapping-test-case-around])
    (test-case m e ...)))

(begin-for-syntax
  (define-splicing-syntax-class shelly-case
    #:attributes (code)
    (pattern (~seq (~datum $) command-line:expr
                   (~optional (~seq (~datum =exit>) exit-cond:expr)
                              #:defaults ([exit-cond #'0]))
                   (~optional (~seq (~datum =stdout>) output-str:expr)
                              #:defaults ([output-str #'#f]))
                   (~optional (~seq (~datum =stderr>) error-str:expr)
                              #:defaults ([error-str #'#f]))
                   (~optional (~seq (~datum <input=) input-str:expr)
                              #:defaults ([input-str #'#f])))
             #:attr
             code
             (quasisyntax/loc
                 #'command-line
               (let ([cmd command-line])
                 (check-case
                  cmd
                  (define output-port (open-output-string))
                  (define error-port (open-output-string))
                  (printf "$ ~a\n" cmd)
                  (match-define
                   (list stdout stdin pid stderr to-proc)
                   (process/ports output-port
                                  (and input-str (open-input-string input-str))
                                  error-port
                                  cmd))
                  (to-proc 'wait)
                  (define cmd-status (to-proc 'exit-code))
                  (when stdout (close-input-port stdout))
                  (when stderr (close-input-port stderr))
                  (when stdin (close-output-port stdin))
                  (define actual-output
                    (get-output-string output-port))
                  (display actual-output)
                  (define actual-error
                    (get-output-string error-port))
                  (display actual-error (current-error-port))
                  #,(syntax/loc #'command-line
                      (when output-str
                        (check-equal? actual-output output-str "stdout")))
                  #,(syntax/loc #'command-line
                      (when error-str
                        (check-equal? actual-error error-str "stderr")))
                  #,(syntax/loc #'command-line
                      (check-equal? cmd-status exit-cond "exit code"))))))
    (pattern (~and (~not (~datum $))
                   code:expr))))

(define-syntax (shelly-begin stx)
  (syntax-parse
      stx
    [(_ case:shelly-case ...)
     (syntax/loc stx (test-begin case.code ...))]))
(define-syntax (shelly-case stx)
  (syntax-parse
      stx
    [(_ m:expr case:shelly-case ...)
     (syntax/loc stx
       (let ()
         (define mv m)
         (check-case mv
                     (printf "# Starting... ~a\n" mv)
                     case.code ...
                     (printf "# Ending... ~a\n" mv))))]))
(define-syntax (shelly-wind stx)
  (syntax-parse
      stx
    [(_ e:expr ... ((~datum finally) after:expr ...))
     (syntax/loc stx
       (dynamic-wind
           void
           (λ ()
             (shelly-begin e ...))
           (λ ()
             (shelly-begin after ...))))]))
;; }}

(require racket/file
         racket/runtime-path)

(define-runtime-path test-directory ".")

(require racket/path
         racket/list
         galaxy/util)

(define (get-info-domain-cache-path)
  (define c (first (current-library-collection-paths)))
  (define p (build-path c "info-domain" "compiled" "cache.rktd"))
  (and (file-exists? p)
       p))

(define (with-fake-root* t)
  (define tmp-dir
    (make-temporary-file ".racket.fake-root~a" 'directory
                         (find-system-path 'home-dir)))
  (define tmp-dir-s
    (path->string tmp-dir))
  (dynamic-wind
      void
      (λ ()
        ;; {{ This part is because I am developing galaxy in a
        ;; collection root link, but would be unnecessary once galaxy
        ;; is in the core
        (copy-file (find-system-path 'links-file)
                   (build-path tmp-dir "links.rktd"))
        (define info-domain-full-path
          (get-info-domain-cache-path))
        (define info-domain-rel-path
          (find-relative-path (find-system-path 'addon-dir)
                              info-domain-full-path))
        (define info-domain-new-path
          (build-path tmp-dir info-domain-rel-path))
        (make-parent-directory* info-domain-new-path)
        (copy-file info-domain-full-path
                   info-domain-new-path)
        ;; }}
        (putenv "PLTADDONDIR"
                tmp-dir-s)
        (t))
      (λ ()
        (delete-directory/files tmp-dir)
        (putenv "PLTADDONDIR"
                ""))))
(define-syntax-rule (with-fake-root e ...)
  (with-fake-root* (λ ()  e ...)))

(define (with-thread start-thread thunk)
  (define thread-id (thread start-thread))
  (dynamic-wind
      void
      thunk
      (λ () (kill-thread thread-id))))

(require web-server/http
         web-server/servlet-env)
(define (start-file-server)
  (serve/servlet (λ (req) (response/xexpr "None"))
                 #:command-line? #t
                 #:port 9999
                 #:extra-files-paths (list (build-path test-directory "test-pkgs"))))

(require meta/galaxy-index/basic/main)
(define *index-ht-1* (make-hash))
(define *index-ht-2* (make-hash))
(define (start-galaxy-server index-ht port)
  (serve/servlet (galaxy-index/basic
                  (λ (pkg-name) 
                    (define r (hash-ref index-ht pkg-name #f))
                    (printf "[>server ~a] ~a = ~a\n" port pkg-name r)
                    r))
                 #:command-line? #t
                 #:servlet-regexp #rx""
                 #:port port))

(define (with-servers* t)
  (with-thread
   (λ () (start-galaxy-server *index-ht-1* 9990))
   (λ ()
     (with-thread
      (λ () (start-galaxy-server *index-ht-2* 9991))
      (λ ()
        (with-thread (λ () (start-file-server))
                     t))))))
(define-syntax-rule (with-servers e ...)
  (with-servers* (λ () e ...)))

(with-servers
 (with-fake-root
  (parameterize ([current-directory test-directory])
    (shelly-begin
     (shelly-case
      "Each command has an associated help"
      $ "raco pkg -h"
      $ "raco pkg install -h"
      $ "raco pkg update -h"
      $ "raco pkg remove -h"
      $ "raco pkg show -h"
      $ "raco pkg create -h"
      $ "raco pkg config -h")

     (shelly-case
      "create"

      (shelly-case
       "create fails on missing directories"
       $ "rm -fr test-pkgs/does-not-exist test-pkgs/does-not-exist.tgz"
       $ "raco pkg create --format tgz test-pkgs/does-not-exist" =exit> 1
       $ "test -f test-pkgs/does-not-exist.tgz" =exit> 1)

      (define-syntax-rule (shelly-create pkg fmt)
        (shelly-case
         (format "create format ~a" fmt)
         $ (format "rm -f test-pkgs/~a.~a test-pkgs/~a.~a.CHECKSUM"
                   pkg fmt pkg fmt)
         $ (format "raco pkg create --format ~a test-pkgs/~a"
                   fmt pkg)
         $ (format "test -f test-pkgs/~a.~a" pkg fmt)
         $ (format "test -f test-pkgs/~a.~a.CHECKSUM" pkg fmt)))

      (shelly-create "galaxy-test1" "tgz")
      (shelly-create "galaxy-test1" "zip")
      (shelly-create "galaxy-test1-v2" "zip")
      (shelly-create "galaxy-test1-conflict" "zip")
      (shelly-create "galaxy-test1" "plt")
      (shelly-create "racket-conflict" "tgz")

      $ "raco pkg create --format txt test-pkgs/galaxy-test1" =exit> 1

      (shelly-create "galaxy-test2" "zip")

      (shelly-case
       "create is robust against ending /s"
       $ "rm -f test-pkgs/galaxy-test1.tgz test-pkgs/galaxy-test1.tgz.CHECKSUM"
       $ "raco pkg create --format tgz test-pkgs/galaxy-test1/"
       $ "test -f test-pkgs/galaxy-test1.tgz"
       $ "test -f test-pkgs/galaxy-test1.tgz.CHECKSUM"))

     (shelly-case
      "create MANIFESTs"
      $ "rm -f test-pkgs/galaxy-test1/MANIFEST"
      $ "raco pkg create --manifest test-pkgs/galaxy-test1/"
      $ "test -f test-pkgs/galaxy-test1/MANIFEST")

     (define (index-ht1-galaxy-test1)
       (hash-set! *index-ht-1* "galaxy-test1"
                  (hasheq 'checksum
                          (file->string "test-pkgs/galaxy-test1.zip.CHECKSUM")
                          'source
                          "http://localhost:9999/galaxy-test1.zip")))
     (index-ht1-galaxy-test1)
     (hash-set! *index-ht-1* "galaxy-test2"
                (hasheq 'checksum
                        (file->string "test-pkgs/galaxy-test2.zip.CHECKSUM")
                        'source
                        "http://localhost:9999/galaxy-test2.zip"))
     (hash-set! *index-ht-2* "galaxy-test2-snd"
                (hasheq 'checksum
                        (file->string "test-pkgs/galaxy-test2.zip.CHECKSUM")
                        'source
                        "http://localhost:9999/galaxy-test2.zip"))

     (define-syntax-rule (shelly-install** message pkg rm-pkg (pre ...) (more ...))
       (with-fake-root
        (shelly-case
         (format "Test installation of ~a" message)
         pre ...
         $ "racket -e '(require galaxy-test1)'" =exit> 1
         $ (format "raco pkg install ~a" pkg)
         $ "racket -e '(require galaxy-test1)'"
         more ...
         $ (format "raco pkg remove ~a" rm-pkg)
         $ "racket -e '(require galaxy-test1)'" =exit> 1)))

     (define-syntax-rule (shelly-install* message pkg rm-pkg more ...)
       (shelly-install** message pkg rm-pkg () (more ...)))

     (define-syntax-rule (shelly-install message pkg more ...)
       (shelly-install* message pkg "galaxy-test1" more ...))

     (shelly-case
      "raco pkg install tests"
      (shelly-install "local package (tgz)" "test-pkgs/galaxy-test1.tgz")
      (shelly-install "local package (zip)" "test-pkgs/galaxy-test1.zip")
      (shelly-install "local package (plt)" "test-pkgs/galaxy-test1.plt")

      (shelly-case
       "invalid package format is an error"
       $ "raco pkg install test-pkgs/galaxy-test1.zip.CHECKSUM" =exit> 1)

      (shelly-install "remote/URL/http package (file, tgz)"
                      "http://localhost:9999/galaxy-test1.tgz")
      (shelly-install "remote/URL/http package (directory)"
                      "http://localhost:9999/galaxy-test1/")

      (shelly-case
       "remote/URL/http directory, non-existant file"
       $ "raco pkg install http://localhost:9999/galaxy-test1.rar" =exit> 1)
      (shelly-case
       "remote/URL/http directory, no manifest fail"
       $ "raco pkg install http://localhost:9999/galaxy-test1/galaxy-test1" =exit> 1)
      (shelly-case
       "remote/URL/http directory, bad manifest"
       $ "raco pkg install http://localhost:9999/galaxy-test1-manifest-error" =exit> 1)

      (shelly-install "remote/github"
                      "github://github.com/jeapostrophe/galaxy/master/tests/galaxy/test-pkgs/galaxy-test1")
      (shelly-install "remote/github with slash"
                      "github://github.com/jeapostrophe/galaxy/master/tests/galaxy/test-pkgs/galaxy-test1/")

      (shelly-case
       "local directory fails when not there (because interpreted as package name that isn't there)"
       $ "raco pkg install test-pkgs/galaxy-test1-not-there" =exit> 1)

      (shelly-install "local package (directory)" "test-pkgs/galaxy-test1")
      (shelly-install "local package (directory with slash)" "test-pkgs/galaxy-test1/")

      (with-fake-root
       (shelly-case
        "linking local directory"
        (shelly-wind
         $ "cp -r test-pkgs/galaxy-test1 test-pkgs/galaxy-test1-linking"
         $ "racket -e '(require galaxy-test1)'" =exit> 1
         $ "raco pkg install --link test-pkgs/galaxy-test1-linking"
         $ "racket -e '(require galaxy-test1)'"
         $ "racket -e '(require galaxy-test1/a)'" =exit> 1
         $ "cp test-pkgs/galaxy-test1-staging/a.rkt test-pkgs/galaxy-test1-linking/galaxy-test1/a.rkt"
         $ "racket -e '(require galaxy-test1/a)'"
         $ "rm -f test-pkgs/galaxy-test1-linking/galaxy-test1/a.rkt"
         $ "racket -e '(require galaxy-test1/a)'" =exit> 1
         $ "raco pkg remove galaxy-test1-linking"
         $ "racket -e '(require galaxy-test1)'" =exit> 1
         (finally
          $ "rm -r test-pkgs/galaxy-test1-linking"))))

      (with-fake-root
       (shelly-case
        "remote/name package, doesn't work when no package there"
        $ "raco pkg config --set indexes http://localhost:9990"
        $ "raco pkg install galaxy-test1-not-there" =exit> 1))
      
      (with-fake-root
       (shelly-case
        "remote/name package"
        $ "raco pkg config --set indexes http://localhost:9990"
        $ "racket -e '(require galaxy-test1)'" =exit> 1
        $ "raco pkg install galaxy-test1"
        $ "racket -e '(require galaxy-test1)'"
        $ "raco pkg remove galaxy-test1"
        $ "racket -e '(require galaxy-test1)'" =exit> 1))

      (with-fake-root
       (shelly-case
        "remote/name package (multi)"
        $ "raco pkg config --set indexes http://localhost:9990 http://localhost:9991"
        $ "racket -e '(require galaxy-test1)'" =exit> 1
        $ "raco pkg install --deps search-auto galaxy-test2-snd"
        $ "racket -e '(require galaxy-test1)'"
        $ "racket -e '(require galaxy-test2)'"
        $ "raco pkg remove galaxy-test2-snd galaxy-test1"
        $ "racket -e '(require galaxy-test1)'" =exit> 1))

      (shelly-case
       "conflicts"
       (shelly-install "double install fails" "test-pkgs/galaxy-test1.zip"
                       $ "raco pkg install test-pkgs/galaxy-test1.zip" =exit> 1)
       (with-fake-root
        (shelly-case
         "conflicts with racket fail"
         $ "test -f test-pkgs/racket-conflict.tgz"
         $ "raco pkg install test-pkgs/racket-conflict.tgz" =exit> 1))
       (shelly-install "conflicts are caught" "test-pkgs/galaxy-test1.zip"
                       $ "test -f test-pkgs/galaxy-test1-conflict.zip"
                       $ "raco pkg install test-pkgs/galaxy-test1-conflict.zip" =exit> 1)
       (shelly-install "conflicts can be forced" "test-pkgs/galaxy-test1.zip"
                       $ "racket -e '(require galaxy-test1/conflict)'" =exit> 42
                       $ "raco pkg install --force test-pkgs/galaxy-test1-conflict.zip" =exit> 0
                       $ "racket -e '(require galaxy-test1/conflict)'" =exit> 42
                       $ "raco pkg remove galaxy-test1-conflict")
       (shelly-install "conflicts can be forced" "test-pkgs/galaxy-test1-conflict.zip"
                       $ "racket -e '(require galaxy-test1/conflict)'" =exit> 43
                       $ "raco pkg install --force test-pkgs/galaxy-test1.zip" =exit> 0
                       $ "racket -e '(require galaxy-test1/conflict)'" =exit> 43
                       $ "raco pkg remove galaxy-test1-conflict"))

      (shelly-case
       "checksums"
       $ "test -f test-pkgs/galaxy-test1.zip"
       $ "cp -f test-pkgs/galaxy-test1.zip test-pkgs/galaxy-test1-bad-checksum.zip"
       $ "test -f test-pkgs/galaxy-test1-conflict.zip.CHECKSUM"
       $ "cp -f test-pkgs/galaxy-test1-conflict.zip.CHECKSUM test-pkgs/galaxy-test1-bad-checksum.zip.CHECKSUM"
       (with-fake-root
        (shelly-case
         "checksums are checked if present (local)"
         $ "racket -e '(require galaxy-test1)'" =exit> 1
         $ "raco pkg install test-pkgs/galaxy-test1-bad-checksum.zip" =exit> 1
         $ "racket -e '(require galaxy-test1)'" =exit> 1))
       $ "cp -f test-pkgs/galaxy-test1.zip test-pkgs/galaxy-test1-no-checksum.zip"

       (shelly-install* "checksums are ignored if missing by default (local)"
                        "test-pkgs/galaxy-test1-no-checksum.zip"
                        "galaxy-test1-no-checksum")

       (with-fake-root
        (shelly-case
         "checksums are checked (remote)"
         $ "racket -e '(require galaxy-test1)'" =exit> 1
         $ "raco pkg install http://localhost:9999/galaxy-test1-bad-checksum.zip" =exit> 1
         $ "racket -e '(require galaxy-test1)'" =exit> 1))
       (with-fake-root
        (shelly-case
         "checksums are required by default remotely (remote)"
         $ "racket -e '(require galaxy-test1)'" =exit> 1
         $ "raco pkg install http://localhost:9999/galaxy-test1-no-checksum.zip" =exit> 1
         $ "racket -e '(require galaxy-test1)'" =exit> 1))
       (shelly-install* "but, bad checksums can be ignored (local)"
                        "--ignore-checksums test-pkgs/galaxy-test1-bad-checksum.zip"
                        "galaxy-test1-bad-checksum")
       (shelly-install* "but, bad checksums can be ignored (remote)"
                        "--ignore-checksums http://localhost:9999/galaxy-test1-bad-checksum.zip"
                        "galaxy-test1-bad-checksum")
       (shelly-install* "but, checksums can be missing if ignored (remote)"
                        "--ignore-checksums http://localhost:9999/galaxy-test1-no-checksum.zip"
                        "galaxy-test1-no-checksum"))

      (shelly-case
       "dependencies"

       $ "test -f test-pkgs/galaxy-test2.zip"
       (with-fake-root
        (shelly-case
         "local - fail (default)"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install test-pkgs/galaxy-test2.zip" =exit> 1
         $ "raco pkg install test-pkgs/galaxy-test1.zip" =exit> 0
         $ "raco pkg install test-pkgs/galaxy-test2.zip" =exit> 0
         $ "racket -e '(require galaxy-test2)'" =exit> 0
         $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
         $ "raco pkg remove galaxy-test2"
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "local - looks at all packages given on cmdline"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install test-pkgs/galaxy-test2.zip test-pkgs/galaxy-test1.zip" =exit> 0
         $ "racket -e '(require galaxy-test2)'" =exit> 0
         $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
         $ "raco pkg remove galaxy-test2"
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "local - fail"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install --deps fail test-pkgs/galaxy-test2.zip" =exit> 1
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "local - force"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install --deps force test-pkgs/galaxy-test2.zip"
         $ "racket -e '(require galaxy-test2)'" =exit> 0
         $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 1
         $ "raco pkg remove galaxy-test2"
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "local - search-ask [y]"
         $ "raco pkg config --set indexes http://localhost:9990"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install --deps search-ask test-pkgs/galaxy-test2.zip" =exit> 0 <input= "y\n"
         $ "racket -e '(require galaxy-test2)'" =exit> 0
         $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
         $ "raco pkg remove galaxy-test2"
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "local - search-ask []"
         $ "raco pkg config --set indexes http://localhost:9990"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install --deps search-ask test-pkgs/galaxy-test2.zip" =exit> 0 <input= "\n"
         $ "racket -e '(require galaxy-test2)'" =exit> 0
         $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
         $ "raco pkg remove galaxy-test2"
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "local - search-ask [n]"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install --deps search-ask test-pkgs/galaxy-test2.zip" =exit> 1 <input= "n\n"
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "local - search-auto"
         $ "raco pkg config --set indexes http://localhost:9990"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install --deps search-auto test-pkgs/galaxy-test2.zip" =exit> 0
         $ "racket -e '(require galaxy-test2)'" =exit> 0
         $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
         $ "raco pkg remove galaxy-test2"
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "remote - search-ask (default) [y]"
         $ "raco pkg config --set indexes http://localhost:9990"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install galaxy-test2" =exit> 0 <input= "y\n"
         $ "racket -e '(require galaxy-test2)'" =exit> 0
         $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
         $ "raco pkg remove galaxy-test2"
         $ "racket -e '(require galaxy-test2)'" =exit> 1))

       (with-fake-root
        (shelly-case
         "remote - fail"
         $ "racket -e '(require galaxy-test2)'" =exit> 1
         $ "raco pkg install --deps fail galaxy-test2" =exit> 1
         $ "racket -e '(require galaxy-test2)'" =exit> 1))))

     (shelly-case
      "update"
      (shelly-install "local packages can't be updated (file)"
                      "test-pkgs/galaxy-test1.zip"
                      $ "raco pkg update galaxy-test1" =exit> 1)
      (shelly-install "local packages can't be updated (directory)"
                      "test-pkgs/galaxy-test1"
                      $ "raco pkg update galaxy-test1" =exit> 1)
      (shelly-wind
       $ "mkdir -p test-pkgs/update-test"
       $ "cp -f test-pkgs/galaxy-test1.zip test-pkgs/update-test/galaxy-test1.zip"
       $ "cp -f test-pkgs/galaxy-test1.zip.CHECKSUM test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"
       (shelly-install* "remote packages can be updated"
                        "http://localhost:9999/update-test/galaxy-test1.zip"
                        "galaxy-test1"
                        $ "raco pkg update galaxy-test1" =exit> 0 =stdout> "No updates available\n"
                        $ "racket -e '(require galaxy-test1/update)'" =exit> 42
                        $ "cp -f test-pkgs/galaxy-test1-v2.zip test-pkgs/update-test/galaxy-test1.zip"
                        $ "cp -f test-pkgs/galaxy-test1-v2.zip.CHECKSUM test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"
                        $ "raco pkg update galaxy-test1" =exit> 0
                        $ "racket -e '(require galaxy-test1/update)'" =exit> 43)
       (finally
        $ "rm -f test-pkgs/update-test/galaxy-test1.zip"
        $ "rm -f test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"))

      (shelly-wind
       $ "mkdir -p test-pkgs/update-test"
       $ "cp -f test-pkgs/galaxy-test1.zip test-pkgs/update-test/galaxy-test1.zip"
       $ "cp -f test-pkgs/galaxy-test1.zip.CHECKSUM test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"
       (shelly-install* "update deps"
                        "http://localhost:9999/update-test/galaxy-test1.zip"
                        "galaxy-test1"
                        $ "raco pkg install test-pkgs/galaxy-test2.zip"
                        $ "raco pkg update --deps galaxy-test2" =exit> 0 =stdout> "No updates available\n"
                        $ "racket -e '(require galaxy-test1/update)'" =exit> 42
                        $ "cp -f test-pkgs/galaxy-test1-v2.zip test-pkgs/update-test/galaxy-test1.zip"
                        $ "cp -f test-pkgs/galaxy-test1-v2.zip.CHECKSUM test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"
                        $ "raco pkg update --deps galaxy-test2" =exit> 0
                        $ "racket -e '(require galaxy-test1/update)'" =exit> 43
                        $ "raco pkg remove galaxy-test2")
       (finally
        $ "rm -f test-pkgs/update-test/galaxy-test1.zip"
        $ "rm -f test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"))

      (shelly-wind
       $ "mkdir -p test-pkgs/update-test"
       $ "cp -f test-pkgs/galaxy-test1.zip test-pkgs/update-test/galaxy-test1.zip"
       $ "cp -f test-pkgs/galaxy-test1.zip.CHECKSUM test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"
       (shelly-install* "update all is default"
                        "http://localhost:9999/update-test/galaxy-test1.zip"
                        "galaxy-test1"
                        $ "raco pkg install test-pkgs/galaxy-test2.zip"
                        $ "raco pkg update" =exit> 0 =stdout> "No updates available\n"
                        $ "racket -e '(require galaxy-test1/update)'" =exit> 42
                        $ "cp -f test-pkgs/galaxy-test1-v2.zip test-pkgs/update-test/galaxy-test1.zip"
                        $ "cp -f test-pkgs/galaxy-test1-v2.zip.CHECKSUM test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"
                        $ "raco pkg update" =exit> 0
                        $ "racket -e '(require galaxy-test1/update)'" =exit> 43
                        $ "raco pkg remove galaxy-test2")
       (finally
        $ "rm -f test-pkgs/update-test/galaxy-test1.zip"
        $ "rm -f test-pkgs/update-test/galaxy-test1.zip.CHECKSUM"))

      (shelly-wind
       $ "cp -f test-pkgs/galaxy-test1.zip test-pkgs/galaxy-test1.zip.bak"
       $ "cp -f test-pkgs/galaxy-test1.zip.CHECKSUM test-pkgs/galaxy-test1.zip.CHECKSUM.bak"
       (shelly-install**
        "named remote packages can be update"
        "galaxy-test1" "galaxy-test1"
        ($ "raco pkg config --set indexes http://localhost:9990")
        ($ "raco pkg update galaxy-test1" =exit> 0 =stdout> "No updates available\n"
           $ "racket -e '(require galaxy-test1/update)'" =exit> 42
           $ "cp test-pkgs/galaxy-test1-v2.zip test-pkgs/galaxy-test1.zip"
           $ "cp test-pkgs/galaxy-test1-v2.zip.CHECKSUM test-pkgs/galaxy-test1.zip.CHECKSUM"
           (index-ht1-galaxy-test1)
           $ "raco pkg update galaxy-test1" =exit> 0
           $ "racket -e '(require galaxy-test1/update)'" =exit> 43))
       (finally
        $ "cp -f test-pkgs/galaxy-test1.zip.bak test-pkgs/galaxy-test1.zip"
        $ "cp -f test-pkgs/galaxy-test1.zip.CHECKSUM.bak test-pkgs/galaxy-test1.zip.CHECKSUM"
        (index-ht1-galaxy-test1))))

     ;; XXX update should support different dep-behavior (like install)

     ;; XXX update should allow updates to thing that are dependencies
     ;; of other things

     ;; XXX update should deal with new deps correctly [* meaning
     ;; should leave the old version in place if they are not
     ;; available]

     ;; XXX if you update a dependency, it should still be considered
     ;; a dependency after the fact

     (shelly-case
      "remove"
      ;; XXX see that remove deletes the entry from show
      (shelly-case "remove of not installed package fails"
                   $ "raco pkg remove not-there" =exit> 1)
      (shelly-install "remove test"
                      "test-pkgs/galaxy-test1.zip")
      (shelly-install "remove of dep fails"
                      "test-pkgs/galaxy-test1.zip"
                      $ "raco pkg install test-pkgs/galaxy-test2.zip"
                      $ "raco pkg remove galaxy-test1" =exit> 1
                      $ "raco pkg remove galaxy-test2")
      (shelly-install "remove of dep can be forced"
                      "test-pkgs/galaxy-test1.zip"
                      $ "raco pkg install test-pkgs/galaxy-test2.zip"
                      $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
                      $ "raco pkg remove --force galaxy-test1"
                      $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 1
                      $ "raco pkg install test-pkgs/galaxy-test1.zip"
                      $ "raco pkg remove galaxy-test2")
      (with-fake-root
       (shelly-case
        "remove two"
        $ "racket -e '(require galaxy-test1)'" =exit> 1
        $ "racket -e '(require galaxy-test2)'" =exit> 1
        $ "raco pkg install test-pkgs/galaxy-test2.zip test-pkgs/galaxy-test1.zip" =exit> 0
        $ "racket -e '(require galaxy-test1)'" =exit> 0
        $ "racket -e '(require galaxy-test2)'" =exit> 0
        $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
        $ "raco pkg remove galaxy-test1 galaxy-test2"
        $ "racket -e '(require galaxy-test1)'" =exit> 1
        $ "racket -e '(require galaxy-test2)'" =exit> 1))
      (with-fake-root
       (shelly-case
        "autoremove"
        $ "raco pkg config --set indexes http://localhost:9990"
        $ "racket -e '(require galaxy-test1)'" =exit> 1
        $ "racket -e '(require galaxy-test2)'" =exit> 1
        $ "raco pkg install --deps search-auto test-pkgs/galaxy-test2.zip" =exit> 0
        $ "racket -e '(require galaxy-test1)'" =exit> 0
        $ "racket -e '(require galaxy-test2)'" =exit> 0
        $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
        $ "raco pkg remove galaxy-test2"
        $ "racket -e '(require galaxy-test1)'" =exit> 0
        $ "raco pkg remove --auto"
        $ "racket -e '(require galaxy-test1)'" =exit> 1
        $ "racket -e '(require galaxy-test2)'" =exit> 1)))

     ;; XXX wrong checksum on pis package
     ;; XXX packages that aren't roots
     ;; XXX lock the installation directory
     ;; XXX cause raco setup to run on the new roots afterwards
     ;; XXX show (list installed, mark the ones that are deps, show checksum and/or location on disk)
     ;; XXX planet compatibility server
     ;; XXX system installation tests
     ;; XXX more config tests (viewing)
     ;; XXX scour github for initial packages
     ;; XXX packages can't contain /
     ))))
