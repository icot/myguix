(use-modules (guix packages)
             (guix download)
             (guix utils)
             (guix build-system gnu)
             (guix build utils)
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages bison)
             (gnu packages perl))

(define-public my-mit-krb5
  (package
    (name "my-mit-krb5")
    (version "1.18")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://web.mit.edu/kerberos/dist/krb5/"
                                   (version-major+minor version)
                                   "/krb5-" version ".tar.gz")
                    (string-append "https://kerberos.org/dist/krb5/"
                                   (version-major+minor version)
                                   "/krb5-" version ".tar.gz")))
              (patches (search-patches "mit-krb5-qualify-short-hostnames.patch"
                                       "mit-krb5-hurd.patch"))
              (sha256
               (base32
                "121c5xsy3x0i4wdkrpw62yhvji6virbh6n30ypazkp0isws3k4bk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)))
    (arguments
     `(;; XXX: On 32-bit systems, 'kdb5_util' hangs on an fcntl/F_SETLKW call
       ;; while running the tests in 'src/tests'. Also disable tests when
       ;; cross-compiling.
       #:tests? ,(and (not (%current-target-system))
                      (string=? (%current-system) "x86_64-linux"))

       ,@(if (%current-target-system)
             '(#:configure-flags
               (list "--localstatedir=/var"
                     "krb5_cv_attr_constructor_destructor=yes"
                     "ac_cv_func_regcomp=yes"
                     "ac_cv_printf_positional=yes"
                     "ac_cv_file__etc_environment=yes"
                     "ac_cv_file__etc_TIMEZONE=no")
               #:make-flags
               (list "CFLAGS+=-DDESTRUCTOR_ATTR_WORKS=1"))
             '(#:configure-flags
               (list "--localstatedir=/var")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'create-link
           (lambda _
             (let* ((libpath (getenv "out"))
                    (origin (format #f "~a/lib/libgssapi_krb5.so" libpath))
                    (target (format #f "~a/lib/libgssapi.so" libpath)))
               (display (canonicalize-path "."))
               (symlink origin target))
             #t))
         (add-after 'unpack 'enter-source-directory
           (lambda _
             (chdir "src")
             #t))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((perl (assoc-ref (or native-inputs inputs) "perl")))
               (substitute* "plugins/kdb/db2/libdb2/test/run.test"
                 (("/bin/cat") (string-append perl "/bin/perl"))
                 (("D/bin/sh") (string-append "D" (which "sh")))
                 (("bindir=/bin/.") (string-append "bindir=" perl "/bin"))))

             ;; avoid service names since /etc/services is unavailable
             (substitute* "tests/resolve/Makefile"
               (("-p telnet") "-p 23"))
             #t)))))
    (synopsis "MIT Kerberos 5")
    (description
     "Massachusetts Institute of Technology implementation of Kerberos.
Kerberos is a network authentication protocol designed to provide strong
authentication for client/server applications by using secret-key
cryptography.")
    (license (license:non-copyleft "file://NOTICE"
                                   "See NOTICE in the distribution."))
    (home-page "https://web.mit.edu/kerberos/")
    (properties '((cpe-name . "kerberos")))))

my-mit-krb5
