(use-modules
 (guix packages)
 (gnu packages kerberos)
 (gnu packages gnuzilla))

(define my-icecat
  (package
    (inherit icecat)
    (name "my-icecat")))

(define replace-mit-krb5
  ;;
  (package-input-rewriting `((,mit-krb5 . ,heimdal))))

(define my-icecat-t
  (replace-mit-krb5 icecat))

my-icecat-t
