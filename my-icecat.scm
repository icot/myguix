(use-modules
 (guix packages)
 (gnu packages kerberos)
 (gnu packages gnuzilla))

(define replace-mit-krb5
  ;;
  (package-input-rewriting `((,mit-krb5 . ,my-mit-krb5))))

(define my-icecat
  (replace-mit-krb5 icecat))

my-icecat
