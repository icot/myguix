(use-modules (gnu)
             (gnu packages))

(define %my-packages
 (append
  '()
  '("emacs-pgtk-native-comp" "libvterm" "gcc-toolchain" "cmake" "make" "autoconf" "automake")
  '("guile-hall" "racket")
  '("nextcloud-client")
  '("bombadillo")
  '("exa" "bat" "fd" "fzf")))

(define %foreign-distro-packages
 (append
  '()
  '("guile" "guile-colorized" "guile-readline")
  '("glibc-locales")))

(define foreign-distro?
   (file-exists? "/etc/debian_version"))

(define %packages
  (if foreign-distro?
      (append %my-packages %foreign-distro-packages)
      (%my-packages)))

(apply values 
 (map specification->package %packages))
