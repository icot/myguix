;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (gnu packages)
             (gnu packages shells)
             (gnu packages chromium)
             (gnu packages wm)
             (gnu packages security-token)
             (gnu packages android)
             (nongnu packages linux)       ; nongnu channel
             (nongnu system linux-initrd)
             (guix channels)
             (guix inferior)
             (srfi srfi-1)) ; first

(use-service-modules desktop networking ssh xorg kerberos)

;; Yubikeys: https://github.com/Yubico/libu2f-host/blob/master/70-u2f.rules
(define %yubikey-udev-rule1
  (udev-rule
   "90-yubikey"
   (string-append "KERNEL==\"hidraw\", "
                  "SUBSYSTEM==\"hidraw\", "
                  "ATTRS{idVendor}==\"1050\", "
                  "ATTRS{idProduct}==\"0113|0114|0115|0116|0120|0200|0402|0403|0406|0407|0410\", "
                  "TAG+=\"uaccess\"")))

(define %yubikey-udev-rule2
  (udev-rule
   "91-myyubikey"
   (string-append "SUBSYSTEM==\"usb\", "
                  "ATTRS{idVendor}==\"1050\", "
                  "ATTRS{idProduct}==\"0113|0114|0115|0116|0120|0200|0402|0403|0406|0407|0410\", "
                  "MODE=0666")))

(define %yubikey-udev-rules
 (file->udev-rule "99-myu2f" (file-append libu2f-host "/lib/udev/rules.d/70-u2f.rules")))
;;;; Custom definitions
(define %my-sudoers
  (plain-file "sudoers"
              "root ALL=(ALL) ALL\n%wheel ALL=(ALL) ALL\n@includedir /etc/sudoers.d\n"))

;; Custom services
(define %custom-services
  (modify-services %desktop-services
    (guix-service-type config =>
      (guix-configuration (inherit config)
        (substitute-urls (cons "http://10.0.0.38:8181" %default-substitute-urls))
        (authorized-keys (cons (local-file "/etc/guix.d/erazer.pub") %default-authorized-guix-keys))))))

(define %custom-services-with-udev
  (modify-services %custom-services
    (udev-service-type config =>
      (udev-configuration (inherit config)
        (rules (append (udev-configuration-rules config)
                       (list %yubikey-udev-rule1
                             %yubikey-udev-rule2)))))))

;; Avoiding kernel recompilation: https://gitlab.com/nonguix/nonguix
(define %my-kernel
   (let*
      ((channels
        (list (channel
               (name 'nonguix)
               (url "https://gitlab.com/nonguix/nonguix")
               (commit "954a6e9ac21590337fa4157a4f7119c80e6bbea5"))
              (channel
               (name 'guix)
               (url "https://git.savannah.gnu.org/git/guix.git")
               (commit "58b85f7f419e77930765647ffc41011c1103066e"))))
       (inferior
        (inferior-for-channels channels)))
      (first (lookup-inferior-packages inferior "linux" "5.12.6"))))


(define %my-packages
  '(
    ;; WM/Env
    "i3-gaps"
    "i3lock"
    "polybar"
    "feh"
    "kitty"
    "xmodmap"
    "xset"
    "rofi"
    "rlwrap"
    "pasystray"
    "imagemagick"
    "stow"
    "git"
    "tig"
    "ripgrep"
    "exa"
    "bat"
    "fd"
    "fzf"
    "curl"
    "ncurses"
    "ncdu"
    "file"
    "icecat"
    "qutebrowser"
    "nss-certs"))

;; TODO
;;
;; From aiadm cern-realm-cernch.conf:
;;   v4_name_convert = { host = { rcmd = host } }
;;
(define %my-kerberos
  (service krb5-service-type
           (krb5-configuration
            (default-realm "CERN.CH")
            (allow-weak-crypto? #f)
            (rdns? #f)
            (permitted-enctypes  (list
                                  "aes256-cts-hmac-sha1-96"
                                  "aes256-cts-hmac-sha384-192"
                                  "camellia256-cts-cmac"
                                  "aes128-cts-hmac-sha1-96"
                                  "aes128-cts-hmac-sha256-128"
                                  "camellia128-cts-cmac"))
            (realms (list
                     (krb5-realm
                      (name "CERN.CH")
                      (default-domain "cern.ch")
                      (admin-server "cerndc.cern.ch")
                      (kdc "cerndc.cern.ch")
                      (kpasswd-server "cerndc.cern.ch")))))))

;; System
(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Zurich")
  (keyboard-layout
    (keyboard-layout "us" "altgr-intl"))
  (host-name "aoi")

  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (target "/dev/sda")
      (keyboard-layout keyboard-layout)))

  ;; < nongnu
  (kernel %my-kernel)
  (initrd microcode-initrd)
  (firmware (cons* iwlwifi-firmware %base-firmware))
  ;; > nongnu

  (swap-devices
    (list (uuid "6bff2c07-fad3-4920-a8a8-fa865aa39775")))

  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "1114202d-0c6f-425c-bc22-8d6b56978f0c"
                     'ext4))
             (type "ext4"))
           %base-file-systems))

  (users (cons* (user-account
                  (name "spike")
                  (comment "Spike")
                  (group "users")
                  (shell (file-append zsh "/bin/zsh"))
                  (home-directory "/home/spike")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  (sudoers-file %my-sudoers)

  (packages
    (append
     (map specification->package %my-packages)
     %base-packages))

  (services
    (append
      (list (service openssh-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout)))
            (screen-locker-service i3lock)
            %my-kerberos
            (udev-rules-service 'myyubis %yubikey-udev-rules))
      %custom-services-with-udev))

  (name-service-switch %mdns-host-lookup-nss))
