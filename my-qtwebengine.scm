(use-modules
 (guix packages)
 ((guix licenses) #:prefix license:)
 (guix download)
 (guix utils)
 (guix build-system cmake)
 (guix build-system qt)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages linux)
 (gnu packages qt)
 (gnu packages gtk)
 (gnu packages cups)
 (gnu packages curl)
 (gnu packages glib)
 (gnu packages video)
 (gnu packages fontutils)
 (gnu packages icu4c)
 (gnu packages serialization)
 (gnu packages ghostscript)
 (gnu packages libevent)
 (gnu packages gnupg)
 (gnu packages image)
 (gnu packages xorg)
 (gnu packages xdisorg)
 (gnu packages xml)
 (gnu packages gl)
 (gnu packages compression)
 (gnu packages nss)
 (gnu packages xiph)
 (gnu packages pciutils)
 (gnu packages protobuf)
 (gnu packages pulseaudio)
 (gnu packages regex)
 (gnu packages valgrind)
 (gnu packages vulkan)
 (gnu packages bison)
 (gnu packages flex)
 (gnu packages gperf)
 (gnu packages ninja)
 (gnu packages perl)
 (gnu packages pkg-config)
 (gnu packages python)
 (gnu packages python-xyz)
 (gnu packages ruby)
 (gnu packages kerberos)
 (gnu packages web-browsers)
 )

(define (qt5-urls component version)
  "Return a list of URLs for VERSION of the Qt5 COMPONENT."
  ;; We can't use a mirror:// scheme because these URLs are not exact copies:
  ;; the layout differs between them.
  (list (string-append "https://download.qt.io/official_releases/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-src-"
                       version ".tar.xz")
        (string-append "https://download.qt.io/archive/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-src-"
                       version ".tar.xz")
        (let ((directory (string-append "qt5" (string-drop component 2))))
          (string-append "http://sources.buildroot.net/" directory "/"
                         component "-everywhere-src-" version ".tar.xz"))
        (string-append "https://distfiles.macports.org/qt5/"
                       component "-everywhere-src-" version ".tar.xz")))

(define-public my-qtwebengine
  (package
    (inherit qtsvg)
    (name "my-qtwebengine")
    (version (package-version qtbase))
    (source
     (origin
       (method url-fetch)
       (uri (qt5-urls "qtwebengine" version))
       (sha256
        (base32
         "1q4idxdm81sx102xc12ixj0xpfx52d6vwvs3jpapnkyq8c7cmby8"))
       (modules '((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (guix build utils)))
       (snippet
        '(begin
           (let ((preserved-third-party-files
                  '("base/third_party/double_conversion"
                    "base/third_party/cityhash"
                    "base/third_party/cityhash_v103"
                    "base/third_party/dynamic_annotations"
                    "base/third_party/icu"
                    "base/third_party/libevent"
                    "base/third_party/nspr"
                    "base/third_party/superfasthash"
                    "base/third_party/symbolize"
                    "base/third_party/xdg_mime"
                    "base/third_party/xdg_user_dirs"
                    "net/third_party/mozilla_security_manager"
                    "net/third_party/nss"
                    "net/third_party/quiche"
                    "net/third_party/uri_template"
                    "third_party/abseil-cpp"
                    "third_party/angle"
                    "third_party/angle/src/common/third_party/base"
                    "third_party/angle/src/common/third_party/smhasher"
                    "third_party/angle/src/common/third_party/xxhash"
                    "third_party/angle/src/third_party/compiler"
                    "third_party/axe-core"
                    "third_party/blink"
                    "third_party/boringssl"
                    "third_party/boringssl/src/third_party/fiat"
                    "third_party/breakpad"
                    "third_party/brotli"
                    "third_party/ced"
                    "third_party/cld_3"
                    "third_party/closure_compiler"
                    "third_party/crashpad"
                    "third_party/crashpad/crashpad/third_party/lss"
                    "third_party/crashpad/crashpad/third_party/zlib"
                    "third_party/crc32c"
                    "third_party/dav1d"
                    "third_party/dawn"
                    "third_party/devtools-frontend"
                    "third_party/devtools-frontend/src/front_end/third_party/fabricjs"
                    "third_party/devtools-frontend/src/front_end/third_party/lighthouse"
                    "third_party/devtools-frontend/src/front_end/third_party/wasmparser"
                    "third_party/devtools-frontend/src/third_party/axe-core"
                    "third_party/emoji-segmenter"
                    "third_party/ffmpeg"
                    "third_party/googletest"
                    "third_party/harfbuzz-ng/utils"
                    "third_party/hunspell"
                    "third_party/iccjpeg"
                    "third_party/icu"
                    "third_party/inspector_protocol"
                    "third_party/jinja2"
                    "third_party/jsoncpp"
                    "third_party/jstemplate"
                    "third_party/khronos"
                    "third_party/leveldatabase"
                    "third_party/libaddressinput"
                    "third_party/libgifcodec"
                    "third_party/libjingle_xmpp"
                    "third_party/libjpeg_turbo"
                    "third_party/libpng"
                    "third_party/libsrtp"
                    "third_party/libsync"
                    "third_party/libudev"
                    "third_party/libvpx"
                    "third_party/libwebm"
                    "third_party/libwebp"
                    "third_party/libxml"
                    "third_party/libxslt"
                    "third_party/libyuv"
                    "third_party/lss"
                    "third_party/mako"
                    "third_party/markupsafe"
                    "third_party/mesa_headers"
                    "third_party/metrics_proto"
                    "third_party/modp_b64"
                    "third_party/nasm"
                    "third_party/one_euro_filter"
                    "third_party/opus"
                    "third_party/ots"
                    "third_party/pdfium"
                    "third_party/pdfium/third_party/agg23"
                    "third_party/pdfium/third_party/base"
                    "third_party/pdfium/third_party/freetype"
                    "third_party/pdfium/third_party/lcms"
                    "third_party/pdfium/third_party/libopenjpeg20"
                    "third_party/pdfium/third_party/skia_shared"
                    "third_party/perfetto"
                    "third_party/pffft"
                    "third_party/ply"
                    "third_party/polymer"
                    "third_party/protobuf"
                    "third_party/protobuf/third_party/six"
                    "third_party/pyjson5"
                    "third_party/re2"
                    "third_party/rnnoise"
                    "third_party/skia"
                    "third_party/skia/include/third_party/skcms/skcms.h"
                    "third_party/skia/include/third_party/vulkan"
                    "third_party/skia/third_party/skcms"
                    "third_party/skia/third_party/vulkanmemoryallocator"
                    "third_party/smhasher"
                    "third_party/snappy"
                    "third_party/sqlite"
                    "third_party/usb_ids"
                    "third_party/usrsctp"
                    "third_party/web-animations-js"
                    "third_party/webrtc"
                    "third_party/webrtc/common_audio/third_party/fft4g"
                    "third_party/webrtc/common_audio/third_party/spl_sqrt_floor"
                    "third_party/webrtc/modules/third_party/fft"
                    "third_party/webrtc/modules/third_party/g711"
                    "third_party/webrtc/modules/third_party/g722"
                    "third_party/webrtc/rtc_base/third_party/base64"
                    "third_party/webrtc/rtc_base/third_party/sigslot"
                    "third_party/webrtc_overrides"
                    "third_party/widevine/cdm/widevine_cdm_common.h"
                    "third_party/widevine/cdm/widevine_cdm_version.h"
                    "third_party/woff2"
                    "third_party/yasm"
                    "third_party/zlib"
                    "url/third_party/mozilla"
                    "v8/src/third_party/utf8-decoder"
                    "v8/src/third_party/valgrind"
                    "v8/src/third_party/siphash"
                    "v8/third_party/v8/builtins"
                    "v8/third_party/inspector_protocol"))
                 (protected (make-regexp "\\.(gn|gyp)i?$")))
             (define preserved-club
               (map (lambda (member)
                      (string-append "./" member))
                    preserved-third-party-files))
             (define (empty? dir)
               (equal? (scandir dir) '("." "..")))
             (define (third-party? file)
               (string-contains file "third_party/"))
             (define (useless? file)
               (any (cute string-suffix? <> file)
                    '(".zip" ".so" ".dll" ".exe" ".jar")))
             (define (parents child)
               ;; Return all parent directories of CHILD up to and including
               ;; the closest "third_party".
               (let* ((dirs (match (string-split child #\/)
                              ((dirs ... last) dirs)))
                      (closest (list-index (lambda (dir)
                                             (string=? "third_party" dir))
                                           (reverse dirs)))
                      (delim (- (length dirs) closest)))
                 (fold (lambda (dir prev)
                         (cons (string-append (car prev) "/" dir)
                               prev))
                       (list (string-join (list-head dirs delim) "/"))
                       (list-tail dirs delim))))
             (define (remove-loudly file)
               (format #t "deleting ~a...~%" file)
               (force-output)
               (delete-file file))
             (define (delete-unwanted-files child stat flag base level)
               (match flag
                 ((or 'regular 'symlink 'stale-symlink)
                  (when (third-party? child)
                    (unless (or (member child preserved-club)
                                (any (cute member <> preserved-club)
                                     (parents child))
                                (regexp-exec protected child))
                      (remove-loudly child)))
                  (when (and (useless? child) (file-exists? child))
                    (remove-loudly child))
                  #t)
                 ('directory-processed
                  (when (empty? child)
                    (rmdir child))
                  #t)
                 (_ #t)))

             (with-directory-excursion "src/3rdparty"
               ;; TODO: Try removing "gn" too for future versions of qtwebengine.
               (delete-file-recursively "ninja")

               (with-directory-excursion "chromium"
                 ;; Delete bundled software and binaries that were not explicitly
                 ;; preserved above.
                 (nftw "." delete-unwanted-files 'depth 'physical)

                 ;; Assert that each preserved item is present to catch removals.
                 (for-each (lambda (third-party)
                             (unless (file-exists? third-party)
                               (error (format #f "~s does not exist!~%" third-party))))
                           preserved-club)

                 ;; Use relative header locations instead of hard coded ones.
                 (substitute*
                     "base/third_party/dynamic_annotations/dynamic_annotations.c"
                   (("base/third_party/valgrind") "valgrind"))
                 (substitute*
                     "third_party/breakpad/breakpad/src/common/linux/libcurl_wrapper.h"
                   (("third_party/curl") "curl"))
                 (substitute*
                     '("components/viz/common/gpu/vulkan_context_provider.h"
                       "components/viz/common/resources/resource_format_utils_vulkan.h"
                       "gpu/config/gpu_util.cc")
                   (("third_party/vulkan/include/")
                    ""))

                 ;; Replace Google Analytics bundle with an empty file and hope
                 ;; no one notices.
                 (mkdir-p "third_party/analytics")
                 (call-with-output-file
                     "third_party/analytics/google-analytics-bundle.js"
                   (lambda (port)
                     (const #t)))))
             ;; Do not enable support for loading the Widevine DRM plugin.
             (substitute* "src/buildtools/config/common.pri"
               (("enable_widevine=true")
                "enable_widevine=false"))
             #t)))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("ninja" ,ninja)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)
       ("python-six" ,python2-six)
       ("ruby" ,ruby)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("atk" ,atk)
       ("cups-minimal" ,cups-minimal)
       ("curl" ,curl)
       ("dbus" ,dbus)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("harbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("jsoncpp" ,jsoncpp)
       ("lcms" ,lcms)
       ("libcap" ,libcap)
       ("libevent" ,libevent)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg-turbo)
       ("libvpx" ,libvpx)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ;; FIXME: libxml2 needs to built with icu support though it links to
       ;; libxml2 configure summary still states "Checking for compatible
       ;; system libxml2... no"
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxtst" ,libxtst)
       ("mesa" ,mesa)
       ("minizip" ,minizip)
       ("mit-krb5" ,mit-krb5)
       ("nss" ,nss)
       ("opus" ,opus)
       ("pciutils" ,pciutils)
       ("protobuf" ,protobuf)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtwebchannel" ,qtwebchannel)
       ("re2" ,re2)
       ("snappy" ,snappy)
       ("udev" ,eudev)
       ("valgrind" ,valgrind)
       ("vulkan-headers" ,vulkan-headers)
       ("xcb-util" ,xcb-util)))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'substitute-source
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (nss (assoc-ref inputs "nss"))
                     (udev (assoc-ref inputs "udev")))
                 ;; Qtwebengine is not installed into the same prefix as
                 ;; qtbase.  Some qtbase QTLibraryInfo constants will not
                 ;; work.  Replace with the full path to the qtwebengine
                 ;; translations and locales in the store.
                 (substitute* "src/core/web_engine_library_info.cpp"
                   (("QLibraryInfo::location\\(QLibraryInfo::TranslationsPath\\)")
                    (string-append "QLatin1String(\"" out "/share/qt5/translations\")"))
                   (("QLibraryInfo::location\\(QLibraryInfo::DataPath\\)")
                    (string-append "QLatin1String(\"" out "/share/qt5\")")))
                 ;; Substitute full dynamic library path for nss.
                 (substitute* "src/3rdparty/chromium/crypto/nss_util.cc"
                   (("libnssckbi.so")
                    (string-append nss "/lib/nss/libnssckbi.so")))
                 ;; Substitute full dynamic library path for udev.
                 (substitute* "src/3rdparty/chromium/device/udev_linux/udev1_loader.cc"
                   (("libudev.so.1")
                    (string-append udev "/lib/libudev.so.1")))
                 #t)))
           (add-before 'configure 'set-env
             (lambda _
               ;; Avoids potential race conditions.
               (setenv "PYTHONDONTWRITEBYTECODE" "1")
               (setenv "NINJAFLAGS"
                       (string-append "-k1" ;less verbose build output
                                      ;; Respect the '--cores' option of 'guix build'.
                                      " -j" (number->string (parallel-job-count))))
               #t))
           (replace 'configure
             (lambda _
               ;; Valid QT_BUILD_PARTS variables are:
               ;; libs tools tests examples demos docs translations
               (invoke "qmake" "QT_BUILD_PARTS = libs tools" "--"
                       "--webengine-printing-and-pdf=no"
                       "--webengine-ffmpeg=system"
                       "--webengine-icu=system"
                       "--webengine-kerberos=yes"
                       "--webengine-pepper-plugins=no")))))
       ;; Tests are disabled due to "Could not find QtWebEngineProcess error"
       ;; It's possible this can be fixed by setting QTWEBENGINEPROCESS_PATH
       ;; before running tests.
       ((#:tests? _ #f) #f)))
    (native-search-paths
     (list (search-path-specification
            (file-type 'regular)
            (separator #f)
            (variable "QTWEBENGINEPROCESS_PATH")
            (files '("lib/qt5/libexec/QtWebEngineProcess")))))
    (home-page "https://wiki.qt.io/QtWebEngine")
    (synopsis "Qt WebEngine module")
    (description "The Qt5WebEngine module provides support for web applications
using the Chromium browser project.  The Chromium source code has Google services
and binaries removed, and adds modular support for using system libraries.")
    (license license:lgpl2.1+)))



(define replace-qtwebengine
  ;;
  (package-input-rewriting `((,qtwebengine . ,my-qtwebengine))))

(define my-qutebrowser
  (package
    (inherit qutebrowser)
    (name "my-qutebrowser")))

(define my-qutebrowser-t
  (replace-qtwebengine my-qutebrowser))

my-qtwebengine
my-qutebrowser-t
