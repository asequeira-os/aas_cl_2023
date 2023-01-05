;;build the whole system
(aas-build:load-asdf-libs
 '(:cl-fad ;;this is already loaded in init
   :swank ;;this is already loaded in init
   ;;following are sub deps - noted here for documentation
   ;;:cl-op this lib is dangerous - messes up compile for other libs
   :flexi-streams ;;due cl-ppcre
   :alexandria ;;due bordeaux-threads
   :chunga
   :cl-base64
   :cl+ssl
   :md5
   :rfc2388
   :trivial-backtrace
   :trivial-gray-streams
   :usocket
   :trivial-features ;;babel
   :puri

   ;;following are what I need
   :group-by
   :cl-ppcre
   :wu-decimal
   :split-sequence
   :fare-csv
   :bordeaux-threads
   :iterate
   :hunchentoot
   :babel
   :ironclad
   :drakma
   :cl-smtp
   :cl-postgres
   ))

(aas-build:compile-load-files
 '(
   ;;   "test/build" ; this is loaded in init also
   "sampling/build"
   "cl/build"
   "log/build"
   "assoc-db/assoc-db"
   "misc/build"
   "util/build"
   "http/build"
   "others/build"
   "graph/build"
   "www-ui-config/build"
   "aas-rpc/build"
   "db/build"
   "extern/build"
   "timezone/build"
   "i18n/build"
   "geo/build"
   "cloud/build"
   "anon-app/build"
   "auth/build"
   "auth/remote/build"
   "fin/cc/build"
   "xed/build"
   "load-xeduler"
   "tests"))
