(in-package :auth-remote)

(defgeneric auto-bind-db (app))

(defmethod auto-bind-db ((app t))
  t)

(defmethod auto-bind-db ((app (eql auth:+auth+)))
  nil)

(defmethod aas-rpc:app-rpc-handler :around
    ((app cloud:application) token rpc-meta function args)
  (declare (ignorable function args))
  (cloud:with-application app
    (let ((anon (aas-rpc::rpc-meta-anonymous rpc-meta))
          (allow-inactive (aas-rpc::rpc-meta-allow-inactive rpc-meta)))
      (or anon (eql app auth:+auth+)
          (auth-remote:auth-verify-remote token allow-inactive))
      (let ((time:*timezone* (if anon
                                 time:*timezone*
                                 (auth-token-tz token)))
            (i18n:*locale* (if anon
                               i18n:*locale* ;;this assumes browser based default is set
                               (auth-token-l token))))
        ;;todo 3 it should be possible to improve this
        ;;in case of proxy rpc if that macro sets something
        ;;i could save a db conn
        (if (and (not anon) (auto-bind-db app))
            (cloud:with-db-for-key (*db* (auth:auth-token-ext-db-key token))
              ;;FYI call-next-method passes the current method's
              ;;original arguments to the next method CLHS 7.7.31
              (call-next-method))
            (call-next-method))))))

