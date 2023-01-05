
(setf auth::*recaptcha-private-key* "UUUUUUUUUUUUUU")

;;i don't have reverse proxy in dev yet, so ports are same
(setf aas-http-server:*reverse-proxy-http-port* 8080)
(setf aas-http-server:*reverse-proxy-https-port* 8083)


;;todo 2 prod change call to start-server instead of restart-server
(or (aas-http-server:restart-server 8080 8083 "/xapi/v1"
                                    "~/git/config/https-cert/server.crt"
                                    "~/git/config/https-cert/server.key")
    (error "lisp http(s) server failed to start"))
