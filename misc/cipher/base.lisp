(in-package :cipher)

(defgeneric digest (string &optional salt))
(defgeneric crc32 (string))
(defgeneric encrypt (string key))
(defgeneric decrypt (data key))


(defmethod digest ((string string) &optional (salt ""))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:concatenate-strings-to-octets :utf-32le string salt))))

(defmethod crc32 ((string string))
  (util:network-bytes-to-number
   (ironclad:digest-sequence
    :crc32 (babel:string-to-octets string :encoding :utf-32le))
   0 32))

(defmethod encrypt ((data null) (key string))
  nil)

(defmethod encrypt ((data string) (key string))
  (let ((key-vector (babel:string-to-octets key :encoding :utf-8 ))
        (bytes (babel:string-to-octets data)))
    (let ((cipher (ironclad:make-cipher :blowfish
                                         :mode :ecb
                                         :key key-vector)))
      (ironclad:encrypt-in-place cipher bytes)
      bytes)))

(defmethod decrypt ((data null) (key string))
  nil)

(defmethod decrypt (data (key string))
  (let ((key-vector (babel:string-to-octets key :encoding :utf-8 )))
    (let ((cipher (ironclad:make-cipher :blowfish
                                        :mode :ecb
                                        :key key-vector)))
      (ironclad:decrypt-in-place cipher data)
      (babel:octets-to-string data :encoding :utf-8))))


