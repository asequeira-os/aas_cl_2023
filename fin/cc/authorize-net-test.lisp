(in-package :fin-cc-test)

;;sample response
;; "\"1\",\"1\",\"1\",\"This transaction has been approved.\",\"K5ND7A\",\"Y\",\"2162379510\",\"\",\"xxx need auth tx description config\",\"1.00\",\"CC\",\"auth_only\",\"\",\"giveaname244\",\"cosby122\",\"\",\"roflane795\",\"plano\",\"TX\",\"75086\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"QQQQQQQQQQQQQ\",\"P\",\"2\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"XXXX0002\",\"American Express\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\""


(deftest authorize-net-tests
  (and (avs-code-tests)))

(deftest avs-code-tests
  (loop for char across "ABEGNPRSUWXYZ" do
       (let ((avs-status (authorize-net-avs-status char)))
         (is avs-status)))
  (verify-error error
      (authorize-net-avs-status #\C))
  (verify-error error
      (authorize-net-avs-status "qqq"))
  (let* ((avs-status (authorize-net-avs-status #\W))
         (ons (avs-status-on avs-status))
         (offs (avs-status-off avs-status)))
    (is (avs-status-flags-zip5 ons))
    (is (avs-status-flags-zip4 ons))
    (is-not (avs-status-flags-street ons))
    (is-not (avs-status-flags-no-info ons))
    (is-not (avs-status-flags-avs-error ons))
    (is-not (avs-status-flags-service-n/a ons))
    (is-not (avs-status-flags-non-us-bank ons))
    (is-not (avs-status-flags-not-applicable ons))
    (is (avs-status-flags-street offs))
    (is (avs-status-flags-zip5 ons))
    (is (avs-status-flags-zip4 ons))
    (is-not (avs-status-flags-no-info offs))
    (is-not (avs-status-flags-avs-error offs))
    (is-not (avs-status-flags-service-n/a offs))
    (is-not (avs-status-flags-non-us-bank offs))
    (is-not (avs-status-flags-not-applicable offs))))


