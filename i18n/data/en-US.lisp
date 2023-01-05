(in-package :i18n-data)

;; parameter syntax
;; use ~A to specify a parameter to be substituted
;; for reordering parameters, instead of ~A specify ~n@*~A
;; where n is the 0 based index
;; example
;;    "foo ~1@*~A ~0@*~A"
;; prints 2nd argument then the 1st argument
;; ref CLHS   22.3.7.1

(with-locale "en-US"
  (set-text test "test string")
  (set-text auth::confirm-email-subject
            "email confirmation from xeduler")
  (set-text auth::confirm-email-body
            "Please <a href=\"~A\">click</a> to confirm your email address
for xeduler.")
  (set-text auth::reset-password-email-subject
            "code to reset your xeduler password")
  (set-text auth::reset-password-email-body
            "Your password reset code is ~A ")


  (set-text auth::id-in-use "Login \"~A\" is already in use.")
  (set-text auth::invalid-login-id "Login id \"~A\" is not accepted.")
  (set-text auth::invalid-password "Password invalid for \"~A\".")
  (set-text auth::invalid-login "Login \"~A\" is not recognized.")
  (set-text auth::fail-password-rules "Password  is not accepted (to do: define rules) .")
  (set-text auth::fail-captcha "Captcha not accepted: ~A")
  (set-text auth::fail-password-reset-min-gap "We have already sent an email. Please wait for the email or try again in half an hour.")
  (set-text auth::token-expired "authentication expired")
  (set-text auth::acl-perm-denied "permission denied")

  (set-text xed::invalid-work-shift "working hours are not valid or conflicting")


  (set-text xed::enum-week-of-month #("First" "Second" "Third" "Fourth" "Last"))
  (set-text xed::enum-recur-freq #(nil nil "Hourly" "Daily" "Monthly" "Yearly"))
  )
