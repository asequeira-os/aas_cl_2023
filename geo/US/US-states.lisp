;;basic info on  states of the United States
(in-package :geo-US)

(defparameter +US-STATE-CODES+MIL+ (list)
  "List of United States state codes including military")
(defparameter +US-STATE-CODES+ (list)
  "List of United States state codes")
(defparameter +US-STATE-NAMES+ (list)
  "List of United States state names")

(defparameter *us-state-code-to-name*
  (make-hash-table :test #'equal :size 100))

(defun make-us-state (name code &key (military nil))
  (push code +US-STATE-CODES+MIL+)
  (unless military
    (push code +US-STATE-CODES+))
  (push name +US-STATE-NAMES+)
  (setf (gethash code *us-state-code-to-name*) name))

(defun US-state-name (code)
  "returns the US state name for this code or nil"
  (values (gethash code *us-state-code-to-name*)))

;;got this list from
;;http://www.usps.com/ncsc/lookups/usps_abbreviations.html
;;says Official USPS Abbreviations
;;todo 2 there are lot of interesting official abbreviations
;;such as   APARTMENT	      APT
;;in the above url. get them?

(make-us-state "Alabama" "AL")
(make-us-state "Alaska" "AK")
(make-us-state "American Samoa" "AS")
(make-us-state "Arizona" "AZ")
(make-us-state "Arkansas" "AR")
(make-us-state "California" "CA")
(make-us-state "Colorado" "CO")
(make-us-state "Connecticut" "CT")
(make-us-state "Delaware" "DE")
(make-us-state "District of Columbia" "DC")
(make-us-state "Federated States of Micronesia" "FM")
(make-us-state "Florida" "FL")
(make-us-state "Georgia" "GA")
(make-us-state "Guam" "GU")
(make-us-state "Hawaii" "HI")
(make-us-state "Idaho" "ID")
(make-us-state "Illinois" "IL")
(make-us-state "Indiana" "IN")
(make-us-state "Iowa" "IA")
(make-us-state "Kansas" "KS")
(make-us-state "Kentucky" "KY")
(make-us-state "Louisiana" "LA")
(make-us-state "Maine" "ME")
(make-us-state "Marshall Islands" "MH")
(make-us-state "Maryland" "MD")
(make-us-state "Massachusetts" "MA")
(make-us-state "Michigan" "MI")
(make-us-state "Minnesota" "MN")
(make-us-state "Mississippi" "MS")
(make-us-state "Missouri" "MO")
(make-us-state "Montana" "MT")
(make-us-state "Nebraska" "NE")
(make-us-state "Nevada" "NV")
(make-us-state "New Hampshire" "NH")
(make-us-state "New Jersey" "NJ")
(make-us-state "New Mexico" "NM")
(make-us-state "New York" "NY")
(make-us-state "North Carolina" "NC")
(make-us-state "North Dakota" "ND")
(make-us-state "Northern Mariana Islands" "MP")
(make-us-state "Ohio" "OH")
(make-us-state "Oklahoma" "OK")
(make-us-state "Oregon" "OR")
(make-us-state "Palau" "PW")
(make-us-state "Pennsylvania" "PA")
(make-us-state "Puerto Rico" "PR")
(make-us-state "Rhode Island" "RI")
(make-us-state "South Carolina" "SC")
(make-us-state "South Dakota" "SD")
(make-us-state "Tennessee" "TN")
(make-us-state "Texas" "TX")
(make-us-state "Utah" "UT")
(make-us-state "Vermont" "VT")
(make-us-state "Virgin Islands" "VI")
(make-us-state "Virginia" "VA")
(make-us-state "Washington" "WA")
(make-us-state "West Virginia" "WV")
(make-us-state "Wisconsin" "WI")
(make-us-state "Wyoming" "WY")

;;military (do i need this?)

(make-us-state "Armed Forces" "AE" :military t)
(make-us-state "Armed Forces Americas (except Canada)" "AA" :military t)
(make-us-state "Armed Forces Pacific" "AP" :military t)

;;post process data
(setf +US-STATE-CODES+MIL+ (nreverse +US-STATE-CODES+MIL+))
(setf +US-STATE-CODES+ (nreverse +US-STATE-CODES+))
(setf +US-STATE-NAMES+ (nreverse +US-STATE-NAMES+))

