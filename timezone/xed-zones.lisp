(in-package :aas-local-time)

;;this defines internal xed ids for timezone names
;;the different names could be pointing to the same zone-tr-data
;;to define timezone names modify the xed data text file in the timezones data dir

(set-tz-id "UTC" 1)

;;Zones for USA
(set-tz-id "US/Alaska Standard Time" 11)
(set-tz-id "US/Central Standard Time" 12)
(set-tz-id "US/Mountain Standard Time" 13)
(set-tz-id "US/Pacific Standard Time" 14)
(set-tz-id "US/Eastern Standard Time" 15)
(set-tz-id "US/Guam" 16)
(set-tz-id "US/Hawaii Standard Time" 17)
(set-tz-id "US/Samoa" 18)

;;Zones for Canada
(set-tz-id "Canada/Pacific Standard Time"  30)
(set-tz-id "Canada/Mountain Standard Time (No DST)" 31)
(set-tz-id "Canada/Mountain Standard/Daylight Time" 32)
(set-tz-id "Canada/Central Standard Time (No DST)" 33)
(set-tz-id "Canada/Central Standard/Daylight Time" 34)
(set-tz-id "Canada/Eastern Standard Time (No DST)" 35)
(set-tz-id "Canada/Eastern Standard/Daylight Time" 36)
(set-tz-id "Canada/Atlantic Standard Time (No DST)" 37)
(set-tz-id "Canada/Atlantic Standard/Daylight Time" 38)
(set-tz-id "Canada/Newfoundland Standard/Daylight Time" 39)

