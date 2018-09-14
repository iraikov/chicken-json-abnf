
(import scheme (chicken base) json-abnf test)

;; examples from RFC 4627
(define inputs
  `(
#<<EOF
[]
EOF
#<<EOF
{}
EOF
#<<EOF
[[{}], "", {}]
EOF
#<<EOF
["\n", "\u0040", "test!"]
EOF
#<<EOF
[false, true, null]
EOF
#<<EOF
{"str":"a","true":true,"false":false}
EOF
#<<EOF
  {
      "Image": {
          "Width":  800,
          "Height": 600,
          "Title":  "View from 15th Floor",
          "Thumbnail": {
              "Url":    "http://www.example.com/image/481989943",
              "Height": 125,
              "Width":  "100"
          },
          "IDs": [116, 943, 234, 38793]
        }
   }
EOF
#<<EOF
   [
      {
         "precision": "zip",
         "Latitude":  37.7668,
         "Longitude": -122.3959,
         "Address":   "",
         "City":      "SAN FRANCISCO",
         "State":     "CA",
         "Zip":       "94107",
         "Country":   "US"
      },
      {
         "precision": "zip",
         "Latitude":  37.371991,
         "Longitude": -122.026020,
         "Address":   "",
         "City":      "SUNNYVALE",
         "State":     "CA",
         "Zip":       "94085",
         "Country":   "US"
      }
   ]
EOF
))

(define outputs
  `(
    ,(list->vector (list))
    ()
    ,(list->vector (list (list->vector (list '())) "" '()))
    ,(list->vector (list (list->string (list #\newline)) (list->string (list #\@)) "test!"))
    ,(list->vector (list #f #t 'null))
    (("str" . "a") ("true" . #t) ("false" . #f))
    (
      ("Image" .
       (
	("Width"  . 800) 
	("Height" . 600) 
	("Title"  . "View from 15th Floor") 
	("Thumbnail" . 
	 ( 
	  ("Url"    . "http://www.example.com/image/481989943") 
	  ("Height" . 125) 
	  ("Width"  . "100"))) 
	("IDs" . #(116 943 234 38793)))))
    #(( ("precision" . "zip") 
	       ("Latitude"  . 37.7668) 
	       ("Longitude" . -122.3959) 
	       ("Address"   . "") 
	       ("City"      . "SAN FRANCISCO") 
	       ("State"     . "CA") 
	       ("Zip"       . "94107") 
	       ("Country"   . "US")) 
       ( ("precision" . "zip") 
	       ("Latitude"  . 37.371991) 
	       ("Longitude" . -122.02602) 
	       ("Address"   . "") 
	       ("City"      . "SUNNYVALE") 
	       ("State"     . "CA") 
	       ("Zip"       . "94085") 
	       ("Country"   . "US")))
    ))
  

(test-group "json test"
	    (for-each
	     (lambda (in out) 
	       (test out (parser in)))
	     inputs outputs))

(test-exit)
