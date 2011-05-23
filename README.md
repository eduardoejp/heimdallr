
Heimdallr
==========

`heimdallr` is a library for Context-Oriented Programming in Clojure.

Usage
-----

Simply add this to your leiningen deps: `[heimdallr "0.1.0"]`

Documentation
------------

The documentation can be found here: http://eduardoejp.github.com/heimdallr/

Examples
--------

	(def *employer* {:name "Q-Box Technologies"})
	(def *person* {:name "Eduardo Julián", :employer *employer*, :address "c/ Manuel Rodriguez Objío, Santo Domingo"})

	(deflayer employer-layer)
	(deflayer address-layer)

	(deflayered to-str [person] (str "Name: " (:name person) "\n"))
	(deflayered to-str employer-layer [person] (str (proceed person) "Employer: " (:name (:employer person)) "\n"))
	(deflayered to-str address-layer [person] (str (proceed person) "Address: " (:address person) "\n"))

	(print (to-str *person*))

	(with-context address-layer
		(with-context employer-layer
		  (print (to-str *person*))))
	
	; The above code is the same as the following.
	
	(with-context [address-layer employer-layer]
		(print (to-str *person*)))

Further Information
-------------------

For more information about Context-Oriented Programming, please visit the following websites:

1.	http://www.jot.fm/issues/issue_2008_03/article4/

2.	http://www.swa.hpi.uni-potsdam.de/cop/
