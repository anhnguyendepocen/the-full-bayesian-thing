; ch 3 - inferences with flips


;; 3.1 inferring a rate

(define n 10)

(define (rate1)
	(mh-query 10000 10
		(define theta (beta 1 1))
		(define k (repeat n (lambda () (boolean->number (flip theta)))))

		theta

		(equal? (sum k) 5)))

(hist (rate1) "figure 3.2")

;;; exercise 3.1.3

(define n 20)

(define (rate1)
	(mh-query 10000 10
		(define theta (beta 1 1))
		(define k (repeat n (lambda () (boolean->number (flip theta)))))

		theta

		(equal? (sum k) 10)))

(hist (rate1) "exercise 3.1.3")

;;; exercise 3.1.5

(define n 100)

(define (rate1)
	(mh-query 5000 10
		(define theta (beta 1 1))
		(define k (repeat n (lambda () (boolean->number (flip theta)))))

		theta

		(equal? (sum k) 99)))

(hist (rate1) "exercise 3.1.5")


;;; exercise 3.1.6

(define n 1)

(define (rate1)
	(mh-query 5000 10
		(define theta (beta 1 1))
		(define k (repeat n (lambda () (boolean->number (flip theta)))))

		theta

		(equal? (sum k) 0)))

(hist (rate1) "exercise 3.1.6")


;; 3.2 difference between two rates



(define (rate2 k1 k2 n1 n2)
	(mh-query 1000 10
		(define theta1 (beta 1 1))
		(define theta2 (beta 1 1))
		(define k1* (repeat n1 (lambda () (boolean->number (flip theta1)))))
		(define k2* (repeat n2 (lambda () (boolean->number (flip theta2)))))
		(define delta (- theta1 theta2))

		delta

		(and (equal? k1 (sum k1*))
			(equal? k2 (sum k2*)))))

(hist (rate2 5 7 10 10) "figure 3.4: difference in rates")

;;; exercise 3.2.1
; (multiviz
(hist (rate2 8 7 10 10) "exericse 3.2.1a")

(hist (rate2 80 70 100 100) "exericse 3.2.1b (10x data of a)") ; This one has a hard time running
	;)


;;; exercise 3.2.2

(hist (rate2 0 0 1 5) "exercise 3.2.2")

;; 3.3 inferring a common rate


(define (rate3 k1 k2 n1 n2)
	(mh-query 3000 10
		(define theta (beta 1 1))
		(define k1* (repeat n1 (lambda () (boolean->number (flip theta)))))
		(define k2* (repeat n2 (lambda () (boolean->number (flip theta)))))

		theta

		(and (equal? k1 (sum k1*))
			(equal? k2 (sum k2*)))))


(hist (rate3 5 7 10 10) "figure 3.7: common rate of two binomial processes")

;;; exercise 3.3.1
(hist (rate3 14 16 20 20) "exercise 3.3.1")

;;; exercise 3.3.2
(hist (rate3 0 10 10 10) "exercise 3.3.2") ; hard time

;;; exercise 3.3.3
(mutlviz 
	(hist (rate3 7 3 10 10) "data 1")
	(hist (rate3 5 5 10 10) "data 2"))


;; 3.4 prior and posterior prediction

(define (rate4 k n)
	(mh-query 10000 10
		(define theta (beta 1 1))
		(define k* (sum (repeat n (lambda () (boolean->number (flip theta))))))
		(define theta-prior (beta 1 1))
		(define prior-predk (sum (repeat n (lambda () (boolean->number (flip theta-prior))))))
		(define post-predk (sum (repeat n (lambda () (boolean->number (flip theta))))))

		(list theta theta-prior prior-predk post-predk)

		(equal? k k*)))

;;; figure 3.9

(define samples (rate4 1 15))

(multiviz
 (density (map first samples) "theta")
 (density (map second samples) "theta prior")
 (hist (map third samples) "prior predictive over k")
 (hist (map fourth samples) "posterior predictive over k"))

;;; exercise 3.4.2

(define (rate4a k n)
	(mh-query 10000 10
		(define theta (beta 10 10))
		(define k* (sum (repeat n (lambda () (boolean->number (flip theta))))))
		(define theta-prior (beta 1 1))
		(define prior-predk (sum (repeat n (lambda () (boolean->number (flip theta-prior))))))
		(define post-predk (sum (repeat n (lambda () (boolean->number (flip theta))))))

		(list theta theta-prior prior-predk post-predk)

		(equal? k k*)))

(define samples (rate4a 1 15))

(multiviz
 (density (map first samples) "theta")
 (density (map second samples) "theta prior")
 (hist (map third samples) "prior predictive over k")
 (hist (map fourth samples) "posterior predictive over k"))

(define (rate4b k n)
	(mh-query 10000 10
		(define theta (beta 1 5))
		(define k* (sum (repeat n (lambda () (boolean->number (flip theta))))))
		(define theta-prior (beta 1 1))
		(define prior-predk (sum (repeat n (lambda () (boolean->number (flip theta-prior))))))
		(define post-predk (sum (repeat n (lambda () (boolean->number (flip theta))))))

		(list theta theta-prior prior-predk post-predk)

		(equal? k k*)))

(define samples (rate4b 1 15))

(multiviz
 (density (map first samples) "theta")
 (density (map second samples) "theta prior")
 (hist (map third samples) "prior predictive over k")
 (hist (map fourth samples) "posterior predictive over k"))


(define (rate4c k n)
	(mh-query 10000 10
		(define theta (beta 0.1 0.1))
		(define k* (sum (repeat n (lambda () (boolean->number (flip theta))))))
		(define theta-prior (beta 1 1))
		(define prior-predk (sum (repeat n (lambda () (boolean->number (flip theta-prior))))))
		(define post-predk (sum (repeat n (lambda () (boolean->number (flip theta))))))

		(list theta theta-prior prior-predk post-predk)

		(equal? k k*)))

(define samples (rate4c 1 15))

(multiviz
 (density (map first samples) "theta")
 (density (map second samples) "theta prior")
 (hist (map third samples) "prior predictive over k")
 (hist (map fourth samples) "posterior predictive over k"))


;;; exercise 3.4.4 Trompetter data
; In October 2009, the Dutch newspaper Trouw reported on research conducted by
; H. Trompetter, a student from the Radboud University in the city of Nijmegen. 
; For her undergraduate thesis, Trompetter had interviewed 121 older adults living 
; in nursing homes. Out of these 121 older adults, 24 (about 20%) indicated that 
; they had at some point been bullied by their fellow residents. Trompetter 
; rejected the suggestion that her study may have been too small to draw reliable 
; conclusions: “If I had talked to more people, the result would have changed by 
; one or two percent at the most.” Is Trompetter correct? 
; find the prior and posterior predictive for the relevant rate parameter 
; and bullying counts. Based on these distributions, do you agree with 
; Trompetter’s claims?

(define samples (rate4 24 141))

(multiviz
 (density (map first samples) "theta")
 (density (map second samples) "theta prior")
 (hist (map third samples) "prior predictive over k")
 (hist (map fourth samples) "posterior predictive over k"))



;; 3.5 posterior prediction





