(define-constant ERR-UNAUTHORIZED u100) ;;; unauthorized access attempt
(define-constant ERR-NOT-FOUND   u101) ;;; requested resource not in storage
(define-constant ERR-ALREADY-EXISTS u102) ;;; duplicate entry detected
(define-constant ERR-PAYMENT     u103) ;;; payment verification failed
(define-constant ERR-BAD-INPUT   u104) ;;; input validation failure
(define-constant ERR-DEADLINE    u105) ;;; installment deadline missed

;; Check if the caller is the expected principal
(define-public (assert-owner (expected principal))
  (if (is-eq tx-sender expected)
      (ok tx-sender)
      (err ERR-UNAUTHORIZED)
  )
)

;; utility for safe division
(define-private (safe-div (n uint) (d uint))
  (if (is-eq d u0)
      (err ERR-BAD-INPUT)
      (ok (/ n d)))
)