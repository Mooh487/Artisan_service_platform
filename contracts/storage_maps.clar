;; Use constants from types.clar
(define-constant MAX-STR-URL u256)
(define-constant MAX-STR-DESC u512)
(define-constant MAX-STR-COMMENT u256)

;; Storage maps for the artisan service platform
(define-map artisans
  ;; key: artisan principal
  ;; value: artisan profile data
  principal
  {profile-url: (string-ascii MAX-STR-URL), description: (string-ascii MAX-STR-DESC)}
)

(define-map bookings
  ;; key: booking-id uint
  ;; value: booking data
  uint
  {artisan: principal, client: principal, service-details: (string-ascii MAX-STR-URL), status: uint}
)

(define-map feedbacks
  ;; key: tuple of artisan principal and feedback-id
  ;; value: feedback data
  {artisan: principal, fid: uint}
  {reviewer: principal, rating: uint, comment: (string-ascii MAX-STR-COMMENT)}
)

(define-map debts
  ;; key: tuple of client principal and booking-id
  ;; value: debt data
  {client: principal, bid: uint}
  {total: uint, paid: uint, installments: uint, next-due-block: uint}
)