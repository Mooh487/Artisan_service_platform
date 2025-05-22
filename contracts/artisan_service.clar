(define-map artisans
  ;; key: artisan principal
  ;; value: { "profile-url": (string-ascii), "description": (string-ascii) }
  principal { profile-url: (string-ascii), description: (string-ascii) }
)

(define-map bookings
  ;; key: booking-id uint
  ;; value: { artisan: principal, client: principal, service-details: (string-ascii), status: uint }
  uint { artisan: principal, client: principal, service-details: (string-ascii), status: uint }
)

(define-map feedbacks
  ;; key: (tuple (artisan) (feedback-id))
  ;; value: { reviewer: principal, rating: uint, comment: (string-ascii) }
  (tuple (artisan principal) (fid uint)) { reviewer: principal, rating: uint, comment: (string-ascii) }
)

(define-map debts
  ;; key: (tuple (client) (booking-id))
  ;; value: { total: uint, paid: uint, installments: uint, next-due-block: uint }
  (tuple (client principal) (bid uint)) { total: uint, paid: uint, installments: uint, next-due-block: uint }
)

(define-constant ERR-UNAUTHORIZED u100)
(define-constant ERR-NOT-FOUND u101)
(define-constant ERR-ALREADY-EXISTS u102)
(define-constant ERR-PAYMENT u103)

;; Register an artisan profile
(define-public (register-artisan (url (string-ascii 256)) (desc (string-ascii 512)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq url u"")) (err ERR-PAYMENT)) ;; Using ERR-PAYMENT as ERR-BAD-INPUT is not defined
    (asserts! (not (is-eq desc u"")) (err ERR-PAYMENT))

    ;; Check if artisan already exists
    (asserts! (is-none (map-get? artisans tx-sender)) (err ERR-ALREADY-EXISTS))

    ;; Store validated inputs
    (let ((validated-url url)
          (validated-desc desc))
      (map-set artisans tx-sender { profile-url: validated-url, description: validated-desc })
      (ok tx-sender)
    )
  )
)

;; Client books an artisan, returns booking-id
(define-public (book-artisan (artisan principal) (details (string-ascii 256)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq details u"")) (err ERR-PAYMENT)) ;; Using ERR-PAYMENT as ERR-BAD-INPUT is not defined
    (asserts! (not (is-eq artisan tx-sender)) (err ERR-PAYMENT)) ;; Can't book yourself

    ;; Check if artisan exists
    (asserts! (is-some (map-get? artisans artisan)) (err ERR-NOT-FOUND))

    ;; Generate booking ID
    (let ((bid (+ (default-to u0 (map-get? bookings (as-max-len (tuple) u0))) u1))
          (validated-artisan artisan)
          (validated-details details))
      (map-set bookings bid {
        artisan: validated-artisan,
        client: tx-sender,
        service-details: validated-details,
        status: u0
      })
      (ok bid)
    )
  )
)

;; Add feedback for artisan
(define-public (submit-feedback (artisan principal) (rating uint) (comment (string-ascii 256)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq artisan tx-sender)) (err ERR-PAYMENT)) ;; Can't review yourself
    (asserts! (not (is-eq comment u"")) (err ERR-PAYMENT))
    (asserts! (>= rating u1) (err ERR-PAYMENT))
    (asserts! (<= rating u5) (err ERR-PAYMENT))

    ;; Check if artisan exists
    (asserts! (is-some (map-get? artisans artisan)) (err ERR-NOT-FOUND))

    ;; Create feedback with validated inputs
    (let* ((fkey (tuple (artisan) (fid (+ (default-to u0 (map-get? (map-get? feedbacks (tuple artisan) .fid)) u0) u1))))
           (feedback-id (get fid fkey))
           (validated-artisan artisan)
           (validated-rating rating)
           (validated-comment comment))
      (map-set feedbacks fkey {
        reviewer: tx-sender,
        rating: validated-rating,
        comment: validated-comment
      })
      (ok feedback-id)
    )
  )
)

;; View functions
(define-read-only (get-artisan (artisan principal))
  (map-get? artisans artisan)
)

(define-read-only (get-booking (bid uint))
  (map-get? bookings bid)
)

(define-read-only (get-feedback (artisan principal) (fid uint))
  (map-get? feedbacks (tuple (artisan) (fid)))
)

;; Set up installment plan: only client of booking can call
(define-public (initiate-installment (bid uint) (installments uint) (total uint) (interval uint))
  ;; Validate booking ID
  (if (> bid u0)
      (match (map-get? bookings bid)
        booking
        (let ((client (get client booking)))
          ;; Validate inputs
          (begin
            (asserts! (is-eq client tx-sender) (err ERR-UNAUTHORIZED))
            (asserts! (> installments u0) (err ERR-PAYMENT))
            (asserts! (> total u0) (err ERR-PAYMENT))
            (asserts! (> interval u0) (err ERR-PAYMENT))

            ;; Check if installment plan already exists
            (asserts! (is-none (map-get? debts (tuple (client) (bid)))) (err ERR-ALREADY-EXISTS))

            ;; Create installment plan with validated inputs
            (let ((next-block (+ (block-height) interval))
                  (validated-total total)
                  (validated-installments installments))
              (map-set debts (tuple (client) (bid)) {
                total: validated-total,
                paid: u0,
                installments: validated-installments,
                next-due-block: next-block
              })
              (ok next-block)
            )
          ))
        (err ERR-NOT-FOUND))
      (err ERR-PAYMENT)
  )
)

;; Pay an installment
(define-public (pay-installment (bid uint))
  ;; Validate booking ID
  (if (> bid u0)
      ;; Check if debt exists for this booking
      (match (map-get? debts (tuple tx-sender bid)) debt
        (let* ((due (get next-due-block debt))
               (per-payment (u/ (get total debt) (get installments debt))))
          ;; Get booking details
          (match (map-get? bookings bid) booking
            (begin
              ;; Validate payment
              (asserts! (>= (stx-get-transfer-amount) per-payment) ERR-PAYMENT)
              (asserts! (< (get paid debt) (get total debt)) ERR-PAYMENT) ;; Check if not fully paid

              ;; Process payment
              (match (stx-transfer? per-payment tx-sender (get artisan booking))
                success
                (begin
                  ;; Update debt record with validated inputs
                  (let ((validated-bid bid)
                        (new-paid (+ (get paid debt) per-payment))
                        (new-next-due-block (+ due u144))) ;; Using 144 blocks (1 day) as interval

                    ;; If fully paid, remove debt record
                    (if (>= new-paid (get total debt))
                        (begin
                          (map-delete debts (tuple tx-sender validated-bid))
                          (ok new-paid)
                        )
                        ;; Otherwise update debt record
                        (begin
                          (map-set debts
                            (tuple tx-sender validated-bid)
                            {total: (get total debt),
                             paid: new-paid,
                             installments: (get installments debt),
                             next-due-block: new-next-due-block
                            }
                          )
                          (ok new-paid)
                        )
                    )
                  )
                )
                (err ERR-PAYMENT)
              )
            )
            (err ERR-NOT-FOUND)
          )
        )
        (err ERR-NOT-FOUND)
      )
      (err ERR-PAYMENT)
  )
)

;; Automatic debt update: called externally or by oracle
(define-public (check-debts)
  ;; This is a placeholder function that would be implemented with proper iteration
  ;; in a production environment. For now, we just return success.
  (ok u0)
)
