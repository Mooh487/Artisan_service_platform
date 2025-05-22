;; Artisan Service Platform Contract
;; A platform for artisans to offer services and clients to book them

;; ========== Constants ==========
(define-constant DEFAULT-INTERVAL u144) ;; approximately 1 day at 10 min/block

;; ========== Error Codes ==========
(define-constant ERR-UNAUTHORIZED u100) ;;; unauthorized access attempt
(define-constant ERR-NOT-FOUND u101) ;;; requested resource not in storage
(define-constant ERR-ALREADY-EXISTS u102) ;;; duplicate entry detected
(define-constant ERR-PAYMENT u103) ;;; payment verification failed
(define-constant ERR-BAD-INPUT u104) ;;; input validation failure
(define-constant ERR-DEADLINE u105) ;;; installment deadline missed

;; ========== Storage Maps ==========
;; Artisan profiles
(define-map artisans principal {profile-url: (string-utf8 128), description: (string-utf8 256)})

;; Bookings
(define-map bookings uint {artisan: principal, client: principal, service-details: (string-utf8 128), status: uint})

;; Feedback records
(define-map feedbacks {artisan: principal, fid: uint} {reviewer: principal, rating: uint, comment: (string-utf8 128)})

;; Installment debt records
(define-map debts {client: principal, bid: uint} {total: uint, paid: uint, installments: uint, next-due-block: uint})

;; ========== Data Variables ==========
(define-data-var next-booking-id uint u1)
(define-data-var next-feedback-id uint u1)

;; ========== Helper Functions ==========
;; Safe division utility
(define-private (safe-div (n uint) (d uint))
  (if (is-eq d u0)
      (err ERR-BAD-INPUT)
      (ok (/ n d)))
)

;; ========== Profile Functions ==========
;; Register an artisan profile
(define-public (register-artisan (url (string-utf8 128)) (desc (string-utf8 256)))
  (let ((sender tx-sender))
    (begin
      ;; Validate inputs
      (asserts! (not (is-eq url u"")) ERR-BAD-INPUT)
      (asserts! (not (is-eq desc u"")) ERR-BAD-INPUT)

      ;; Check if artisan already exists
      (asserts! (not (is-some (map-get? artisans sender))) ERR-ALREADY-EXISTS)

      ;; Store validated inputs
      (let ((validated-url url)
            (validated-desc desc))
        (map-set artisans sender {profile-url: validated-url, description: validated-desc})
        (ok sender)
      )
    )
  )
)

;; Update an existing profile
(define-public (update-profile (url (string-utf8 128)) (desc (string-utf8 256)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq url u"")) ERR-BAD-INPUT)
    (asserts! (not (is-eq desc u"")) ERR-BAD-INPUT)

    ;; Check if profile exists
    (asserts! (is-some (map-get? artisans tx-sender)) ERR-NOT-FOUND)

    ;; Store validated inputs
    (let ((validated-url url)
          (validated-desc desc))
      (map-set artisans tx-sender {profile-url: validated-url, description: validated-desc})
      (ok tx-sender)
    )
  )
)

;; Get artisan profile
(define-read-only (get-artisan-profile (artisan principal))
  (match (map-get? artisans artisan)
    profile (ok profile)
    (err ERR-NOT-FOUND))
)

;; ========== Booking Functions ==========
;; Book an artisan
(define-public (book-artisan (artisan principal) (details (string-utf8 128)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq details u"")) ERR-BAD-INPUT)
    (asserts! (not (is-eq artisan tx-sender)) ERR-BAD-INPUT) ;; Can't book yourself

    ;; Check if artisan exists
    (asserts! (is-some (map-get? artisans artisan)) ERR-NOT-FOUND)

    ;; Create booking with validated inputs
    (let ((bid (var-get next-booking-id))
          (validated-details details)
          (validated-artisan artisan))
      (var-set next-booking-id (+ bid u1))
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

;; Cancel a booking
(define-public (cancel-booking (bid uint))
  ;; Validate booking ID
  (if (> bid u0)
      (match (map-get? bookings bid)
        booking
        (begin
          ;; Check if caller is the client
          (asserts! (is-eq tx-sender (get client booking)) ERR-UNAUTHORIZED)

          ;; Delete booking with validated ID
          (let ((validated-bid bid))
            (map-delete bookings validated-bid)
            (ok validated-bid)
          )
        )
        (err ERR-NOT-FOUND))
      (err ERR-BAD-INPUT)
  )
)

;; Get booking details
(define-read-only (get-booking (bid uint))
  (match (map-get? bookings bid)
    booking (ok booking)
    (err ERR-NOT-FOUND))
)

;; ========== Feedback Functions ==========
;; Submit feedback for an artisan
(define-public (submit-feedback (artisan principal) (rating uint) (comment (string-utf8 128)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq artisan tx-sender)) ERR-BAD-INPUT) ;; Can't review yourself
    (asserts! (not (is-eq comment u"")) ERR-BAD-INPUT)
    (asserts! (>= rating u1) ERR-BAD-INPUT)
    (asserts! (<= rating u5) ERR-BAD-INPUT)

    ;; Check if artisan exists
    (asserts! (is-some (map-get? artisans artisan)) ERR-NOT-FOUND)

    ;; Create feedback with validated inputs
    (let ((fid (var-get next-feedback-id))
          (validated-artisan artisan)
          (validated-rating rating)
          (validated-comment comment))
      (var-set next-feedback-id (+ fid u1))
      (map-set feedbacks
        {artisan: validated-artisan, fid: fid}
        {reviewer: tx-sender, rating: validated-rating, comment: validated-comment}
      )
      (ok fid)
    )
  )
)

;; Get specific feedback
(define-read-only (get-feedback (artisan principal) (fid uint))
  (match (map-get? feedbacks {artisan: artisan, fid: fid})
    feedback (ok feedback)
    (err ERR-NOT-FOUND))
)

;; ========== Installment Functions ==========
;; Initiate an installment plan
(define-public (initiate-installment (bid uint) (installments uint) (total uint) (interval uint))
  ;; Validate booking ID
  (if (> bid u0)
      (match (map-get? bookings bid)
        booking
        (begin
          ;; Validate inputs
          (asserts! (is-eq tx-sender (get client booking)) ERR-UNAUTHORIZED)
          (asserts! (> installments u0) ERR-BAD-INPUT)
          (asserts! (> total u0) ERR-BAD-INPUT)
          (asserts! (> interval u0) ERR-BAD-INPUT)

          ;; Check if installment plan already exists
          (asserts! (is-none (map-get? debts {client: tx-sender, bid: bid})) ERR-ALREADY-EXISTS)

          ;; Create installment plan with validated inputs
          (let ((next-block (+ u0 interval))
                (validated-bid bid)
                (validated-total total)
                (validated-installments installments))
            (map-set debts
              {client: tx-sender, bid: validated-bid}
              {total: validated-total,
               paid: u0,
               installments: validated-installments,
               next-due-block: next-block}
            )
            (ok next-block)
          )
        )
        (err ERR-NOT-FOUND)
      )
      (err ERR-BAD-INPUT)
  )
)

;; Pay an installment
(define-public (pay-installment (bid uint))
  ;; Validate booking ID
  (if (> bid u0)
      ;; Check if debt exists for this booking
      (match (map-get? debts {client: tx-sender, bid: bid}) debt
        (let ((per-pay (unwrap-panic (safe-div (get total debt) (get installments debt)))))
          ;; Get booking details
          (let ((artisan-booking (unwrap! (map-get? bookings bid) (err ERR-NOT-FOUND))))
            (begin
              ;; Validate payment
              (asserts! (>= (stx-get-balance tx-sender) per-pay) ERR-PAYMENT)
              (asserts! (< (get paid debt) (get total debt)) ERR-BAD-INPUT) ;; Check if not fully paid

              ;; Process payment
              (try! (stx-transfer? per-pay tx-sender (get artisan artisan-booking)))

              ;; Update debt record with validated inputs
              (let ((validated-bid bid)
                    (new-paid (+ (get paid debt) per-pay))
                    (new-next-due-block (+ (get next-due-block debt) DEFAULT-INTERVAL)))

                ;; If fully paid, remove debt record
                (if (>= new-paid (get total debt))
                    (begin
                      (map-delete debts {client: tx-sender, bid: validated-bid})
                      (ok new-paid)
                    )
                    ;; Otherwise update debt record
                    (begin
                      (map-set debts
                        {client: tx-sender, bid: validated-bid}
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
          )
        )
        (err ERR-NOT-FOUND)
      )
      (err ERR-BAD-INPUT)
  )
)
