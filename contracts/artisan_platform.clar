;; Artisan Service Platform - Consolidated Contract
;; This contract combines all functionality from the separate contracts

;; ========== Constants ==========
(define-constant MAX-STR-URL u256)
(define-constant MAX-STR-DESC u512)
(define-constant MAX-STR-COMMENT u256)
(define-constant DEFAULT-INTERVAL u144) ;; approximately 1 day at 10 min/block

;; ========== Error Codes ==========
(define-constant ERR-UNAUTHORIZED u100) ;;; unauthorized access attempt
(define-constant ERR-NOT-FOUND u101) ;;; requested resource not in storage
(define-constant ERR-ALREADY-EXISTS u102) ;;; duplicate entry detected
(define-constant ERR-PAYMENT u103) ;;; payment verification failed
(define-constant ERR-BAD-INPUT u104) ;;; input validation failure
(define-constant ERR-DEADLINE u105) ;;; installment deadline missed

;; ========== Storage Maps ==========
;; Artisan profiles - key is the artisan's principal
(define-map artisans
  principal
  { profile-url: (string-ascii MAX-STR-URL), description: (string-ascii MAX-STR-DESC) }
)

;; Bookings - key is the booking ID
(define-map bookings
  uint
  { artisan: principal, client: principal, service-details: (string-ascii MAX-STR-URL), status: uint }
)

;; Feedback records - key is artisan principal and feedback ID
(define-map feedbacks
  { artisan: principal, fid: uint }
  { reviewer: principal, rating: uint, comment: (string-ascii MAX-STR-COMMENT) }
)

;; Installment debt records - key is client principal and booking ID
(define-map debts
  { client: principal, bid: uint }
  { total: uint, paid: uint, installments: uint, next-due-block: uint }
)

;; ========== Data Variables ==========
(define-data-var next-booking-id uint u1)
(define-data-var next-feedback-id uint u1)

;; ========== Helper Functions ==========
;; Check if the caller is the expected principal
(define-public (assert-owner (expected principal))
  (if (is-eq tx-sender expected)
      (ok tx-sender)
      (err ERR-UNAUTHORIZED)
  )
)

;; Generate a new booking ID
(define-private (generate-booking-id)
  (let ((current (var-get next-booking-id)))
    (var-set next-booking-id (+ current u1))
    current
  )
)

;; Safe division utility
(define-private (safe-div (n uint) (d uint))
  (if (is-eq d u0)
      (err ERR-BAD-INPUT)
      (ok (/ n d)))
)

;; ========== Profile Functions ==========
;; Register an artisan profile
(define-public (register-artisan (url (string-ascii MAX-STR-URL)) (desc (string-ascii MAX-STR-DESC)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq url u"")) ERR-BAD-INPUT)
    (asserts! (not (is-eq desc u"")) ERR-BAD-INPUT)

    ;; Check if artisan already exists
    (asserts! (not (is-some (map-get? artisans tx-sender))) ERR-ALREADY-EXISTS)

    ;; Store validated inputs
    (let ((validated-url url)
          (validated-desc desc))
      (map-set artisans tx-sender {profile-url: validated-url, description: validated-desc})
      (ok tx-sender)
    )
  )
)

;; Update an existing profile
(define-public (update-profile (url (string-ascii MAX-STR-URL)) (desc (string-ascii MAX-STR-DESC)))
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
(define-public (book-artisan (artisan principal) (details (string-ascii MAX-STR-URL)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq details u"")) ERR-BAD-INPUT)
    (asserts! (not (is-eq artisan tx-sender)) ERR-BAD-INPUT) ;; Can't book yourself

    ;; Check if artisan exists
    (asserts! (is-some (map-get? artisans artisan)) ERR-NOT-FOUND)

    ;; Create booking with validated inputs
    (let ((bid (generate-booking-id))
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
  (map-get? bookings bid)
)

;; ========== Feedback Functions ==========
;; Submit feedback for an artisan
(define-public (submit-feedback (artisan principal) (rating uint) (comment (string-ascii MAX-STR-COMMENT)))
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
  (map-get? feedbacks {artisan: artisan, fid: fid})
)

;; Get all feedbacks (placeholder)
(define-read-only (get-all-feedbacks (artisan principal))
  ;; placeholder: iterate keys in UI off-chain
  (ok "Use off-chain indexer to fetch all feedbacks for artisan")
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
          (let ((next-block (+ block-height interval))
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
