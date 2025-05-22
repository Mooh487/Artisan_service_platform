;; Artisan Service Platform - Consolidated Contract

;; Error codes
(define-constant ERR-NOT-FOUND u101)
(define-constant ERR-UNAUTHORIZED u100)
(define-constant ERR-ALREADY-EXISTS u102)
(define-constant ERR-PAYMENT u103)
(define-constant ERR-BAD-INPUT u104)
(define-constant ERR-DEADLINE u105)
(define-constant ERR-DISPUTE-EXISTS u106)
(define-constant ERR-DISPUTE-RESOLVED u107)
(define-constant ERR-CANCELLATION-PERIOD u108)

;; Booking status constants
(define-constant STATUS-PENDING u0)
(define-constant STATUS-CONFIRMED u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-CANCELLED u3)
(define-constant STATUS-DISPUTED u4)

;; Data variables
(define-data-var next-id uint u1)
(define-data-var next-feedback-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var platform-fee-percent uint u5) ;; 5% platform fee
(define-data-var cancellation-period uint u144) ;; 1 day cancellation period
(define-data-var current-block-height uint u0) ;; Mock for block-height

;; Simple artisan profile map
(define-map artisans principal {url: (string-utf8 128), description: (string-utf8 256)})

;; Booking map - now with status field
(define-map bookings uint {
  artisan: principal,
  client: principal,
  details: (string-utf8 128),
  status: uint,
  price: uint,
  created-at: uint,
  completed-at: uint
})

;; Feedback map
(define-map feedbacks {artisan: principal, fid: uint} {
  reviewer: principal,
  rating: uint,
  comment: (string-utf8 128),
  booking-id: uint
})

;; Installment debt map
(define-map debts {client: principal, bid: uint} {
  total: uint,
  paid: uint,
  installments: uint,
  next-due-block: uint
})

;; Dispute map
(define-map disputes uint {
  booking-id: uint,
  client: principal,
  artisan: principal,
  reason: (string-utf8 128),
  status: uint,
  created-at: uint,
  resolved-at: uint
})

;; Escrow map - holds funds for bookings
(define-map escrows uint {
  amount: uint,
  artisan-amount: uint,
  platform-fee: uint,
  released: bool
})

;; Register an artisan
(define-public (register-artisan (url (string-utf8 128)) (desc (string-utf8 256)))
  (begin
    ;; Check if artisan already exists
    (asserts! (is-none (map-get? artisans tx-sender)) (err ERR-ALREADY-EXISTS))

    ;; Validate inputs are not empty strings
    (asserts! (not (is-eq url u"")) (err ERR-BAD-INPUT))
    (asserts! (not (is-eq desc u"")) (err ERR-BAD-INPUT))

    ;; Register the artisan
    (map-set artisans tx-sender {url: url, description: desc})
    (ok tx-sender)
  )
)

;; Get artisan profile
(define-read-only (get-artisan (who principal))
  (ok (map-get? artisans who))
)

;; Book an artisan with payment
(define-public (book-artisan (artisan principal) (details (string-utf8 128)) (price uint))
  (begin
    ;; Validate artisan exists
    (asserts! (is-some (map-get? artisans artisan)) (err ERR-NOT-FOUND))

    ;; Validate details are not empty
    (asserts! (not (is-eq details u"")) (err ERR-BAD-INPUT))

    ;; Validate artisan is not the client
    (asserts! (not (is-eq artisan tx-sender)) (err ERR-BAD-INPUT))

    ;; Validate price is greater than zero
    (asserts! (> price u0) (err ERR-BAD-INPUT))

    ;; Check if client has enough balance
    (asserts! (>= (stx-get-balance tx-sender) price) (err ERR-PAYMENT))

    ;; Calculate platform fee
    (let ((platform-fee (/ (* price (var-get platform-fee-percent)) u100))
          (artisan-amount (- price platform-fee))
          (id (var-get next-id)))

      ;; Create booking
      (var-set next-id (+ id u1))

      ;; Transfer STX to escrow
      (match (stx-transfer? price tx-sender (as-contract tx-sender))
        success
        (begin
          ;; Create booking record
          (map-set bookings id {
            artisan: artisan,
            client: tx-sender,
            details: details,
            status: STATUS-PENDING,
            price: price,
            created-at: (var-get current-block-height),
            completed-at: u0
          })

          ;; Create escrow record
          (map-set escrows id {
            amount: price,
            artisan-amount: artisan-amount,
            platform-fee: platform-fee,
            released: false
          })

          (ok id)
        )
        error (err ERR-PAYMENT)
      )
    )
  )
)

;; Get booking details
(define-read-only (get-booking (id uint))
  (ok (map-get? bookings id))
)

;; Cancel a booking (client can cancel within cancellation period)
(define-public (cancel-booking (bid uint))
  ;; Validate booking ID is not zero
  (if (> bid u0)
      ;; Continue with valid booking ID
      (match (map-get? bookings bid)
        booking
        (begin
          ;; Check if caller is the client
          (asserts! (is-eq tx-sender (get client booking)) (err ERR-UNAUTHORIZED))

          ;; Check if booking is in pending status
          (asserts! (is-eq (get status booking) STATUS-PENDING) (err ERR-BAD-INPUT))

          ;; Check if within cancellation period
          (asserts! (<= (- (var-get current-block-height) (get created-at booking)) (var-get cancellation-period)) (err ERR-CANCELLATION-PERIOD))

          ;; Get escrow details
          (match (map-get? escrows bid)
            escrow
            (begin
              ;; Check if escrow is not released
              (asserts! (not (get released escrow)) (err ERR-BAD-INPUT))

              ;; Refund client
              (match (as-contract (stx-transfer? (get amount escrow) tx-sender (get client booking)))
                success
                (begin
                  ;; Update booking status
                  (let ((validated-bid bid))
                    (map-set bookings validated-bid {
                      artisan: (get artisan booking),
                      client: (get client booking),
                      details: (get details booking),
                      status: STATUS-CANCELLED,
                      price: (get price booking),
                      created-at: (get created-at booking),
                      completed-at: (var-get current-block-height)
                    })

                    ;; Update escrow
                    (map-set escrows validated-bid {
                      amount: (get amount escrow),
                      artisan-amount: (get artisan-amount escrow),
                      platform-fee: (get platform-fee escrow),
                      released: true
                    })

                    (ok validated-bid)
                  )
                )
                error (err ERR-PAYMENT)
              )
            )
            (err ERR-NOT-FOUND)
          )
        )
        (err ERR-NOT-FOUND)
      )
      ;; Invalid booking ID
      (err ERR-BAD-INPUT)
  )
)

;; Complete a booking (artisan marks as complete)
(define-public (complete-booking (bid uint))
  ;; Validate booking ID is not zero
  (if (> bid u0)
      ;; Continue with valid booking ID
      (match (map-get? bookings bid)
        booking
        (begin
          ;; Check if caller is the artisan
          (asserts! (is-eq tx-sender (get artisan booking)) (err ERR-UNAUTHORIZED))

          ;; Check if booking is in pending status
          (asserts! (is-eq (get status booking) STATUS-PENDING) (err ERR-BAD-INPUT))

          ;; Get escrow details
          (match (map-get? escrows bid)
            escrow
            (begin
              ;; Check if escrow is not released
              (asserts! (not (get released escrow)) (err ERR-BAD-INPUT))

              ;; Update booking status
              (let ((validated-bid bid))
                (map-set bookings validated-bid {
                  artisan: (get artisan booking),
                  client: (get client booking),
                  details: (get details booking),
                  status: STATUS-COMPLETED,
                  price: (get price booking),
                  created-at: (get created-at booking),
                  completed-at: (var-get current-block-height)
                })

                ;; Release funds to artisan and platform
                (match (as-contract (stx-transfer? (get artisan-amount escrow) tx-sender (get artisan booking)))
                  success-artisan
                  (begin
                    ;; Transfer platform fee
                    (match (as-contract (stx-transfer? (get platform-fee escrow) tx-sender 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM))
                      success-platform
                      (begin
                        ;; Update escrow
                        (map-set escrows validated-bid {
                          amount: (get amount escrow),
                          artisan-amount: (get artisan-amount escrow),
                          platform-fee: (get platform-fee escrow),
                          released: true
                        })

                        (ok validated-bid)
                      )
                      error (err ERR-PAYMENT)
                    )
                  )
                  error (err ERR-PAYMENT)
                )
              )
            )
            (err ERR-NOT-FOUND)
          )
        )
        (err ERR-NOT-FOUND)
      )
      ;; Invalid booking ID
      (err ERR-BAD-INPUT)
  )
)

;; Confirm completion (client confirms the booking is complete)
(define-public (confirm-completion (bid uint))
  ;; Validate booking ID is not zero
  (if (> bid u0)
      ;; Continue with valid booking ID
      (match (map-get? bookings bid)
        booking
        (begin
          ;; Check if caller is the client
          (asserts! (is-eq tx-sender (get client booking)) (err ERR-UNAUTHORIZED))

          ;; Check if booking is in pending status
          (asserts! (is-eq (get status booking) STATUS-PENDING) (err ERR-BAD-INPUT))

          ;; Get escrow details
          (match (map-get? escrows bid)
            escrow
            (begin
              ;; Check if escrow is not released
              (asserts! (not (get released escrow)) (err ERR-BAD-INPUT))

              ;; Update booking status
              (let ((validated-bid bid))
                (map-set bookings validated-bid {
                  artisan: (get artisan booking),
                  client: (get client booking),
                  details: (get details booking),
                  status: STATUS-COMPLETED,
                  price: (get price booking),
                  created-at: (get created-at booking),
                  completed-at: (var-get current-block-height)
                })

                ;; Release funds to artisan and platform
                (match (as-contract (stx-transfer? (get artisan-amount escrow) tx-sender (get artisan booking)))
                  success-artisan
                  (begin
                    ;; Transfer platform fee
                    (match (as-contract (stx-transfer? (get platform-fee escrow) tx-sender 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM))
                      success-platform
                      (begin
                        ;; Update escrow
                        (map-set escrows validated-bid {
                          amount: (get amount escrow),
                          artisan-amount: (get artisan-amount escrow),
                          platform-fee: (get platform-fee escrow),
                          released: true
                        })

                        (ok validated-bid)
                      )
                      error (err ERR-PAYMENT)
                    )
                  )
                  error (err ERR-PAYMENT)
                )
              )
            )
            (err ERR-NOT-FOUND)
          )
        )
        (err ERR-NOT-FOUND)
      )
      ;; Invalid booking ID
      (err ERR-BAD-INPUT)
  )
)

;; Create a dispute (client can dispute a booking)
(define-public (create-dispute (bid uint) (reason (string-utf8 128)))
  (match (map-get? bookings bid)
    booking
    (begin
      ;; Check if caller is the client
      (asserts! (is-eq tx-sender (get client booking)) (err ERR-UNAUTHORIZED))

      ;; Check if booking is in pending status
      (asserts! (is-eq (get status booking) STATUS-PENDING) (err ERR-BAD-INPUT))

      ;; Validate reason is not empty
      (asserts! (not (is-eq reason u"")) (err ERR-BAD-INPUT))

      ;; Check if dispute already exists
      (asserts! (is-none (map-get? booking-disputes bid)) (err ERR-DISPUTE-EXISTS))

      ;; Create dispute
      (let ((did (var-get next-dispute-id)))
        (var-set next-dispute-id (+ did u1))

        ;; Update booking status
        (map-set bookings bid {
          artisan: (get artisan booking),
          client: (get client booking),
          details: (get details booking),
          status: STATUS-DISPUTED,
          price: (get price booking),
          created-at: (get created-at booking),
          completed-at: u0
        })

        ;; Create dispute record
        (map-set disputes did {
          booking-id: bid,
          client: (get client booking),
          artisan: (get artisan booking),
          reason: reason,
          status: u0, ;; 0 = pending, 1 = resolved in favor of client, 2 = resolved in favor of artisan
          created-at: (var-get current-block-height),
          resolved-at: u0
        })

        ;; Map booking to dispute
        (map-set booking-disputes bid did)

        (ok did)
      )
    )
    (err ERR-NOT-FOUND)
  )
)

;; Resolve a dispute (platform admin resolves a dispute)
(define-public (resolve-dispute (did uint) (in-favor-of-client bool))
  ;; Validate dispute ID is not zero
  (if (> did u0)
      (begin
        ;; Check if caller is the platform admin
        (asserts! (is-eq tx-sender 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM) (err ERR-UNAUTHORIZED))

        ;; Get dispute details
        (match (map-get? disputes did)
          dispute
          (begin
            ;; Check if dispute is not resolved
            (asserts! (is-eq (get status dispute) u0) (err ERR-DISPUTE-RESOLVED))

            ;; Get booking details
            (match (map-get? bookings (get booking-id dispute))
              booking
              (begin
                ;; Get escrow details
                (match (map-get? escrows (get booking-id dispute))
                  escrow
                  (begin
                    ;; Check if escrow is not released
                    (asserts! (not (get released escrow)) (err ERR-BAD-INPUT))

                    ;; Resolve in favor of client or artisan
                    (let ((validated-did did))
                      (if in-favor-of-client
                          ;; Refund client
                          (match (as-contract (stx-transfer? (get amount escrow) tx-sender (get client dispute)))
                            success
                            (begin
                              ;; Update dispute
                              (map-set disputes validated-did {
                                booking-id: (get booking-id dispute),
                                client: (get client dispute),
                                artisan: (get artisan dispute),
                                reason: (get reason dispute),
                                status: u1, ;; resolved in favor of client
                                created-at: (get created-at dispute),
                                resolved-at: (var-get current-block-height)
                              })

                              ;; Update escrow
                              (map-set escrows (get booking-id dispute) {
                                amount: (get amount escrow),
                                artisan-amount: (get artisan-amount escrow),
                                platform-fee: (get platform-fee escrow),
                                released: true
                              })

                              (ok validated-did)
                            )
                            error (err ERR-PAYMENT)
                          )
                          ;; Pay artisan
                          (match (as-contract (stx-transfer? (get artisan-amount escrow) tx-sender (get artisan dispute)))
                            success-artisan
                            (begin
                              ;; Transfer platform fee
                              (match (as-contract (stx-transfer? (get platform-fee escrow) tx-sender 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM))
                                success-platform
                                (begin
                                  ;; Update dispute
                                  (map-set disputes validated-did {
                                    booking-id: (get booking-id dispute),
                                    client: (get client dispute),
                                    artisan: (get artisan dispute),
                                    reason: (get reason dispute),
                                    status: u2, ;; resolved in favor of artisan
                                    created-at: (get created-at dispute),
                                    resolved-at: (var-get current-block-height)
                                  })

                                  ;; Update escrow
                                  (map-set escrows (get booking-id dispute) {
                                    amount: (get amount escrow),
                                    artisan-amount: (get artisan-amount escrow),
                                    platform-fee: (get platform-fee escrow),
                                    released: true
                                  })

                                  (ok validated-did)
                                )
                                error (err ERR-PAYMENT)
                              )
                            )
                            error (err ERR-PAYMENT)
                          )
                      )
                    )
                  )
                  (err ERR-NOT-FOUND)
                )
              )
              (err ERR-NOT-FOUND)
            )
          )
          (err ERR-NOT-FOUND)
        )
      )
      ;; Invalid dispute ID
      (err ERR-BAD-INPUT)
  )
)

;; Submit feedback for an artisan after a completed booking
(define-public (submit-feedback (bid uint) (rating uint) (comment (string-utf8 128)))
  ;; Validate booking ID is not zero
  (if (> bid u0)
      ;; Continue with valid booking ID
      (match (map-get? bookings bid)
        booking
        (begin
          ;; Validate booking is completed
          (asserts! (is-eq (get status booking) STATUS-COMPLETED) (err ERR-BAD-INPUT))

          ;; Validate caller is the client
          (asserts! (is-eq tx-sender (get client booking)) (err ERR-UNAUTHORIZED))

          ;; Get artisan from booking
          (let ((artisan (get artisan booking))
                (validated-bid bid))

            ;; Validate rating is between 1 and 5
            (asserts! (and (>= rating u1) (<= rating u5)) (err ERR-BAD-INPUT))

            ;; Validate comment is not empty
            (asserts! (not (is-eq comment u"")) (err ERR-BAD-INPUT))

            ;; Create feedback
            (let ((fid (var-get next-feedback-id)))
              (var-set next-feedback-id (+ fid u1))
              (map-set feedbacks {artisan: artisan, fid: fid} {
                reviewer: tx-sender,
                rating: rating,
                comment: comment,
                booking-id: validated-bid
              })
              (ok fid)
            )
          )
        )
        (err ERR-NOT-FOUND)
      )
      ;; Invalid booking ID
      (err ERR-BAD-INPUT)
  )
)

;; Get specific feedback
(define-read-only (get-feedback (artisan principal) (fid uint))
  (ok (map-get? feedbacks {artisan: artisan, fid: fid}))
)

;; Define the interval for installment payments (in blocks)
(define-constant DEFAULT-INTERVAL u144) ;; approximately 1 day at 10 min/block

;; Safe division utility
(define-private (safe-div (n uint) (d uint))
  (if (is-eq d u0)
      (err ERR-BAD-INPUT)
      (ok (/ n d)))
)

;; Map to track disputes by booking ID
(define-map booking-disputes uint uint) ;; booking-id -> dispute-id

;; Initiate an installment plan
(define-public (initiate-installment (bid uint) (installments uint) (total uint) (interval uint))
  ;; Validate booking ID is not zero
  (if (> bid u0)
      ;; Continue with valid booking ID
      (match (map-get? bookings bid)
        booking
        (begin
          ;; Check if caller is the client
          (asserts! (is-eq tx-sender (get client booking)) (err ERR-UNAUTHORIZED))

          ;; Validate inputs
          (asserts! (> installments u0) (err ERR-BAD-INPUT))
          (asserts! (> total u0) (err ERR-BAD-INPUT))
          (asserts! (>= interval DEFAULT-INTERVAL) (err ERR-BAD-INPUT))

          ;; Check if installment plan already exists
          (asserts! (is-none (map-get? debts {client: tx-sender, bid: bid})) (err ERR-ALREADY-EXISTS))

          ;; Create installment plan
          (let ((next-block (+ (var-get current-block-height) interval))
                (validated-bid bid))
            (map-set debts {client: tx-sender, bid: validated-bid}
              {total: total, paid: u0, installments: installments, next-due-block: next-block})
            (ok next-block)
          )
        )
        (err ERR-NOT-FOUND)
      )
      ;; Invalid booking ID
      (err ERR-BAD-INPUT)
  )
)

;; Pay an installment
(define-public (pay-installment (bid uint))
  ;; Validate booking ID is not zero
  (if (> bid u0)
      ;; Continue with valid booking ID
      (let ((debt-opt (map-get? debts {client: tx-sender, bid: bid})))
        (match debt-opt
          debt
          (begin
            ;; Check if payment is due
            (asserts! (<= (get next-due-block debt) (var-get current-block-height)) (err ERR-DEADLINE))

            ;; Check if all installments are already paid
            (let ((per-pay-result (safe-div (get total debt) (get installments debt))))
              (match per-pay-result
                per-pay
                (begin
                  ;; Check if there are remaining payments
                  (asserts! (< (get paid debt) (get total debt)) (err ERR-BAD-INPUT))

                  ;; Get booking details
                  (match (map-get? bookings bid)
                    booking
                    (begin
                      ;; Check if client has enough balance
                      (asserts! (>= (stx-get-balance tx-sender) per-pay) (err ERR-PAYMENT))

                      ;; Transfer STX to artisan
                      (match (stx-transfer? per-pay tx-sender (get artisan booking))
                        success
                        (begin
                          ;; Update debt record
                          (let ((new-paid (+ (get paid debt) per-pay))
                                (new-next-due-block (+ (get next-due-block debt) DEFAULT-INTERVAL))
                                (validated-bid bid))

                            ;; If this is the final payment, remove the debt record
                            (if (>= new-paid (get total debt))
                                (begin
                                  (map-delete debts {client: tx-sender, bid: validated-bid})
                                  (ok new-paid)
                                )
                                (begin
                                  (map-set debts {client: tx-sender, bid: validated-bid}
                                    {total: (get total debt),
                                     paid: new-paid,
                                     installments: (get installments debt),
                                     next-due-block: new-next-due-block
                                    })
                                  (ok new-paid)
                                )
                            )
                          )
                        )
                        error (err ERR-PAYMENT)
                      )
                    )
                    (err ERR-NOT-FOUND)
                  )
                )
                error (err ERR-BAD-INPUT)
              )
            )
          )
          (err ERR-NOT-FOUND)
        )
      )
      ;; Invalid booking ID
      (err ERR-BAD-INPUT)
  )
)
