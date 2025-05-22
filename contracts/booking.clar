(define-data-var next-booking-id uint u1)

(define-private (generate-booking-id)
  (let ((current (var-get next-booking-id)))
    (var-set next-booking-id (+ current u1))
    current
  )
)

(define-public (book-artisan (artisan principal) (details (string-ascii MAX-STR-URL)))
  (begin
    ;; Validate inputs
    (asserts! (not (is-eq details u"")) ERR-BAD-INPUT)
    (asserts! (not (is-eq artisan tx-sender)) ERR-BAD-INPUT) ;; Can't book yourself

    ;; Check if artisan exists
    (asserts! (is-some (map-get artisans artisan)) ERR-NOT-FOUND)

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

(define-public (cancel-booking (bid uint))
  ;; Validate booking ID
  (if (> bid u0)
      (match (map-get bookings bid)
        booking
        (begin
          ;; Check if caller is the client
          (try! (assert-owner (get client booking)))

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

(define-read-only (get-booking (bid uint))
  (map-get bookings bid)
)