(define-data-var next-feedback-id uint u1)

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
        (tuple (validated-artisan) (fid))
        { reviewer: tx-sender, rating: validated-rating, comment: validated-comment }
      )
      (ok fid)
    )
  )
)

(define-read-only (get-feedback (artisan principal) (fid uint))
  (map-get feedbacks (tuple (artisan) (fid)))
)

(define-read-only (get-all-feedbacks (artisan principal))
  ;; placeholder: iterate keys in UI off-chain
  (ok "Use off-chain indexer to fetch all feedbacks for artisan")
)