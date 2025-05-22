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
      (map-set artisans tx-sender { profile-url: validated-url, description: validated-desc })
      (ok tx-sender)
    )
  )
)

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
      (map-set artisans tx-sender { profile-url: validated-url, description: validated-desc })
      (ok tx-sender)
    )
  )
)

(define-read-only (get-artisan-profile (artisan principal))
  (match (map-get? artisans artisan)
    profile (ok profile)
    (err ERR-NOT-FOUND))
)