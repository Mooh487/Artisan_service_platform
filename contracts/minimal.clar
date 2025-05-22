;; Minimal Artisan Service Platform Contract

;; Constants
(define-constant ERR-NOT-FOUND u101)
(define-constant ERR-UNAUTHORIZED u100)

;; Data variables
(define-data-var next-id uint u1)

;; Simple artisan profile map
(define-map artisans principal {url: (string-utf8 128), description: (string-utf8 256)})

;; Simple booking map
(define-map bookings uint {artisan: principal, client: principal, details: (string-utf8 128)})

;; Register an artisan
(define-public (register-artisan (url (string-utf8 128)) (desc (string-utf8 256)))
  ;; Validate inputs
  (begin
    (asserts! (not (is-eq url u"")) (err u104)) ;; ERR-BAD-INPUT
    (asserts! (not (is-eq desc u"")) (err u104)) ;; ERR-BAD-INPUT

    ;; Store validated inputs
    (let ((validated-url url)
          (validated-desc desc))
      (ok (map-set artisans tx-sender {url: validated-url, description: validated-desc}))
    )
  )
)

;; Get artisan profile
(define-read-only (get-artisan (who principal))
  (ok (map-get? artisans who))
)

;; Book an artisan
(define-public (book-artisan (artisan principal) (details (string-utf8 128)))
  ;; Validate inputs
  (begin
    (asserts! (not (is-eq details u"")) (err u104)) ;; ERR-BAD-INPUT
    (asserts! (not (is-eq artisan tx-sender)) (err u104)) ;; ERR-BAD-INPUT - Can't book yourself

    ;; Check if artisan exists
    (asserts! (is-some (map-get? artisans artisan)) (err ERR-NOT-FOUND))

    ;; Create booking with validated inputs
    (let ((id (var-get next-id))
          (validated-artisan artisan)
          (validated-details details))
      (var-set next-id (+ id u1))
      (map-set bookings id {
        artisan: validated-artisan,
        client: tx-sender,
        details: validated-details
      })
      (ok id)
    )
  )
)

;; Get booking details
(define-read-only (get-booking (id uint))
  (ok (map-get? bookings id))
)
