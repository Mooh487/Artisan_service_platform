(define-public (initiate-installment (bid uint) (installments uint) (total uint) (interval uint))
  (match (map-get bookings bid)
    booking
    (begin
      (assert-owner (get client booking))
      (asserts! (> installments u0) ERR-BAD-INPUT)
      (asserts! (> total u0) ERR-BAD-INPUT)
      (let ((next-block (+ (block-height) interval)))
        (map-set debts (tuple (get client booking) (bid))
          { total: total, paid: u0, installments: installments, next-due-block: next-block })
        (ok next-block)
      )
    )
    (err ERR-NOT-FOUND)
  )
)

(define-public (pay-installment (bid uint))
  (match (map-get debts (tuple tx-sender bid)) debt
    (let* ((per-pay (unwrap-panic (safe-div (get total debt) (get installments debt))))
           (artisan (get artisan (unwrap-panic (map-get bookings bid)))))
      (begin
        (asserts! (>= (stx-get-transfer-amount) per-pay) ERR-PAYMENT)
        (stx-transfer? per-pay tx-sender artisan)
        (map-set debts (tuple tx-sender bid)
          { total: (get total debt),
            paid: (+ (get paid debt) per-pay),
            installments: (get installments debt),
            next-due-block: (+ (get next-due-block debt) interval)
          })
        (ok (get paid debt))
      )
    )
    (err ERR-NOT-FOUND)
  )
)
