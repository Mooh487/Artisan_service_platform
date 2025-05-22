;; Constants for string lengths
(define-constant MAX-STR-URL u256)
(define-constant MAX-STR-DESC u512)
(define-constant MAX-STR-COMMENT u256)

;; Type definitions for the artisan service platform
;; These are used across multiple contracts

;; Define the interval for installment payments (in blocks)
(define-constant DEFAULT-INTERVAL u144) ;; approximately 1 day at 10 min/block