;; Clarity Token Staking Smart Contract

;; SIP-010 Token Trait Definition
(define-trait sip-010-token
  (
    (transfer (uint principal principal (optional (buff 34)) ) (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 12) uint))
    (get-decimals () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Comprehensive Error Constants
(define-constant ERR-UNAUTHORIZED (err u1))
(define-constant ERR-LOW-BALANCE (err u2))
(define-constant ERR-POSITION-NOT-FOUND (err u3))
(define-constant ERR-WITHDRAWAL-NOT-ALLOWED (err u4))
(define-constant ERR-EXISTING-POSITION (err u5))
(define-constant ERR-INVALID-QUANTITY (err u6))
(define-constant ERR-YIELD-CALCULATION (err u7))
(define-constant ERR-INVALID-TOKEN (err u8))
(define-constant ERR-TRANSFER-ERROR (err u9))

;; Validation Functions
(define-private (is-valid-token (token <sip-010-token>))
  (begin
    ;; Attempt to get token name to validate contract
    (match (contract-call? token get-name)
      name true
      error false
    )
  )
)

(define-private (is-valid-quantity (quantity uint))
  (and (> quantity u0) (<= quantity MAXIMUM_DEPOSIT))
)

;; Storage Maps
(define-map positions 
  { participant: principal }
  {
    quantity: uint,
    entry-block: uint,
    commitment-period: uint,
    collected-yield: uint
  }
)

;; Tracks total staked amount
(define-data-var total-deposited uint u0)

;; Staking parameters
(define-constant MINIMUM_DEPOSIT u100)  ;; Minimum deposit of 100 tokens
(define-constant MAXIMUM_DEPOSIT u10000)  ;; Maximum deposit of 10,000 tokens
(define-constant BASE_YIELD_RATE u5)  ;; 5% base yield rate
(define-constant MAX_COMMITMENT_PERIOD u52560)  ;; Approximately 1 year (52560 blocks)

;; Deposit tokens with enhanced validation
(define-public (deposit 
  (token <sip-010-token>) 
  (quantity uint) 
  (commitment-period uint)
)
  (begin
    ;; Validate token contract
    (asserts! (is-valid-token token) ERR-INVALID-TOKEN)
    
    ;; Validate input parameters
    (asserts! (is-valid-quantity quantity) ERR-INVALID-QUANTITY)
    (asserts! (<= commitment-period MAX_COMMITMENT_PERIOD) ERR-INVALID-QUANTITY)
    
    ;; Check if already has position
    (asserts! 
      (is-none (map-get? positions { participant: tx-sender })) 
      ERR-EXISTING-POSITION
    )
    
    ;; Verify user has sufficient balance
    (let ((user-balance (unwrap! 
          (contract-call? token get-balance tx-sender)
          ERR-LOW-BALANCE
        )))
      (asserts! (>= user-balance quantity) ERR-LOW-BALANCE)
    )
    
    ;; Transfer tokens to contract
    (let ((transfer-result 
            (contract-call? token transfer 
              quantity 
              tx-sender 
              (as-contract tx-sender) 
              none
            )))
      (asserts! (is-ok transfer-result) ERR-TRANSFER-ERROR)
    )
    
    ;; Record position
    (map-set positions 
      { participant: tx-sender }
      {
        quantity: quantity,
        entry-block: block-height,
        commitment-period: commitment-period,
        collected-yield: u0
      }
    )
    
    ;; Update total deposited amount
    (var-set total-deposited (+ (var-get total-deposited) quantity))
    
    (ok true)
  )
)

;; Calculate yield with additional safety checks
(define-private (calculate-yield (position-info {
  quantity: uint, 
  entry-block: uint, 
  commitment-period: uint, 
  collected-yield: uint
}))
  (let 
    (
      (current-block block-height)
      (blocks-held (- current-block (get entry-block position-info)))
      (yield-rate BASE_YIELD_RATE)
      (max-yield (/ (* (get quantity position-info) yield-rate blocks-held) u100))
    )
    (if (> blocks-held (get commitment-period position-info))
      max-yield
      (/ (* (get quantity position-info) yield-rate blocks-held) u100)
    )
  )
)

;; Collect accumulated yield with enhanced validation
(define-public (collect-yield (token <sip-010-token>))
  (begin
    ;; Validate token contract
    (asserts! (is-valid-token token) ERR-INVALID-TOKEN)
    
    (let 
      (
        (position-info (unwrap! 
          (map-get? positions { participant: tx-sender }) 
          ERR-POSITION-NOT-FOUND
        ))
        (uncollected-yield (- 
          (calculate-yield position-info)
          (get collected-yield position-info)
        ))
      )
      ;; Validate yield calculation
      (asserts! (> uncollected-yield u0) ERR-YIELD-CALCULATION)
      
      ;; Transfer yield
      (let ((transfer-result 
              (as-contract (contract-call? token transfer 
                uncollected-yield 
                (as-contract tx-sender) 
                tx-sender 
                none
              ))))
        (asserts! (is-ok transfer-result) ERR-TRANSFER-ERROR)
      )
      
      ;; Update collected yield
      (map-set positions 
        { participant: tx-sender }
        (merge position-info { 
          collected-yield: (+ (get collected-yield position-info) uncollected-yield) 
        })
      )
      
      (ok uncollected-yield)
    )
  )
)

;; Withdraw tokens with enhanced validation and penalty mechanism
(define-public (withdraw (token <sip-010-token>))
  (begin
    ;; Validate token contract
    (asserts! (is-valid-token token) ERR-INVALID-TOKEN)
    
    (let 
      (
        (position-info (unwrap! 
          (map-get? positions { participant: tx-sender }) 
          ERR-POSITION-NOT-FOUND
        ))
        (current-block block-height)
        (blocks-held (- current-block (get entry-block position-info)))
        (early-exit-fee (if (< blocks-held (get commitment-period position-info)) u10 u0))
        (fee-amount (/ (* (get quantity position-info) early-exit-fee) u100))
        (withdraw-amount (- (get quantity position-info) fee-amount))
      )
      ;; Validate withdrawal conditions
      (asserts! 
        (>= blocks-held (/ (get commitment-period position-info) u2)) 
        ERR-WITHDRAWAL-NOT-ALLOWED
      )
      
      ;; Collect any pending yield
      (try! (collect-yield token))
      
      ;; Transfer tokens back to user
      (let ((transfer-result 
              (as-contract (contract-call? token transfer 
                withdraw-amount 
                (as-contract tx-sender) 
                tx-sender 
                none
              ))))
        (asserts! (is-ok transfer-result) ERR-TRANSFER-ERROR)
      )
      
      ;; Remove position entry
      (map-delete positions { participant: tx-sender })
      
      ;; Update total deposited amount
      (var-set total-deposited (- (var-get total-deposited) (get quantity position-info)))
      
      (ok withdraw-amount)
    )
  )
)

;; View functions
(define-read-only (get-position-info (participant principal))
  (map-get? positions { participant: participant })
)

(define-read-only (get-total-deposited)
  (var-get total-deposited)
)

;; Admin functions with enhanced security
(define-public (update-yield-rate 
  (token <sip-010-token>) 
  (new-rate uint)
)
  (begin
    ;; Validate token contract and authorization
    (asserts! (is-valid-token token) ERR-INVALID-TOKEN)
    (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERR-UNAUTHORIZED)
    
    ;; Placeholder for yield rate update logic
    ;; Additional implementation would go here
    (ok true)
  )
)

;; Initialization
(define-constant CONTRACT_ADMIN tx-sender)

;; Yield pool management with additional validation
(define-public (add-to-yield-pool 
  (token <sip-010-token>) 
  (quantity uint)
)
  (begin
    ;; Validate token contract
    (asserts! (is-valid-token token) ERR-INVALID-TOKEN)
    
    ;; Validate amount
    (asserts! (is-valid-quantity quantity) ERR-INVALID-QUANTITY)
    
    ;; Transfer tokens to yield pool
    (let ((transfer-result 
            (contract-call? token transfer 
              quantity 
              tx-sender 
              (as-contract tx-sender) 
              none
            )))
      (asserts! (is-ok transfer-result) ERR-TRANSFER-ERROR)
    )
    
    ;; Update yield pool
    (var-set yield-reserve 
      (+ (var-get yield-reserve) 
         (unwrap! 
           (contract-call? token get-balance (as-contract tx-sender)) 
           ERR-LOW-BALANCE
         )
      )
    )
    
    (ok true)
  )
)

;; Yield pool tracking
(define-data-var yield-reserve uint u0)