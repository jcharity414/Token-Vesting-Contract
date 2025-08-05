;; Token Vesting Contract
;; A comprehensive vesting system for team tokens and investor allocations
;; Supports cliff periods, linear vesting, and multiple beneficiary management

;; =================================
;; CONSTANTS & ERROR CODES
;; =================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INVALID_DURATION (err u102))
(define-constant ERR_VESTING_NOT_FOUND (err u103))
(define-constant ERR_NO_TOKENS_VESTED (err u104))
(define-constant ERR_CLIFF_NOT_REACHED (err u105))
(define-constant ERR_VESTING_ALREADY_EXISTS (err u106))
(define-constant ERR_INSUFFICIENT_BALANCE (err u107))
(define-constant ERR_VESTING_COMPLETED (err u108))
(define-constant ERR_INVALID_CLIFF (err u109))

;; Block height constants (assuming ~10 minute blocks)
(define-constant BLOCKS_PER_DAY u144)
(define-constant BLOCKS_PER_MONTH u4320)  ;; 30 days
(define-constant BLOCKS_PER_YEAR u52560)  ;; 365 days

;; =================================
;; DATA STRUCTURES
;; =================================

;; Vesting schedule structure
(define-map vesting-schedules
  { beneficiary: principal }
  {
    total-amount: uint,           ;; Total tokens to be vested
    released-amount: uint,        ;; Tokens already released
    start-block: uint,           ;; When vesting starts
    cliff-duration: uint,        ;; Cliff period in blocks
    vesting-duration: uint,      ;; Total vesting duration in blocks
    revoked: bool                ;; Whether vesting is revoked
  }
)

;; Track total tokens allocated for vesting
(define-data-var total-allocated uint u0)

;; Token contract reference (SIP-010 compatible)
(define-data-var token-contract (optional principal) none)

;; Contract pause state
(define-data-var contract-paused bool false)

;; =================================
;; AUTHORIZATION FUNCTIONS
;; =================================

(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (assert-contract-owner)
  (ok (asserts! (is-contract-owner) ERR_UNAUTHORIZED))
)

(define-private (assert-not-paused)
  (ok (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED))
)

;; =================================
;; VESTING CALCULATIONS
;; =================================

(define-private (calculate-vested-amount (schedule (tuple (total-amount uint) (released-amount uint) (start-block uint) (cliff-duration uint) (vesting-duration uint) (revoked bool))))
  (let (
    (current-block stacks-block-height)
    (start-block (get start-block schedule))
    (cliff-end (+ start-block (get cliff-duration schedule)))
    (vesting-end (+ start-block (get vesting-duration schedule)))
    (total-amount (get total-amount schedule))
  )
    (if (get revoked schedule)
      u0
      (if (< current-block cliff-end)
        u0
        (if (>= current-block vesting-end)
          total-amount
          (let (
            (elapsed-blocks (- current-block cliff-end))
            (vesting-blocks (- vesting-end cliff-end))
          )
            (/ (* total-amount elapsed-blocks) vesting-blocks)
          )
        )
      )
    )
  )
)

(define-private (calculate-releasable-amount (beneficiary principal))
  (match (map-get? vesting-schedules { beneficiary: beneficiary })
    schedule
    (let (
      (vested-amount (calculate-vested-amount schedule))
      (released-amount (get released-amount schedule))
    )
      (if (> vested-amount released-amount)
        (- vested-amount released-amount)
        u0
      )
    )
    u0
  )
)

;; =================================
;; ADMIN FUNCTIONS
;; =================================

(define-public (set-token-contract (new-token-contract principal))
  (begin
    (try! (assert-contract-owner))
    (var-set token-contract (some new-token-contract))
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (try! (assert-contract-owner))
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (try! (assert-contract-owner))
    (var-set contract-paused false)
    (ok true)
  )
)

(define-public (create-vesting-schedule
  (beneficiary principal)
  (total-amount uint)
  (cliff-duration uint)
  (vesting-duration uint)
)
  (begin
    (try! (assert-contract-owner))
    (try! (assert-not-paused))

    ;; Validate inputs
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> vesting-duration u0) ERR_INVALID_DURATION)
    (asserts! (<= cliff-duration vesting-duration) ERR_INVALID_CLIFF)

    ;; Check if vesting already exists
    (asserts! (is-none (map-get? vesting-schedules { beneficiary: beneficiary })) ERR_VESTING_ALREADY_EXISTS)

    ;; Create the vesting schedule
    (map-set vesting-schedules
      { beneficiary: beneficiary }
      {
        total-amount: total-amount,
        released-amount: u0,
        start-block: stacks-block-height,
        cliff-duration: cliff-duration,
        vesting-duration: vesting-duration,
        revoked: false
      }
    )

    ;; Update total allocated
    (var-set total-allocated (+ (var-get total-allocated) total-amount))

    (ok true)
  )
)

(define-public (revoke-vesting (beneficiary principal))
  (begin
    (try! (assert-contract-owner))
    (try! (assert-not-paused))

    (match (map-get? vesting-schedules { beneficiary: beneficiary })
      schedule
      (begin
        ;; Calculate any remaining vested tokens to release before revoking
        (let (
          (releasable (calculate-releasable-amount beneficiary))
        )
          ;; If there are releasable tokens, release them first
          (if (> releasable u0)
            (try! (release-tokens-internal beneficiary releasable))
            true
          )
        )

        ;; Mark as revoked
        (map-set vesting-schedules
          { beneficiary: beneficiary }
          (merge schedule { revoked: true })
        )

        (ok true)
      )
      ERR_VESTING_NOT_FOUND
    )
  )
)

;; =================================
;; BENEFICIARY FUNCTIONS
;; =================================

(define-public (release-tokens)
  (let (
    (beneficiary tx-sender)
    (releasable-amount (calculate-releasable-amount beneficiary))
  )
    (try! (assert-not-paused))
    (asserts! (> releasable-amount u0) ERR_NO_TOKENS_VESTED)

    (try! (release-tokens-internal beneficiary releasable-amount))
    (ok releasable-amount)
  )
)

(define-private (release-tokens-internal (beneficiary principal) (amount uint))
  (match (map-get? vesting-schedules { beneficiary: beneficiary })
    schedule
    (begin
      ;; Update released amount
      (map-set vesting-schedules
        { beneficiary: beneficiary }
        (merge schedule { released-amount: (+ (get released-amount schedule) amount) })
      )

      ;; Transfer tokens (would integrate with actual SIP-010 token)
      ;; For now, we'll emit a print statement
      (print {
        event: "tokens-released",
        beneficiary: beneficiary,
        amount: amount,
        block-height: stacks-block-height
      })

      (ok true)
    )
    ERR_VESTING_NOT_FOUND
  )
)

;; =================================
;; READ-ONLY FUNCTIONS
;; =================================

(define-read-only (get-vesting-schedule (beneficiary principal))
  (map-get? vesting-schedules { beneficiary: beneficiary })
)

(define-read-only (get-vested-amount (beneficiary principal))
  (match (map-get? vesting-schedules { beneficiary: beneficiary })
    schedule (calculate-vested-amount schedule)
    u0
  )
)

(define-read-only (get-releasable-amount (beneficiary principal))
  (calculate-releasable-amount beneficiary)
)

(define-read-only (get-total-allocated)
  (var-get total-allocated)
)

(define-read-only (is-cliff-reached (beneficiary principal))
  (match (map-get? vesting-schedules { beneficiary: beneficiary })
    schedule
    (let (
      (current-block stacks-block-height)
      (cliff-end (+ (get start-block schedule) (get cliff-duration schedule)))
    )
      (>= current-block cliff-end)
    )
    false
  )
)

(define-read-only (is-vesting-complete (beneficiary principal))
  (match (map-get? vesting-schedules { beneficiary: beneficiary })
    schedule
    (let (
      (current-block stacks-block-height)
      (vesting-end (+ (get start-block schedule) (get vesting-duration schedule)))
    )
      (>= current-block vesting-end)
    )
    false
  )
)

(define-read-only (get-contract-info)
  {
    owner: CONTRACT_OWNER,
    total-allocated: (var-get total-allocated),
    token-contract: (var-get token-contract),
    paused: (var-get contract-paused),
    current-block: stacks-block-height
  }
)

;; =================================
;; UTILITY FUNCTIONS
;; =================================

(define-read-only (blocks-to-days (blocks uint))
  (/ blocks BLOCKS_PER_DAY)
)

(define-read-only (days-to-blocks (days uint))
  (* days BLOCKS_PER_DAY)
)

(define-read-only (months-to-blocks (months uint))
  (* months BLOCKS_PER_MONTH)
)

(define-read-only (years-to-blocks (years uint))
  (* years BLOCKS_PER_YEAR)
)

;; =================================
;; BATCH OPERATIONS
;; =================================

(define-public (create-multiple-vesting-schedules
  (schedules (list 50 {beneficiary: principal, amount: uint, cliff: uint, duration: uint}))
)
  (begin
    (try! (assert-contract-owner))
    (try! (assert-not-paused))

    (fold create-single-schedule schedules (ok true))
  )
)

(define-private (create-single-schedule
  (schedule {beneficiary: principal, amount: uint, cliff: uint, duration: uint})
  (prev-result (response bool uint))
)
  (match prev-result
    success (create-vesting-schedule
              (get beneficiary schedule)
              (get amount schedule)
              (get cliff schedule)
              (get duration schedule))
    error (err error)
  )
)
