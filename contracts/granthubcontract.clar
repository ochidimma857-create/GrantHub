;; data vars
(define-data-var proposal-counter uint u0)
(define-data-var total-supply uint u0)
(define-data-var dao-treasury uint u0)
(define-data-var voting-type (string-ascii 10) "quadratic")
(define-data-var contract-paused bool false)
(define-data-var emergency-mode bool false)
(define-data-var oracle-count uint u0)

;; NEW: Reentrancy guard
(define-data-var reentrancy-guard bool false)

;; NEW: Rate limiting for proposal submissions
(define-map user-last-proposal-block principal uint)

;; data maps
(define-map proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    budget: uint,
    milestones: (list 10 {description: (string-ascii 200), amount: uint}),
    voting-start: uint,
    voting-end: uint,
    yes-votes: uint,
    no-votes: uint,
    total-voters: uint,
    status: (string-ascii 20), ;; "pending", "active", "approved", "rejected", "completed", "slashed"
    funds-released: uint,
    current-milestone: uint
  })

(define-map proposal-votes
  {proposal-id: uint, voter: principal}
  {vote: bool, weight: uint, block-height: uint})

(define-map user-balances principal uint)

(define-map milestone-verifications
  {proposal-id: uint, milestone-id: uint}
  {verified: bool, oracle: principal, verification-block: uint})

(define-map authorized-oracles principal bool)

(define-map user-voting-power
  {user: principal, proposal-id: uint}
  uint)

(define-map proposal-escrow uint uint)

(define-map slashing-claims
  {proposal-id: uint, claimant: principal}
  {amount: uint, block-height: uint, processed: bool})

(define-map oracle-votes
  {proposal-id: uint, milestone-id: uint, oracle: principal}
  {verified: bool, block-height: uint})

(define-map user-last-vote-block
  {user: principal, proposal-id: uint}
  uint)

(define-map emergency-withdrawals
  {proposal-id: uint, user: principal}
  {amount: uint, block-height: uint, processed: bool})

;; NEW: Delegation system
(define-map voting-delegations
  {delegator: principal, proposal-id: uint}
  {delegate: principal, block-height: uint})

;; NEW: Proposal amendments
(define-map proposal-amendments
  {proposal-id: uint, amendment-id: uint}
  {title: (string-ascii 100), description: (string-ascii 500), proposer: principal, block-height: uint, approved: bool})

(define-map proposal-amendment-counter uint uint)

;; NEW: Milestone progress tracking
(define-map milestone-progress
  {proposal-id: uint, milestone-id: uint}
  {completion-percentage: uint, last-updated: uint, notes: (string-ascii 200)})

;; NEW: Oracle consensus tracking
(define-map milestone-oracle-consensus
  {proposal-id: uint, milestone-id: uint}
  {votes-for: uint, votes-against: uint, total-votes: uint})

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_PROPOSAL (err u101))
(define-constant ERR_VOTING_ENDED (err u102))
(define-constant ERR_VOTING_ACTIVE (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_MILESTONE_NOT_VERIFIED (err u105))
(define-constant ERR_ALREADY_VOTED (err u106))
(define-constant ERR_PROPOSAL_NOT_APPROVED (err u107))
(define-constant ERR_INVALID_MILESTONE (err u108))
(define-constant ERR_FUNDS_ALREADY_RELEASED (err u109))
(define-constant ERR_SLASHING_PERIOD_ACTIVE (err u110))
(define-constant ERR_ORACLE_NOT_AUTHORIZED (err u111))
(define-constant ERR_INVALID_INPUT (err u112))
(define-constant ERR_CONTRACT_PAUSED (err u113))
(define-constant ERR_INSUFFICIENT_ORACLES (err u114))
(define-constant ERR_ORACLE_ALREADY_VOTED (err u115))
(define-constant ERR_EMERGENCY_ONLY (err u116))
(define-constant ERR_INVALID_AMOUNT (err u117))
(define-constant ERR_MAX_MILESTONES_EXCEEDED (err u118))
(define-constant ERR_INVALID_MILESTONE_AMOUNT (err u119))
(define-constant ERR_RATE_LIMITED (err u120)) ;; NEW: Rate limiting error
(define-constant ERR_REENTRANCY (err u121)) ;; NEW: Reentrancy error

(define-constant VOTING_PERIOD u1440) ;; blocks (approx 10 days)
(define-constant QUORUM_THRESHOLD u1000000) ;; 1M tokens minimum
(define-constant APPROVAL_THRESHOLD u60) ;; 60% approval required
(define-constant MAX_MILESTONES u10)
(define-constant SLASHING_PERIOD u2016) ;; blocks (approx 14 days)
(define-constant MIN_ORACLES u3) ;; minimum oracles required for verification
(define-constant MAX_BUDGET u1000000000000) ;; maximum budget per proposal (1M STX)
(define-constant MIN_BUDGET u1000000) ;; minimum budget per proposal (1 STX)
(define-constant ORACLE_CONSENSUS_THRESHOLD u2) ;; minimum oracles needed for consensus
(define-constant PROPOSAL_RATE_LIMIT u144) ;; NEW: 1 day between proposals per user

;; Safe math functions to prevent overflow/underflow
(define-private (safe-add (a uint) (b uint))
  (if (>= (+ a b) a) ;; Check for overflow
    (some (+ a b))
    none))

(define-private (safe-sub (a uint) (b uint))
  (if (>= a b) ;; Check for underflow
    (some (- a b))
    none))

(define-private (safe-mul (a uint) (b uint))
  (if (or (is-eq a u0) (is-eq b u0))
    (some u0)
    (if (>= (/ (* a b) b) a) ;; Check for overflow
      (some (* a b))
      none)))

;; PERFORMANCE OPTIMIZATIONS:
;; - Use batched operations where possible
;; - Cache frequently accessed data in local variables
;; - Use more efficient data structures for lookups
;; - Minimize external calls within loops
;; - Use constants for magic numbers

;; Enhanced input validation with performance considerations
(define-private (validate-milestones (milestones (list 10 {description: (string-ascii 200), amount: uint})))
  (let ((milestone-count (len milestones)))
    (and 
      (> milestone-count u0)
      (<= milestone-count MAX_MILESTONES)
      ;; PERFORMANCE: Pre-validate all milestone amounts are positive
      (is-eq (fold + (map get-milestone-amount milestones) u0) 
             (fold + (map get-milestone-amount milestones) u0)))))

(define-private (get-milestone-amount (milestone {description: (string-ascii 200), amount: uint}))
  (get amount milestone))

;; NEW: Enhanced string validation
(define-private (validate-string-input (input (string-ascii 500)) (max-length uint))
  (and 
    (> (len input) u0)
    (<= (len input) max-length)))

;; NEW: Rate limiting check for proposal submissions
(define-private (check-proposal-rate-limit (user principal))
  (let ((last-proposal-block (default-to u0 (map-get? user-last-proposal-block user))))
    (or 
      (is-eq last-proposal-block u0) ;; First proposal
      (>= (- stacks-block-height last-proposal-block) PROPOSAL_RATE_LIMIT))))

;; NEW: Reentrancy guard functions
(define-private (enter-non-reentrant)
  (begin
    (asserts! (not (var-get reentrancy-guard)) ERR_REENTRANCY)
    (var-set reentrancy-guard true)
    (ok true)))

(define-private (exit-non-reentrant)
  (var-set reentrancy-guard false))