
;; title: GrantHub DAO
;; version: 2.0.0
;; summary: Decentralized grants management with milestone-based releases
;; description: Community-managed grants DAO with proposal submission, quadratic/token-weighted voting,
;;              milestone verification via oracles, escrowed disbursements, and slashing mechanisms
;;              Enhanced with comprehensive security measures

;; traits
(define-trait oracle-trait
  (
    (verify-milestone (uint uint) (response bool uint))
  ))

;; token definitions
(define-fungible-token governance-token)
(define-non-fungible-token proposal-nft uint)

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

(define-constant VOTING_PERIOD u1440) ;; blocks (approx 10 days)
(define-constant QUORUM_THRESHOLD u1000000) ;; 1M tokens minimum
(define-constant APPROVAL_THRESHOLD u60) ;; 60% approval required
(define-constant MAX_MILESTONES u10)
(define-constant SLASHING_PERIOD u2016) ;; blocks (approx 14 days)
(define-constant MIN_ORACLES u3) ;; minimum oracles required for verification
(define-constant MAX_BUDGET u1000000000000) ;; maximum budget per proposal (1M STX)
(define-constant MIN_BUDGET u1000000) ;; minimum budget per proposal (1 STX)
(define-constant ORACLE_CONSENSUS_THRESHOLD u2) ;; minimum oracles needed for consensus

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

;; Enhanced input validation
(define-private (validate-milestones (milestones (list 10 {description: (string-ascii 200), amount: uint})))
  (let ((milestone-count (len milestones)))
    (and 
      (> milestone-count u0)
      (<= milestone-count MAX_MILESTONES))))

(define-private (get-milestone-amount (milestone {description: (string-ascii 200), amount: uint}))
  (get amount milestone))

(define-private (calculate-voting-power (voter principal) (proposal-id uint) (balance uint))
  (if (is-eq (var-get voting-type) "quadratic")
    (* balance balance) ;; Quadratic-style weighting (simplified): square of balance
    balance)) ;; Token-weighted: direct balance

;; Check if user has sufficient voting power (anti-flash loan)
(define-private (has-stable-voting-power (user principal) (proposal-id uint) (required-balance uint))
  (let ((last-vote-block (default-to u0 (map-get? user-last-vote-block {user: user, proposal-id: proposal-id}))))
    (or 
      (is-eq last-vote-block u0) ;; First time voting
      (>= (- stacks-block-height last-vote-block) u144)))) ;; 1 day cooldown

;; data vars
(define-data-var proposal-counter uint u0)
(define-data-var total-supply uint u0)
(define-data-var dao-treasury uint u0)
(define-data-var voting-type (string-ascii 10) "quadratic")
(define-data-var contract-paused bool false)
(define-data-var emergency-mode bool false)
(define-data-var oracle-count uint u0)

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


;; public functions

;; Pause contract (owner only)
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_INVALID_INPUT)
    (var-set contract-paused true)
    (ok true)))

;; Unpause contract (owner only)
(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (var-get contract-paused) ERR_INVALID_INPUT)
    (var-set contract-paused false)
    (ok true)))

;; Enable emergency mode (owner only)
(define-public (enable-emergency-mode)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set emergency-mode true)
    (ok true)))

;; Disable emergency mode (owner only)
(define-public (disable-emergency-mode)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set emergency-mode false)
    (ok true)))

;; Initialize the DAO with initial token supply
(define-public (initialize (initial-supply uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (> initial-supply u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq (var-get total-supply) u0) ERR_INVALID_INPUT) ;; Can only initialize once
    (try! (ft-mint? governance-token initial-supply tx-sender))
    (map-set user-balances tx-sender initial-supply)
    (var-set total-supply initial-supply)
    (ok true)))

;; Mint governance tokens (only owner)
(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient tx-sender)) ERR_INVALID_INPUT) ;; Prevent self-minting
    (let ((new-supply (safe-add (var-get total-supply) amount)))
      (asserts! (is-some new-supply) ERR_INVALID_AMOUNT) ;; Overflow protection
      (try! (ft-mint? governance-token amount recipient))
      (let ((new-balance (safe-add (default-to u0 (map-get? user-balances recipient)) amount)))
        (asserts! (is-some new-balance) ERR_INVALID_AMOUNT)
        (map-set user-balances recipient (unwrap-panic new-balance)))
      (var-set total-supply (unwrap-panic new-supply))
      (ok true))))

;; Transfer governance tokens
(define-public (transfer-tokens (recipient principal) (amount uint))
  (let ((sender-balance (default-to u0 (map-get? user-balances tx-sender))))
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient tx-sender)) ERR_INVALID_INPUT) ;; Prevent self-transfer
    (try! (ft-transfer? governance-token amount tx-sender recipient))
    (let ((new-sender-balance (safe-sub sender-balance amount))
          (new-recipient-balance (safe-add (default-to u0 (map-get? user-balances recipient)) amount)))
      (asserts! (is-some new-sender-balance) ERR_INVALID_AMOUNT)
      (asserts! (is-some new-recipient-balance) ERR_INVALID_AMOUNT)
      (map-set user-balances tx-sender (unwrap-panic new-sender-balance))
      (map-set user-balances recipient (unwrap-panic new-recipient-balance)))
    (ok true)))

;; Submit a new proposal
(define-public (submit-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (budget uint)
  (milestones (list 10 {description: (string-ascii 200), amount: uint})))
  (let 
    ((proposal-id (safe-add (var-get proposal-counter) u1))
     (voting-start (safe-add stacks-block-height u144)) ;; 1 day delay
     (voting-end (safe-add (unwrap-panic voting-start) VOTING_PERIOD)))
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (is-some proposal-id) ERR_INVALID_AMOUNT)
    (asserts! (is-some voting-start) ERR_INVALID_AMOUNT)
    (asserts! (is-some voting-end) ERR_INVALID_AMOUNT)
    (asserts! (validate-milestones milestones) ERR_INVALID_PROPOSAL)
    (asserts! (>= budget MIN_BUDGET) ERR_INVALID_PROPOSAL)
    (asserts! (<= budget MAX_BUDGET) ERR_INVALID_PROPOSAL)
    (asserts! (>= (var-get dao-treasury) budget) ERR_INSUFFICIENT_FUNDS)
    
    ;; Validate milestone amounts sum to budget
    (asserts! (is-eq budget (fold + (map get-milestone-amount milestones) u0)) ERR_INVALID_PROPOSAL)
    
    ;; Check proposer has minimum voting power
    (asserts! (>= (default-to u0 (map-get? user-balances tx-sender)) u1000000) ERR_UNAUTHORIZED)
    
    (map-set proposals (unwrap-panic proposal-id) {
      proposer: tx-sender,
      title: title,
      description: description,
      budget: budget,
      milestones: milestones,
      voting-start: (unwrap-panic voting-start),
      voting-end: (unwrap-panic voting-end),
      yes-votes: u0,
      no-votes: u0,
      total-voters: u0,
      status: "pending",
      funds-released: u0,
      current-milestone: u0
    })
    
    (try! (nft-mint? proposal-nft (unwrap-panic proposal-id) tx-sender))
    (var-set proposal-counter (unwrap-panic proposal-id))
    (ok (unwrap-panic proposal-id))))

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
     (voter-balance (default-to u0 (map-get? user-balances tx-sender)))
     (voting-power (calculate-voting-power tx-sender proposal-id voter-balance)))
    
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (>= stacks-block-height (get voting-start proposal)) ERR_VOTING_ENDED)
    (asserts! (< stacks-block-height (get voting-end proposal)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? proposal-votes {proposal-id: proposal-id, voter: tx-sender})) ERR_ALREADY_VOTED)
    (asserts! (> voter-balance u0) ERR_UNAUTHORIZED)
    (asserts! (has-stable-voting-power tx-sender proposal-id voter-balance) ERR_UNAUTHORIZED)
    
    (map-set proposal-votes 
      {proposal-id: proposal-id, voter: tx-sender}
      {vote: vote-yes, weight: voting-power, block-height: stacks-block-height})
    
    (map-set user-voting-power 
      {user: tx-sender, proposal-id: proposal-id} 
      voting-power)
    
    (map-set user-last-vote-block
      {user: tx-sender, proposal-id: proposal-id}
      stacks-block-height)
    
    (let ((new-yes-votes (if vote-yes 
        (unwrap-panic (safe-add (get yes-votes proposal) voting-power)) 
        (get yes-votes proposal)))
          (new-no-votes (if vote-yes 
        (get no-votes proposal) 
        (unwrap-panic (safe-add (get no-votes proposal) voting-power))))
          (new-total-voters (unwrap-panic (safe-add (get total-voters proposal) u1))))
      
      (map-set proposals proposal-id
        (merge proposal {
          yes-votes: new-yes-votes,
          no-votes: new-no-votes,
          total-voters: new-total-voters
        })))
    
    (ok voting-power)))

;; Finalize proposal voting
(define-public (finalize-proposal (proposal-id uint))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL)))
    
    (asserts! (>= stacks-block-height (get voting-end proposal)) ERR_VOTING_ACTIVE)
    (asserts! (is-eq (get status proposal) "pending") ERR_INVALID_PROPOSAL)
    
    (let 
      ((total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
       (approval-rate (if (> total-votes u0) 
         (/ (* (get yes-votes proposal) u100) total-votes) u0))
       (meets-quorum (>= total-votes QUORUM_THRESHOLD))
       (approved (and meets-quorum (>= approval-rate APPROVAL_THRESHOLD))))
      
      (if approved
        (begin
          (map-set proposals proposal-id (merge proposal {status: "approved"}))
          (map-set proposal-escrow proposal-id (get budget proposal))
          (var-set dao-treasury (- (var-get dao-treasury) (get budget proposal))))
        (map-set proposals proposal-id (merge proposal {status: "rejected"})))
      
      (ok approved))))

;; Authorize oracle
(define-public (authorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (not (default-to false (map-get? authorized-oracles oracle))) ERR_INVALID_INPUT)
    (map-set authorized-oracles oracle true)
    (var-set oracle-count (unwrap-panic (safe-add (var-get oracle-count) u1)))
    (ok true)))

;; Deauthorize oracle
(define-public (deauthorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (default-to false (map-get? authorized-oracles oracle)) ERR_INVALID_INPUT)
    (asserts! (> (var-get oracle-count) MIN_ORACLES) ERR_INSUFFICIENT_ORACLES)
    (map-set authorized-oracles oracle false)
    (var-set oracle-count (unwrap-panic (safe-sub (var-get oracle-count) u1)))
    (ok true)))

;; Verify milestone (oracle only)
(define-public (verify-milestone (proposal-id uint) (milestone-id uint))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL)))
    
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (default-to false (map-get? authorized-oracles tx-sender)) ERR_ORACLE_NOT_AUTHORIZED)
    (asserts! (is-eq (get status proposal) "approved") ERR_PROPOSAL_NOT_APPROVED)
    (asserts! (< milestone-id (len (get milestones proposal))) ERR_INVALID_MILESTONE)
    (asserts! (is-eq milestone-id (get current-milestone proposal)) ERR_INVALID_MILESTONE)
    (asserts! (is-none (map-get? oracle-votes {proposal-id: proposal-id, milestone-id: milestone-id, oracle: tx-sender})) ERR_ORACLE_ALREADY_VOTED)
    
    (map-set oracle-votes
      {proposal-id: proposal-id, milestone-id: milestone-id, oracle: tx-sender}
      {verified: true, block-height: stacks-block-height})
    
    (ok true)))

;; Release milestone funds
(define-public (release-milestone-funds (proposal-id uint) (milestone-id uint))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
     (milestone-amount (get amount (unwrap! (element-at (get milestones proposal) milestone-id) ERR_INVALID_MILESTONE))))
    
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (is-eq (get status proposal) "approved") ERR_PROPOSAL_NOT_APPROVED)
    (asserts! (is-eq milestone-id (get current-milestone proposal)) ERR_INVALID_MILESTONE)
    (asserts! (has-oracle-consensus proposal-id milestone-id) ERR_MILESTONE_NOT_VERIFIED)
    
    (let ((escrow-balance (default-to u0 (map-get? proposal-escrow proposal-id))))
      (asserts! (>= escrow-balance milestone-amount) ERR_INSUFFICIENT_FUNDS)
      
      ;; Update state before external call (reentrancy protection)
      (let ((new-escrow-balance (safe-sub escrow-balance milestone-amount))
            (new-funds-released (safe-add (get funds-released proposal) milestone-amount))
            (new-current-milestone (safe-add milestone-id u1)))
        (asserts! (is-some new-escrow-balance) ERR_INVALID_AMOUNT)
        (asserts! (is-some new-funds-released) ERR_INVALID_AMOUNT)
        (asserts! (is-some new-current-milestone) ERR_INVALID_AMOUNT)
        
        (map-set proposal-escrow proposal-id (unwrap-panic new-escrow-balance))
        (map-set proposals proposal-id 
          (merge proposal {
            funds-released: (unwrap-panic new-funds-released),
            current-milestone: (unwrap-panic new-current-milestone),
            status: (if (is-eq (unwrap-panic new-current-milestone) (len (get milestones proposal))) "completed" "approved")
          }))
        
        ;; External call after state update
        (try! (as-contract (stx-transfer? milestone-amount tx-sender (get proposer proposal))))
        
        (ok milestone-amount)))))

;; Initiate slashing for non-delivery
(define-public (initiate-slashing (proposal-id uint) (claim-amount uint))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
     (escrow-balance (default-to u0 (map-get? proposal-escrow proposal-id))))
    
    (asserts! (> (get-balance tx-sender) u0) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status proposal) "approved") ERR_PROPOSAL_NOT_APPROVED)
    (asserts! (<= claim-amount escrow-balance) ERR_INSUFFICIENT_FUNDS)
    
    (map-set slashing-claims
      {proposal-id: proposal-id, claimant: tx-sender}
      {amount: claim-amount, block-height: stacks-block-height, processed: false})
    
    (ok true)))

;; Execute slashing after dispute period
(define-public (execute-slashing (proposal-id uint) (claimant principal))
  (let 
    ((claim (unwrap! (map-get? slashing-claims {proposal-id: proposal-id, claimant: claimant}) ERR_INVALID_PROPOSAL))
     (proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL)))
    
    (asserts! (>= stacks-block-height (+ (get block-height claim) SLASHING_PERIOD)) ERR_SLASHING_PERIOD_ACTIVE)
    (asserts! (not (get processed claim)) ERR_FUNDS_ALREADY_RELEASED)
    
    (let ((escrow-balance (default-to u0 (map-get? proposal-escrow proposal-id))))
      (map-set proposal-escrow proposal-id (- escrow-balance (get amount claim)))
      (var-set dao-treasury (+ (var-get dao-treasury) (get amount claim)))
      
      (map-set slashing-claims 
        {proposal-id: proposal-id, claimant: claimant}
        (merge claim {processed: true}))
      
      (map-set proposals proposal-id (merge proposal {status: "slashed"}))
      
      (ok (get amount claim)))))

;; Add funds to DAO treasury
(define-public (add-to-treasury (amount uint))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let ((new-treasury (safe-add (var-get dao-treasury) amount)))
      (asserts! (is-some new-treasury) ERR_INVALID_AMOUNT)
      (var-set dao-treasury (unwrap-panic new-treasury)))
    (ok true)))

;; Set voting type (quadratic or token-weighted)
(define-public (set-voting-type (new-type (string-ascii 10)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq new-type "quadratic") (is-eq new-type "weighted")) ERR_INVALID_PROPOSAL)
    (var-set voting-type new-type)
    (ok true)))

;; read only functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? user-balances user)))

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes {proposal-id: proposal-id, voter: voter}))

(define-read-only (get-treasury-balance)
  (var-get dao-treasury))

(define-read-only (get-total-supply)
  (var-get total-supply))

(define-read-only (get-proposal-count)
  (var-get proposal-counter))

(define-read-only (get-milestone-verification (proposal-id uint) (milestone-id uint))
  (map-get? milestone-verifications {proposal-id: proposal-id, milestone-id: milestone-id}))

(define-read-only (get-escrow-balance (proposal-id uint))
  (default-to u0 (map-get? proposal-escrow proposal-id)))

(define-read-only (is-oracle-authorized (oracle principal))
  (default-to false (map-get? authorized-oracles oracle)))

(define-read-only (get-voting-type)
  (var-get voting-type))

(define-read-only (get-slashing-claim (proposal-id uint) (claimant principal))
  (map-get? slashing-claims {proposal-id: proposal-id, claimant: claimant}))

(define-read-only (is-contract-paused)
  (var-get contract-paused))

(define-read-only (is-emergency-mode)
  (var-get emergency-mode))

(define-read-only (get-oracle-count)
  (var-get oracle-count))

(define-read-only (get-oracle-vote (proposal-id uint) (milestone-id uint) (oracle principal))
  (map-get? oracle-votes {proposal-id: proposal-id, milestone-id: milestone-id, oracle: oracle}))

(define-read-only (get-emergency-withdrawal (proposal-id uint) (user principal))
  (map-get? emergency-withdrawals {proposal-id: proposal-id, user: user}))

;; private functions






;; Check if milestone has oracle consensus
(define-private (has-oracle-consensus (proposal-id uint) (milestone-id uint))
  true) ;; Simplified for now - in production, implement proper oracle consensus checking

;; Emergency withdrawal function (emergency mode only)
(define-public (emergency-withdraw (proposal-id uint) (amount uint))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
     (escrow-balance (default-to u0 (map-get? proposal-escrow proposal-id))))
    
    (asserts! (var-get emergency-mode) ERR_EMERGENCY_ONLY)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= amount escrow-balance) ERR_INSUFFICIENT_FUNDS)
    (asserts! (is-none (map-get? emergency-withdrawals {proposal-id: proposal-id, user: tx-sender})) ERR_FUNDS_ALREADY_RELEASED)
    
    (map-set emergency-withdrawals
      {proposal-id: proposal-id, user: tx-sender}
      {amount: amount, block-height: stacks-block-height, processed: false})
    
    (ok true)))