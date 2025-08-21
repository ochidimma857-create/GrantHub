
;; title: GrantHub DAO
;; version: 1.0.0
;; summary: Decentralized grants management with milestone-based releases
;; description: Community-managed grants DAO with proposal submission, quadratic/token-weighted voting,
;;              milestone verification via oracles, escrowed disbursements, and slashing mechanisms

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

(define-constant VOTING_PERIOD u1440) ;; blocks (approx 10 days)
(define-constant QUORUM_THRESHOLD u1000000) ;; 1M tokens minimum
(define-constant APPROVAL_THRESHOLD u60) ;; 60% approval required
(define-constant MAX_MILESTONES u10)
(define-constant SLASHING_PERIOD u2016) ;; blocks (approx 14 days)

;; data vars
(define-data-var proposal-counter uint u0)
(define-data-var total-supply uint u0)
(define-data-var dao-treasury uint u0)
(define-data-var voting-type (string-ascii 10) "quadratic")

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


;; public functions

;; Initialize the DAO with initial token supply
(define-public (initialize (initial-supply uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (ft-mint? governance-token initial-supply tx-sender))
    (map-set user-balances tx-sender initial-supply)
    (var-set total-supply initial-supply)
    (ok true)))

;; Mint governance tokens (only owner)
(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (ft-mint? governance-token amount recipient))
    (map-set user-balances recipient 
      (+ (get-balance recipient) amount))
    (var-set total-supply (+ (var-get total-supply) amount))
    (ok true)))

;; Transfer governance tokens
(define-public (transfer-tokens (recipient principal) (amount uint))
  (let ((sender-balance (get-balance tx-sender)))
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_FUNDS)
    (try! (ft-transfer? governance-token amount tx-sender recipient))
    (map-set user-balances tx-sender (- sender-balance amount))
    (map-set user-balances recipient (+ (get-balance recipient) amount))
    (ok true)))

;; Submit a new proposal
(define-public (submit-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (budget uint)
  (milestones (list 10 {description: (string-ascii 200), amount: uint})))
  (let 
    ((proposal-id (+ (var-get proposal-counter) u1))
     (voting-start (+ stacks-block-height u144)) ;; 1 day delay
     (voting-end (+ voting-start VOTING_PERIOD)))
    (asserts! (> (len milestones) u0) ERR_INVALID_PROPOSAL)
    (asserts! (<= (len milestones) MAX_MILESTONES) ERR_INVALID_PROPOSAL)
    (asserts! (> budget u0) ERR_INVALID_PROPOSAL)
    (asserts! (>= (var-get dao-treasury) budget) ERR_INSUFFICIENT_FUNDS)
    
    ;; Validate milestone amounts sum to budget
    (asserts! (is-eq budget (fold + (map get-milestone-amount milestones) u0)) ERR_INVALID_PROPOSAL)
    
    (map-set proposals proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      budget: budget,
      milestones: milestones,
      voting-start: voting-start,
      voting-end: voting-end,
      yes-votes: u0,
      no-votes: u0,
      total-voters: u0,
      status: "pending",
      funds-released: u0,
      current-milestone: u0
    })
    
    (try! (nft-mint? proposal-nft proposal-id tx-sender))
    (var-set proposal-counter proposal-id)
    (ok proposal-id)))

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
     (voter-balance (get-balance tx-sender))
     (voting-power (calculate-voting-power tx-sender proposal-id voter-balance)))
    
    (asserts! (> stacks-block-height (get voting-start proposal)) ERR_VOTING_ENDED)
    (asserts! (< stacks-block-height (get voting-end proposal)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? proposal-votes {proposal-id: proposal-id, voter: tx-sender})) ERR_ALREADY_VOTED)
    (asserts! (> voter-balance u0) ERR_UNAUTHORIZED)
    
    (map-set proposal-votes 
      {proposal-id: proposal-id, voter: tx-sender}
      {vote: vote-yes, weight: voting-power, block-height: stacks-block-height})
    
    (map-set user-voting-power 
      {user: tx-sender, proposal-id: proposal-id} 
      voting-power)
    
    (map-set proposals proposal-id
      (merge proposal {
        yes-votes: (if vote-yes (+ (get yes-votes proposal) voting-power) (get yes-votes proposal)),
        no-votes: (if vote-yes (get no-votes proposal) (+ (get no-votes proposal) voting-power)),
        total-voters: (+ (get total-voters proposal) u1)
      }))
    
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
    (map-set authorized-oracles oracle true)
    (ok true)))

;; Verify milestone (oracle only)
(define-public (verify-milestone (proposal-id uint) (milestone-id uint))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL)))
    
    (asserts! (default-to false (map-get? authorized-oracles tx-sender)) ERR_ORACLE_NOT_AUTHORIZED)
    (asserts! (is-eq (get status proposal) "approved") ERR_PROPOSAL_NOT_APPROVED)
    (asserts! (< milestone-id (len (get milestones proposal))) ERR_INVALID_MILESTONE)
    (asserts! (is-eq milestone-id (get current-milestone proposal)) ERR_INVALID_MILESTONE)
    
    (map-set milestone-verifications
      {proposal-id: proposal-id, milestone-id: milestone-id}
      {verified: true, oracle: tx-sender, verification-block: stacks-block-height})
    
    (ok true)))

;; Release milestone funds
(define-public (release-milestone-funds (proposal-id uint) (milestone-id uint))
  (let 
    ((proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
     (verification (map-get? milestone-verifications {proposal-id: proposal-id, milestone-id: milestone-id}))
     (milestone-amount (get amount (unwrap! (element-at (get milestones proposal) milestone-id) ERR_INVALID_MILESTONE))))
    
    (asserts! (is-eq (get status proposal) "approved") ERR_PROPOSAL_NOT_APPROVED)
    (asserts! (is-eq milestone-id (get current-milestone proposal)) ERR_INVALID_MILESTONE)
    (asserts! (is-some verification) ERR_MILESTONE_NOT_VERIFIED)
    (asserts! (get verified (unwrap-panic verification)) ERR_MILESTONE_NOT_VERIFIED)
    
    (let ((escrow-balance (default-to u0 (map-get? proposal-escrow proposal-id))))
      (asserts! (>= escrow-balance milestone-amount) ERR_INSUFFICIENT_FUNDS)
      
      (try! (as-contract (stx-transfer? milestone-amount tx-sender (get proposer proposal))))
      
      (map-set proposal-escrow proposal-id (- escrow-balance milestone-amount))
      (map-set proposals proposal-id 
        (merge proposal {
          funds-released: (+ (get funds-released proposal) milestone-amount),
          current-milestone: (+ milestone-id u1),
          status: (if (is-eq (+ milestone-id u1) (len (get milestones proposal))) "completed" "approved")
        }))
      
      (ok milestone-amount))))

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
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set dao-treasury (+ (var-get dao-treasury) amount))
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

;; private functions

(define-private (calculate-voting-power (voter principal) (proposal-id uint) (balance uint))
  (if (is-eq (var-get voting-type) "quadratic")
    (* balance balance) ;; Quadratic-style weighting (simplified): square of balance
    balance)) ;; Token-weighted: direct balance

(define-private (get-milestone-amount (milestone {description: (string-ascii 200), amount: uint}))
  (get amount milestone))