
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