(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-streak (err u103))
(define-constant err-already-claimed (err u104))
(define-constant err-invalid-hours (err u105))
(define-constant err-already-logged-today (err u106))
(define-constant err-invalid-challenge (err u107))
(define-constant err-challenge-not-active (err u108))

(define-data-var next-nft-id uint u1)
(define-data-var next-challenge-id uint u0)
(define-data-var practice-reward-pool uint u0)

(define-map practice-logs
  { user: principal, date: uint }
  { hours: uint, subject: (string-ascii 50), verified: bool, notes: (string-ascii 200) }
)

(define-map user-streaks
  { user: principal }
  {
    current-streak: uint,
    longest-streak: uint,
    last-practice-date: uint,
    total-hours: uint,
    weekly-hours: uint,
    monthly-hours: uint
  }
)

(define-map subject-stats
  { user: principal, subject: (string-ascii 50) }
  { total-hours: uint, streak: uint, level: uint, xp: uint }
)

(define-map nft-achievements
  { nft-id: uint }
  {
    owner: principal,
    achievement-type: (string-ascii 50),
    streak-length: uint,
    subject: (string-ascii 50),
    minted-at: uint,
    transferable: bool
  }
)

(define-map user-badges
  { user: principal, badge-type: (string-ascii 50) }
  { earned-at: uint, level: uint }
)

(define-map study-groups
  { group-id: uint }
  {
    creator: principal,
    name: (string-ascii 100),
    subject: (string-ascii 50),
    members: uint,
    total-hours: uint,
    active: bool
  }
)

(define-map group-memberships
  { user: principal, group-id: uint }
  { joined-at: uint, contribution-hours: uint }
)

(define-public (create-study-group (name (string-ascii 100)) (subject (string-ascii 50)))
  (let ((group-id (var-get next-challenge-id)))
    (map-set study-groups
      { group-id: group-id }
      {
        creator: tx-sender,
        name: name,
        subject: subject,
        members: u1,
        total-hours: u0,
        active: true
      }
    )
    (map-set group-memberships
      { user: tx-sender, group-id: group-id }
      { joined-at: block-height, contribution-hours: u0 }
    )
    (var-set next-challenge-id (+ group-id u1))
    (ok group-id)
  )
)

(define-public (join-study-group (group-id uint))
  (let ((group (unwrap! (map-get? study-groups { group-id: group-id }) err-not-found)))
    (asserts! (get active group) err-challenge-not-active)
    (asserts! (is-none (map-get? group-memberships { user: tx-sender, group-id: group-id })) err-already-claimed)
    
    (map-set group-memberships
      { user: tx-sender, group-id: group-id }
      { joined-at: block-height, contribution-hours: u0 }
    )
    (map-set study-groups
      { group-id: group-id }
      (merge group { members: (+ (get members group) u1) })
    )
    (ok true)
  )
)