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

(define-public (log-practice (hours uint) (subject (string-ascii 50)) (notes (string-ascii 200)))
  (let (
    (user tx-sender)
    (today (/ block-height u144)) ;; Approximate daily blocks
    (current-streaks (default-to { current-streak: u0, longest-streak: u0, last-practice-date: u0, total-hours: u0, weekly-hours: u0, monthly-hours: u0 }
                                 (map-get? user-streaks { user: user })))
  )
    (asserts! (and (> hours u0) (<= hours u24)) err-invalid-hours)
    (asserts! (is-none (map-get? practice-logs { user: user, date: today })) err-already-logged-today)
    
    (map-set practice-logs
      { user: user, date: today }
      { hours: hours, subject: subject, verified: false, notes: notes }
    )
    
    (let (
      (new-streak (if (is-eq (get last-practice-date current-streaks) (- today u1))
                    (+ (get current-streak current-streaks) u1)
                    u1))
      (new-longest (if (> new-streak (get longest-streak current-streaks))
                     new-streak
                     (get longest-streak current-streaks)))
      (week-start (- today (mod today u7)))
      (month-start (- today (mod today u30)))
    )
      (map-set user-streaks
        { user: user }
        {
          current-streak: new-streak,
          longest-streak: new-longest,
          last-practice-date: today,
          total-hours: (+ (get total-hours current-streaks) hours),
          weekly-hours: (+ (get weekly-hours current-streaks) hours),
          monthly-hours: (+ (get monthly-hours current-streaks) hours)
        }
      )
      
      ;; Update subject-specific stats
      (let ((subject-data (default-to { total-hours: u0, streak: u0, level: u0, xp: u0 }
                                     (map-get? subject-stats { user: user, subject: subject }))))
        (map-set subject-stats
          { user: user, subject: subject }
          {
            total-hours: (+ (get total-hours subject-data) hours),
            streak: new-streak,
            level: (/ (+ (get total-hours subject-data) hours) u20), ;; Level up every 20 hours
            xp: (+ (get xp subject-data) (* hours u10))
          }
        )
      )
      (ok new-streak)
    )
  )
)

(define-map practice-challenges
  { challenge-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 300),
    subject: (string-ascii 50),
    target-hours: uint,
    duration-days: uint,
    reward-amount: uint,
    participants: uint,
    completed: uint,
    active: bool,
    start-date: uint,
    end-date: uint
  }
)

(define-map challenge-participants
  { challenge-id: uint, user: principal }
  { hours-logged: uint, completed: bool, reward-claimed: bool }
)

(define-map mentor-verifications
  { mentor: principal }
  { verified: bool, specialties: (list 5 (string-ascii 50)) }
)

(define-public (create-practice-challenge (title (string-ascii 100)) (description (string-ascii 300)) (subject (string-ascii 50)) (target-hours uint) (duration-days uint) (reward-amount uint))
  (let ((challenge-id (var-get next-challenge-id)))
    (asserts! (>= (var-get practice-reward-pool) reward-amount) err-invalid-challenge)
    (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
    
    (map-set practice-challenges
      { challenge-id: challenge-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        subject: subject,
        target-hours: target-hours,
        duration-days: duration-days,
        reward-amount: reward-amount,
        participants: u0,
        completed: u0,
        active: true,
        start-date: (/ block-height u144),
        end-date: (+ (/ block-height u144) duration-days)
      }
    )
    (var-set next-challenge-id (+ challenge-id u1))
    (var-set practice-reward-pool (+ (var-get practice-reward-pool) reward-amount))
    (ok challenge-id)
  )
)

(define-public (join-challenge (challenge-id uint))
  (let ((challenge (unwrap! (map-get? practice-challenges { challenge-id: challenge-id }) err-not-found)))
    (asserts! (get active challenge) err-challenge-not-active)
    (asserts! (is-none (map-get? challenge-participants { challenge-id: challenge-id, user: tx-sender })) err-already-claimed)
    
    (map-set challenge-participants
      { challenge-id: challenge-id, user: tx-sender }
      { hours-logged: u0, completed: false, reward-claimed: false }
    )
    (map-set practice-challenges
      { challenge-id: challenge-id }
      (merge challenge { participants: (+ (get participants challenge) u1) })
    )
    (ok true)
  )
)

(define-public (claim-challenge-reward (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? practice-challenges { challenge-id: challenge-id }) err-not-found))
    (participation (unwrap! (map-get? challenge-participants { challenge-id: challenge-id, user: tx-sender }) err-not-found))
  )
    (asserts! (get completed participation) err-invalid-challenge)
    (asserts! (not (get reward-claimed participation)) err-already-claimed)
    (asserts! (>= (get hours-logged participation) (get target-hours challenge)) err-invalid-challenge)
    
    (let ((reward-per-participant (/ (get reward-amount challenge) (get completed challenge))))
      (try! (as-contract (stx-transfer? reward-per-participant tx-sender tx-sender)))
      (map-set challenge-participants
        { challenge-id: challenge-id, user: tx-sender }
        (merge participation { reward-claimed: true })
      )
    )
    (ok true)
  )
)

(define-public (fund-reward-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set practice-reward-pool (+ (var-get practice-reward-pool) amount))
    (ok true)
  )
)