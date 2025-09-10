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
      { joined-at: stacks-block-height, contribution-hours: u0 }
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
      { joined-at: stacks-block-height, contribution-hours: u0 }
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
    (today (/ stacks-block-height u144)) ;; Approximate daily blocks
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
        start-date: (/ stacks-block-height u144),
        end-date: (+ (/ stacks-block-height u144) duration-days)
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

(define-public (claim-streak-nft (streak-days uint) (subject (string-ascii 50)))
  (let (
    (user tx-sender)
    (user-streak (unwrap! (map-get? user-streaks { user: user }) err-not-found))
    (nft-id (var-get next-nft-id))
  )
    (asserts! (>= (get current-streak user-streak) streak-days) err-invalid-streak)
    (asserts! (or (is-eq streak-days u7) (is-eq streak-days u30) (is-eq streak-days u100) (is-eq streak-days u365)) err-invalid-streak)
    
    (let ((achievement-type (if (is-eq streak-days u7)
                              "Week Warrior"
                              (if (is-eq streak-days u30)
                                "Month Master"
                                (if (is-eq streak-days u100)
                                  "Century Scholar"
                                  "Year Legend")))))
      (map-set nft-achievements
        { nft-id: nft-id }
        {
          owner: user,
          achievement-type: achievement-type,
          streak-length: streak-days,
          subject: subject,
          minted-at: stacks-block-height,
          transferable: true
        }
      )
      (var-set next-nft-id (+ nft-id u1))
      (ok nft-id)
    )
  )
)

(define-public (earn-subject-badge (subject (string-ascii 50)) (badge-type (string-ascii 50)))
  (let (
    (user tx-sender)
    (subject-data (unwrap! (map-get? subject-stats { user: user, subject: subject }) err-not-found))
  )
    (asserts! (>= (get total-hours subject-data) u50) err-invalid-streak) ;; Require 50+ hours
    
    (map-set user-badges
      { user: user, badge-type: badge-type }
      { earned-at: stacks-block-height, level: (get level subject-data) }
    )
    (ok true)
  )
)

(define-public (verify-practice-log (user principal) (date uint))
  (let ((log (unwrap! (map-get? practice-logs { user: user, date: date }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (map-set practice-logs
      { user: user, date: date }
      (merge log { verified: true })
    )
    (ok true)
  )
)

(define-public (transfer-nft (nft-id uint) (recipient principal))
  (let ((nft (unwrap! (map-get? nft-achievements { nft-id: nft-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get owner nft)) err-unauthorized)
    (asserts! (get transferable nft) err-unauthorized)
    
    (map-set nft-achievements
      { nft-id: nft-id }
      (merge nft { owner: recipient })
    )
    (ok true)
  )
)

(define-read-only (get-practice-log (user principal) (date uint))
  (map-get? practice-logs { user: user, date: date })
)

(define-read-only (get-user-streaks (user principal))
  (map-get? user-streaks { user: user })
)

(define-read-only (get-subject-stats (user principal) (subject (string-ascii 50)))
  (map-get? subject-stats { user: user, subject: subject })
)

(define-read-only (get-nft-achievement (nft-id uint))
  (map-get? nft-achievements { nft-id: nft-id })
)

(define-read-only (get-study-group (group-id uint))
  (map-get? study-groups { group-id: group-id })
)

(define-read-only (get-practice-challenge (challenge-id uint))
  (map-get? practice-challenges { challenge-id: challenge-id })
)

(define-read-only (get-user-badges (user principal) (badge-type (string-ascii 50)))
  (map-get? user-badges { user: user, badge-type: badge-type })
)

(define-read-only (get-leaderboard-stats (user principal))
  (let ((streaks (map-get? user-streaks { user: user })))
    (if (is-some streaks)
      (let ((s (unwrap-panic streaks)))
        {
          total-hours: (get total-hours s),
          current-streak: (get current-streak s),
          longest-streak: (get longest-streak s),
          weekly-hours: (get weekly-hours s),
          monthly-hours: (get monthly-hours s)
        }
      )
      { total-hours: u0, current-streak: u0, longest-streak: u0, weekly-hours: u0, monthly-hours: u0 }
    )
  )
)

(define-read-only (get-platform-stats)
  {
    total-nfts-minted: (var-get next-nft-id),
    total-challenges: (var-get next-challenge-id),
    reward-pool: (var-get practice-reward-pool)
  }
)