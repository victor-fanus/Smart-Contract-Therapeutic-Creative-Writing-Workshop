;; =================================================================
;; Smart Contract Therapeutic Creative Writing Workshop
;; A trauma-informed system for healing through storytelling
;; =================================================================

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-workshop-not-active (err u105))
(define-constant err-insufficient-reputation (err u106))
(define-constant err-feedback-limit-reached (err u107))
(define-constant err-story-not-published (err u108))

;; Data Variables
(define-data-var next-workshop-id uint u1)
(define-data-var next-story-id uint u1)
(define-data-var next-prompt-id uint u1)
(define-data-var next-feedback-id uint u1)

;; Workshop structure for therapeutic writing sessions
(define-map workshops
  { workshop-id: uint }
  {
    facilitator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    cultural-theme: (string-ascii 50),
    trauma-informed-level: uint, ;; 1-5 scale for trauma sensitivity
    is-active: bool,
    created-at: uint,
    max-participants: uint,
    current-participants: uint,
    healing-focus: (string-ascii 100)
  }
)

;; Writing prompts with therapeutic focus
(define-map writing-prompts
  { prompt-id: uint }
  {
    workshop-id: uint,
    creator: principal,
    title: (string-ascii 100),
    prompt-text: (string-ascii 1000),
    therapeutic-category: (string-ascii 50), ;; resilience, healing, identity, etc.
    cultural-context: (string-ascii 100),
    safety-guidelines: (string-ascii 300),
    created-at: uint,
    usage-count: uint
  }
)

;; Stories created by participants
(define-map stories
  { story-id: uint }
  {
    author: principal,
    workshop-id: uint,
    prompt-id: uint,
    title: (string-ascii 100),
    content-hash: (buff 32), ;; IPFS hash for story content
    emotional-tags: (string-ascii 200),
    processing-stage: (string-ascii 50), ;; draft, sharing, published
    is-published: bool,
    created-at: uint,
    updated-at: uint,
    healing-reflection: (string-ascii 500)
  }
)

;; Peer feedback system with empathy focus
(define-map feedback
  { feedback-id: uint }
  {
    story-id: uint,
    reviewer: principal,
    feedback-type: (string-ascii 30), ;; supportive, constructive, celebration
    content-hash: (buff 32), ;; IPFS hash for feedback content
    empathy-rating: uint, ;; 1-10 scale
    healing-insight: (string-ascii 300),
    created-at: uint,
    is-validated: bool
  }
)

;; Participant profiles with healing journey tracking
(define-map participants
  { participant: principal }
  {
    display-name: (string-ascii 50),
    healing-journey-stage: (string-ascii 50),
    cultural-background: (string-ascii 100),
    preferred-themes: (string-ascii 200),
    stories-shared: uint,
    feedback-given: uint,
    community-reputation: uint,
    joined-at: uint,
    last-active: uint,
    healing-milestones: uint
  }
)

;; Workshop participation tracking
(define-map workshop-participants
  { workshop-id: uint, participant: principal }
  { joined-at: uint, contribution-level: uint }
)

;; Community publication tracking
(define-map publications
  { story-id: uint }
  {
    publication-date: uint,
    community-votes: uint,
    healing-impact-score: uint,
    featured: bool,
    cultural-celebration: bool
  }
)

;; Read-only functions

;; Get workshop details
(define-read-only (get-workshop (workshop-id uint))
  (map-get? workshops { workshop-id: workshop-id })
)

;; Get story details
(define-read-only (get-story (story-id uint))
  (map-get? stories { story-id: story-id })
)

;; Get writing prompt
(define-read-only (get-prompt (prompt-id uint))
  (map-get? writing-prompts { prompt-id: prompt-id })
)

;; Get participant profile
(define-read-only (get-participant (participant principal))
  (map-get? participants { participant: participant })
)

;; Get feedback for story
(define-read-only (get-feedback (feedback-id uint))
  (map-get? feedback { feedback-id: feedback-id })
)

;; Check if participant is in workshop
(define-read-only (is-workshop-participant (workshop-id uint) (participant principal))
  (is-some (map-get? workshop-participants { workshop-id: workshop-id, participant: participant }))
)

;; Get publication info
(define-read-only (get-publication-info (story-id uint))
  (map-get? publications { story-id: story-id })
)

;; Calculate healing progress score
(define-read-only (calculate-healing-progress (participant principal))
  (let ((profile (map-get? participants { participant: participant })))
    (match profile
      participant-data
        (+
          (* (get stories-shared participant-data) u10)
          (* (get feedback-given participant-data) u5)
          (* (get healing-milestones participant-data) u20)
        )
      u0
    )
  )
)

;; Public functions

;; Create therapeutic writing workshop
(define-public (create-workshop
    (title (string-ascii 100))
    (description (string-ascii 500))
    (cultural-theme (string-ascii 50))
    (trauma-informed-level uint)
    (max-participants uint)
    (healing-focus (string-ascii 100))
  )
  (let ((workshop-id (var-get next-workshop-id)))
    (asserts! (<= trauma-informed-level u5) err-invalid-input)
    (asserts! (> max-participants u0) err-invalid-input)
    (asserts! (<= (len title) u100) err-invalid-input)

    (map-set workshops
      { workshop-id: workshop-id }
      {
        facilitator: tx-sender,
        title: title,
        description: description,
        cultural-theme: cultural-theme,
        trauma-informed-level: trauma-informed-level,
        is-active: true,
        created-at: stacks-block-height,
        max-participants: max-participants,
        current-participants: u0,
        healing-focus: healing-focus
      }
    )

    (var-set next-workshop-id (+ workshop-id u1))
    (ok workshop-id)
  )
)

;; Register participant with healing journey info
(define-public (register-participant
    (display-name (string-ascii 50))
    (healing-journey-stage (string-ascii 50))
    (cultural-background (string-ascii 100))
    (preferred-themes (string-ascii 200))
  )
  (begin
    (asserts! (<= (len display-name) u50) err-invalid-input)

    (map-set participants
      { participant: tx-sender }
      {
        display-name: display-name,
        healing-journey-stage: healing-journey-stage,
        cultural-background: cultural-background,
        preferred-themes: preferred-themes,
        stories-shared: u0,
        feedback-given: u0,
        community-reputation: u100, ;; Starting reputation
        joined-at: stacks-block-height,
        last-active: stacks-block-height,
        healing-milestones: u0
      }
    )
    (ok true)
  )
)

;; Join workshop
(define-public (join-workshop (workshop-id uint))
  (let ((workshop (unwrap! (map-get? workshops { workshop-id: workshop-id }) err-not-found)))
    (asserts! (get is-active workshop) err-workshop-not-active)
    (asserts! (< (get current-participants workshop) (get max-participants workshop)) err-invalid-input)
    (asserts! (is-none (map-get? workshop-participants { workshop-id: workshop-id, participant: tx-sender })) err-already-exists)
    (asserts! (is-some (map-get? participants { participant: tx-sender })) err-unauthorized)

    (map-set workshop-participants
      { workshop-id: workshop-id, participant: tx-sender }
      { joined-at: stacks-block-height, contribution-level: u0 }
    )

    (map-set workshops
      { workshop-id: workshop-id }
      (merge workshop {
        current-participants: (+ (get current-participants workshop) u1)
      })
    )

    (ok true)
  )
)

;; Create therapeutic writing prompt
(define-public (create-writing-prompt
    (workshop-id uint)
    (title (string-ascii 100))
    (prompt-text (string-ascii 1000))
    (therapeutic-category (string-ascii 50))
    (cultural-context (string-ascii 100))
    (safety-guidelines (string-ascii 300))
  )
  (let ((prompt-id (var-get next-prompt-id))
        (workshop (unwrap! (map-get? workshops { workshop-id: workshop-id }) err-not-found)))

    (asserts! (or (is-eq tx-sender (get facilitator workshop)) (>= (get-reputation tx-sender) u500)) err-unauthorized)
    (asserts! (<= (len title) u100) err-invalid-input)
    (asserts! (<= (len prompt-text) u1000) err-invalid-input)

    (map-set writing-prompts
      { prompt-id: prompt-id }
      {
        workshop-id: workshop-id,
        creator: tx-sender,
        title: title,
        prompt-text: prompt-text,
        therapeutic-category: therapeutic-category,
        cultural-context: cultural-context,
        safety-guidelines: safety-guidelines,
        created-at: stacks-block-height,
        usage-count: u0
      }
    )

    (var-set next-prompt-id (+ prompt-id u1))
    (ok prompt-id)
  )
)

;; Submit story for therapeutic sharing
(define-public (submit-story
    (workshop-id uint)
    (prompt-id uint)
    (title (string-ascii 100))
    (content-hash (buff 32))
    (emotional-tags (string-ascii 200))
    (healing-reflection (string-ascii 500))
  )
  (let ((story-id (var-get next-story-id)))
    (asserts! (is-workshop-participant workshop-id tx-sender) err-unauthorized)
    (asserts! (is-some (map-get? writing-prompts { prompt-id: prompt-id })) err-not-found)
    (asserts! (<= (len title) u100) err-invalid-input)
    (asserts! (<= (len emotional-tags) u200) err-invalid-input)

    (map-set stories
      { story-id: story-id }
      {
        author: tx-sender,
        workshop-id: workshop-id,
        prompt-id: prompt-id,
        title: title,
        content-hash: content-hash,
        emotional-tags: emotional-tags,
        processing-stage: "draft",
        is-published: false,
        created-at: stacks-block-height,
        updated-at: stacks-block-height,
        healing-reflection: healing-reflection
      }
    )

    ;; Update participant stats
    (try! (update-participant-stories tx-sender))

    ;; Update prompt usage
    (try! (update-prompt-usage prompt-id))

    (var-set next-story-id (+ story-id u1))
    (ok story-id)
  )
)

;; Provide empathetic feedback
(define-public (provide-feedback
    (story-id uint)
    (feedback-type (string-ascii 30))
    (content-hash (buff 32))
    (empathy-rating uint)
    (healing-insight (string-ascii 300))
  )
  (let ((feedback-id (var-get next-feedback-id))
        (story (unwrap! (map-get? stories { story-id: story-id }) err-not-found))
        (reviewer-profile (unwrap! (map-get? participants { participant: tx-sender }) err-unauthorized)))

    (asserts! (not (is-eq tx-sender (get author story))) err-unauthorized)
    (asserts! (and (>= empathy-rating u1) (<= empathy-rating u10)) err-invalid-input)
    (asserts! (>= (get community-reputation reviewer-profile) u200) err-insufficient-reputation)
    (asserts! (<= (len healing-insight) u300) err-invalid-input)

    (map-set feedback
      { feedback-id: feedback-id }
      {
        story-id: story-id,
        reviewer: tx-sender,
        feedback-type: feedback-type,
        content-hash: content-hash,
        empathy-rating: empathy-rating,
        healing-insight: healing-insight,
        created-at: stacks-block-height,
        is-validated: false
      }
    )

    ;; Update reviewer's feedback count
    (try! (update-participant-feedback tx-sender))

    (var-set next-feedback-id (+ feedback-id u1))
    (ok feedback-id)
  )
)

;; Publish story to community
(define-public (publish-story (story-id uint))
  (let ((story (unwrap! (map-get? stories { story-id: story-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get author story)) err-unauthorized)
    (asserts! (not (get is-published story)) err-already-exists)

    (map-set stories
      { story-id: story-id }
      (merge story {
        is-published: true,
        processing-stage: "published",
        updated-at: stacks-block-height
      })
    )

    (map-set publications
      { story-id: story-id }
      {
        publication-date: stacks-block-height,
        community-votes: u0,
        healing-impact-score: u0,
        featured: false,
        cultural-celebration: false
      }
    )

    (ok true)
  )
)

;; Vote for healing impact
(define-public (vote-healing-impact (story-id uint))
  (let ((publication (unwrap! (map-get? publications { story-id: story-id }) err-story-not-published))
        (voter-profile (unwrap! (map-get? participants { participant: tx-sender }) err-unauthorized)))

    (asserts! (>= (get community-reputation voter-profile) u300) err-insufficient-reputation)

    (map-set publications
      { story-id: story-id }
      (merge publication {
        community-votes: (+ (get community-votes publication) u1),
        healing-impact-score: (+ (get healing-impact-score publication) (get community-reputation voter-profile))
      })
    )

    (ok true)
  )
)

;; Mark healing milestone
(define-public (mark-healing-milestone (participant principal))
  (let ((profile (unwrap! (map-get? participants { participant: participant }) err-not-found)))
    (asserts! (or (is-eq tx-sender participant) (is-eq tx-sender contract-owner)) err-unauthorized)

    (map-set participants
      { participant: participant }
      (merge profile {
        healing-milestones: (+ (get healing-milestones profile) u1),
        community-reputation: (+ (get community-reputation profile) u50)
      })
    )

    (ok true)
  )
)

;; Helper functions

;; Get participant reputation
(define-private (get-reputation (participant principal))
  (default-to u0
    (get community-reputation (map-get? participants { participant: participant }))
  )
)

;; Update participant story count
(define-private (update-participant-stories (participant principal))
  (let ((profile (unwrap! (map-get? participants { participant: participant }) err-not-found)))
    (map-set participants
      { participant: participant }
      (merge profile {
        stories-shared: (+ (get stories-shared profile) u1),
        last-active: stacks-block-height,
        community-reputation: (+ (get community-reputation profile) u25)
      })
    )
    (ok true)
  )
)

;; Update participant feedback count
(define-private (update-participant-feedback (participant principal))
  (let ((profile (unwrap! (map-get? participants { participant: participant }) err-not-found)))
    (map-set participants
      { participant: participant }
      (merge profile {
        feedback-given: (+ (get feedback-given profile) u1),
        last-active: stacks-block-height,
        community-reputation: (+ (get community-reputation profile) u15)
      })
    )
    (ok true)
  )
)

;; Update prompt usage count
(define-private (update-prompt-usage (prompt-id uint))
  (let ((prompt (unwrap! (map-get? writing-prompts { prompt-id: prompt-id }) err-not-found)))
    (map-set writing-prompts
      { prompt-id: prompt-id }
      (merge prompt {
        usage-count: (+ (get usage-count prompt) u1)
      })
    )
    (ok true)
  )
)

;; Administrative functions (contract owner only)

;; Feature story for cultural celebration
(define-public (feature-story (story-id uint) (cultural-celebration bool))
  (let ((publication (unwrap! (map-get? publications { story-id: story-id }) err-story-not-published)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    (map-set publications
      { story-id: story-id }
      (merge publication {
        featured: true,
        cultural-celebration: cultural-celebration
      })
    )

    (ok true)
  )
)

;; Validate feedback quality
(define-public (validate-feedback (feedback-id uint))
  (let ((feedback-data (unwrap! (map-get? feedback { feedback-id: feedback-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    (map-set feedback
      { feedback-id: feedback-id }
      (merge feedback-data { is-validated: true })
    )

    ;; Reward validated feedback provider
    (let ((reviewer-profile (unwrap! (map-get? participants { participant: (get reviewer feedback-data) }) err-not-found)))
      (map-set participants
        { participant: (get reviewer feedback-data) }
        (merge reviewer-profile {
          community-reputation: (+ (get community-reputation reviewer-profile) u30)
        })
      )
    )

    (ok true)
  )
)

;; Deactivate workshop
(define-public (deactivate-workshop (workshop-id uint))
  (let ((workshop (unwrap! (map-get? workshops { workshop-id: workshop-id }) err-not-found)))
    (asserts! (or (is-eq tx-sender (get facilitator workshop)) (is-eq tx-sender contract-owner)) err-owner-only)

    (map-set workshops
      { workshop-id: workshop-id }
      (merge workshop { is-active: false })
    )

    (ok true)
  )
)
