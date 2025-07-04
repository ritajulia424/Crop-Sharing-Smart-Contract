(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INVALID_PERCENTAGE (err u102))
(define-constant ERR_CROP_NOT_FOUND (err u103))
(define-constant ERR_CROP_ALREADY_HARVESTED (err u104))
(define-constant ERR_CROP_NOT_HARVESTED (err u105))
(define-constant ERR_ALREADY_CLAIMED (err u106))
(define-constant ERR_INSUFFICIENT_BALANCE (err u107))
(define-constant ERR_INVALID_PARTICIPANT (err u108))
(define-constant ERR_INSURANCE_NOT_FOUND (err u109))
(define-constant ERR_INSURANCE_ALREADY_EXISTS (err u110))
(define-constant ERR_CLAIM_PERIOD_EXPIRED (err u111))
(define-constant ERR_INSUFFICIENT_INSURANCE_POOL (err u112))
(define-constant ERR_INVALID_INSURANCE_AMOUNT (err u113))

(define-data-var insurance-pool uint u0)
(define-data-var insurance-fee-rate uint u5)

(define-data-var crop-counter uint u0)

(define-map crops
  uint
  {
    farmer: principal,
    crop-type: (string-ascii 50),
    total-investment: uint,
    harvest-revenue: uint,
    farmer-percentage: uint,
    investor-percentage: uint,
    coop-percentage: uint,
    is-harvested: bool,
    created-at: uint
  }
)

(define-map crop-investors
  { crop-id: uint, investor: principal }
  { amount-invested: uint, claimed: bool }
)

(define-map crop-coops
  { crop-id: uint, coop: principal }
  { service-fee: uint, claimed: bool }
)

(define-map user-investments
  principal
  { total-invested: uint, active-crops: uint }
)

(define-map user-earnings
  principal
  { total-earned: uint, pending-claims: uint }
)

(define-read-only (get-crop (crop-id uint))
  (map-get? crops crop-id)
)

(define-read-only (get-crop-investor (crop-id uint) (investor principal))
  (map-get? crop-investors { crop-id: crop-id, investor: investor })
)

(define-read-only (get-crop-coop (crop-id uint) (coop principal))
  (map-get? crop-coops { crop-id: crop-id, coop: coop })
)

(define-read-only (get-user-investments (user principal))
  (default-to { total-invested: u0, active-crops: u0 }
    (map-get? user-investments user))
)

(define-read-only (get-user-earnings (user principal))
  (default-to { total-earned: u0, pending-claims: u0 }
    (map-get? user-earnings user))
)

(define-read-only (get-crop-counter)
  (var-get crop-counter)
)

(define-public (create-crop 
  (crop-type (string-ascii 50))
  (farmer-percentage uint)
  (investor-percentage uint)
  (coop-percentage uint))
  (let ((crop-id (+ (var-get crop-counter) u1)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (+ farmer-percentage investor-percentage coop-percentage) u100) ERR_INVALID_PERCENTAGE)
    (map-set crops crop-id {
      farmer: tx-sender,
      crop-type: crop-type,
      total-investment: u0,
      harvest-revenue: u0,
      farmer-percentage: farmer-percentage,
      investor-percentage: investor-percentage,
      coop-percentage: coop-percentage,
      is-harvested: false,
      created-at: stacks-block-height
    })
    (var-set crop-counter crop-id)
    (ok crop-id)
  )
)

(define-public (invest-in-crop (crop-id uint) (amount uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND))
        (existing-investment (default-to { amount-invested: u0, claimed: false }
          (map-get? crop-investors { crop-id: crop-id, investor: tx-sender })))
        (user-inv (get-user-investments tx-sender)))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-harvested crop-data)) ERR_CROP_ALREADY_HARVESTED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set crop-investors 
      { crop-id: crop-id, investor: tx-sender }
      { amount-invested: (+ (get amount-invested existing-investment) amount), claimed: false })
    (map-set crops crop-id 
      (merge crop-data { total-investment: (+ (get total-investment crop-data) amount) }))
    (map-set user-investments tx-sender {
      total-invested: (+ (get total-invested user-inv) amount),
      active-crops: (if (is-eq (get amount-invested existing-investment) u0)
        (+ (get active-crops user-inv) u1)
        (get active-crops user-inv))
    })
    (ok true)
  )
)

(define-public (register-coop-service (crop-id uint) (service-fee uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND)))
    (asserts! (> service-fee u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-harvested crop-data)) ERR_CROP_ALREADY_HARVESTED)
    (try! (stx-transfer? service-fee tx-sender (as-contract tx-sender)))
    (map-set crop-coops 
      { crop-id: crop-id, coop: tx-sender }
      { service-fee: service-fee, claimed: false })
    (ok true)
  )
)

(define-public (record-harvest (crop-id uint) (revenue uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> revenue u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-harvested crop-data)) ERR_CROP_ALREADY_HARVESTED)
    (try! (stx-transfer? revenue tx-sender (as-contract tx-sender)))
    (map-set crops crop-id 
      (merge crop-data { 
        harvest-revenue: revenue, 
        is-harvested: true 
      }))
    (ok true)
  )
)

(define-public (claim-farmer-share (crop-id uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND))
        (farmer-share (/ (* (get harvest-revenue crop-data) (get farmer-percentage crop-data)) u100))
        (user-earn (get-user-earnings tx-sender)))
    (asserts! (is-eq tx-sender (get farmer crop-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-harvested crop-data) ERR_CROP_NOT_HARVESTED)
    (asserts! (> farmer-share u0) ERR_INVALID_AMOUNT)
    (try! (as-contract (stx-transfer? farmer-share tx-sender (get farmer crop-data))))
    (map-set user-earnings tx-sender {
      total-earned: (+ (get total-earned user-earn) farmer-share),
      pending-claims: (get pending-claims user-earn)
    })
    (ok farmer-share)
  )
)

(define-public (claim-investor-share (crop-id uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND))
        (investor-data (unwrap! (map-get? crop-investors { crop-id: crop-id, investor: tx-sender }) ERR_INVALID_PARTICIPANT))
        (total-investor-pool (/ (* (get harvest-revenue crop-data) (get investor-percentage crop-data)) u100))
        (investor-share (if (> (get total-investment crop-data) u0)
          (/ (* total-investor-pool (get amount-invested investor-data)) (get total-investment crop-data))
          u0))
        (user-earn (get-user-earnings tx-sender)))
    (asserts! (get is-harvested crop-data) ERR_CROP_NOT_HARVESTED)
    (asserts! (not (get claimed investor-data)) ERR_ALREADY_CLAIMED)
    (asserts! (> investor-share u0) ERR_INVALID_AMOUNT)
    (map-set crop-investors 
      { crop-id: crop-id, investor: tx-sender }
      (merge investor-data { claimed: true }))
    (try! (as-contract (stx-transfer? investor-share tx-sender tx-sender)))
    (map-set user-earnings tx-sender {
      total-earned: (+ (get total-earned user-earn) investor-share),
      pending-claims: (get pending-claims user-earn)
    })
    (ok investor-share)
  )
)

(define-public (claim-coop-share (crop-id uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND))
        (coop-data (unwrap! (map-get? crop-coops { crop-id: crop-id, coop: tx-sender }) ERR_INVALID_PARTICIPANT))
        (coop-share (/ (* (get harvest-revenue crop-data) (get coop-percentage crop-data)) u100))
        (user-earn (get-user-earnings tx-sender)))
    (asserts! (get is-harvested crop-data) ERR_CROP_NOT_HARVESTED)
    (asserts! (not (get claimed coop-data)) ERR_ALREADY_CLAIMED)
    (asserts! (> coop-share u0) ERR_INVALID_AMOUNT)
    (map-set crop-coops 
      { crop-id: crop-id, coop: tx-sender }
      (merge coop-data { claimed: true }))
    (try! (as-contract (stx-transfer? coop-share tx-sender tx-sender)))
    (map-set user-earnings tx-sender {
      total-earned: (+ (get total-earned user-earn) coop-share),
      pending-claims: (get pending-claims user-earn)
    })
    (ok coop-share)
  )
)

(define-read-only (calculate-potential-returns (crop-id uint) (investor principal))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND))
        (investor-data (unwrap! (map-get? crop-investors { crop-id: crop-id, investor: investor }) ERR_INVALID_PARTICIPANT)))
    (if (and (get is-harvested crop-data) (> (get total-investment crop-data) u0))
      (let ((total-investor-pool (/ (* (get harvest-revenue crop-data) (get investor-percentage crop-data)) u100)))
        (ok (/ (* total-investor-pool (get amount-invested investor-data)) (get total-investment crop-data))))
      (ok u0))
  )
)

(define-read-only (get-crop-summary (crop-id uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND)))
    (ok {
      crop-type: (get crop-type crop-data),
      total-investment: (get total-investment crop-data),
      harvest-revenue: (get harvest-revenue crop-data),
      is-harvested: (get is-harvested crop-data),
      farmer-share: (/ (* (get harvest-revenue crop-data) (get farmer-percentage crop-data)) u100),
      investor-pool: (/ (* (get harvest-revenue crop-data) (get investor-percentage crop-data)) u100),
      coop-pool: (/ (* (get harvest-revenue crop-data) (get coop-percentage crop-data)) u100)
    })
  )
)


(define-map crop-insurance
  uint
  {
    coverage-amount: uint,
    premium-paid: uint,
    claim-deadline: uint,
    is-claimed: bool,
    purchaser: principal
  }
)

(define-map insurance-contributors
  principal
  { contributed-amount: uint, share-percentage: uint }
)

(define-read-only (get-insurance-pool)
  (var-get insurance-pool)
)

(define-read-only (get-crop-insurance (crop-id uint))
  (map-get? crop-insurance crop-id)
)

(define-read-only (get-insurance-contributor (contributor principal))
  (map-get? insurance-contributors contributor)
)

(define-read-only (calculate-insurance-premium (coverage-amount uint))
  (/ (* coverage-amount (var-get insurance-fee-rate)) u100)
)

(define-public (contribute-to-insurance-pool (amount uint))
  (let ((current-pool (var-get insurance-pool))
        (existing-contrib (default-to { contributed-amount: u0, share-percentage: u0 }
          (map-get? insurance-contributors tx-sender))))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let ((new-pool (+ current-pool amount))
          (new-contrib-amount (+ (get contributed-amount existing-contrib) amount)))
      (var-set insurance-pool new-pool)
      (map-set insurance-contributors tx-sender {
        contributed-amount: new-contrib-amount,
        share-percentage: (/ (* new-contrib-amount u100) new-pool)
      })
      (ok true)
    )
  )
)

(define-public (purchase-crop-insurance (crop-id uint) (coverage-amount uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND))
        (premium (calculate-insurance-premium coverage-amount)))
    (asserts! (not (get is-harvested crop-data)) ERR_CROP_ALREADY_HARVESTED)
    (asserts! (> coverage-amount u0) ERR_INVALID_INSURANCE_AMOUNT)
    (asserts! (is-none (map-get? crop-insurance crop-id)) ERR_INSURANCE_ALREADY_EXISTS)
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    (map-set crop-insurance crop-id {
      coverage-amount: coverage-amount,
      premium-paid: premium,
      claim-deadline: (+ stacks-block-height u1000),
      is-claimed: false,
      purchaser: tx-sender
    })
    (ok true)
  )
)

(define-public (claim-crop-insurance (crop-id uint))
  (let ((crop-data (unwrap! (map-get? crops crop-id) ERR_CROP_NOT_FOUND))
        (insurance-data (unwrap! (map-get? crop-insurance crop-id) ERR_INSURANCE_NOT_FOUND))
        (current-pool (var-get insurance-pool)))
    (asserts! (is-eq tx-sender (get purchaser insurance-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-harvested crop-data) ERR_CROP_NOT_HARVESTED)
    (asserts! (<= stacks-block-height (get claim-deadline insurance-data)) ERR_CLAIM_PERIOD_EXPIRED)
    (asserts! (not (get is-claimed insurance-data)) ERR_ALREADY_CLAIMED)
    (asserts! (<= (get harvest-revenue crop-data) (/ (get coverage-amount insurance-data) u4)) ERR_INVALID_AMOUNT)
    (asserts! (>= current-pool (get coverage-amount insurance-data)) ERR_INSUFFICIENT_INSURANCE_POOL)
    (try! (as-contract (stx-transfer? (get coverage-amount insurance-data) tx-sender (get purchaser insurance-data))))
    (var-set insurance-pool (- current-pool (get coverage-amount insurance-data)))
    (map-set crop-insurance crop-id (merge insurance-data { is-claimed: true }))
    (ok (get coverage-amount insurance-data))
  )
)