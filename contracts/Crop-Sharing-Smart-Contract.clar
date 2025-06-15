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
