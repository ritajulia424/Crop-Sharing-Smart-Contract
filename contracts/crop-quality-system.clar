(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_INVALID_RATING (err u201))
(define-constant ERR_ALREADY_RATED (err u202))
(define-constant ERR_CROP_NOT_FOUND (err u203))
(define-constant ERR_INSUFFICIENT_VALIDATORS (err u204))

(define-data-var min-validators uint u3)
(define-data-var rating-window uint u200)

(define-map quality-ratings
  { crop-id: uint, validator: principal }
  { rating: uint, timestamp: uint }
)

(define-map crop-quality-summary
  uint
  {
    total-ratings: uint,
    rating-sum: uint,
    average-rating: uint,
    is-finalized: bool
  }
)

(define-map farmer-reputation
  principal
  {
    total-crops: uint,
    quality-sum: uint,
    reputation-score: uint,
    last-updated: uint
  }
)

(define-map authorized-validators
  principal
  { is-active: bool, validation-count: uint }
)

(define-read-only (get-crop-quality (crop-id uint))
  (map-get? crop-quality-summary crop-id)
)

(define-read-only (get-farmer-reputation (farmer principal))
  (default-to 
    { total-crops: u0, quality-sum: u0, reputation-score: u0, last-updated: u0 }
    (map-get? farmer-reputation farmer))
)

(define-read-only (get-validator-rating (crop-id uint) (validator principal))
  (map-get? quality-ratings { crop-id: crop-id, validator: validator })
)

(define-read-only (is-validator-authorized (validator principal))
  (default-to { is-active: false, validation-count: u0 }
    (map-get? authorized-validators validator))
)

(define-public (authorize-validator (validator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-validators validator { is-active: true, validation-count: u0 })
    (ok true)
  )
)

(define-public (submit-quality-rating (crop-id uint) (rating uint))
  (let ((validator-status (is-validator-authorized tx-sender))
        (existing-rating (map-get? quality-ratings { crop-id: crop-id, validator: tx-sender })))
    (asserts! (get is-active validator-status) ERR_UNAUTHORIZED)
    (asserts! (and (>= rating u1) (<= rating u10)) ERR_INVALID_RATING)
    (asserts! (is-none existing-rating) ERR_ALREADY_RATED)
    (map-set quality-ratings 
      { crop-id: crop-id, validator: tx-sender }
      { rating: rating, timestamp: stacks-block-height })
    (try! (update-crop-quality-summary crop-id rating))
    (map-set authorized-validators tx-sender 
      (merge validator-status { validation-count: (+ (get validation-count validator-status) u1) }))
    (ok true)
  )
)

(define-private (update-crop-quality-summary (crop-id uint) (new-rating uint))
  (let ((current-summary (default-to 
          { total-ratings: u0, rating-sum: u0, average-rating: u0, is-finalized: false }
          (map-get? crop-quality-summary crop-id)))
        (new-total (+ (get total-ratings current-summary) u1))
        (new-sum (+ (get rating-sum current-summary) new-rating))
        (new-average (/ new-sum new-total)))
    (map-set crop-quality-summary crop-id {
      total-ratings: new-total,
      rating-sum: new-sum,
      average-rating: new-average,
      is-finalized: (>= new-total (var-get min-validators))
    })
    (if (>= new-total (var-get min-validators))
      (begin
        (try! (update-farmer-reputation crop-id new-average))
        (ok true))
      (ok true))
  )
)

(define-private (update-farmer-reputation (crop-id uint) (quality-score uint))
  (let ((crop-data (unwrap! (get-crop-farmer crop-id) ERR_CROP_NOT_FOUND))
        (farmer-principal (get farmer crop-data))
        (current-rep (get-farmer-reputation farmer-principal))
        (new-total-crops (+ (get total-crops current-rep) u1))
        (new-quality-sum (+ (get quality-sum current-rep) quality-score))
        (new-reputation (/ new-quality-sum new-total-crops)))
    (map-set farmer-reputation farmer-principal {
      total-crops: new-total-crops,
      quality-sum: new-quality-sum,
      reputation-score: new-reputation,
      last-updated: stacks-block-height
    })
    (ok true)
  )
)

(define-read-only (get-crop-farmer (crop-id uint))
  (contract-call? .Crop-Sharing-Smart-Contract get-crop crop-id)
)