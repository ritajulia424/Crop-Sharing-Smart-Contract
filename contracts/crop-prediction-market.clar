(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_CROP_NOT_FOUND (err u501))
(define-constant ERR_ALREADY_PREDICTED (err u502))
(define-constant ERR_PREDICTION_CLOSED (err u503))
(define-constant ERR_INVALID_PREDICTION (err u504))
(define-constant ERR_NO_HARVEST_DATA (err u505))
(define-constant ERR_ALREADY_CLAIMED (err u506))
(define-constant ERR_INSUFFICIENT_POOL (err u507))

(define-data-var prediction-fee uint u10000)
(define-data-var accuracy-threshold uint u10)

(define-map crop-prediction-pools
  uint
  {
    total-predictions: uint,
    total-pool: uint,
    average-prediction: uint,
    prediction-sum: uint,
    is-resolved: bool
  }
)

(define-map user-predictions
  { crop-id: uint, predictor: principal }
  {
    predicted-yield: uint,
    predicted-at: uint,
    claimed: bool
  }
)

(define-map predictor-stats
  principal
  {
    total-predictions: uint,
    correct-predictions: uint,
    total-rewards: uint,
    accuracy-rate: uint
  }
)

(define-read-only (get-prediction-pool (crop-id uint))
  (default-to 
    { total-predictions: u0, total-pool: u0, average-prediction: u0, prediction-sum: u0, is-resolved: false }
    (map-get? crop-prediction-pools crop-id))
)

(define-read-only (get-user-prediction (crop-id uint) (predictor principal))
  (map-get? user-predictions { crop-id: crop-id, predictor: predictor })
)

(define-read-only (get-predictor-stats (predictor principal))
  (default-to 
    { total-predictions: u0, correct-predictions: u0, total-rewards: u0, accuracy-rate: u0 }
    (map-get? predictor-stats predictor))
)

(define-public (predict-crop-yield (crop-id uint) (predicted-yield uint))
  (let ((crop-data (unwrap! (contract-call? .Crop-Sharing-Smart-Contract get-crop crop-id) ERR_CROP_NOT_FOUND))
        (pool-data (get-prediction-pool crop-id))
        (fee (var-get prediction-fee)))
    (asserts! (not (get is-harvested crop-data)) ERR_PREDICTION_CLOSED)
    (asserts! (> predicted-yield u0) ERR_INVALID_PREDICTION)
    (asserts! (is-none (get-user-prediction crop-id tx-sender)) ERR_ALREADY_PREDICTED)
    (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
    (map-set user-predictions 
      { crop-id: crop-id, predictor: tx-sender }
      { predicted-yield: predicted-yield, predicted-at: stacks-block-height, claimed: false })
    (let ((new-total (+ (get total-predictions pool-data) u1))
          (new-sum (+ (get prediction-sum pool-data) predicted-yield))
          (new-pool (+ (get total-pool pool-data) fee)))
      (map-set crop-prediction-pools crop-id {
        total-predictions: new-total,
        total-pool: new-pool,
        average-prediction: (/ new-sum new-total),
        prediction-sum: new-sum,
        is-resolved: false
      }))
    (ok true)
  )
)

(define-public (claim-prediction-reward (crop-id uint))
  (let ((crop-data (unwrap! (contract-call? .Crop-Sharing-Smart-Contract get-crop crop-id) ERR_CROP_NOT_FOUND))
        (prediction (unwrap! (get-user-prediction crop-id tx-sender) ERR_INVALID_PREDICTION))
        (pool-data (get-prediction-pool crop-id))
        (actual-yield (get harvest-revenue crop-data))
        (predictor-data (get-predictor-stats tx-sender)))
    (asserts! (get is-harvested crop-data) ERR_NO_HARVEST_DATA)
    (asserts! (not (get claimed prediction)) ERR_ALREADY_CLAIMED)
    (let ((prediction-error (if (> (get predicted-yield prediction) actual-yield)
                              (- (get predicted-yield prediction) actual-yield)
                              (- actual-yield (get predicted-yield prediction))))
          (error-percentage (if (> actual-yield u0) (/ (* prediction-error u100) actual-yield) u100)))
      (map-set user-predictions 
        { crop-id: crop-id, predictor: tx-sender }
        (merge prediction { claimed: true }))
      (if (<= error-percentage (var-get accuracy-threshold))
        (let ((reward (/ (get total-pool pool-data) (get total-predictions pool-data))))
          (asserts! (>= (get total-pool pool-data) reward) ERR_INSUFFICIENT_POOL)
          (try! (as-contract (stx-transfer? reward tx-sender tx-sender)))
          (map-set predictor-stats tx-sender {
            total-predictions: (+ (get total-predictions predictor-data) u1),
            correct-predictions: (+ (get correct-predictions predictor-data) u1),
            total-rewards: (+ (get total-rewards predictor-data) reward),
            accuracy-rate: (/ (* (+ (get correct-predictions predictor-data) u1) u100) 
                             (+ (get total-predictions predictor-data) u1))
          })
          (ok reward))
        (begin
          (map-set predictor-stats tx-sender {
            total-predictions: (+ (get total-predictions predictor-data) u1),
            correct-predictions: (get correct-predictions predictor-data),
            total-rewards: (get total-rewards predictor-data),
            accuracy-rate: (/ (* (get correct-predictions predictor-data) u100) 
                             (+ (get total-predictions predictor-data) u1))
          })
          (ok u0)))
    )
  )
)
