(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_CROP_NOT_FOUND (err u301))
(define-constant ERR_INVALID_MILESTONE (err u302))
(define-constant ERR_MILESTONE_EXISTS (err u303))
(define-constant ERR_CROP_COMPLETED (err u304))

(define-map crop-milestones
  { crop-id: uint, milestone-type: (string-ascii 20) }
  { 
    recorded-at: uint,
    notes: (string-ascii 200),
    recorded-by: principal,
    is-on-schedule: bool
  }
)

(define-map crop-performance-metrics
  uint
  {
    total-milestones: uint,
    on-time-milestones: uint,
    performance-score: uint,
    last-updated: uint
  }
)

(define-read-only (get-milestone (crop-id uint) (milestone-type (string-ascii 20)))
  (map-get? crop-milestones { crop-id: crop-id, milestone-type: milestone-type })
)

(define-read-only (get-crop-performance (crop-id uint))
  (default-to 
    { total-milestones: u0, on-time-milestones: u0, performance-score: u100, last-updated: u0 }
    (map-get? crop-performance-metrics crop-id))
)

(define-read-only (calculate-performance-score (total-milestones uint) (on-time-milestones uint))
  (if (> total-milestones u0)
    (/ (* on-time-milestones u100) total-milestones)
    u100)
)

(define-public (record-milestone 
  (crop-id uint) 
  (milestone-type (string-ascii 20)) 
  (notes (string-ascii 200))
  (is-on-schedule bool))
  (let ((crop-data (unwrap! (contract-call? .Crop-Sharing-Smart-Contract get-crop crop-id) ERR_CROP_NOT_FOUND))
        (current-metrics (get-crop-performance crop-id)))
    (asserts! (is-eq tx-sender (get farmer crop-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get is-harvested crop-data)) ERR_CROP_COMPLETED)
    (asserts! (is-none (get-milestone crop-id milestone-type)) ERR_MILESTONE_EXISTS)
    (map-set crop-milestones 
      { crop-id: crop-id, milestone-type: milestone-type }
      {
        recorded-at: stacks-block-height,
        notes: notes,
        recorded-by: tx-sender,
        is-on-schedule: is-on-schedule
      })
    (let ((new-total (+ (get total-milestones current-metrics) u1))
          (new-on-time (+ (get on-time-milestones current-metrics) (if is-on-schedule u1 u0))))
      (map-set crop-performance-metrics crop-id {
        total-milestones: new-total,
        on-time-milestones: new-on-time,
        performance-score: (calculate-performance-score new-total new-on-time),
        last-updated: stacks-block-height
      }))
    (ok true)
  )
)

(define-read-only (get-crop-timeline (crop-id uint))
  (let ((crop-data (unwrap! (contract-call? .Crop-Sharing-Smart-Contract get-crop crop-id) ERR_CROP_NOT_FOUND))
        (performance (get-crop-performance crop-id)))
    (ok {
      farmer: (get farmer crop-data),
      crop-type: (get crop-type crop-data),
      created-at: (get created-at crop-data),
      is-harvested: (get is-harvested crop-data),
      performance-score: (get performance-score performance),
      milestone-count: (get total-milestones performance)
    })
  )
)
