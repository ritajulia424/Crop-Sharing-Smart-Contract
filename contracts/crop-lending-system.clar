(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_CROP_NOT_FOUND (err u401))
(define-constant ERR_LOAN_NOT_FOUND (err u402))
(define-constant ERR_LOAN_ALREADY_EXISTS (err u403))
(define-constant ERR_INVALID_AMOUNT (err u404))
(define-constant ERR_LOAN_ALREADY_FUNDED (err u405))
(define-constant ERR_LOAN_NOT_FUNDED (err u406))
(define-constant ERR_CROP_HARVESTED (err u407))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u408))
(define-constant ERR_LOAN_ALREADY_REPAID (err u409))

(define-data-var loan-counter uint u0)
(define-data-var default-interest-rate uint u15)
(define-data-var max-ltv-ratio uint u60)

(define-map crop-loans
  uint
  {
    crop-id: uint,
    borrower: principal,
    lender: (optional principal),
    loan-amount: uint,
    interest-rate: uint,
    total-repayment: uint,
    is-funded: bool,
    is-repaid: bool,
    requested-at: uint,
    funded-at: (optional uint)
  }
)

(define-map lender-portfolio
  principal
  {
    total-lent: uint,
    total-interest-earned: uint,
    active-loans: uint,
    completed-loans: uint
  }
)

(define-read-only (get-loan (loan-id uint))
  (map-get? crop-loans loan-id)
)

(define-read-only (get-lender-stats (lender principal))
  (default-to 
    { total-lent: u0, total-interest-earned: u0, active-loans: u0, completed-loans: u0 }
    (map-get? lender-portfolio lender))
)

(define-read-only (calculate-max-loan (crop-id uint))
  (let ((crop-data (unwrap! (contract-call? .Crop-Sharing-Smart-Contract get-crop crop-id) ERR_CROP_NOT_FOUND)))
    (ok (/ (* (get total-investment crop-data) (var-get max-ltv-ratio)) u100)))
)

(define-public (request-crop-loan (crop-id uint) (loan-amount uint))
  (let ((crop-data (unwrap! (contract-call? .Crop-Sharing-Smart-Contract get-crop crop-id) ERR_CROP_NOT_FOUND))
        (max-loan (/ (* (get total-investment crop-data) (var-get max-ltv-ratio)) u100))
        (loan-id (+ (var-get loan-counter) u1))
        (interest-rate (var-get default-interest-rate))
        (interest-amount (/ (* loan-amount interest-rate) u100))
        (total-repayment (+ loan-amount interest-amount)))
    (asserts! (is-eq tx-sender (get farmer crop-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get is-harvested crop-data)) ERR_CROP_HARVESTED)
    (asserts! (> loan-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= loan-amount max-loan) ERR_INSUFFICIENT_COLLATERAL)
    (map-set crop-loans loan-id {
      crop-id: crop-id,
      borrower: tx-sender,
      lender: none,
      loan-amount: loan-amount,
      interest-rate: interest-rate,
      total-repayment: total-repayment,
      is-funded: false,
      is-repaid: false,
      requested-at: stacks-block-height,
      funded-at: none
    })
    (var-set loan-counter loan-id)
    (ok loan-id)
  )
)

(define-public (fund-loan (loan-id uint))
  (let ((loan-data (unwrap! (map-get? crop-loans loan-id) ERR_LOAN_NOT_FOUND)))
    (asserts! (not (get is-funded loan-data)) ERR_LOAN_ALREADY_FUNDED)
    (try! (stx-transfer? (get loan-amount loan-data) tx-sender (get borrower loan-data)))
    (map-set crop-loans loan-id (merge loan-data {
      lender: (some tx-sender),
      is-funded: true,
      funded-at: (some stacks-block-height)
    }))
    (let ((lender-stats (get-lender-stats tx-sender)))
      (map-set lender-portfolio tx-sender {
        total-lent: (+ (get total-lent lender-stats) (get loan-amount loan-data)),
        total-interest-earned: (get total-interest-earned lender-stats),
        active-loans: (+ (get active-loans lender-stats) u1),
        completed-loans: (get completed-loans lender-stats)
      }))
    (ok true)
  )
)

(define-public (repay-loan-from-harvest (loan-id uint))
  (let ((loan-data (unwrap! (map-get? crop-loans loan-id) ERR_LOAN_NOT_FOUND))
        (crop-data (unwrap! (contract-call? .Crop-Sharing-Smart-Contract get-crop (get crop-id loan-data)) ERR_CROP_NOT_FOUND))
        (lender-principal (unwrap! (get lender loan-data) ERR_LOAN_NOT_FUNDED)))
    (asserts! (get is-funded loan-data) ERR_LOAN_NOT_FUNDED)
    (asserts! (not (get is-repaid loan-data)) ERR_LOAN_ALREADY_REPAID)
    (asserts! (get is-harvested crop-data) ERR_CROP_NOT_FOUND)
    (asserts! (is-eq tx-sender (get borrower loan-data)) ERR_UNAUTHORIZED)
    (try! (stx-transfer? (get total-repayment loan-data) tx-sender lender-principal))
    (map-set crop-loans loan-id (merge loan-data { is-repaid: true }))
    (let ((lender-stats (get-lender-stats lender-principal))
          (interest-earned (- (get total-repayment loan-data) (get loan-amount loan-data))))
      (map-set lender-portfolio lender-principal {
        total-lent: (get total-lent lender-stats),
        total-interest-earned: (+ (get total-interest-earned lender-stats) interest-earned),
        active-loans: (- (get active-loans lender-stats) u1),
        completed-loans: (+ (get completed-loans lender-stats) u1)
      }))
    (ok (get total-repayment loan-data))
  )
)
