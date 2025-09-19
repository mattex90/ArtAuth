;; ArtAuth - A decentralized art authentication and provenance tracking platform
;; Enables transparent artwork verification from artist to collector

;; Data storage
(define-map artist-profiles principal {
  active: bool,
  mediums: (list 10 uint),
  authenticity-score: uint,
  last-creation: uint,
  artwork-count: uint
})

(define-map artwork-records uint {
  creator: principal,
  editions: uint,
  authenticity-level: uint,
  active: bool,
  medium-type: uint,
  total-verifications: uint,
  created-at: uint
})

(define-map authentication-logs {authenticator: principal, artwork-id: uint} {
  timestamp: uint,
  verified: bool
})

(define-map medium-types uint (string-ascii 64))

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_PARAMS (err u101))
(define-constant ERR_ARTIST_NOT_FOUND (err u102))
(define-constant ERR_ARTWORK_NOT_FOUND (err u103))
(define-constant ERR_INSUFFICIENT_EDITIONS (err u104))
(define-constant ERR_ALREADY_REGISTERED (err u105))
(define-constant ERR_ALREADY_AUTHENTICATED (err u106))
(define-constant ERR_INVALID_PRINCIPAL (err u107))
(define-constant ERR_INVALID_VALUE (err u108))
(define-constant ERR_MEDIUM_NOT_FOUND (err u109))

(define-constant ZERO_ADDRESS 'SP000000000000000000002Q6VF78)
(define-constant MIN_AUTHENTICITY_LEVEL u1)
(define-constant MAX_AUTHENTICITY_LEVEL u1000)
(define-constant MIN_ARTWORK_EDITIONS u1000)
(define-constant MAX_MEDIUM_TYPE_ID u1000)

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var next-artwork-id uint u1)
(define-data-var gallery-fee-percent uint u5)
(define-data-var gallery-balance uint u0)

;; Admin functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq new-owner ZERO_ADDRESS)) ERR_INVALID_PRINCIPAL)
    (ok (var-set contract-owner new-owner))))

(define-public (set-gallery-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u20) ERR_INVALID_PARAMS)
    (ok (var-set gallery-fee-percent new-fee))))

(define-public (add-medium-type (type-id uint) (type-name (string-ascii 64)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (> (len type-name) u0) ERR_INVALID_PARAMS)
    (asserts! (< type-id MAX_MEDIUM_TYPE_ID) ERR_INVALID_PARAMS)
    (asserts! (is-none (map-get? medium-types type-id)) ERR_ALREADY_REGISTERED)
    (ok (map-set medium-types type-id type-name))))

;; Artist functions
(define-public (register-artist (mediums (list 10 uint)))
  (begin
    (asserts! (is-none (map-get? artist-profiles tx-sender)) ERR_ALREADY_REGISTERED)
    (asserts! (validate-mediums mediums) ERR_INVALID_PARAMS)
    (ok (map-set artist-profiles tx-sender {
      active: true,
      mediums: mediums,
      authenticity-score: u0,
      last-creation: u0,
      artwork-count: u0
    }))))

(define-public (update-mediums (mediums (list 10 uint)))
  (let ((artist-profile (unwrap! (map-get? artist-profiles tx-sender) ERR_ARTIST_NOT_FOUND)))
    (asserts! (validate-mediums mediums) ERR_INVALID_PARAMS)
    (ok (map-set artist-profiles tx-sender (merge artist-profile {mediums: mediums})))))

(define-public (deactivate-artist)
  (let ((artist-profile (unwrap! (map-get? artist-profiles tx-sender) ERR_ARTIST_NOT_FOUND)))
    (ok (map-set artist-profiles tx-sender (merge artist-profile {active: false})))))

(define-public (reactivate-artist)
  (let ((artist-profile (unwrap! (map-get? artist-profiles tx-sender) ERR_ARTIST_NOT_FOUND)))
    (ok (map-set artist-profiles tx-sender (merge artist-profile {active: true})))))

;; Artwork functions
(define-public (register-artwork (editions uint) (authenticity-level uint) (medium-type uint) (stx-amount uint))
  (begin
    (asserts! (>= editions MIN_ARTWORK_EDITIONS) ERR_INVALID_PARAMS)
    (asserts! (and (>= authenticity-level MIN_AUTHENTICITY_LEVEL) (<= authenticity-level MAX_AUTHENTICITY_LEVEL)) ERR_INVALID_PARAMS)
    (asserts! (is-some (map-get? medium-types medium-type)) ERR_MEDIUM_NOT_FOUND)
    (asserts! (>= stx-amount editions) ERR_INSUFFICIENT_EDITIONS)
    
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
    
    (let ((artwork-id (var-get next-artwork-id)))
      (map-set artwork-records artwork-id {
        creator: tx-sender,
        editions: editions,
        authenticity-level: authenticity-level,
        active: true,
        medium-type: medium-type,
        total-verifications: u0,
        created-at: u0
      })
      
      (var-set next-artwork-id (+ artwork-id u1))
      (ok artwork-id))))

(define-public (withdraw-artwork (artwork-id uint))
  (let ((artwork (unwrap! (map-get? artwork-records artwork-id) ERR_ARTWORK_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator artwork)) ERR_NOT_AUTHORIZED)
    (ok (map-set artwork-records artwork-id (merge artwork {active: false})))))

(define-public (restore-artwork (artwork-id uint))
  (let ((artwork (unwrap! (map-get? artwork-records artwork-id) ERR_ARTWORK_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator artwork)) ERR_NOT_AUTHORIZED)
    (ok (map-set artwork-records artwork-id (merge artwork {active: true})))))

(define-public (add-artwork-editions (artwork-id uint) (additional-editions uint))
  (let ((artwork (unwrap! (map-get? artwork-records artwork-id) ERR_ARTWORK_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator artwork)) ERR_NOT_AUTHORIZED)
    (asserts! (> additional-editions u0) ERR_INVALID_PARAMS)
    
    (try! (stx-transfer? additional-editions tx-sender (as-contract tx-sender)))
    
    (ok (map-set artwork-records artwork-id 
      (merge artwork {editions: (+ (get editions artwork) additional-editions)})))))

;; Helper function to check medium match
(define-private (check-medium-match (medium-type uint) (mediums (list 10 uint)))
  (or
    (and (> (len mediums) u0) (is-eq medium-type (unwrap-panic (element-at mediums u0))))
    (and (> (len mediums) u1) (is-eq medium-type (unwrap-panic (element-at mediums u1))))
    (and (> (len mediums) u2) (is-eq medium-type (unwrap-panic (element-at mediums u2))))
    (and (> (len mediums) u3) (is-eq medium-type (unwrap-panic (element-at mediums u3))))
    (and (> (len mediums) u4) (is-eq medium-type (unwrap-panic (element-at mediums u4))))
    (and (> (len mediums) u5) (is-eq medium-type (unwrap-panic (element-at mediums u5))))
    (and (> (len mediums) u6) (is-eq medium-type (unwrap-panic (element-at mediums u6))))
    (and (> (len mediums) u7) (is-eq medium-type (unwrap-panic (element-at mediums u7))))
    (and (> (len mediums) u8) (is-eq medium-type (unwrap-panic (element-at mediums u8))))
    (and (> (len mediums) u9) (is-eq medium-type (unwrap-panic (element-at mediums u9))))
  ))

;; Authentication functions
(define-public (authenticate-artwork (artwork-id uint))
  (let (
    (artist-profile (unwrap! (map-get? artist-profiles tx-sender) ERR_ARTIST_NOT_FOUND))
    (artwork (unwrap! (map-get? artwork-records artwork-id) ERR_ARTWORK_NOT_FOUND))
    (auth-key {authenticator: tx-sender, artwork-id: artwork-id})
  )
    (asserts! (get active artist-profile) ERR_ARTIST_NOT_FOUND)
    (asserts! (get active artwork) ERR_ARTWORK_NOT_FOUND)
    (asserts! (is-none (map-get? authentication-logs auth-key)) ERR_ALREADY_AUTHENTICATED)
    (asserts! (>= (get editions artwork) (get authenticity-level artwork)) ERR_INSUFFICIENT_EDITIONS)
    (asserts! (check-medium-match (get medium-type artwork) (get mediums artist-profile)) ERR_INVALID_PARAMS)
    
    (let (
      (authenticity-level (get authenticity-level artwork))
      (gallery-fee (/ (* authenticity-level (var-get gallery-fee-percent)) u100))
      (artist-authenticity (- authenticity-level gallery-fee))
    )
      (map-set authentication-logs auth-key {timestamp: u0, verified: true})
      
      (map-set artwork-records artwork-id (merge artwork {
        editions: (- (get editions artwork) authenticity-level),
        total-verifications: (+ (get total-verifications artwork) u1)
      }))
      
      (map-set artist-profiles tx-sender (merge artist-profile {
        authenticity-score: (+ (get authenticity-score artist-profile) artist-authenticity),
        artwork-count: (+ (get artwork-count artist-profile) u1)
      }))
      
      (var-set gallery-balance (+ (var-get gallery-balance) gallery-fee))
      
      (ok artist-authenticity))))

(define-public (claim-authenticity-rewards)
  (let ((artist-profile (unwrap! (map-get? artist-profiles tx-sender) ERR_ARTIST_NOT_FOUND)))
    (let ((authenticity-score (get authenticity-score artist-profile)))
      (asserts! (> authenticity-score u0) ERR_INSUFFICIENT_EDITIONS)
      
      (try! (as-contract (stx-transfer? authenticity-score tx-sender tx-sender)))
      
      (map-set artist-profiles tx-sender (merge artist-profile {
        authenticity-score: u0,
        last-creation: u0
      }))
      
      (ok authenticity-score))))

(define-public (withdraw-gallery-fees)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (let ((amount (var-get gallery-balance)))
      (asserts! (> amount u0) ERR_INSUFFICIENT_EDITIONS)
      
      (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))
      
      (var-set gallery-balance u0)
      
      (ok amount))))

;; Helper functions
(define-private (is-valid-medium-type (medium-type uint))
  (is-some (map-get? medium-types medium-type)))

(define-private (count-valid-medium-types (mediums (list 10 uint)))
  (+ 
    (if (and (> (len mediums) u0) (is-valid-medium-type (unwrap-panic (element-at mediums u0)))) u1 u0)
    (if (and (> (len mediums) u1) (is-valid-medium-type (unwrap-panic (element-at mediums u1)))) u1 u0)
    (if (and (> (len mediums) u2) (is-valid-medium-type (unwrap-panic (element-at mediums u2)))) u1 u0)
    (if (and (> (len mediums) u3) (is-valid-medium-type (unwrap-panic (element-at mediums u3)))) u1 u0)
    (if (and (> (len mediums) u4) (is-valid-medium-type (unwrap-panic (element-at mediums u4)))) u1 u0)
    (if (and (> (len mediums) u5) (is-valid-medium-type (unwrap-panic (element-at mediums u5)))) u1 u0)
    (if (and (> (len mediums) u6) (is-valid-medium-type (unwrap-panic (element-at mediums u6)))) u1 u0)
    (if (and (> (len mediums) u7) (is-valid-medium-type (unwrap-panic (element-at mediums u7)))) u1 u0)
    (if (and (> (len mediums) u8) (is-valid-medium-type (unwrap-panic (element-at mediums u8)))) u1 u0)
    (if (and (> (len mediums) u9) (is-valid-medium-type (unwrap-panic (element-at mediums u9)))) u1 u0)
  ))

(define-private (validate-mediums (mediums (list 10 uint)))
  (let ((mediums-len (len mediums)))
    (and 
      (> mediums-len u0)
      (<= mediums-len u10)
      (is-eq mediums-len (count-valid-medium-types mediums)))))

;; Read-only functions
(define-read-only (get-artist-profile (artist principal))
  (map-get? artist-profiles artist))

(define-read-only (get-artwork-record (artwork-id uint))
  (map-get? artwork-records artwork-id))

(define-read-only (get-medium-type (type-id uint))
  (map-get? medium-types type-id))

(define-read-only (get-gallery-fee)
  (var-get gallery-fee-percent))

(define-read-only (get-gallery-balance)
  (var-get gallery-balance))

(define-read-only (get-authentication-log (authenticator principal) (artwork-id uint))
  (map-get? authentication-logs {authenticator: authenticator, artwork-id: artwork-id}))