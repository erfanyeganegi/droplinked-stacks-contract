(define-constant err-droplinked-admin-only (err u100))

(define-constant err-publisher-only (err u200))
(define-constant err-producer-only (err u201))

(define-constant err-invalid-price (err u300))
(define-constant err-invalid-commission (err u301))
(define-constant err-invalid-type (err u302))
(define-constant err-invalid-request-id (err u202))

(define-constant err-request-duplicate (err u300))
(define-constant err-request-accepted (err u301))

(define-data-var droplinked-admin principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(define-data-var droplinked-destination principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

(define-constant TYPE_DIGITAL 0x00)
(define-constant TYPE_POD 0x01)
(define-constant TYPE_PHYSICAL 0x02)

(define-constant STATUS_PENDING 0x00)
(define-constant STATUS_ACCEPTED 0x01)

(define-constant DROPLINKED_FEE u100)

;; allows currently droplinked-admin to set new address as admin
(define-public 
  (set-droplinked-admin
    (admin principal)
  )
  (begin 
    (asserts! (is-eq (var-get droplinked-admin) tx-sender) err-droplinked-admin-only)
    (ok (var-set droplinked-admin admin))
  )
)

;; allows currently droplinked-admin to set new droplinked destination address
(define-public 
  (set-droplinked-destination
    (destination principal)
  )
  (begin
    (asserts! (is-eq (var-get droplinked-admin) tx-sender) err-droplinked-admin-only)
    (ok (var-set droplinked-destination destination))
  )
)

(define-public 
  (create-product
    (producer principal)
    (uri (string-ascii 256))
    (price uint)
    (amount uint)
    (commission uint)
    (type (buff 1))
    (recipient principal)
    (destination principal)
    (beneficiaries (list 16 { percentage: bool, address: principal, value: uint }))
    (issuer { address: principal, value: uint })
  )
  (let 
    (
      (product-id (try! (contract-call? .droplinked-token mint amount recipient uri)))
    )
    (asserts! (is-eq producer tx-sender) err-producer-only)
    (asserts! (>= price u1) err-invalid-price)
    (asserts! (and (>= commission u0) (<= commission u100)) err-invalid-commission)
    (asserts! 
      (or 
        (is-eq type TYPE_DIGITAL)
        (is-eq type TYPE_POD)
        (is-eq type TYPE_PHYSICAL)
      )
      err-invalid-type
    )
    (try! (contract-call? .droplinked-base insert-product product-id producer price commission type destination beneficiaries issuer))
    (ok product-id)
  )
)

(define-public 
  (create-request
    (product-id uint)
    (publisher principal)
  )
  (begin
    (asserts! (is-eq publisher tx-sender) err-publisher-only)
    (asserts! (is-eq (contract-call? .droplinked-base has-producer-requested-product? product-id publisher) false) err-request-duplicate)
    (let 
      (
        (request-id (try! (contract-call? .droplinked-base insert-request product-id publisher STATUS_PENDING)))
      )
      (try! (contract-call? .droplinked-base insert-is-requested product-id publisher))
      (ok request-id)
    )
  )
)

(define-public 
  (cancel-request
    (request-id uint)
    (publisher principal)
  )
  (let 
    (
      (request (unwrap! (contract-call? .droplinked-base get-request? request-id) err-invalid-request-id))
      (product-id (get product-id request))
      (producer (unwrap-panic (contract-call? .droplinked-base get-producer? product-id)))
    )
    (asserts! (is-eq publisher tx-sender) err-publisher-only)
    (asserts! (is-eq publisher (get publisher request)) err-publisher-only)
    (asserts! (is-eq (get status request) STATUS_PENDING) err-request-accepted)
    (try! (contract-call? .droplinked-base remove-is-requested product-id publisher))
    (ok request-id)
  )
)

(define-public 
  (accept-request
    (request-id uint)
    (producer principal)
  )
  (let 
    (
      (request (unwrap! (contract-call? .droplinked-base get-request? request-id) err-invalid-request-id))
      (product-producer (unwrap-panic (contract-call? .droplinked-base get-producer? (get product-id request))))
    )
    (asserts! (is-eq producer tx-sender) err-producer-only)
    (asserts! (is-eq producer product-producer) err-producer-only)
    (try! (contract-call? .droplinked-base update-request-status request-id STATUS_ACCEPTED))
    (ok request-id)
  )
)

(define-public 
  (purchase-product
  (purchaser principal)
    (shop principal)
    (cart (list 64 
      {
        id: uint,
        affiliate: bool,
        amount: uint
      }
    ))
  )
  (begin
    (asserts! (is-eq purchaser tx-sender) (err u200))
    (fold purchase-product-iter cart (ok shop))
  )
)

(define-public 
  (reject-request
    (request-id uint)
    (producer principal)
  )
  (let 
    (
      (request (unwrap! (contract-call? .droplinked-base get-request? request-id) err-invalid-request-id))
      (product-id (get product-id request))
      (publisher (get publisher request))
      (product-producer (unwrap-panic (contract-call? .droplinked-base get-producer? (get product-id request))))
    )
    (asserts! (is-eq producer tx-sender) err-producer-only)
    (asserts! (is-eq producer product-producer) err-producer-only)
    (try! (contract-call? .droplinked-base remove-request request-id))
    (try! (contract-call? .droplinked-base remove-is-requested product-id publisher))
    (ok request-id)
  )
)

;; #[allow(unchecked_data)]
(define-private 
  (purchase-product-iter
    (item 
      {
        id: uint,
        affiliate: bool,
        amount: uint
      }
    )
    (shop-response (response principal uint))
  )
  (match shop-response
    shop
    (if (get affiliate item)
      (let 
        (
          (request (unwrap! (contract-call? .droplinked-base get-request? (get id item)) (err u200)))
          (status (get status request))
          (publisher (get publisher request))
          (product-id (get product-id request))
          (producer (unwrap-panic (contract-call? .droplinked-base get-producer? product-id)))
        )
        (asserts! (is-eq status STATUS_ACCEPTED) (err u200))
        (asserts! (is-eq publisher shop) (err u200))
        (ok shop)
      )
      (let 
        (
          (producer shop)
          (product-id (get id item))
        )
        (ok shop)
      )
    )
    previous-err
    (err u200)
  )
)

;; #[allow(unchecked_data)]
(define-private 
  (purchase-product-transfers
    (purchaser principal)
    (product-id uint)
    (amount uint)
    (producer principal)
    (optional-publisher (optional principal))
  )
  (let 
    (
      (price (unwrap! (contract-call? .droplinked-base get-price? product-id) (err u200)))
      (commission (unwrap! (contract-call? .droplinked-base get-commission? product-id) (err u200)))
      (type (unwrap! (contract-call? .droplinked-base get-type? product-id) (err u200)))
      (destination (unwrap! (contract-call? .droplinked-base get-destination? product-id) (err u200)))
      (issuer (unwrap! (contract-call? .droplinked-base get-royalty? product-id) (err u200)))
    )
    (let 
      (
        (publisher-share (if (is-some optional-publisher) (apply-percentage price commission) u0))
        (royalty-share (apply-percentage price (get value issuer)))
        (droplinked-share (apply-percentage price DROPLINKED_FEE))
      )
      (try! (stx-transfer? droplinked-share purchaser (var-get droplinked-destination)))
      (try! (stx-transfer? royalty-share purchaser (get address issuer)))
      (try! (match optional-publisher publisher 
        (if (is-eq publisher-share u0) 
          (ok true)
          (stx-transfer? publisher-share purchaser publisher)
        )
        (ok true)
      ))
      (let 
        (
          (producer-share 
            (- price 
              publisher-share
              royalty-share
              droplinked-share 
              (try! (pay-product-benificiaries purchaser price (contract-call? .droplinked-base get-benificiary-link? product-id)))
            )
          )
        )
        (try! (stx-transfer? producer-share purchaser producer))
        (try! (contract-call? .droplinked-token transfer product-id amount producer purchaser))
        (ok true)
      )
    )
  )
)

;; #[allow(unchecked_data)]
(define-private 
  (apply-percentage
    (value uint)
    (percentage uint)
  )
  (/ (* value percentage) u10000)
)

;; #[allow(unchecked_data)]
(define-private 
  (pay-product-benificiaries
    (purchaser principal)
    (price uint)
    (optional-benificiary-link (optional uint))
  )
  (match optional-benificiary-link benificiary-link
    (ok (get beneficiaries-share
      (try! (fold pay-product-beneficiaries-iter 0x00000000000000000000000000000000 
        (ok 
          {
            purchaser: purchaser,
            price: price,
            next: (some benificiary-link),
            beneficiaries-share: u0
          }
        )
      )
    )))
    (ok u0)
  )
)

;; #[allow(unchecked_data)]
(define-private 
  (pay-product-beneficiaries-iter
    (i (buff 1))
    (previous-response (response 
      {
        purchaser: principal,
        price: uint,
        next: (optional uint),
        beneficiaries-share: uint
      }
      uint
    ))
  )
  (match previous-response result
    (let 
      (
        (purchaser (get purchaser result))
        (price (get price result))
        (next (get next result))
      )
      (match next benificiary-id 
        (let
          (
            (beneficiary (unwrap-panic (contract-call? .droplinked-base get-benificiary? benificiary-id)))
            (beneficiary-share 
              (if (get percentage beneficiary)
                (apply-percentage price (get value beneficiary))
                (get value beneficiary)
              )
            )
            (next-beneficiary (get next beneficiary))
          )
          (try! (stx-transfer? beneficiary-share purchaser (get address beneficiary)))
          (ok 
            {
              purchaser: purchaser,
              price: price,
              next: next-beneficiary,
              beneficiaries-share: (+ (get beneficiaries-share result) beneficiary-share)
            }
          )
        )
        (ok result)
      )
    )
    previous-err
    previous-response
  )
)

;; retrieves current droplinked-admin
(define-read-only 
  (get-droplinked-admin)
  (var-get droplinked-admin)
)

;; retrieves current droplinked-destination
(define-read-only 
  (get-droplinked-destination)
  (var-get droplinked-destination)
)