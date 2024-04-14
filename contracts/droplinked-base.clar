;; droplinked-base contract serves as a data storage layer for the droplinked-operator-contract.
;; in order maintain data integrity and security, only the droplinked-operator contract is authorized to modify the state of the droplinked-base-contract through public functions.
(define-constant err-droplinked-operator-only (err u100))

(define-constant err-invalid-product-id (err u200))

;; (product-id) => producer
;;
;; maps each product-id to its producer
(define-map producers uint principal)

;; (request-id) => (product-id, producer, publisher, status)
;;
;; when a publisher requests a product, a unique request-id is generated and used to store details about that request.
;; request status is represented by a single byte:
;;   - 0x00: request is pending, awaiting the producer's approval.
;;   - 0x01: producer has greenlit the request, indicating acceptance.
;; rejected requests are purged, meaning a request-id request was denied.
(define-map requests uint 
  {
    product-id: uint,
    publisher: principal,
    status: (buff 1)
  }
)

;; (product-id, producer, publisher) => (is-requested)
;;
;; is-requested map serves as a fast-lookup mechanism to prevent duplicate requests and ensure a streamlined workflow.
;; is-requested map is used to efficiently check if a specific publisher has ever requested a particular product from a particular producer.
;;    - a "true" value for a key indicates that a request for this combination (product-id, producer, publisher) was created (whether rejected or accepted).
;; - prevents duplicate requests and avoid creating a new request if a previous one for the same combination was denied.
(define-map is-requested 
  { 
    product-id: uint,
    publisher: principal
  }
  bool
)

;; (product-id) => (price)
;;
;; stores product price.
(define-map prices uint uint)

;; (product-id) => (commission)
;;
;; stores producer commissions per product.
(define-map commissions uint uint)

;; (product-id) => (product-type)
;;
;; stores product type.
;; product type is represented by a single byte:
;;  - 0x00: indicates digital product
;;  - 0x01: indicates print-on-demand product
;;  - 0x02: indicates physical product
(define-map types uint (buff 1))

;; (product-id) => (destination)
;;
;; stores payment destination addresses for each product.
(define-map destinations uint principal)

;; stores identifier of the most recent request.
(define-data-var last-request-id uint u0)

(define-public
  (insert-product
    (product-id uint)
    (producer principal)
    (price uint)
    (commission uint)
    (type (buff 1))
    (destination principal)
  )
  (begin
    (asserts! (is-eq contract-caller .droplinked-operator) err-droplinked-operator-only)
    (map-insert producers product-id producer)
    (map-insert prices product-id price)
    (map-insert commissions product-id commission)
    (map-insert types product-id type)
    (map-insert destinations product-id destination)
    (ok true)
  )
)

(define-public 
  (insert-request
    (product-id uint)
    (publisher principal)
    (status (buff 1))
  )
  (let 
    (
      (request-id (+ (var-get last-request-id) u1))
    )
    (asserts! (is-eq contract-caller .droplinked-operator) err-droplinked-operator-only)
    (map-insert requests 
      request-id
      {
        product-id: product-id,
        publisher: publisher,
        status: status
      }
    )
    (var-set last-request-id request-id)
    (ok request-id)
  )
)

(define-public 
  (remove-request
    (request-id uint)
  )
  (begin
    (asserts! (is-eq contract-caller .droplinked-operator) err-droplinked-operator-only)
    (ok (map-delete requests request-id))
  )
)

(define-public
  (update-request-status
    (request-id uint)
    (status (buff 1))
  )
  (let 
    (
      (request (unwrap-panic (map-get? requests request-id)))
    )
    (asserts! (is-eq contract-caller .droplinked-operator) err-droplinked-operator-only)
    (ok 
      (map-set requests request-id 
        (merge 
          {
            product-id: (get product-id request),
            publisher: (get publisher request)
          }
          {
            status: status
          }
        )
      )
    )
  )
)

(define-public 
  (insert-is-requested
    (product-id uint)
    (publisher principal)
  )
  (begin 
    (asserts! (is-eq contract-caller .droplinked-operator) err-droplinked-operator-only)
    (ok
      (map-insert is-requested 
        {
          product-id: product-id,
          publisher: publisher
        }
        true
      )
    )
  )
)

(define-public 
  (remove-is-requested
    (product-id uint)
    (publisher principal)
  )
  (begin 
    (asserts! (is-eq contract-caller .droplinked-operator) err-droplinked-operator-only)
    (ok
      (map-delete is-requested 
        {
          product-id: product-id,
          publisher: publisher
        }
      )
    )
  )
)

(define-read-only 
  (get-price?
    (product-id uint)
  )
  (map-get? prices product-id)
)

(define-read-only 
  (get-commission?
    (product-id uint)
  )
  (map-get? commissions product-id)
)

(define-read-only 
  (get-destination?
    (product-id uint)
  )
  (map-get? destinations product-id)
)

(define-read-only 
  (get-type?
    (product-id uint)
  )
  (map-get? types product-id)
)

(define-read-only 
  (has-producer-requested-product?
    (product-id uint)
    (publisher principal)
  )
  (is-some 
    (map-get? is-requested  
      {
        product-id: product-id,
        publisher: publisher
      }
    )
  )
)

(define-read-only 
  (get-request?
    (request-id uint)
  )
  (map-get? requests request-id)
)

(define-read-only 
  (get-producer?
    (product-id uint)
  )
  (map-get? producers product-id)
)

(define-read-only 
  (get-last-request-id)
  (var-get last-request-id)
)