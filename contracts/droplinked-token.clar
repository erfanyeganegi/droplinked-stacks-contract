(impl-trait .sft-trait.sft-trait)

(define-constant err-droplinked-operator-only (err u100))

(define-fungible-token product)
(define-non-fungible-token sku { id: uint, owner: principal })

(define-map balances { id: uint, owner: principal } uint)

(define-map supplies uint uint)

(define-map uris uint (string-ascii 256))

(define-data-var last-sku-id uint u0)

(define-public 
  (mint 
    (amount uint)
    (recipient principal)
    (uri (string-ascii 256))
  )
  (let 
    (
      (id (+ (var-get last-sku-id) u1))
    )
    (asserts! (is-eq contract-caller .droplinked-operator) err-droplinked-operator-only)
    (try! (nft-mint? sku { id: id, owner: recipient } recipient))
    (try! (ft-mint? product amount recipient))
    (map-insert supplies id amount)
    (map-insert balances { id: id, owner: recipient } amount)
    (map-insert uris id uri)
    (print { type: "sft_mint", token-id: id, amount: amount, recipient: recipient })
    (ok id)
  )
)

(define-public
  (transfer
    (id uint)
    (amount uint)
    (sender principal)
    (recipient principal)
  )
  (let
    (
      (sender-balance (unwrap-panic (get-balance id sender)))
      (recipient-balance (unwrap-panic (get-balance id recipient)))
    )
    (asserts! (is-eq contract-caller .droplinked-operator) err-droplinked-operator-only)
    (try! (ft-transfer? product amount sender recipient))
    (try! (burn-and-mint { id: id, owner: sender }))
    (try! (burn-and-mint { id: id, owner: recipient }))
    (map-set balances { id: id, owner: sender } (- sender-balance amount))
    (map-set balances { id: id, owner: recipient } amount)
    (print { type: "sft_transfer", token-id: id, amount: amount, sender: sender, recipient: recipient })
    (ok true)
  )
)

(define-public 
  (transfer-memo
    (id uint)
    (amount uint)
    (sender principal)
    (recipient principal)
    (memo (buff 34))
  ) 
  (begin 
    (try! (transfer id amount sender recipient))
    (print memo)
    (ok true)
  )
)

(define-read-only 
  (get-balance
    (id uint)
    (owner principal)
  )
  (ok (default-to u0 (map-get? balances { id: id, owner: owner })))
)

(define-read-only
  (get-overall-balance
    (owner principal)
  )
  (ok (ft-get-balance product owner))
)

(define-read-only
  (get-overall-supply)
  (ok (ft-get-supply product))
)

(define-read-only
  (get-total-supply
    (id uint)
  )
  (ok (default-to u0 (map-get? supplies id)))
)

(define-read-only
  (get-decimals
    (id uint)
  )
  (ok u0)
)

(define-read-only 
  (get-token-uri
    (id uint)
  )
  (ok (map-get? uris id))
)

(define-private 
  (burn-and-mint
    (sku-id { id: uint, owner: principal })
  )
  (begin 
    (and 
      (is-some (nft-get-owner? sku sku-id))
      (try! (nft-burn? sku sku-id (get owner sku-id)))
    )
    (nft-mint? sku sku-id (get owner sku-id))
  )
)