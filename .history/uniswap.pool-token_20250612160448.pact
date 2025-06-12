(namespace "uniswap")

;; Each liquidity pool is a separate module
(module pool-TEMPLATE 'admin-keyset
  (defconst FEE:decimal 0.003)

  (defschema pool-state
    token-module:string
    token:string
    reserve-kda:decimal
    reserve-token:decimal
  )
  (deftable pool:{pool-state})

  ;; LP shares are fungible tokens via fungible-v2
  (use fungible-v2)

  ;; LP shares name will be "UNILP-<token>"
  (defun get-lp-name (token:string):string
    (format "UNILP-{}" [token])
  )

  ;; Init function (called by factory)
  (defun init-pool (token-module:string token:string)
    (enforce (not (contains pool "state")) "Pool already initialized")
    (insert pool "state" {
      "token-module": token-module,
      "token": token,
      "reserve-kda": 0.0,
      "reserve-token": 0.0
    })

    ;; Initialize LP token module
    (let ((lp-name (get-lp-name token)))
      (create-fungible-v2 lp-name "Uniswap V1 LP Token" "UNI-LP" 6)
    )
  )

  ;; Add liquidity
  (defun add-liquidity (kda:decimal token-amt:decimal account:string)
    (with-read pool "state"
      { "token-module": token-module, "token": token, "reserve-kda": rkda, "reserve-token": rtoken } ->

      (if (= rkda 0.0)
        ;; first liquidity provider
        (seq
          (coin.transfer (read-keyset "sender") account kda)
          ((resolve token-module).transfer (read-keyset "sender") account token token-amt)
          (update pool "state" {
            "reserve-kda": kda,
            "reserve-token": token-amt
          })
          (mint-fungible-v2 (get-lp-name token) account kda)
        )
        ;; subsequent liquidity providers
        (let* (
          (expected-token (* token-amt (/ rkda rtoken)))
          (lp-supply (total-supply-v2 (get-lp-name token)))
          (liquidity (* kda (/ lp-supply rkda)))
        )
          (enforce (>= token-amt expected-token) "Wrong ratio")
          (coin.transfer (read-keyset "sender") account kda)
          ((resolve token-module).transfer (read-keyset "sender") account token token-amt)
          (update pool "state" {
            "reserve-kda": (+ rkda kda),
            "reserve-token": (+ rtoken token-amt)
          })
          (mint-fungible-v2 (get-lp-name token) account liquidity)
        )
      )
    )
  )

  ;; Remove liquidity
  (defun remove-liquidity (lp-amt:decimal account:string)
    (with-read pool "state"
      { "token-module": token-module, "token": token, "reserve-kda": rkda, "reserve-token": rtoken } ->

      (let* (
        (lp-supply (total-supply-v2 (get-lp-name token)))
        (kda-out (* lp-amt (/ rkda lp-supply)))
        (token-out (* lp-amt (/ rtoken lp-supply)))
      )
        (burn-fungible-v2 (get-lp-name token) (read-keyset "sender") lp-amt)
        (coin.transfer account (read-keyset "sender") kda-out)
        ((resolve token-module).transfer account (read-keyset "sender") token token-out)
        (update pool "state" {
          "reserve-kda": (- rkda kda-out),
          "reserve-token": (- rtoken token-out)
        })
      )
    )
  )

  ;; Swap KDA for Token
  (defun swap-kda-token (kda-in:decimal min-token-out:decimal account:string)
    (with-read pool "state"
      { "token-module": token-module, "token": token, "reserve-kda": rkda, "reserve-token": rtoken } ->

      (let* (
        (kda-in-fee (* kda-in (- 1.0 FEE)))
        (num (* kda-in-fee rtoken))
        (den (+ rkda kda-in-fee))
        (token-out (/ num den))
      )
        (enforce (>= token-out min-token-out) "Slippage exceeded")
        (coin.transfer (read-keyset "sender") account kda-in)
        ((resolve token-module).transfer account (read-keyset "sender") token token-out)
        (update pool "state" {
          "reserve-kda": (+ rkda kda-in),
          "reserve-token": (- rtoken token-out)
        })
      )
    )
  )

  ;; Swap Token for KDA
  (defun swap-token-kda (token-in:decimal min-kda-out:decimal account:string)
    (with-read pool "state"
      { "token-module": token-module, "token": token, "reserve-kda": rkda, "reserve-token": rtoken } ->

      (let* (
        (token-in-fee (* token-in (- 1.0 FEE)))
        (num (* token-in-fee rkda))
        (den (+ rtoken token-in-fee))
        (kda-out (/ num den))
      )
        (enforce (>= kda-out min-kda-out) "Slippage exceeded")
        ((resolve token-module).transfer (read-keyset "sender") account token token-in)
        (coin.transfer account (read-keyset "sender") kda-out)
        (update pool "state" {
          "reserve-kda": (- rkda kda-out),
          "reserve-token": (+ rtoken token-in)
        })
      )
    )
  )
)
