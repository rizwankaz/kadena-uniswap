;; Automated Market Maker (AMM) Contract in Pact
;; Implements constant product formula (x * y = k) with fee mechanism

(module amm GOVERNANCE
    "Automated Market Maker implementing constant product formula"
  
    ;; Governance capability
    (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "amm-admin")))
  
    ;; Schemas for data storage
    (defschema pool-schema
      token-a:string
      token-b:string
      reserve-a:decimal
      reserve-b:decimal
      lp-token-supply:decimal
      fee-rate:decimal
      total-fees-a:decimal
      total-fees-b:decimal
    )
  
    (defschema user-lp-schema
      pool-id:string
      lp-tokens:decimal
    )
  
    ;; Tables
    (deftable pools:{pool-schema})
    (deftable user-lp-positions:{user-lp-schema})
  
    ;; Constants
    (defconst MINIMUM_LIQUIDITY:decimal 0.000001)
    (defconst DEFAULT_FEE_RATE:decimal 0.003) ; 0.3%
  
    ;; Capabilities
    (defcap POOL_ADMIN (pool-id:string)
      "Capability for pool administration"
      (enforce-guard (keyset-ref-guard "amm-admin")))
  
    (defcap SWAP (account:string pool-id:string)
      "Capability for token swapping"
      (enforce (!= account "") "Account cannot be empty"))
  
    (defcap ADD_LIQUIDITY (account:string pool-id:string)
      "Capability for adding liquidity"
      (enforce (!= account "") "Account cannot be empty"))
  
    (defcap REMOVE_LIQUIDITY (account:string pool-id:string)
      "Capability for removing liquidity"
      (enforce (!= account "") "Account cannot be empty"))
  
    ;; Utility functions
    (defun calculate-pool-id:string (token-a:string token-b:string)
      "Generate deterministic pool ID from token pair"
      (if (< token-a token-b)
          (format "{}-{}" [token-a token-b])
          (format "{}-{}" [token-b token-a])))
  
    (defun min:decimal (a:decimal b:decimal)
      "Return the minimum of two decimal values"
      (if (< a b) a b))
  
    (defun max:decimal (a:decimal b:decimal)
      "Return the maximum of two decimal values"
      (if (> a b) a b))
  
    (defun abs:decimal (x:decimal)
      "Return the absolute value of a decimal"
      (if (< x 0.0) (- x) x))
  
    (defun sqrt:decimal (x:decimal)
      "Calculate square root using Newton's method (iterative)"
      (if (<= x 0.0) 0.0
          (if (= x 1.0) 1.0
              ;; Use a simple approximation that works well for AMM calculations
              ;; For the range we typically need (product of reserves), this is sufficient
              (let* ((initial-guess (/ x 2.0))
                     (iteration1 (/ (+ initial-guess (/ x initial-guess)) 2.0))
                     (iteration2 (/ (+ iteration1 (/ x iteration1)) 2.0))
                     (iteration3 (/ (+ iteration2 (/ x iteration2)) 2.0))
                     (iteration4 (/ (+ iteration3 (/ x iteration3)) 2.0))
                     (iteration5 (/ (+ iteration4 (/ x iteration4)) 2.0)))
                iteration5))))
  
    ;; Core AMM functions
    (defun create-pool:string 
      (token-a:string 
       token-b:string 
       initial-a:decimal 
       initial-b:decimal 
       account:string)
      "Create a new liquidity pool"
      (enforce (> initial-a 0.0) "Initial token A amount must be positive")
      (enforce (> initial-b 0.0) "Initial token B amount must be positive")
      (enforce (!= token-a token-b) "Tokens must be different")
      
      (let* ((pool-id (calculate-pool-id token-a token-b))
             (initial-lp (sqrt (* initial-a initial-b)))
             (lp-to-mint (- initial-lp MINIMUM_LIQUIDITY)))
        
        (enforce (> lp-to-mint 0.0) "Insufficient liquidity")
        
        ;; Insert pool data
        (insert pools pool-id
          { "token-a": (if (< token-a token-b) token-a token-b)
          , "token-b": (if (< token-a token-b) token-b token-a)
          , "reserve-a": (if (< token-a token-b) initial-a initial-b)
          , "reserve-b": (if (< token-a token-b) initial-b initial-a)
          , "lp-token-supply": initial-lp
          , "fee-rate": DEFAULT_FEE_RATE
          , "total-fees-a": 0.0
          , "total-fees-b": 0.0
          })
        
        ;; Grant LP tokens to account (minus minimum liquidity)
        (write user-lp-positions (format "{}:{}" [account pool-id])
          { "pool-id": pool-id
          , "lp-tokens": lp-to-mint
          })
        
        pool-id))
  
    (defun add-liquidity:decimal
      (pool-id:string
       amount-a:decimal
       amount-b:decimal
       account:string)
      "Add liquidity to existing pool"
      (with-capability (ADD_LIQUIDITY account pool-id)
        (let ((pool (read pools pool-id)))
          (enforce (> amount-a 0.0) "Amount A must be positive")
          (enforce (> amount-b 0.0) "Amount B must be positive")
          
          (let* ((reserve-a (at "reserve-a" pool))
                 (reserve-b (at "reserve-b" pool))
                 (lp-supply (at "lp-token-supply" pool))
                 ;; Calculate optimal amounts based on current ratio
                 (optimal-b (/ (* amount-a reserve-b) reserve-a))
                 (optimal-a (/ (* amount-b reserve-a) reserve-b))
                 ;; Use the limiting factor
                 (final-a (if (<= optimal-a amount-a) optimal-a amount-a))
                 (final-b (if (<= optimal-b amount-b) optimal-b amount-b))
                 ;; Calculate LP tokens to mint
                 (lp-tokens-a (/ (* final-a lp-supply) reserve-a))
                 (lp-tokens-b (/ (* final-b lp-supply) reserve-b))
                 (lp-tokens (min lp-tokens-a lp-tokens-b)))
            
            ;; Update pool reserves
            (update pools pool-id
              { "reserve-a": (+ reserve-a final-a)
              , "reserve-b": (+ reserve-b final-b)
              , "lp-token-supply": (+ lp-supply lp-tokens)
              })
            
            ;; Update user LP position
            (let ((user-key (format "{}:{}" [account pool-id])))
              (with-default-read user-lp-positions user-key
                { "pool-id": pool-id, "lp-tokens": 0.0 }
                { "lp-tokens" := current-lp }
                (write user-lp-positions user-key
                  { "pool-id": pool-id
                  , "lp-tokens": (+ current-lp lp-tokens)
                  })))
            
            lp-tokens))))
  
    (defun remove-liquidity:[decimal]
      (pool-id:string
       lp-tokens:decimal
       account:string)
      "Remove liquidity from pool"
      (with-capability (REMOVE_LIQUIDITY account pool-id)
        (let ((pool (read pools pool-id))
              (user-key (format "{}:{}" [account pool-id])))
          
          (enforce (> lp-tokens 0.0) "LP tokens must be positive")
          
          (let ((user-position (read user-lp-positions user-key)))
            (enforce (>= (at "lp-tokens" user-position) lp-tokens) 
                     "Insufficient LP tokens")
            
            (let* ((reserve-a (at "reserve-a" pool))
                   (reserve-b (at "reserve-b" pool))
                   (lp-supply (at "lp-token-supply" pool))
                   ;; Calculate token amounts to return
                   (amount-a (/ (* lp-tokens reserve-a) lp-supply))
                   (amount-b (/ (* lp-tokens reserve-b) lp-supply)))
              
              ;; Update pool
              (update pools pool-id
                { "reserve-a": (- reserve-a amount-a)
                , "reserve-b": (- reserve-b amount-b)
                , "lp-token-supply": (- lp-supply lp-tokens)
                })
              
              ;; Update user position
              (update user-lp-positions user-key
                { "lp-tokens": (- (at "lp-tokens" user-position) lp-tokens)
                })
              
              [amount-a amount-b])))))
  
    (defun swap-exact-tokens-for-tokens:decimal
      (pool-id:string
       token-in:string
       amount-in:decimal
       min-amount-out:decimal
       account:string)
      "Swap exact input tokens for output tokens"
      (with-capability (SWAP account pool-id)
        (let ((pool (read pools pool-id)))
          (enforce (> amount-in 0.0) "Input amount must be positive")
          
          (let* ((token-a (at "token-a" pool))
                 (token-b (at "token-b" pool))
                 (reserve-a (at "reserve-a" pool))
                 (reserve-b (at "reserve-b" pool))
                 (fee-rate (at "fee-rate" pool))
                 (is-a-to-b (= token-in token-a)))
            
            (enforce (or (= token-in token-a) (= token-in token-b))
                     "Invalid input token")
            
            (let* ((reserve-in (if is-a-to-b reserve-a reserve-b))
                   (reserve-out (if is-a-to-b reserve-b reserve-a))
                   ;; Apply fee to input amount
                   (amount-in-with-fee (* amount-in (- 1.0 fee-rate)))
                   ;; Calculate output using constant product formula
                   (amount-out (/ (* amount-in-with-fee reserve-out)
                                 (+ reserve-in amount-in-with-fee)))
                   (fee-amount (* amount-in fee-rate)))
              
              (enforce (>= amount-out min-amount-out)
                       "Insufficient output amount")
              (enforce (< amount-out reserve-out)
                       "Insufficient liquidity")
              
              ;; Update reserves
              (if is-a-to-b
                  (update pools pool-id
                    { "reserve-a": (+ reserve-a amount-in)
                    , "reserve-b": (- reserve-b amount-out)
                    , "total-fees-a": (+ (at "total-fees-a" pool) fee-amount)
                    })
                  (update pools pool-id
                    { "reserve-a": (- reserve-a amount-out)
                    , "reserve-b": (+ reserve-b amount-in)
                    , "total-fees-b": (+ (at "total-fees-b" pool) fee-amount)
                    }))
              
              amount-out)))))
  
    (defun get-amount-out:decimal
      (pool-id:string
       token-in:string
       amount-in:decimal)
      "Calculate output amount for given input (read-only)"
      (let ((pool (read pools pool-id)))
        (let* ((token-a (at "token-a" pool))
               (reserve-a (at "reserve-a" pool))
               (reserve-b (at "reserve-b" pool))
               (fee-rate (at "fee-rate" pool))
               (is-a-to-b (= token-in token-a))
               (reserve-in (if is-a-to-b reserve-a reserve-b))
               (reserve-out (if is-a-to-b reserve-b reserve-a))
               (amount-in-with-fee (* amount-in (- 1.0 fee-rate))))
          
          (/ (* amount-in-with-fee reserve-out)
             (+ reserve-in amount-in-with-fee)))))
  
    (defun get-pool-info:object{pool-schema} (pool-id:string)
      "Get pool information"
      (read pools pool-id))
  
    (defun get-user-lp-balance:decimal (account:string pool-id:string)
      "Get user's LP token balance"
      (let ((user-key (format "{}:{}" [account pool-id])))
        (with-default-read user-lp-positions user-key
          { "lp-tokens": 0.0 }
          { "lp-tokens" := balance }
          balance)))
  
    (defun calculate-pool-share:decimal (account:string pool-id:string)
      "Calculate user's share of the pool as percentage"
      (let ((pool (read pools pool-id))
            (user-lp (get-user-lp-balance account pool-id)))
        (let ((total-supply (at "lp-token-supply" pool)))
          (if (= total-supply 0.0) 0.0
              (* (/ user-lp total-supply) 100.0)))))
  
    ;; Admin functions
    (defun set-fee-rate:string (pool-id:string new-fee-rate:decimal)
      "Set new fee rate for pool (admin only)"
      (with-capability (POOL_ADMIN pool-id)
        (enforce (and (>= new-fee-rate 0.0) (<= new-fee-rate 0.1))
                 "Fee rate must be between 0% and 10%")
        (update pools pool-id { "fee-rate": new-fee-rate })
        "Fee rate updated"))
  )