;; AMM Contract Test Suite
;; Run these tests in your Pact REPL after loading the AMM contract

;; First, set up the governance keyset
(define-keyset "amm-admin" (read-keyset "amm-admin"))

;; Load the AMM contract (assuming it's in a file called amm.pact)
;; (load "amm.pact")

;; Test 1: Create a new pool
(begin-tx "Create Pool Test")
(use amm)

;; Test pool creation
(expect "Pool created successfully"
  "COIN-USDC"
  (create-pool "COIN" "USDC" 1000.0 2000.0 "alice"))

;; Verify pool data
(expect "Pool has correct token A"
  "COIN"
  (at "token-a" (get-pool-info "COIN-USDC")))

(expect "Pool has correct token B"
  "USDC"
  (at "token-b" (get-pool-info "COIN-USDC")))

(expect "Pool has correct reserve A"
  1000.0
  (at "reserve-a" (get-pool-info "COIN-USDC")))

(expect "Pool has correct reserve B"
  2000.0
  (at "reserve-b" (get-pool-info "COIN-USDC")))

;; Check LP token supply (should be sqrt(1000 * 2000) = 1414.213562...)
(expect "LP token supply calculated correctly"
  true
  (> (at "lp-token-supply" (get-pool-info "COIN-USDC")) 1414.0))

(commit-tx)

;; Test 2: Check user LP balance
(begin-tx "Check LP Balance")
(use amm)

(expect "Alice has LP tokens"
  true
  (> (get-user-lp-balance "alice" "COIN-USDC") 1414.0))

(expect "Pool share calculation"
  true
  (> (calculate-pool-share "alice" "COIN-USDC") 99.0))

(commit-tx)

;; Test 3: Add liquidity to existing pool
(begin-tx "Add Liquidity Test")
(use amm)

;; Add proportional liquidity
(let ((lp-tokens-minted (add-liquidity "COIN-USDC" 500.0 1000.0 "bob")))
  (expect "LP tokens minted for Bob"
    true
    (> lp-tokens-minted 700.0)))

;; Check updated reserves
(expect "Reserve A increased"
  1500.0
  (at "reserve-a" (get-pool-info "COIN-USDC")))

(expect "Reserve B increased"
  3000.0
  (at "reserve-b" (get-pool-info "COIN-USDC")))

;; Check Bob's LP balance
(expect "Bob has LP tokens"
  true
  (> (get-user-lp-balance "bob" "COIN-USDC") 700.0))

(commit-tx)

;; Test 4: Price calculation (read-only)
(begin-tx "Price Calculation Test")
(use amm)

;; Test price calculation for COIN -> USDC
(let ((amount-out (get-amount-out "COIN-USDC" "COIN" 100.0)))
  (expect "Swap calculation returns positive amount"
    true
    (> amount-out 0.0))
  (expect "Swap calculation respects slippage"
    true
    (< amount-out 200.0))) ; Should be less than 2:1 ratio due to slippage

;; Test price calculation for USDC -> COIN
(let ((amount-out (get-amount-out "COIN-USDC" "USDC" 200.0)))
  (expect "Reverse swap calculation"
    true
    (and (> amount-out 0.0) (< amount-out 100.0))))

(commit-tx)

;; Test 5: Execute swap
(begin-tx "Swap Test")
(use amm)

;; Get initial reserves
(let* ((initial-pool (get-pool-info "COIN-USDC"))
       (initial-reserve-a (at "reserve-a" initial-pool))
       (initial-reserve-b (at "reserve-b" initial-pool)))

  ;; Execute swap: 100 COIN for USDC
  (let ((amount-out (swap-exact-tokens-for-tokens 
                      "COIN-USDC" "COIN" 100.0 150.0 "charlie")))
    
    (expect "Swap executed successfully"
      true
      (> amount-out 150.0))
    
    ;; Check reserves updated
    (let ((updated-pool (get-pool-info "COIN-USDC")))
      (expect "Reserve A increased"
        (+ initial-reserve-a 100.0)
        (at "reserve-a" updated-pool))
      
      (expect "Reserve B decreased"
        true
        (< (at "reserve-b" updated-pool) initial-reserve-b))
      
      (expect "Fees collected"
        true
        (> (at "total-fees-a" updated-pool) 0.0)))))

(commit-tx)

;; Test 6: Remove liquidity
(begin-tx "Remove Liquidity Test")
(use amm)

;; Get Bob's LP balance
(let ((bob-lp-balance (get-user-lp-balance "bob" "COIN-USDC")))
  
  ;; Remove half of Bob's liquidity
  (let ((half-lp (/ bob-lp-balance 2.0)))
    (let ((amounts-returned (remove-liquidity "COIN-USDC" half-lp "bob")))
      
      (expect "Received token A"
        true
        (> (at 0 amounts-returned) 0.0))
      
      (expect "Received token B"
        true
        (> (at 1 amounts-returned) 0.0))
      
      ;; Check Bob's remaining LP balance
      (expect "LP balance reduced"
        true
        (< (get-user-lp-balance "bob" "COIN-USDC") bob-lp-balance)))))

(commit-tx)

;; Test 7: Error cases
(begin-tx "Error Handling Tests")
(use amm)

;; Test duplicate pool creation
(expect-failure "Cannot create duplicate pool"
  "Pool already exists"
  (create-pool "COIN" "USDC" 100.0 200.0 "alice"))

;; Test invalid swap token
(expect-failure "Invalid token for swap"
  "Invalid input token"
  (swap-exact-tokens-for-tokens "COIN-USDC" "INVALID" 100.0 0.0 "alice"))

;; Test insufficient liquidity removal
(expect-failure "Cannot remove more LP tokens than owned"
  "Insufficient LP tokens"
  (remove-liquidity "COIN-USDC" 999999.0 "bob"))

;; Test zero amounts
(expect-failure "Cannot add zero liquidity"
  "Amount A must be positive"
  (add-liquidity "COIN-USDC" 0.0 100.0 "alice"))

(commit-tx)

;; Test 8: Fee rate adjustment (admin only)
(begin-tx "Admin Functions Test")
(use amm)

;; Test fee rate change
(expect "Fee rate updated"
  "Fee rate updated"
  (set-fee-rate "COIN-USDC" 0.005))

(expect "New fee rate applied"
  0.005
  (at "fee-rate" (get-pool-info "COIN-USDC")))

;; Test invalid fee rate
(expect-failure "Invalid fee rate rejected"
  "Fee rate must be between 0% and 10%"
  (set-fee-rate "COIN-USDC" 0.15))

(commit-tx)

;; Test 9: Pool with reversed token order
(begin-tx "Token Order Test")
(use amm)

;; Create pool with tokens in different order
(expect "Pool created with consistent ordering"
  "COIN-USDC"
  (create-pool "USDC" "COIN" 2000.0 1000.0 "dave"))

;; Should create same pool ID regardless of input order
(let ((pool-info (get-pool-info "COIN-USDC")))
  (expect "Tokens ordered consistently"
    "COIN"
    (at "token-a" pool-info)))

(commit-tx)

;; Test 10: Mathematical precision test
(begin-tx "Precision Test")
(use amm)

;; Create a pool with very small amounts
(expect "Small pool created"
  "TOKEN1-TOKEN2"
  (create-pool "TOKEN1" "TOKEN2" 0.001 0.002 "precision-test"))

;; Test small swap
(let ((small-swap-out (get-amount-out "TOKEN1-TOKEN2" "TOKEN1" 0.0001)))
  (expect "Small swap calculation"
    true
    (> small-swap-out 0.0)))

(commit-tx)

;; Summary test - display final state
(begin-tx "Final State Summary")
(use amm)

(print "=== FINAL POOL STATES ===")
(print (format "COIN-USDC Pool: {}" [(get-pool-info "COIN-USDC")]))
(print (format "TOKEN1-TOKEN2 Pool: {}" [(get-pool-info "TOKEN1-TOKEN2")]))

(print "=== USER LP BALANCES ===")
(print (format "Alice LP balance: {}" [(get-user-lp-balance "alice" "COIN-USDC")]))
(print (format "Bob LP balance: {}" [(get-user-lp-balance "bob" "COIN-USDC")]))
(print (format "Alice pool share: {}%" [(calculate-pool-share "alice" "COIN-USDC")]))
(print (format "Bob pool share: {}%" [(calculate-pool-share "bob" "COIN-USDC")]))

(commit-tx)

;; Performance test - multiple swaps
(begin-tx "Performance Test")
(use amm)

(print "=== PERFORMANCE TEST: Multiple Swaps ===")
;; Execute multiple small swaps to test gas efficiency
(map (lambda (i) 
       (swap-exact-tokens-for-tokens "COIN-USDC" "COIN" 1.0 0.5 "trader"))
     [1 2 3 4 5])

(print (format "Pool after 5 small swaps: {}" [(get-pool-info "COIN-USDC")]))

(commit-tx)