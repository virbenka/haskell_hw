# Task 1

1) ((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s)) -> λ q. ((q (((q ((λ p. p) r)) s)r)) s)) -> 
λ q. ((q (((q r) s)r)) s)

2) ((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b] -> 
((λ a. λ b. b a (a b x) (λ b. x)) (λ b. b))[x := b] -> ((λ a. λ b. b a (a b b) (λ b. b)) (λ b. b))
