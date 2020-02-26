# Task 1

1) ((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s)) -> λ q. ((q (((q ((λ p. p) r)) s)r)) s)) -> 
λ q. ((q (((q r) s)r)) s)

2) ((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b] -> 
((λ a. λ b. b a (a b x) (λ b. x)) (λ b. b))[x := b] -> ((λ a. λ b. b a (a b b) (λ b. b)) (λ b. b))


я пока не разобралась с тем как пользоваться stack, а остается 5 минут, поэтому просто закину сюда что-то, что уже сделала из номера два
потом разложу нормально по файлам, не знаю, как это оценивать, но вот на всяк

associator
  :: (a, (b, c))
  -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

pairProd
  :: (a -> b)
  -> (c -> d)
  -> (a,c)
  -> (b,d)
 pairProd f g (a, c) = ((f a), (g c))
 
 weirdFunction
  :: (d -> d -> b)
  -> (a -> b -> c)
  -> (d -> b)
  -> d -> b
weirdFunction f g e d = e d
