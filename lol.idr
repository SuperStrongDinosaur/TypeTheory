
module Main

--my_cong : {f : a -> b} -> {x : a} -> {y : a} -> (x = y) -> (f x = f y)
--my_cong Refl = Refl

--cong_zero_cong : {a : Nat} -> Z + a = a + Z -> S(Z + a) = S(a + Z)
--cong_zero_cong p = my_cong p

lemmaAbbA : a = b -> b = a
lemmaAbbA Refl = Refl

lemma0 : (a:Nat) -> (b:Nat) -> S(a + b) = a + S b
lemma0 Z b = Refl 
lemma0 (S k) b = cong(lemma0 k b)  
                       
lemma1 : (a:Nat) -> (b:Nat) -> a + S b = S (a + b)
lemma1 a b = lemmaAbbA (lemma0 a b) 

zeroComm : (a:Nat) -> Z + a = a + Z
zeroComm Z = Refl
zeroComm (S k) = cong (zeroComm k)

dualComm : (a:Nat) -> (b:Nat) -> a + b = b + a
dualComm Z b = zeroComm b
dualComm (S k) b = believe_me ()

zero_comm : (a : Nat) -> Z + a = a + Z
zero_comm Z = Refl
zero_comm (S a) = cong (zero_comm a)

main:IO()
main = putStrLn "Hello, world!"