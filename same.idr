plus_commutes_Z : (n : Nat) -> plus Z n = plus n Z
plus_commutes_Z Z = Refl
plus_commutes_Z (S k) =
	rewrite plus_commutes_Z k in Refl

plus_commutes_S : (k : Nat) -> (n : Nat) -> S (plus n k) = plus n (S k)
plus_commutes_S k Z = Refl
plus_commutes_S k (S j) =
	rewrite plus_commutes_S k j in Refl

plus_commutes : (m, n : Nat) -> m + n = n + m
plus_commutes Z n = plus_commutes_Z n
plus_commutes (S k) n =
	rewrite plus_commutes k n in
	plus_commutes_S k n