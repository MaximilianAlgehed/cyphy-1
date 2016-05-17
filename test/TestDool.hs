module TestDool where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

-- import Zelus ()
import Dool

data D = D Dool deriving (Show)

instance Arbitrary D where
  arbitrary =
    do d <- arbitrary
       return (D (if d >= 0 then d+1 else d-1))

-- relations


prop_lte_gte_eq :: Double -> Double -> Bool
prop_lte_gte_eq a b = (lte &&. gte =>. eq) == (eq =>. lte &&. gte)
  where
    lte = a <=. b
    gte = a >=. b
    eq = a ==. b

-- prop_transitive_lte
-- prop_transitive_lt
-- prop_transitive_gt
-- prop_transitive_gte

-- prop_antisymmetric_lte
-- prop_antisymmetric_lt
-- prop_antisymmetric_gt
-- prop_antisymmetric_gte


-- prop_negate_lte
-- prop_negate_lt
-- prop_negate_gt
-- prop_negate_gte

-- <=.
-- <.
-- >.
-- >=.



-- logical connectives

prop_identity_con (D d) =
  (d &&. true) <= true        -- weakenened identity  (d &&. true) == d
prop_identity_dis (D d) =
  (d ||. false) >= false      -- weakenened identity  (d ||. false) == d
prop_dominance_con (D d) =
  (d &&. false) <= false      -- weakenened dominance  (d &&. false) == false
prop_dominance_dis (D d) =
  (d ||. true) >= true        -- weakenened dominance  (d ||. true) == true
prop_tautology (D d) =
  (d ||. nt d) >= true        -- weakenened tautology  (d ||. nt d) == true
prop_contradiction (D d) =
  (d &&. nt d) <= false       -- weakenened contradiction (d &&. nt d) == false
prop_idempotence_con (D d) =
  (d ||. d) == d
prop_idempotence_dis (D d) =
  (d &&. d) == d
prop_double_negation (D d) =
  (nt (nt d)) == d
prop_commutative_con (D d1) (D d2) =
  (d1 &&. d2) == (d2 &&. d1)
prop_commutative_dis (D d1) (D d2) =
  (d1 ||. d2) == (d2 ||. d1)
prop_associative_con (D d1) (D d2) (D d3) =
  (d1 &&. (d2 &&. d3)) == ((d1 &&. d2) &&. d3)
prop_associative_dis (D d1) (D d2) (D d3) =
  (d1 ||. (d2 ||. d3)) == ((d1 ||. d2) ||. d3)
prop_distributive_con (D d1) (D d2) (D d3) =
  (d1 &&. (d2 ||. d3)) == ((d1 &&. d2) ||. (d1 &&. d3))
prop_distributive_dis(D d1) (D d2) (D d3) =
  (d1 ||. (d2 &&. d3)) == ((d1 ||. d2) &&. (d1 ||. d3))
prop_demorgan_con (D d1) (D d2) =
  (nt (d1 &&. d2)) == (nt d1 ||. nt d2)
prop_demorgan_dis (D d1) (D d2) =
  (nt (d1 ||. d2)) == (nt d1 &&. nt d2)
prop_implication (D d1) (D d2) =
  (d1 =>. d2) == (nt d1 ||. d2)
prop_contrapositive (D d1) (D d2) =
  (d1 =>. d2) == (nt d2 =>. nt d1)

main = defaultMain $
  testGroup "test dool"
    [ relations
    , connectives
    ]

relations =  testGroup "relations" []

connectives = testGroup "logical connectives"
  [ testProperty "weakened identity con"
      (prop_identity_con :: D -> Bool)
  , testProperty "weakened identity dis"
      (prop_identity_dis :: D -> Bool)
  , testProperty "weakened dominance con"
      (prop_dominance_con :: D -> Bool)
  , testProperty "weakened dominance dis"
      (prop_dominance_dis :: D -> Bool)
  , testProperty "weakened tautology"
      (prop_tautology :: D -> Bool)
  , testProperty "weakened contradiction"
      (prop_contradiction :: D -> Bool)
  , testProperty "idempotence con"
      (prop_idempotence_con :: D -> Bool)
  , testProperty "idempotence dis"
      (prop_idempotence_dis :: D -> Bool)
  , testProperty "double negation"
      (prop_double_negation :: D -> Bool)
  , testProperty "commutative con"
      (prop_commutative_con :: D -> D -> Bool)
  , testProperty "commutative dis"
      (prop_commutative_dis :: D -> D -> Bool)
  , testProperty "associative con"
      (prop_associative_con :: D -> D -> D -> Bool)
  , testProperty "associative dis"
      (prop_associative_dis :: D -> D -> D -> Bool)
  , testProperty "distributive con"
      (prop_distributive_con :: D -> D -> D -> Bool)
  , testProperty "distributive dis"
      (prop_distributive_dis :: D -> D -> D -> Bool)
  , testProperty "demorgan con"
      (prop_demorgan_con :: D -> D -> Bool)
  , testProperty "demorgan dis"
      (prop_demorgan_dis :: D -> D -> Bool)
  , testProperty "implication"
      (prop_implication :: D -> D -> Bool)
  , testProperty "contrapositive"
      (prop_contrapositive :: D -> D -> Bool)
  ]
