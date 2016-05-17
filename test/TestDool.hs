module TestDool where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

-- import Zelus ()
import Dool

data D = D Dool

instance Show D where
  show (D d) = "truthness = " ++ show (value d)

instance Arbitrary D where
  arbitrary = arbitrary >>= return . D . (==.0)

-- relations

prop_transitive_eq d1 d2 d3 =
  isTrue (d1 ==. d2 &&. d2 ==. d3 =>. d1 ==. d3)
prop_transitive_lte d1 d2 d3 =
  isTrue (d1 <=. d2 &&. d2 <=. d3 =>. d1 <=. d3)
prop_transitive_lt d1 d2 d3 =
  isTrue (d1 <. d2 &&. d2 <. d3 =>. d1 <. d3)
prop_transitive_gt d1 d2 d3 =
  isTrue (d1 >. d2 &&. d2 >. d3 =>. d1 >. d3)
prop_transitive_gte d1 d2 d3 =
  isTrue ((d1 >=. d2 &&. d2 >=. d3) =>. (d1 >=. d3))
prop_antisymmetric_lte d1 d2 =
  isTrue (d1 <=. d2 &&. d2 <=. d1 =>. d1 ==. d2)
prop_antisymmetric_gte d1 d2 =
  isTrue (d1 >=. d2 &&. d2 >=. d1 =>. d1 ==. d2)
prop_assymetric_lt d1 d2 =
  isTrue (d1 <. d2 =>. nt (d2 <. d1))
prop_assymetric_gt d1 d2 =
  isTrue (d1 >. d2 =>. nt (d2 >. d1))
prop_reflexive_eq d =
  isTrue (d ==. d)
prop_reflexive_lte d =
  isTrue (d <=. d)
prop_reflexive_gte d =
  isTrue (d >=. d)
prop_not_eq d1 d2 =
  (d1 ==. d2) == nt (d1 /=. d2)
prop_not_neq d1 d2 =
  (d1 /=. d2) == nt (d1 ==. d2)
prop_not_lte d1 d2 =
  (d1 <=. d2) == nt (d1 >. d2)
prop_not_lt d1 d2 =
  (d1 <. d2) == nt (d1 >=. d2)
prop_not_gt d1 d2 =
  (d1 >. d2) == nt (d1 <=. d2)
prop_not_gte d1 d2 =
  (d1 >=. d2) == nt (d1 <. d2)
prop_total_lte d1 d2 =
  isTrue (d1 <=. d2 ||. d2 <=. d1)
prop_total_gte d1 d2 =
  isTrue (d1 >=. d2 ||. d2 >=. d1)

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

relations =  testGroup "relations"
  [ testProperty "transitive eq"
      (prop_transitive_eq :: Double -> Double -> Double -> Bool)
  , testProperty "transitive lte"
      (prop_transitive_lte :: Double -> Double -> Double -> Bool)
  , testProperty "transitive lt"
      (prop_transitive_lt :: Double -> Double -> Double -> Bool)
  , testProperty "transitive gt"
      (prop_transitive_gt :: Double -> Double -> Double -> Bool)
  , testProperty "transitive gte"
      (prop_transitive_gte :: Double -> Double -> Double -> Bool)
  , testProperty "antisymmetric lte"
      (prop_antisymmetric_lte :: Double -> Double -> Bool)
  , testProperty ""
      (prop_antisymmetric_gte :: Double -> Double -> Bool)
  , testProperty "assymetric lt"
      (prop_assymetric_lt :: Double -> Double -> Bool)
  , testProperty "assymetric gt"
      (prop_assymetric_gt :: Double -> Double -> Bool)
  , testProperty "reflexive eq"
      (prop_reflexive_eq :: Double -> Bool)
  , testProperty "reflexive lte"
      (prop_reflexive_lte :: Double -> Bool)
  , testProperty "reflexive gte"
      (prop_reflexive_gte :: Double -> Bool)
  , testProperty "not eq"
      (prop_not_eq :: Double -> Double -> Bool)
  , testProperty "not neq"
      (prop_not_neq :: Double -> Double -> Bool)
  , testProperty "not lte"
      (prop_not_lte :: Double -> Double -> Bool)
  , testProperty "not lt"
      (prop_not_lt :: Double -> Double -> Bool)
  , testProperty "not gt"
     (prop_not_gt :: Double -> Double -> Bool)
  , testProperty "not gte"
      (prop_not_gte :: Double -> Double -> Bool)
  , testProperty "total lte"
      (prop_total_lte :: Double -> Double -> Bool)
  , testProperty "total gte"
      (prop_total_gte :: Double -> Double -> Bool)
  ]

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
