module TestDool where

import Test.Tasty
import Test.Tasty.QuickCheck

import Dool

data D = D Dool

instance Show D where
  show (D d) = show d

instance Arbitrary D where
  arbitrary = arbitrary >>= return . D . (==.0)

-- relations

prop_transitive_eq_1 d1 d2 d3 =
  isTrue (d1 ==. d2 &&. d2 ==. d3 =>. d1 ==. d3)
prop_transitive_eq_2 d =
  isTrue (d ==. d &&. d ==. d =>. d ==. d)
prop_transitive_lte_1 d1 d2 d3 =
  isTrue (d1 <=. d2 &&. d2 <=. d3 =>. d1 <=. d3)
prop_transitive_lte_2 d =
  isTrue (d <=. d &&. d <=. d =>. d <=. d)
prop_transitive_lt d1 d2 d3 =
  isTrue (d1 <. d2 &&. d2 <. d3 =>. d1 <. d3)
prop_transitive_gt d1 d2 d3 =
  isTrue (d1 >. d2 &&. d2 >. d3 =>. d1 >. d3)
prop_transitive_gte_1 d1 d2 d3 =
  isTrue ((d1 >=. d2 &&. d2 >=. d3) =>. (d1 >=. d3))
prop_transitive_gte_2 d =
  isTrue ((d >=. d &&. d >=. d) =>. (d >=. d))
prop_antisymmetric_lte_1 d1 d2 =
  isTrue (d1 <=. d2 &&. d2 <=. d1 =>. d1 ==. d2)
prop_antisymmetric_lte_2 d =
  isTrue (d <=. d &&. d <=. d =>. d ==. d)
prop_antisymmetric_gte_1 d1 d2 =
  isTrue (d1 >=. d2 &&. d2 >=. d1 =>. d1 ==. d2)
prop_antisymmetric_gte_2 d =
  isTrue (d >=. d &&. d >=. d =>. d ==. d)
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
prop_total_lte_1 d1 d2 =
  isTrue (d1 <=. d2 ||. d2 <=. d1)
prop_total_lte_2 d =
  isTrue (d <=. d ||. d <=. d)
prop_total_gte_1 d1 d2 =
  isTrue (d1 >=. d2 ||. d2 >=. d1)
prop_total_gte_2 d =
  isTrue (d >=. d ||. d >=. d)

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

relations = testGroup "relations"
  [ testProperty "transitive eq" prop_transitive_eq_1
  , testProperty "transitive eq edge case" prop_transitive_eq_2
  , testProperty "transitive lte" prop_transitive_lte_1
  , testProperty "transitive lte edge case" prop_transitive_lte_2
  , testProperty "transitive lt" prop_transitive_lt
  , testProperty "transitive gt" prop_transitive_gt
  , testProperty "transitive gte" prop_transitive_gte_1
  , testProperty "transitive gte edge case" prop_transitive_gte_2
  , testProperty "antisymmetric lte" prop_antisymmetric_lte_1
  , testProperty "antisymmetric lte edge case" prop_antisymmetric_lte_2
  , testProperty "antisymmetric gte" prop_antisymmetric_gte_1
  , testProperty "antisymmetric gte edge case" prop_antisymmetric_gte_2
  , testProperty "assymetric lt" prop_assymetric_lt
  , testProperty "assymetric gt" prop_assymetric_gt
  , testProperty "reflexive eq" prop_reflexive_eq
  , testProperty "reflexive lte" prop_reflexive_lte
  , testProperty "reflexive gte" prop_reflexive_gte
  , testProperty "not eq" prop_not_eq
  , testProperty "not neq" prop_not_neq
  , testProperty "not lte" prop_not_lte
  , testProperty "not lt" prop_not_lt
  , testProperty "not gt" prop_not_gt
  , testProperty "not gte" prop_not_gte
  , testProperty "total lte" prop_total_lte_1
  , testProperty "total lte edge case" prop_total_lte_2
  , testProperty "total gte" prop_total_gte_1
  , testProperty "total gte edge case" prop_total_gte_2
  ]

connectives = testGroup "logical connectives"
  [ testProperty "weakened identity con" prop_identity_con
  , testProperty "weakened identity dis" prop_identity_dis
  , testProperty "weakened dominance con" prop_dominance_con
  , testProperty "weakened dominance dis" prop_dominance_dis
  , testProperty "weakened tautology" prop_tautology
  , testProperty "weakened contradiction" prop_contradiction
  , testProperty "idempotence con" prop_idempotence_con
  , testProperty "idempotence dis" prop_idempotence_dis
  , testProperty "double negation" prop_double_negation
  , testProperty "commutative con" prop_commutative_con
  , testProperty "commutative dis" prop_commutative_dis
  , testProperty "associative con" prop_associative_con
  , testProperty "associative dis" prop_associative_dis
  , testProperty "distributive con" prop_distributive_con
  , testProperty "distributive dis" prop_distributive_dis
  , testProperty "demorgan con" prop_demorgan_con
  , testProperty "demorgan dis" prop_demorgan_dis
  , testProperty "implication" prop_implication
  , testProperty "contrapositive" prop_contrapositive
  ]
