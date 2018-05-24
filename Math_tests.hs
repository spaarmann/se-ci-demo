import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Math

main = defaultMain tests

tests = [
  testGroup "Commutativity"
    [
      testProperty "add_comm" prop_add_comm,
      testProperty "mult_comm" prop_mult_comm
    ],
  testGroup "Inverses"
    [
      testProperty "add_sub_inverse" prop_add_sub_inverse,
      testProperty "mult_divide_inverse" prop_mult_divide_inverse
    ]
  ]

prop_add_comm x y = (add x y) == (add y x)
  where types = (x :: Rational, y :: Rational)
prop_mult_comm x y = (mult x y) == (mult y x)
  where types = (x :: Rational, y :: Rational)

prop_add_sub_inverse x y = (sub (add x y) y) == x
  where types = (x :: Rational, y :: Rational)
prop_mult_divide_inverse x y = y /= 0 ==> (mult (divide x y) y) == x
  where types = (x :: Rational, y :: Rational)
