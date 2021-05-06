# tasty-inspection-tasting

Integrate [`inspection-testing`](http://hackage.haskell.org/package/inspection-testing)
into [`tasty`](http://hackage.haskell.org/package/tasty) test suites.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

import Test.Tasty
import Test.Tasty.Inspection

lhs :: (a -> b) -> Maybe a -> Bool
lhs f x = case fmap f x of
  Nothing -> True
  Just{}  -> False

rhs :: (a -> b) -> Maybe a -> Bool
rhs _ Nothing = True
rhs _ Just{}  = False

main :: IO ()
main = defaultMain $(inspectTest $ 'lhs === 'rhs)
```
