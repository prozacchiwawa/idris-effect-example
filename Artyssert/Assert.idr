module Main

import Effects
import Effect.System

data AssertionState = NotFailed | Failed
data AssertionInfo : AssertionState -> Type where
     DI : AssertionInfo s

data AssertionEff : Effect where
     FailAssertion : sig AssertionEff () (AssertionInfo NotFailed) (AssertionInfo Failed)
     OkAssertion : sig AssertionEff () (AssertionInfo NotFailed) (AssertionInfo NotFailed)

ASSERTION : AssertionState -> EFFECT
ASSERTION t = MkEff (AssertionInfo t) AssertionEff

assert : String -> (x : Bool) -> Eff () [ASSERTION NotFailed] [ASSERTION (if x then NotFailed else Failed)]
assert s False = call FailAssertion
assert s True = call OkAssertion

doTryAssert : Eff () [ASSERTION NotFailed]
doTryAssert =
  do
    assert "foo" True

implementation Handler AssertionEff IO where
  handle res FailAssertion k = do
    putStrLn "Assertion failed"
    k () DI
  handle res OkAssertion k = do
    k () DI

--main : IO ()
--main = runInit [ASSERTION NotFailed] doTryAssert
