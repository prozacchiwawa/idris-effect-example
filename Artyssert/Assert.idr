module Main

import Effects
import Effect.StdIO
import Effect.System

{- A basic data type describing the kinds of things that the state of our effect
 - can be.  In our case, it's an as-yet unfailed assertion or a failed assertion.
 -
 - This can be thought of as 'ordinary data'.
 -}
data AssertionState = NotFailed | Failed

{- A type level function that wraps an effect state into a type. -}
data AssertionInfo : AssertionState -> Type where
     DI : AssertionInfo s

{- An effect is an ADT yielding Effect with specially constructed cases.
 - The cases are built by 'sig' in Effects, which takes either 3 or 4 arguments.
 - The 3 argument form is like:
 -  'sig eff 
         (result-type or unit if not readable result)
         (always-switches-to-this-state)'
 - The 4 argument form is like:
 -  'sig eff 
         (result-type or unit of not readable result) 
         (accept-this-state) 
         (move-to-this-state)'
 - Note that if parameters are bound, they can be used in these expressions.
 -}
data AssertionEff : Effect where
     FailAssertion : sig AssertionEff () (AssertionInfo NotFailed) (AssertionInfo Failed)
     OkAssertion : sig AssertionEff () (AssertionInfo NotFailed) (AssertionInfo NotFailed)

{- The EFFECT is what makes what would in purescript have been called an 
 - 'effect row' before 0.12.  This wraps the AssertionState value into a
 - proper effect that idris' effect system recognizes as such.  It includes
 - a reference to our ADT yielding Effect which it will use to find a Handler
 - instance, which contains the actions the runtime system will take when
 - confronted with a 'call' of one of the data labels in the Effect.
 -}
ASSERTION : AssertionState -> EFFECT
ASSERTION t = MkEff (AssertionInfo t) AssertionEff

{- A Handler contains code which runs in a monad and performs actions that
 - a specific effect requires.  These are generally IO actions, and will in
 - many cases call FFI wrapped IO functions.  In any case, it implements the
 - actual resulting mutations to the universe represented by the labels in
 - the Effect.  The final action of these forwards a result via the injected
 - function k which takes the result of the operation (here all unit) and
 - the resulting operating state of the effect (here reflected via the DI
 - constructor above to be whatever the associated effect action label desired.
 -}
implementation Handler AssertionEff IO where
  handle res FailAssertion k = do
    putStrLn "Assertion failed"
    k () DI
  handle res OkAssertion k = do
    k () DI

{- Default does this:
 - Given an expected start state for an Effect (expressed in Type form), return
 - a compatible Type that satisfies the desired type.  Any expression that yields
 - this type works here.  I don't quite understand why the Hangman example uses
 - a GADT for this, but I'm sometimes confused by Idris GADTs anyway.  In my case,
 - I just reflected the type via the DI constructor.
 -}
implementation Default (AssertionInfo NotFailed) where
  default = DI

{- Yay, we've got to the point where we're making a callable function.  If the
 - expression x given is False, then the type given to ASSERTION will be 
 - (AssertionInfo Failed), which doesn't match the expected outcome of
 - (AssertionInfo NotFailed), which means the code is malformed.  In order for
 - code using this effect to be well typed, we must prove that x is True in
 - every case.
 -}
assert : (x : Bool) -> Eff () [ASSERTION NotFailed] [ASSERTION (if x then NotFailed else Failed)]
assert False = call FailAssertion
assert True = call OkAssertion

{- Try calling it.  Hangman example splits the code into 'game', which gives
 - Eff three arguments; the result of the whole expression, a starting effect
 - state and an expected ending effect state and 'runGame', which uses the
 - two argument form as below.  This seems necessary to allow the Default
 - instance above to apply to three argument invocation.
 -}
doTryAssert : Eff () [ASSERTION NotFailed] [ASSERTION NotFailed]
doTryAssert =
  do
    assert True

-- FailAssert is not well typed.
-- failAssert : Eff () [ASSERTION NotFailed] [ASSERTION NotFailed]
-- failAssert =
--   do
--     assert False

{- A function suitable for use with Effect's "run" function.
 - calls doTryAssert above (3 argument form of Eff) with an expected target
 - effect row and uses Default instance above to yield the starting state.
 -}
runAssertions : Eff () [ASSERTION NotFailed]
runAssertions = do
  doTryAssert

main : IO ()
main = run runAssertions
