Replay
======

Design
------
The replay monad is implemented in the module `Control.Monad.Replay`. For a
complete overview of all the bindings that this module exports the reader is
refered to the haddock documentation for the module. Below I highlight some
design-considerations as well point out some minor ways in which I have deviated
form the specification.

### The `ReplayT` type
`ReplayT` is a type-synonym for the more complicated type:

    ConsumerT (Item r) (ExceptT q (WriterT (Trace r) m)) a

And by this virtue should be considered a shallow embeddding of the problem
domain. The advantage of it being a type-synonum is that one gets the functor-,
applicative- and monad-instances for free. The down-side is that it also exposes
some instance-declarations that one might be interested in hiding. For instance,
we might not be interested in users of the library interacting directly with the
state that the monad is wrapping. Another downside is that type-synonyms can not
have instance-declarations.[^1] One could solve both of these problems by making
`ReplayT` a newtype:

    newtype ReplayT q r m a = ReplayT
        ConsumerT (Item r) (ExceptT q (WriterT (Trace r) m)) a

But since we are not going be declaring any instance-declarations for `ReplayT`
(see the discussion on `MonadTrans` and `MonadIO` below) this is not a
hinderance.

### The `ConsumerT` type
I wrote a small wrapper around the state-monad-transformer and called it
`ConsumerT`. Haddock documentation has also been written for this module if it
is of any interest. `ConsumerT` has as internal state a list of values and
allows the head of this list to be inspected with `peek` and allows for that
value to be "popped" with `next`:

    peek, next :: MonadConsumer s m => m (Maybe s)

### Generalization of functions
I have deviated slightly from the specification in that `Replay` does not wrap
the `IO`-monad, but rather the `Identity`-monad. The reason for doing this is
that it is similiar in style to the way the `mtl`-library is implemented.

`Replay` as defined in the specification can then be implemented like so:

    ReplayT q r IO a

I have also generalized `io` to:

    io :: (Show a, Read a, MonadIO m) => IO a -> ReplayT q r m  a

From:

    io :: (Show a, Read a)            => IO a -> ReplayT q r IO a
    
Again, it is simply a matter of instantiating the type of `m` to `IO` to get the
type from the spec (since `IO` is indeed an instance of `MonadIO`).

I have also chosen to swap the type-parameters of `ReplayT`. This is also to be
more in line with the style from `mtl`.

[^1]: This is, however, possible with `TypeSynonymInstances`.

`MonadIO` and `MonadTrans`
--------------------------
It would be nice if it was possible to make ReplayT an instance of `MonadIO` and
`MonadTrans` since the semantics of these typeclasses are captured by `io` and
`liftR` respectively. However, this is unfortunately not possible. Consider for
instance the type-class `MonadIO`:

    class Monad m => MonadIO (m :: * -> *) where
        liftIO :: IO a -> m a
        
It would be nice to make `MonadIO m => ReplayT q r m` an instance of this
type-class in the following way:

    instance MonadIO m => MonadIO (ReplayT q r m)

So now we can compare the type of `io` with that of `liftIO` specialized to this
case:
      
    io     :: (MonadIO m, Show a, Read a) => IO a -> ReplayT q r m a
    liftIO :: (MonadIO m)                 => IO a -> ReplayT q r m a

It is clear to see that it is in no way possible to implement `liftIO` in terms
of `io` because we do not have the `Show` and `Read` constraints available. We
could of course define `liftIO` in some other way, but it wouldn't be able to do
what `io` does.

The pattern repeats itself if we try to make `ReplayT` an instance of `MonadTrans`:

    class MonadTrans (t :: (* -> *) -> * -> *) where
        lift :: Monad m => m a -> t m a

    instance MonadTrans m => MonadTrans (ReplayT q r)

Now, `lift` should have the type:

    (Monad m)                 => m a -> ReplayT q r m a
    
But `liftR` has the type

    (Monad m, Show a, Read a) => m a -> ReplayT q r m a
    
Again, we need more constraints than we have at our disposal.

Testing
-------
Tests can be found in the following files:

    test/Test.hs
    test/TestCase.hs
    test/TestProp.hs

The first file contains the `main`-function that executes the other tests.
Test-cases are found in `TestCase.hs` and property-based testing using the
library `QuickCheck` can be found in `TestProp.hs`.

I've chosen to follow the layout of the handed-out test-file for the test-cases,
but I believe that they are very limited. I have identified two problems:

 * We can not test how `ask` works if there are no results for the question in
   the input-stream.
 * We cannot test the "memoization"-mechanism of the replay monad. That is, we
   can not test that the result of an some computation is deferred in preference
   for a previously computed value in the input-stream.

For this reason I've just added 2 very simple test-cases in this module.

The property-based checks that I have written test the two interface-methods
`ask` and `liftR`. The test-cases for these functions are `testAsk` and
`testLift` respectively. The test-cases also serve as a nice concise
specification for how these functions work. Note that the types of these
functions are more general than what is really tested. I have made `Arbitrary a
=> Item a` an instance of `Arbitrary` in such a way that only the results `True`
and `False` are ever output to the trace. I did this intentionally since `lift`
if not total (it used `read` to decode values from `String`'s). I have not
written test-cases for `io` since it is just a special case of `liftR`.

I have not written a test-case for `run` since it does what `runReplayT` does
and then throws away some of that information. So it should be easy to *proove*
the correctness of `run` assuming `runReplayT` is correct.
