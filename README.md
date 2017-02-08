Replay
======

Design
------
I've made `ReplayT` a type-synonym. Thereby it's wrapper around the more complicated type

    ConsumerT (Item r) (ExceptT q (WriterT (Trace r) m)) a

Issues with the current design
------------------------------
The advantage of this is that it is now an instance of `MonadError`,
`MonadWriter` etc.. for free. The downside is that I cannot override any
instance declarations. For instance I would like `ReplayT` to be an instance of
`MonadIO` where `liftIO = io` but this is sadly not possible.

    instance MonadIO (ReplayT q r IO) where
      liftIO = io

I've tried enabling `FlexibleInstances` and `TypeSynonymInstances` but then I
get this error:

    src/Control/Monad/Replay.hs:54:10-33: error: …
        • The type synonym ‘ReplayT’ should have 4 arguments, but has been given 3
        • In the instance declaration for ‘MonadIO (ReplayT q r IO)’
    Compilation failed.

And if I add the extra parameter in the instance-declaration I get the error
that the thing I'm declaraing has the wrong kind.

In general I would like to know how to design custom monad-transformers using
previously defined monad-transformers simlar to what I'm trying to acheive.
