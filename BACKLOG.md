* Make `ReplayT` a `newtype`
* Add instance-declarations for `ReplayT`:
  - MonadIO
  - MonadTrans
      question says this is not possible because of the Show- and
      Read-constraints
  - Monad{Reader,Writer,Consumer}
* Write test-cases for Replay
