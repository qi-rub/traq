-- | Back-trace Errors with pretty printing
module QCompose.Data.Errors (MyError (..), throwErrorMessage) where

import Control.Monad.Except (MonadError, throwError)

data MyError
  = MessageE String
  | CatchE {curr_error, prev_error :: MyError}
  deriving (Eq)

instance Show MyError where
  show (MessageE e) = e
  show CatchE{prev_error, curr_error} = unlines [show prev_error, show curr_error]

throwErrorMessage :: (MonadError MyError m) => String -> m a
throwErrorMessage = throwError . MessageE
