-- | Back-trace Errors with pretty printing
module Traq.Data.Errors (MyError (..), throwErrorMessage) where

import Control.Monad.Except (MonadError, throwError)

data MyError
  = MessageE String
  | CatchE {curr_error, prev_error :: MyError}
  deriving (Eq)

maxErrorMessageLength :: Int
maxErrorMessageLength = 800

shorten :: String -> String
shorten s | length s < maxErrorMessageLength = s
shorten s = take maxErrorMessageLength s ++ " [...]"

instance Show MyError where
  show (MessageE e) = shorten e
  show CatchE{prev_error, curr_error} = unlines [show prev_error, show curr_error]

throwErrorMessage :: (MonadError MyError m) => String -> m a
throwErrorMessage = throwError . MessageE
