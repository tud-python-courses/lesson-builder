module Util where

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left err) = Left $ f err
mapLeft _ (Right v)  = Right v
