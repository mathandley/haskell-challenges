module Lib
  ( forceElems
  ) 
where

-- Doesn't work with newtype
data Box a = Box a

unBox :: Box a -> a
unBox (Box x) = x

instance Functor Box where
  fmap f box = pure f <*> box

instance Applicative Box where
  pure = Box
  
  -- Doesn't work without the case
  fBox <*> xBox = Box (case xBox of { Box x -> unBox fBox x })

forceElems :: Traversable t => t a -> t a
forceElems  
  = unBox 
  . traverse (\x -> x `seq` Box x)
