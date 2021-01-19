module Lib
  ( forceElems
  ) 
where

-- Doesn't work with newtype as forcing to WHNF penetrates the 
-- wrapper, so too strict
data Box a = Box a

unBox :: Box a -> a
unBox (Box x) = x

instance Functor Box where
  fmap f box = pure f <*> box

instance Applicative Box where
  pure = Box
  
  -- @xBox@ has the form @(\x -> x `seq` Box x) a :: Box a@ 
  -- so eval to WHNF to trigger @seq@
  --
  -- fBox <*> xBox = Box (case xBox of { Box x -> unBox fBox x })
  fBox <*> xBox = Box (xBox `seq` unBox fBox (unBox xBox))

forceElems :: Traversable t => t a -> t a
forceElems  = unBox . traverse (\x -> x `seq` Box x)
   
-- newtype Codensity a = Codensity { runCodensity :: forall r . (a -> r) -> r }
--
-- instance Functor Codensity where
--  -- Too strict for test 8, but rest fine
--  -- fmap f (Codensity k1) = Codensity (\k -> k1 (k . f))     
--  fmap f cod = pure f <*> cod

-- instance Applicative Codensity where
--  pure x = Codensity (\k -> k x)

--  Codensity kf <*> Codensity kx = Codensity (\k -> k (kx (kf id)))
--  -- Too strict in general?
--  -- Codensity kf <*> Codensity kx = Codensity (\k -> kf (\f -> kx (\a -> k (f a)))) 

-- forceElems :: Traversable t => t a -> t a
-- forceElems  = (`runCodensity` id) . traverse (\x -> x `seq` pure x)
