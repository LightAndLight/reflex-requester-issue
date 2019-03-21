{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
module Main where

import Reflex
import Reflex.Host.Basic
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Char
import Data.Functor.Identity
import Data.Map (Map)
import Debug.Trace (trace, traceShow)

import qualified Data.Map as Map

network ::
  forall t m.
  ( Reflex t, MonadHold t m, MonadFix m
  , Requester t m, Request m ~ Identity, Response m ~ Identity
  , Adjustable t m
  , PostBuild t m
  ) =>
  Event t String ->
  m (Dynamic t (Map Int String))
network eLine = do
  ePostBuild <- getPostBuild

  rec
    let
      {-
       The problem is that `eLine` (line 63) and 'all the `eLine'`s (line 65)' from the
       requester are considered different events. So eModify fires twice per input; once
       for the `eLine` event, and once for the requester's event. This would be fine, but for
       some reason both events sample `dStuff` at the same time.

       I would expect one of two things to happen: either eModify fires only once per input,
       or eModify fires twice, and the dynamic is updated in between firings

      -}
      eModify :: Event t (Map Int String)
      eModify =
        traceEvent "event" $
        attachWith
          (\cur m ->
             Map.foldr
               (\v ->
                  if all isDigit v
                  then Map.insert (read v) ""
                  else id)
               (trace ("current: " <> show (fst <$> cur)) m)
               m)
          (current dStuff)
          (switchDyn $ mergeMap . fmap snd <$> dStuff)

    dStuff :: Dynamic t (Map Int (String, Event t String)) <-
      listHoldWithKey
        mempty
        (mergeWith
          (Map.unionWith (<|>))
          [ Map.singleton 0 (Just "") <$ ePostBuild
          , fmap Just <$> eModify
          ])
        (\k v -> do
            if k == 0
              then pure (v, eLine)
              else do
                eLine' <- fmap (fmap runIdentity) . requesting $ Identity <$> eLine
                pure (v, eLine'))

  pure $ fmap fst <$> dStuff

main :: IO ()
main =
  basicHostForever $ do
    (eLine, sendLine) <- newTriggerEvent
    repeatUntilQuit (getLine >>= sendLine) never
    rec
      (dMap, eRequests) <- runRequesterT (network eLine) eResponses
      eResponses <- performEvent $ traverseRequesterData pure <$> eRequests
    performEvent_ $ liftIO . putStrLn . ("value: " <>) . show <$> updated dMap
    pure ()
