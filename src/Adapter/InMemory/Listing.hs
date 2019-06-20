module Adapter.InMemory.Listing where

import ClassyPrelude
import Control.Monad.Except
import Data.Has

import qualified Domain.Listing as D
import qualified Domain.Filter as F

data State = State
  { stateUnverifiedListings :: Map D.ListingID D.Listing
  , stateVerifiedListings :: Map D.ListingID D.VerifiedListing
  , stateListingIDCounter :: D.ListingID
  } deriving (Eq, Ord, Show)

initialState :: State
initialState = State
  { stateUnverifiedListings = mempty
  , stateVerifiedListings = mempty
  , stateListingIDCounter = 0
  }

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addSubmittedListing :: InMemory r m => D.Listing -> m (Either D.NewListingError D.ListingID)
addSubmittedListing listing = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverified = stateUnverifiedListings state
        isDuplicate = any (== listing) unverified
    when isDuplicate $ throwError D.ListingExistError
    let newListingIDCounter = stateListingIDCounter state + 1
        modifiedListing = listing { D.listingID = newListingIDCounter } -- overwrite garbage id
        newUnverifiedListings = insertMap newListingIDCounter modifiedListing unverified
        newState = state
          { stateUnverifiedListings = newUnverifiedListings
          , stateListingIDCounter = newListingIDCounter
          }
    lift $ writeTVar tvar newState
    return newListingIDCounter

addVerifiedListing :: InMemory r m => D.VerifiedListing -> m (Either D.NewListingError D.ListingID)
addVerifiedListing listing = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let verified = stateVerifiedListings state
        isDuplicate = any (== listing) verified
    when isDuplicate $ throwError D.ListingExistError
    let newListingIDCounter = stateListingIDCounter state + 1
        modifiedListing = listing { D.verifiedListingID = newListingIDCounter } -- override garbage id
        newVerifiedListings = insertMap newListingIDCounter modifiedListing verified
        newState = state
          { stateVerifiedListings = newVerifiedListings 
          , stateListingIDCounter = newListingIDCounter
          }

    lift $ writeTVar tvar newState 
    return newListingIDCounter

confirmListing :: InMemory r m
               => D.ListingID -> D.VerifiedListing -> m (Either D.ExistingListingError ())
confirmListing lId verified = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedListings state
    case lookup lId unverifieds of
      Nothing -> throwError D.ListingNotFound
      Just listing -> do
        let newUnverifieds = deleteMap lId unverifieds
            newVerifieds  = insertMap lId verified (stateVerifiedListings state)
            newState = state
              { stateUnverifiedListings = newUnverifieds
              , stateVerifiedListings = newVerifieds
              }
        lift $ writeTVar tvar newState

getSubmittedListing :: InMemory r m
                    => D.ListingID -> m (Either D.ExistingListingError D.Listing)
getSubmittedListing lId = do
  tvar <- asks getter
  atomically . runExceptT $ do
    unverifieds <- lift $ stateUnverifiedListings <$> readTVar tvar
    case lookup lId unverifieds of
      Nothing -> throwError D.ListingNotFound
      Just listing -> return listing

getVerifiedListing :: InMemory r m
                    => D.ListingID -> m (Either D.ExistingListingError D.VerifiedListing)
getVerifiedListing lId = do
  tvar <- asks getter
  atomically . runExceptT $ do
    verifieds <- lift $ stateVerifiedListings <$> readTVar tvar
    case lookup lId verifieds of
      Nothing -> throwError D.ListingNotFound
      Just listing -> return listing

getSubmittedListings :: InMemory r m => m [D.Listing]
getSubmittedListings = do
  tvar <- asks getter
  unverifieds <- stateUnverifiedListings <$> readTVarIO tvar
  return $  snd <$> mapToList unverifieds

getVerifiedListings :: InMemory r m
            => D.ListFilter -> m [D.VerifiedListing]
getVerifiedListings filterer = do
  tvar <- asks getter
  verifieds <- stateVerifiedListings <$> readTVarIO tvar
  let verifiedsList = snd <$> mapToList verifieds
  return $ filter (F.filterWith filterer) verifiedsList

deleteSubmittedListing :: InMemory r m => D.ListingID -> m ()
deleteSubmittedListing lId = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let unverifieds = stateUnverifiedListings state
        newUnverifieds = deleteMap lId unverifieds
        newState = state { stateUnverifiedListings = newUnverifieds }
    writeTVar tvar newState 

deleteVerifiedListing :: InMemory r m => D.ListingID -> m ()
deleteVerifiedListing lId = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let verifieds = stateVerifiedListings state
        newVerifieds = deleteMap lId verifieds
        newState = state { stateVerifiedListings = verifieds }
    writeTVar tvar newState 

updateSubmittedListing :: InMemory r m
                       => D.ListingID -> D.Listing -> m (Either D.ExistingListingError ())
updateSubmittedListing lId listing = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedListings state
{-
        listingExists = maybe False (const True) (lookup lId unverifieds)
    unless listingExists $ throwError D.ListingNotFound
-}
    case lookup lId unverifieds of
      Nothing -> throwError D.ListingNotFound
      _       -> do
        let newUnverifieds = updateMap (const $ Just listing) lId unverifieds
            newState = state { stateUnverifiedListings = newUnverifieds } 
        lift $ writeTVar tvar newState

updateVerifiedListing :: InMemory r m
                       => D.ListingID -> D.VerifiedListing -> m (Either D.ExistingListingError ())
updateVerifiedListing lId listing = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let verifieds = stateVerifiedListings state
    case lookup lId verifieds of
      Nothing -> throwError D.ListingNotFound
      _       -> do
        let newVerifieds = updateMap (const $ Just listing) lId verifieds
            newState = state { stateVerifiedListings = newVerifieds } 
        lift $ writeTVar tvar newState
