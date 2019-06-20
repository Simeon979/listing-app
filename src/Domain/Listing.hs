module Domain.Listing
  ( Listing(..)
  , Phone (..)
  , VerifiedListing(..)
  , ListingType(..)
  , PriceFilter(..)
  , ListFilter(..)
  , ListingID
  , NewListingError(..)
  , ExistingListingError(..)

  , ListingRepo(..)
  ) where

import ClassyPrelude

newtype Phone = Phone { getPhone :: Text }
  deriving (Eq, Ord, Show)

data Listing = Listing
  { location :: Text
  , address :: Text
  , phoneNumber :: Phone
  , price   :: Maybe Double
  , listingID :: ListingID
  , listingType :: Maybe ListingType
  } deriving (Eq, Ord)

data VerifiedListing = VerifiedListing
  { verifiedLocation :: Text
  , verifiedAddress :: Text
  , verifiedPhoneNumber :: Phone
  , verifiedPrice   :: Double
  , verifiedListingID :: ListingID
  , verifiedListingType :: ListingType
  } deriving (Eq, Ord)

instance Show Listing where
  show listing = unpack $ location listing <> " " <> address listing <> " " <> tshow (price listing)

instance Show VerifiedListing where
  show listing = unpack $ verifiedLocation listing <> " " <> verifiedAddress listing <> " " <> tshow (verifiedPrice listing)

data ListingType = Complete | WithoutKitchen | WithoutBathroom | RoomOnly
  deriving (Eq, Ord, Show)

data PriceFilter = Range Double Double | UpperLimit Double | LowerLimit Double
  deriving (Eq, Ord, Show)

data ListFilter = FilterBy
  { filterLocation :: Maybe Text
  , filterPrice   :: Maybe PriceFilter
  , filterListingType   :: Maybe ListingType
  } deriving (Eq, Ord, Show)


type ListingID = Int

data NewListingError = ListingExistError
  | ListingInfoNotCompleteError
  deriving (Eq, Ord, Show)

data ExistingListingError = ListingNotFound
  deriving (Eq, Ord, Show)

class (Monad m) => ListingRepo m where
  -- add a user submitted list, needs to be verified
  addSubmittedListing :: Listing -> m (Either NewListingError ListingID)
  -- add an admin submitted listing, does not need to be verified
  addVerifiedListing :: VerifiedListing -> m (Either NewListingError ListingID)
  -- admin confirm a submitted list
  confirmListing :: ListingID -> VerifiedListing -> m (Either ExistingListingError ())
  getSubmittedListing :: ListingID -> m (Either ExistingListingError Listing)
  getVerifiedListing :: ListingID -> m (Either ExistingListingError VerifiedListing)
  -- get submitted listings
  getSubmittedListings :: m [Listing]
  -- get available admin verified listings according to filter
  getVerifiedListings :: ListFilter -> m [VerifiedListing]
  updateSubmittedListing :: ListingID -> Listing -> m (Either ExistingListingError ())
  -- given the id of a listing, update it with a supplied verified listing 
  updateVerifiedListing :: ListingID -> VerifiedListing -> m (Either ExistingListingError ())
  -- delete a user submitted listing
  deleteSubmittedListing :: ListingID -> m ()
  -- delete an admin verified listing
  deleteVerifiedListing :: ListingID -> m ()
