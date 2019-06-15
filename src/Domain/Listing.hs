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
  { address :: Text
  , phoneNumber :: Phone
  , price   :: Maybe Double
  , listingID :: Maybe ListingID
  , listingType :: Maybe ListingType
  } deriving (Eq, Ord, Show)

data VerifiedListing = VerifiedListing
  { verifiedAddress :: Text
  , verifiedPhoneNumber :: Phone
  , verifiedPrice   :: Double
  , verifiedListingID :: ListingID
  , verifiedListingType :: ListingType
  } deriving (Eq, Ord, Show)


data ListingType = Complete | WithoutKitchen | WithoutBathroom | RoomOnly
  deriving (Eq, Ord, Show)

data PriceFilter = Range Double Double | UpperLimit Double | LowerLimit Double
  deriving (Eq, Ord, Show)

data ListFilter = FilterBy
  { filterAddress :: Maybe Text
  , filterPrice   :: PriceFilter
  , filterListingType   :: Maybe ListingType
  } deriving (Eq, Ord, Show)


type ListingID = Int

data NewListingError = ListingExistError
  | ListingInfoNotCompleteError
  deriving (Eq, Ord, Show)

data ExistingListingError = ListingNotFound
  deriving (Eq, Ord, Show)

class (Monad m) => ListingRepo m where
  addSubmittedListing :: Listing -> m (Either NewListingError ())
  addVerifiedListing :: VerifiedListing -> m (Either NewListingError ())
  confirmListing :: ListingID -> VerifiedListing -> m ()
  getListing :: ListingID -> m (Either ExistingListingError Listing)
  getListings :: ListFilter -> m [Listing]
  updateListing :: ListingID -> m (Either ExistingListingError ())
  deleteListing :: ListingID -> m (Either ExistingListingError ())
