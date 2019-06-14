module Domain.Listing
  ( Listing(..)
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

data Listing = Listing
  { address :: Text
  , phoneNumber :: Phone
  , price   :: Maybe Double
  , listingID :: Maybe ListingID
  , listingType :: Maybe ListingType
  }

data VerifiedListing = VerifiedListing
  { verifiedAddress :: Text
  , verifiedPhoneNumber :: Phone
  , verifiedPrice   :: Double
  , verifiedListingID :: ListingID
  , verifiedListingType :: ListingType
  }

data ListingType = Complete | WithoutKitchen | WithoutBathroom | RoomOnly

data PriceFilter = Range Double Double | UpperLimit Double | LowerLimit Double

data ListFilter = FilterBy
  { filterAddress :: Maybe Text
  , filterPrice   :: PriceFilter
  , filterListingType   :: Maybe ListingType
  }

type ListingID = Int

data NewListingError = ListingExistError
  | ListingInfoNotCompleteError

data ExistingListingError = ListingNotFound

class (Monad m) => ListingRepo m where
  addListing :: Listing -> m (Either NewListingError ())
  confirmListing :: ListingID -> VerifiedListing -> m ()
  getListing :: ListingID -> m (Either ExistingListingError Listing)
  getListings :: ListFilter -> m [Listing]
  updateListing :: ListingID -> m (Either ExistingListingError ())
  deleteListing :: ListingID -> m (Either ExistingListingError ())
