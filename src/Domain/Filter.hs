module Domain.Filter (filterWith) where

import ClassyPrelude
import qualified Domain.Listing as D

filterWith :: D.ListFilter -> D.VerifiedListing -> Bool
filterWith (D.FilterBy mayLocation mayPrice mayType) listing =
  let valLocation = D.verifiedLocation listing
      valPrice    = D.verifiedPrice listing
      valType     = D.verifiedListingType listing
  in mayCheckParam mayLocation valLocation 
       && mayCheckParam mayType valType
       && mayCheckPrice mayPrice valPrice

mayCheckParam :: Eq a => Maybe a -> a -> Bool
mayCheckParam Nothing _ = True
mayCheckParam (Just param) val = param == val

mayCheckPrice :: Maybe D.PriceFilter -> Double -> Bool
mayCheckPrice Nothing _ = True
mayCheckPrice (Just (D.Range uppLim lowLim)) price  = lowLim <= price && price <= uppLim
mayCheckPrice (Just (D.UpperLimit uppLim)) price = price <= uppLim
mayCheckPrice (Just (D.LowerLimit lowLim)) price = lowLim <= price
