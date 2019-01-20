-- | Custom type for dates, in this case Month.
-- This should, in time, be switched out in favour of a proper package
-- designed to handle all the complexities of dates.
module Language.Drasil.Data.Date
  ( -- Types
    Month(..)
  ) where

-- | Month abbreviations
data Month = Jan
           | Feb
           | Mar
           | Apr
           | May
           | Jun
           | Jul
           | Aug
           | Sep
           | Oct
           | Nov
           | Dec deriving (Eq, Ord)

instance Show Month where
  show Jan = "January"
  show Feb = "February"
  show Mar = "March"
  show Apr = "April"
  show May = "May"
  show Jun = "June"
  show Jul = "July"
  show Aug = "August"
  show Sep = "September"
  show Oct = "October"
  show Nov = "November"
  show Dec = "December"
