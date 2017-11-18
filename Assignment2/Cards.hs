-- | Data types for card games
module Cards where

import Test.QuickCheck
import System.Random

-- | A card has a rank and belongs to a suit.
data Card = Card { rank :: Rank, suit :: Suit }
            deriving (Eq, Show)

instance Arbitrary Card where
  arbitrary = do suit <- arbitrary
                 rank <- arbitrary
                 return (Card rank suit)

-- | All the different suits.
data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

instance Arbitrary Suit where
  arbitrary = oneof [ return Hearts, return Spades
                    , return Diamonds, return Clubs ]

-- | A rank is either a numeric card, a face card, or an ace. The
-- numeric cards range from two to ten.
data Rank = Numeric Integer | Jack | Queen | King | Ace
            deriving (Eq, Show)

instance Arbitrary Rank where
  arbitrary = frequency [ (1, return Jack)
                        , (1, return Queen)
                        , (1, return King)
                        , (1, return Ace)
                        , (9, do n <- choose (2, 10)
                                 return (Numeric n))
                        ]

-- | A hand of cards. This data type can also be used to represent a
-- deck of cards.
data Hand = Empty | Add Card Hand
            deriving (Eq, Show)

-- | This instance on average yields larger hands than the one given in
-- the lecture.
instance Arbitrary Hand where
  arbitrary = frequency [  (1,  return Empty)
                        ,  (10, do card  <-  arbitrary
                                   hand  <-  arbitrary
                                   return (Add card hand))
                        ]

-- | The size of a hand.
size :: Num a => Hand -> a
size Empty            = 0
size (Add card hand)  = 1 + size hand


-- We also need to be able to generate random number generators. (This
-- does not really belong in this file, but is placed here to reduce
-- the number of files needed.)
instance Arbitrary StdGen where
  arbitrary = do n <- arbitrary
                 return (mkStdGen n)
