module BlackJack where
import Cards
import RunGame
import System.Random
import Test.QuickCheck hiding (shuffle)

implementation = Interface
    { iEmpty    = empty
    , iFullDeck = fullDeck
    , iValue    = value
    , iGameOver = gameOver
    , iWinner   = winner 
    , iDraw     = draw
    , iPlayBank = playBank
    , iShuffle  = shuffle
    }

{-
   Execution of `size hand2` is
   = size Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)
   = 1 + size (Add (Card Jack Spades) Empty)
   = 1 + (1 + size Empty)
   = 1 + (1 + (0))
   = 2
-}


hand0 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) (Add (Card Ace Clubs) Empty))
hand1 = Add (Card King Hearts) Empty 

-- initialize empty hand
empty :: Hand
empty = Empty

-- return total rank of cards in hand
value :: Hand -> Integer
value hand 
    | value' hand <= 21 = value' hand
    -- if value is over 21, fix Aces
    | otherwise         = value' hand - 10 * numberOfAces hand 
    where value' Empty           = 0
          value' (Add card hand) = valueCard card + value' hand

-- return value of card
valueCard :: Card -> Integer
valueCard Card {rank=r, suit=s} = valueRank r

-- return value of rank
valueRank :: Rank -> Integer
valueRank Ace         = 11
valueRank (Numeric n) = n
-- covers cases for jack, queen, king
valueRank _           = 10

-- return number of aces in hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                      = 0
numberOfAces (Add (Card Ace suit) hand) = 1 + numberOfAces hand
numberOfAces (Add (Card _ suit) hand)   = numberOfAces hand

-- return True if value of cards in hand is over 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- return winner of two hands. first hand is guest, other is bank
winner :: Hand -> Hand -> Player
winner guestHand bankHand
    | guest <= 21 && (bank > 21 || bank < guest) = Guest
    | otherwise                                  = Bank
    where guest = value guestHand
          bank  = value bankHand

-- add second hand at the end of first hand
(<+) :: Hand -> Hand -> Hand
(<+) Empty             hand1 = hand1
(<+) (Add card0 hand0) hand1 = Add card0 ((<+) hand0 hand1)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand0 hand1 = size hand0 + size hand1 == size (hand0 <+ hand1)

-- fullDeck :: Hand
fullDeck :: Hand
fullDeck = foldl (<+) Empty $ map handOfSuit [Hearts, Spades, Diamonds, Clubs]

-- return hand with all ranks of given suit
handOfSuit :: Suit -> Hand
handOfSuit suit = handOfSuit' allRanks
    where handOfSuit' []           = Empty
          handOfSuit' (rank:ranks) = Add (Card rank suit) (handOfSuit' ranks)

-- return list of all ranks
allRanks :: [Rank]
allRanks = map Numeric [2..10] ++ [Jack, Queen, King, Ace]

-- deck, hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand0             = error "draw: deck is empty"
draw (Add card0 deck0) hand0 = (deck0, Add card0 hand0)

-- bank draws until value>=16
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand 
    | value bankHand < 16 = playBank' deck' bankHand'
    | otherwise           = bankHand
    where (deck', bankHand') = draw deck bankHand

-- shuffle by removing card from deck1 and add to deck0, 
-- until deck1 becomes empty.
shuffle :: StdGen -> Hand -> Hand
shuffle gen Empty = Empty
shuffle gen deck  = removedHand <+ shuffle gen' remainingHand
    where (i, gen') = randomR (0, size deck-1) gen
          (removedHand, remainingHand) = removeHand deck i

-- split hand into 3 hands: part0|part1|part2
-- part1 is the hand we want to remove. then
-- return part1, part0 and part2 combined.
removeHand :: Hand -> Int -> (Hand, Hand)
removeHand hand n = (part1, part0 <+ part2)
    where (part0, rest) = splitCards hand n
          (part1, part2) = splitCards rest 1

-- split hand at nth position.
splitCards :: Hand -> Int -> (Hand, Hand)
splitCards deck  0           = (Empty, deck)
splitCards Empty n           = error "splitCards: deck is empty"
splitCards (Add card deck) n = ((Add card deck'), deck'')
    where (deck', deck'')    = splitCards deck (n-1)

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
        c `belongsTo` h == c `belongsTo` shuffle g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)

main :: IO ()
main = runGame implementation
