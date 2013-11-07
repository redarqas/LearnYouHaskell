import Control.Monad

-- Monad plus use case

data Context = Home | Mobile | Business deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+355-652-55512")]
nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"), (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]
twalumba = [(Business, "+260-02-55-5121")]

-- Call to a friend, home number if not mobile number

onePersonalPhone :: [(Context, String)] -> Maybe Phone
onePersonalPhone an = case lookup Home an of 
                        Nothing -> lookup Mobile an
                        p -> p

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones an = map snd numbers 
          where numbers = case filter (contextIs Business) an of 
                            [] -> filter (contextIs Mobile) an
                            rs -> rs 

contextIs :: Context -> (Context, Phone) -> Bool
contextIs a (c, _) = a == c

-- Extract the pattern with MonadPlus instance

oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone an = lookup Home an `mplus` lookup Mobile an  



allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones an = map snd $ filter(contextIs Business) an `mplus`
                                 filter(contextIs Mobile) an


