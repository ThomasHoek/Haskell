
data Persoon = Persoon { naam :: String  
,adres :: String  
,telefoonnummer :: Maybe Int  
,huisdieren :: Int } deriving (Show)  

data Huisdieren = Dog String | Cat String | Hamster Int
 
type PersoonHuisdieren = [Eithertriple Honden Katten Knaagdieren]

-- data TripleEither a b c =  Either a | Either b |  Either c



newpersoon = Persoon "Lisa" "1234bz" Nothing 5 

-- type MijnProduct = TripleEither Honden Katten Knaagdieren 
