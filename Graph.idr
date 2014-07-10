
module Graph

infixr 7 ::

||| RDF Relation
data Relation = relation String

instance Show Relation where
  show (relation name) = name


||| RDF Statement
record Statement : Type -> Type -> Type where
       statement : (subject : a)
                -> (predicate : Relation)
                -> (object : b)
                -> Statement a b

instance (Show a, Show b) => Show (Statement a b) where
  show (statement subject predicate object) =
    show subject ++ " -- " ++ show predicate ++ " --> " ++ show object


||| RDF Graph
data Graph : Nat -> Type -> Type where
  Empty : Graph Z a
  (::)  : (r : a) -> (g : Graph n a) -> Graph (S n) a

instance Show a => Show (Graph n a) where
  show Empty    = "Empty"
  show (s :: g) = show s ++ "\n" ++ show g


||| Example Usage
record Person : Type where
       person : (name : String) -> (age  : Int)
    -> Person

instance Show Person where
  show p = name p

bob : Person
bob = person "Bob" 23

ana : Person
ana = person "Ana" 19

tom : Person
tom = person "Tom" 30

knows : Person -> Person -> Statement Person Person
knows p1 p2 = statement p1 (relation "knows") p2


namespace Main
  main : IO ()
  main = do
    print $ (ana `knows` bob) :: (bob `knows` tom) :: Empty

