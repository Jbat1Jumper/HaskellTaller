type BookName = String
type BookId = Int
type BookAuthor = String
type BookAuthors = [BookAuthor]

--data BookInfo = Book BookId BookName BookAuthors
--			  | AnonimousBook BookId BookName
--			 deriving(Eq, Show)
			  
--bookId :: BookInfo -> BookId
--bookId (Book id _ _) = id
--bookId (AnonimousBook id _) = id

data BookInfo = Book {
				bookId :: BookId,
				bookName :: BookName,
				bookAuthors :: BookAuthors }
			  | AnonymousBook {
				bookId :: BookId,
				bookName :: BookName }
			deriving(Eq, Show)

--weird
sameId :: BookInfo -> BookInfo -> Bool
sameId (Book id1 _ _) (Book id2 _ _) = id1 == id2
sameId (Book id1 _ _) (AnonymousBook id2 _) = id1 == id2
sameId (AnonymousBook id1 _) (Book id2 _ _) = id1 == id2
sameId (AnonymousBook id1 _) (AnonymousBook id2 _) = id1 == id2

--much much better
sameId2 :: BookInfo -> BookInfo -> Bool
sameId2 a b = bookId(a) == bookId(b)

sameAuthors :: BookInfo -> BookInfo -> Bool
sameAuthors (AnonymousBook _ _) _ = undefined
sameAuthors _ (AnonymousBook _ _) = undefined
sameAuthors a b = bookAuthors(a) == bookAuthors(b)

hasAuthor :: BookAuthor -> BookInfo -> Bool
hasAuthor _ (AnonymousBook _ _) = undefined
hasAuthor author b = isIn (bookAuthors(b)) author

isIn :: (Eq a) => [a] -> a -> Bool
isIn [] _ = False
isIn xs x | head xs == x = True
isIn xs x | head xs /= x = isIn (tail xs) x