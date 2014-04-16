type BookName = String
type BookId = Int
type BookAuthors = [String]

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


sameId (Book id1 _ _) (Book id2 _ _) = id1 == id2
