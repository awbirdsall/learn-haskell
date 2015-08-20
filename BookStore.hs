-- file: ch03/BookStore.hs
-- defining a new type requires a type constructor and a value constructor
data BookInfo = Book Int String [String]
                deriving (Show)
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

-- creating a new value of a type requires the value constructor 'function' 
myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- a synonym gives a more descriptive name to a type
type CustomerID = Int
type ReviewBody = String
-- often the type and value constructor have the same name
data BookReview = BookReview BookInfo CustomerID ReviewBody

-- a synonym can also name a verbose type
type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

-- algebraic data type can have multiple value constructors
data BillingInfo = CreditCard CardNumber CardHolder Address | CashOnDelivery | Invoice CustomerID
    deriving (Show)

-- pattern matching on algebraic data type:
bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

-- "_" is wildcard for pattern matching. Avoids compiler complaints of unused
-- variables.
nicerID      (Book id _ _) = id
nicerTitle   (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

-- more succinctly, can use record syntax to make accessor function for each named field at same time as making the type
data Customer = Customer {
    customerID      :: CustomerID,
    customerName    :: String,
    customerAddress :: Address
    } deriving (Show)

-- usual application syntax can still be used
customer1 = Customer 271828 "J.R. Hacker" ["255 Syntax Ct","Milpitas CA","USA"]

-- or you can use a more verbose notaion with the record syntax
customer2 = Customer {
    customerID = 271828,
    customerAddress = ["1048576 Disk Drive","Milpitas CA","USA"],
    customerName = "Jane Q. Citizen"
}

