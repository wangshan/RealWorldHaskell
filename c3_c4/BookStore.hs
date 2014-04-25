data BookInfo = Book Int String [String] deriving (Show)

myBookInfo = Book 1396543205 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDevelory
                 | Invoice CustomerID
                 deriving (Show)

myBill = CreditCard "2901650221064486" "Thomas Gradgrind" ["London", "England"]

bookID (Book id _ _) = id
bookTitle (Book _ title _) = title
