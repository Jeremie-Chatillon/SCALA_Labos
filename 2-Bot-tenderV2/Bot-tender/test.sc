case class Book (title: String, authors: List[String])

val books: Set[Book] = Set(
  Book("Structure and Interpretation of Computer Programs",
    List("Abelson, Harald", "Sussman, Gerald J.")
  ),
  Book("Introduction to Functional Programming",
    List("Bird, Richard", "Wadler, Phil")
  ),
  Book("Effective Java",
    List("Bloch, Joshua")
  ),
  Book("Java Puzzlers",
    List("Bloch, Joshua", "Gafter, Neal")
  ),
  Book("Programming in Scala",
    List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")
  )
)

def authorTwoBooks(books: Set[Book]) : Set[String] = {
  for {
    b1 <- books
    b2 <- books
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1
}

authorTwoBooks(books)
