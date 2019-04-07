package Chat

import Chat.Tokens._
import Data.Products.ProductsType
import Tree._
import Data.Products.default_products
import Data.UsersInfo._


// TODO - step 4
class Parser(tokenizer: Tokenizer) {
  import tokenizer._

  var curTuple: (String, Token) = ("unknown", UNKNOWN)
  
  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = nextToken()

  /** "Eats" the expected token, or terminates with an error. */
  private def eat(token: Token): Unit = if (token == curToken) readToken() else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  // TODO (BONUS): find a way to display the string value of the tokens (e.g. "BIERE") instead of their integer value (e.g. 6).
  private def expected(token: Token, more: Token*): Nothing =
    fatalError(" expected: " +
      (token :: more.toList).mkString(" or ") +
      ", found: " + curToken)

  def fatalError(msg: String): Nothing = {
    println("Fatal error", msg)
    new Exception().printStackTrace()
    sys.exit(1)
  }

  /** the root method of the parser: parses an entry phrase */
  // TODO Je m'appelle
  def parsePhrases() : ExprTree = {
    if (curToken == BONJOUR) eat(BONJOUR)
    if (curToken == JE ){
      eat(JE)
      /** ETRE */
      if(curToken == ETRE) {
        eat(ETRE)
        /** ASSOIFEE */
        if (curToken == ASSOIFFE) {
          // Here we do not "eat" the token, because we want to have a custom 2-parameters "expected" if the user gave a wrong token.
          readToken()
          Thirsty()
        }
        /** AFFAME */
        else if (curToken == AFFAME) {
          readToken()
          Hungry()
        /** PSEUDO */
        } else if (curToken == PSEUDO){
          Identification(curValue)
        } else expected(ASSOIFFE, AFFAME, PSEUDO)
      /** VOULOIR*/
      } else if (curToken == VOULOIR ) {
        readToken()
        /** Check if logged */
        if (userConnected() == null) {
          NotIdentified()
        } else {
          /** CONNAITRE */
          if (curToken == CONNAITRE) {
            eat(CONNAITRE)
            eat(MON)
            /** SOLDE */
            eat(SOLDE)
            Solde()
          } else {
            /** COMMANDER **/
            if (curToken == COMMANDER) {
              readToken()
            }
            Order(parsePhraseCommand())
          }
        }
      } else if( curToken == ME) {
        readToken()
        eat(APPELLER)
        if(curToken == PSEUDO) {
          Identification(curValue)
        } else expected(PSEUDO)
      } else expected(ETRE, VOULOIR, ME)
    /** LOGOUT */
    } else if (curToken == AUREVOIR) {
      readToken()
      LogOut()
    /** COMBIEN */
    } else if (curToken == COMBIEN) {
      readToken()
      /** COUTER */
      if(curToken == COUTER) {
        readToken()
        Price(parsePhraseCommand())
      } else expected(COUTER)
    } else expected(BONJOUR, JE, AUREVOIR, COMBIEN)
  }

  def parsePhraseCommand() : OrderRequest = {
    var tempExprTree : OrderRequest = null

    /** Nombre d'élément de la commande */
    if(curToken == NUM){
      val nbElement  = curValue
      readToken()
      /** CROISSANT */
      if (curToken == CROISSANT) {
        readToken()
        if(curToken == MARQUE) {
          tempExprTree = ProductOrder(nbElement.toInt,Product(ProductsType.CROISSANT,curValue))
          readToken()
        } else {
          // MARQUE PAR DEFAUT maison
          tempExprTree = ProductOrder(nbElement.toInt,Product(ProductsType.CROISSANT,default_products(ProductsType.CROISSANT)))
        }
      /** BIERE */
      } else if (curToken == BIERE) {
        readToken()
        if (curToken == MARQUE) {
          tempExprTree = ProductOrder(nbElement.toInt, Product(ProductsType.BIERE, curValue))
          readToken()
        } else if (curToken != UNKNOWN) {
          // Biere par défaut
          tempExprTree = ProductOrder(nbElement.toInt, Product(ProductsType.BIERE, default_products(ProductsType.BIERE)))
        } else expected(MARQUE)
      } else if(curToken == MARQUE) {
        tempExprTree = ProductOrder(nbElement.toInt, Product(ProductsType.BIERE, curValue))
        readToken()
      } else expected(BIERE, CROISSANT, MARQUE)
    } else expected(NUM)
    /** OU */
    if(curToken == OU){
      readToken()
      Or(tempExprTree, parsePhraseCommand())
    /** ET */
    } else if(curToken == ET){
      readToken()
      And(tempExprTree, parsePhraseCommand())
    } else {
      tempExprTree
    }
  }

  // Start the process by reading the first token.
  readToken()
}
