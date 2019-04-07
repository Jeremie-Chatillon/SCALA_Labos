package Chat

import Data.Products.ProductsType.ProductType
import Data.Products._
import Data.UsersInfo.getSolde
import Data.UsersInfo._
// TODO - step 3
object Tree {

  /**
    * This sealed trait represents a node of the tree and contains methods to compute it and write its output text in console.
    */
  sealed trait ExprTree {
    /**
      * Compute the price of the current node, then returns it. If the node is not a computational node, the method
      * returns 0.0.
      * For example if we had a "+" node, we would add the values of its two children, then return the result.
      * @return the result of the computation
      */
    def computePrice: Double = this match {
      case Identification(user: String) => 0.0
      case Order(orderRequest: OrderRequest) => orderRequest.computePrice
      case Product(productType: ProductType, brand: String) => products(productType, brand) // TODO Tester si l'élément n'existe pas
      case ProductOrder(number: Int, product: Product) => number * product.computePrice
      case Or(p1: OrderRequest, p2 : OrderRequest) => Math.min(p1.computePrice, p2.computePrice)
      case And(p1: OrderRequest, p2 : OrderRequest) => p1.computePrice + p2.computePrice
      case Price(orderRequest: OrderRequest) => orderRequest.computePrice
      case _ => 0.0
    }

    /**
      * Return the output text of the current node, in order to write it in console.
      * @return the output text of the current node
      */
    def reply: String = this match {
      // Example cases
      case Identification(user: String) => {
        login(user)
        "Bonjour, " + user + " !"
      }
      case LogOut() => {
        logout()
        "À la revoyure !"
      }
      case NotIdentified() => "Veuillez d'abord vous identifier."
      case Order(orderRequest: OrderRequest) => "Voici donc " + orderRequest.reply + " ! Cela coûte CHF " + orderRequest.computePrice + " et votre nouveau solde est de CHF " + purchase(orderRequest.computePrice) + "."
      case Product(productType: ProductType, brand: String) => productType + " " + brand // TODO Tester si l'élément n'existe pas
      case ProductOrder(number: Int, product: Product) => number.toString + " " + product.reply
      case Or(p1: OrderRequest, p2 : OrderRequest) => (if(p1.computePrice <= p2.computePrice) p1.reply else p2.reply) + " Car c'est le moins cher et que tu es pauvre"
      case And(p1: OrderRequest, p2 : OrderRequest) => p1.reply + " et " + p2.reply
      case Price(orderRequest: OrderRequest) => "Cela coûte CHF " + orderRequest.computePrice
      case Solde() => "Le montant actuel de votre solde est de CHF " + getSolde()
      case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case _ => ""
    }
  }

  /**
    * Declarations of the nodes' types.
    */
  // Example cases
  case class Identification(user: String) extends ExprTree

  case class Order(orderRequest: OrderRequest) extends ExprTree

  trait OrderRequest extends ExprTree
  case class ProductOrder(number : Int, product: Product) extends OrderRequest
  case class Product(productType: ProductType, brand : String) extends ExprTree

  case class Or(p1: OrderRequest, p2: OrderRequest) extends OrderRequest
  case class And(p1: OrderRequest, p2: OrderRequest) extends OrderRequest

  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree

  case class Solde() extends ExprTree
  case class LogOut() extends ExprTree

  case class Price(orderRequest: OrderRequest) extends OrderRequest
  case class NotIdentified() extends ExprTree
}
