package Data

import Chat.Tokens
import Chat.Tokens.Token

object Products {
  // TODO: step 2 - here your will have an attribute that will contain the products (e.g. "bière"), their types (e.g. "Boxer"), and their prices (e.g. 2.0).
  // TODO: step 2 - You will also have to find a way to store the default type/brand of a product.
  val products : Map[(this.ProductsType.ProductType, String), Float] = Map(
    (this.ProductsType.BIERE,"boxer") -> 1.0f,
    (this.ProductsType.BIERE,"farmer") -> 1.0f,
    (this.ProductsType.BIERE,"wittekop") -> 2.0f,
    (this.ProductsType.BIERE, "punkipa") -> 3.0f,
    (this.ProductsType.BIERE, "jackhammer") -> 3.0f,
    (this.ProductsType.BIERE, "tenebreuse") -> 4.0f,
    (this.ProductsType.CROISSANT, "maison") -> 2.0f,
    (this.ProductsType.CROISSANT, "cailler") -> 2.0f,
  )

  // TODO optimisé avec une fonction
  val default_products : Map[this.ProductsType.ProductType, String] = Map (
    this.ProductsType.BIERE -> "boxer",
    this.ProductsType.CROISSANT -> "maison"
  )

  object ProductsType {
    type ProductType = String

    val BIERE = "biere"
    val CROISSANT = "croissant"
  }
}
