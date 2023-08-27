package com.siriusxm.example

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.{NonNegative, Positive}
import eu.timepit.refined.string.MatchesRegex
import io.circe.generic.JsonCodec
import io.circe.refined._

package object cart {

  object model {
    // Product name is a non-empty string without any white-space
    type ProductName = String Refined MatchesRegex["^[^\\s]+$"]
    type Quantity = Int Refined Positive
    type Price = BigDecimal Refined NonNegative

    @JsonCodec
    final case class Product(title: String, price: Price)

    final case class ProductCart(product: Product, quantity: Quantity) {
      def subTotal: BigDecimal = product.price.value * quantity.value
    }

    final case class CartTotals(subTotal: BigDecimal, tax: BigDecimal) {
      def total: BigDecimal = subTotal + tax
    }

    final case class Cart(products: List[(ProductName, ProductCart)], totals: CartTotals)
  }
}
