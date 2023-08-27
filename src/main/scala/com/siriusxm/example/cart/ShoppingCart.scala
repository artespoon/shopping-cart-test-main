package com.siriusxm.example.cart

import cats.Applicative
import cats.effect.{Ref, Sync}
import cats.syntax.all._
import com.siriusxm.example.cart.model._
import eu.timepit.refined.auto._

trait ShoppingCart[F[_]] {
  def addProduct(productName: ProductName, quantity: Quantity): F[Unit]

  def addProducts(products: List[(ProductName, Quantity)])(implicit F: Applicative[F]): F[Unit] =
    products.traverse_ { case (productName, quantity) => addProduct(productName, quantity) }

  def getCart: F[Cart]
}

object ShoppingCart {

  private val tax = 0.125

  def makeInMemory[F[_]: Sync](productsClient: ProductsClient[F]): F[ShoppingCart[F]] =
    Ref.of[F, Map[ProductName, ProductCart]](Map.empty).map(new Impl(productsClient, _))

  private class Impl[F[_]: Sync](
      productsClient: ProductsClient[F],
      cart: Ref[F, Map[ProductName, ProductCart]])
      extends ShoppingCart[F] {

    def addProduct(productName: ProductName, quantity: Quantity): F[Unit] =
      for {
        product <- productsClient.fetchProduct(productName)
        _ <- cart.update(_.updatedWith(productName)(_ => Some(ProductCart(product, quantity))))
      } yield ()

    def getCart: F[Cart] =
      cart.get.map { cart =>
        val subTotal = cart.values.toList.map(_.subTotal).sum.setScale(2, BigDecimal.RoundingMode.UP)
        val taxTotal = (subTotal * tax).setScale(2, BigDecimal.RoundingMode.UP)
        Cart(
          cart.toList,
          CartTotals(
            subTotal,
            taxTotal
          )
        )
      }
  }
}
