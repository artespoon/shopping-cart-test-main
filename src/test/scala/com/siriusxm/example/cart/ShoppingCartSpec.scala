package com.siriusxm.example.cart

import cats.effect.{IO, Ref}
import com.siriusxm.example.cart.model._
import eu.timepit.refined.auto._
import munit.CatsEffectSuite

class ShoppingCartSpec extends CatsEffectSuite {

  test("should return shopping cart totals when cart is empty") {
    for {
      products <- Ref.of[IO, Map[ProductName, Price]](Map.empty)
      shoppingCart <- ShoppingCart.makeInMemory[IO](productsClient(products))
      cart <- shoppingCart.getCart
    } yield {
      assertEquals(cart.products, List.empty)
      assertEquals(cart.totals, CartTotals(BigDecimal(0), BigDecimal(0)))
    }
  }

  test("should successfully add multiple products to shopping cart using addProduct method") {
    val product1Name: ProductName = "product1"
    val product2Name: ProductName = "product2"

    val products = Map[ProductName, Price](
      product1Name -> BigDecimal("2.52"),
      product2Name -> BigDecimal("9.98")
    )

    for {
      products <- Ref.of[IO, Map[ProductName, Price]](products)
      shoppingCart <- ShoppingCart.makeInMemory[IO](productsClient(products))
      _ <- shoppingCart.addProduct(product1Name, 2)
      _ <- shoppingCart.addProduct(product2Name, 1)
      cart <- shoppingCart.getCart
    } yield {
      assertEquals(
        cart.products.map { case (productName, productCart) =>
          (productName, productCart.product.price.value, productCart.quantity.value)
        }.toSet,
        Set(
          (product1Name, BigDecimal("2.52"), 2),
          (product2Name, BigDecimal("9.98"), 1)
        )
      )
    }
  }

  test("should successfully add multiple products to shopping cart using addProducts method") {
    val product1Name: ProductName = "product1"
    val product2Name: ProductName = "product2"

    val products = Map[ProductName, Price](
      product1Name -> BigDecimal("2.52"),
      product2Name -> BigDecimal("9.98")
    )

    for {
      products <- Ref.of[IO, Map[ProductName, Price]](products)
      shoppingCart <- ShoppingCart.makeInMemory[IO](productsClient(products))
      _ <- shoppingCart.addProducts(List((product1Name, 2), (product2Name, 1)))
      cart <- shoppingCart.getCart
    } yield {
      assertEquals(
        cart.products.map { case (productName, productCart) =>
          (productName, productCart.product.price.value, productCart.quantity.value)
        }.toSet,
        Set(
          (product1Name, BigDecimal("2.52"), 2),
          (product2Name, BigDecimal("9.98"), 1)
        )
      )
    }
  }

  test("should return correct cart totals when added multiple products") {
    val product1Name: ProductName = "product1"
    val product2Name: ProductName = "product2"

    val products = Map[ProductName, Price](
      product1Name -> BigDecimal("2.52"),
      product2Name -> BigDecimal("9.98")
    )

    for {
      products <- Ref.of[IO, Map[ProductName, Price]](products)
      shoppingCart <- ShoppingCart.makeInMemory[IO](productsClient(products))
      _ <- shoppingCart.addProduct(product1Name, 2)
      _ <- shoppingCart.addProduct(product2Name, 1)
      cart <- shoppingCart.getCart
    } yield {
      val cartTotals = CartTotals(subTotal = BigDecimal("15.02"), tax = BigDecimal("1.88"))
      assertEquals(cart.totals, cartTotals)
      assertEquals(cartTotals.total, BigDecimal("16.90"))
    }
  }

  test("should successfully override the product in shopping cart when adding the same product again") {
    val productName: ProductName = "product"
    val products = Map[ProductName, Price](productName -> BigDecimal(0))

    for {
      products <- Ref.of[IO, Map[ProductName, Price]](products)
      shoppingCart <- ShoppingCart.makeInMemory[IO](productsClient(products))
      _ <- shoppingCart.addProduct(productName, 2)
      _ <- shoppingCart.addProduct(productName, 9)
      cart <- shoppingCart.getCart
    } yield {
      assertEquals(
        cart.products.map { case (productName, productCart) =>
          (productName, productCart.quantity.value)
        }.toSet,
        Set((productName, 9))
      )
    }
  }

  test("should fail when products client fails to retrieve a product") {
    for {
      products <- Ref.of[IO, Map[ProductName, Price]](Map.empty)
      shoppingCart <- ShoppingCart.makeInMemory[IO](productsClient(products))
      _ <- interceptMessageIO[RuntimeException]("Failed to retrieve the product") {
        shoppingCart.addProduct("nonExistingProduct", 1)
      }
    } yield ()
  }

  def productsClient(products: Ref[IO, Map[ProductName, Price]]): ProductsClient[IO] = new ProductsClient[IO] {
    override def fetchProduct(productName: ProductName): IO[Product] = products.get.map(products =>
      products
        .get(productName)
        .fold(throw new RuntimeException("Failed to retrieve the product"))(Product(productName.value, _))
    )
  }
}
