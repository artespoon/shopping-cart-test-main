package com.siriusxm.example.cart

import cats.effect.Async
import com.siriusxm.example.cart.model._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.client.Client
import org.http4s.implicits._

trait ProductsClient[F[_]] {
  def fetchProduct(productName: ProductName): F[Product]
}

object ProductsClient {

  private val baseUri = uri"https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main"

  def make[F[_]: Async](httpClient: Client[F]): ProductsClient[F] = new ProductsClient[F] {
    override def fetchProduct(productName: ProductName): F[Product] =
      httpClient.expect[Product](baseUri / s"${productName.value.toLowerCase}.json")
  }
}
