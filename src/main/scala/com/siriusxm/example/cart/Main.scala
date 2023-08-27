package com.siriusxm.example.cart

import cats.effect.{IO, IOApp}
import org.http4s.ember.client.EmberClientBuilder

object Main extends IOApp.Simple {
  def run: IO[Unit] =
    EmberClientBuilder
      .default[IO]
      .build
      .use(httpClient => {
        val productsClient = ProductsClient.make(httpClient)
        for {
          _ <- ShoppingCart.makeInMemory(productsClient)
        } yield ()
      })
}
