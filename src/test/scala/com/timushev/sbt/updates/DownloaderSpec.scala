package com.timushev.sbt.updates

import java.io.ByteArrayInputStream
import java.net.{URL, URLConnection, URLStreamHandler}

import com.timushev.sbt.updates.authentication.RepositoryAuthentication
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import sbt.ConsoleLogger

class DownloaderSpec extends AnyFreeSpec with Matchers {
  "Downloader" - {
    "identify the plugin while preserving repository headers and authentication" in {
      var connection: URLConnection = null
      val url                       = new URL(
        null,
        "https://repo.example/metadata",
        new URLStreamHandler {
          override def openConnection(url: URL): URLConnection = {
            connection = new URLConnection(url) {
              override def connect(): Unit = ()
              override def getInputStream  = new ByteArrayInputStream(Array[Byte](42))
            }
            connection
          }
        }
      )
      val authentication = RepositoryAuthentication(
        Some("repository"),
        None,
        None,
        "user",
        "password",
        Seq("X-Repository-Token" -> "token")
      )
      val downloader = new Downloader("repository", Seq(authentication), ConsoleLogger())
      val stream     = downloader.startDownload(url)
      try stream.read() shouldBe 42
      finally stream.close()
      connection.getRequestProperty("User-Agent") shouldBe "sbt-updates"
      connection.getRequestProperty("Accept") shouldBe "*/*"
      connection.getRequestProperty("Authorization") shouldBe "Basic dXNlcjpwYXNzd29yZA=="
      connection.getRequestProperty("X-Repository-Token") shouldBe "token"
    }
  }
}
