package com.timushev.sbt.updates.metadata

import java.io.ByteArrayInputStream
import java.net.{URI, URL}
import java.nio.charset.StandardCharsets.UTF_8

import com.timushev.sbt.updates.Downloader
import com.timushev.sbt.updates.versions.Version
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import sbt.{ConsoleLogger, ModuleID, Patterns, Resolver}

import scala.concurrent.Await
import scala.concurrent.duration._

class MetadataLoaderSpec extends AnyFreeSpec with Matchers {
  private val module = ModuleID("org.example", "sample", "1.0")
  private val root   = new URI("https://repo.example/").toURL

  private def downloader(expectedUrl: String, response: String) = new Downloader("test", Nil, ConsoleLogger()) {
    override def startDownload(url: URL) = {
      url.toString shouldBe expectedUrl
      new ByteArrayInputStream(response.getBytes(UTF_8))
    }
  }

  "Metadata loaders" - {
    "fetch Maven metadata and return its available versions" in {
      val repo   = Resolver.url("test", root)(Resolver.mavenStylePatterns)
      val loader = new MavenMetadataLoader(
        repo,
        downloader(
          "https://repo.example/org/example/sample/maven-metadata.xml",
          "<metadata><versioning><versions><version>1.0</version><version>2.0</version></versions></versioning></metadata>"
        )
      )
      Await.result(loader.getVersions(module), 5.seconds) shouldBe Seq(Version("1.0"), Version("2.0"))
    }
    "fetch Ivy directory listings with optional cross-version attributes" in {
      val repo = Resolver.url("test", root)(
        Patterns(
          "[organisation]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[artifact].[ext]"
        ).withIsMavenCompatible(false)
      )
      val loader = new IvyMetadataLoader(
        repo,
        downloader(
          "https://repo.example/org.example/sample/scala_3/",
          "<html><a href=\"1.0/\">1.0/</a><a href=\"2.0/\">2.0/</a></html>"
        )
      )
      Await.result(loader.getVersions(module.extra("scalaVersion" -> "3")), 5.seconds) shouldBe
        Seq(Version("1.0"), Version("2.0"))
    }
    "skip patterns with unresolved attributes before the revision without downloading" in {
      val repo   = Resolver.url("test", root)(Patterns("[unknown]/[revision]/[artifact]"))
      val unused = new Downloader("test", Nil, ConsoleLogger()) {
        override def startDownload(url: URL) = fail("Unexpected download: " + url)
      }
      for (loader <- Seq(new MavenMetadataLoader(repo, unused), new IvyMetadataLoader(repo, unused)))
        Await.result(loader.getVersions(module), 5.seconds) shouldBe empty
    }
  }
}
