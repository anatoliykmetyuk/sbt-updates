package com.timushev.sbt.updates.metadata

import java.io.FileNotFoundException
import java.net.URI

import com.timushev.sbt.updates.Downloader
import com.timushev.sbt.updates.metadata.extractor.HtmlVersionExtractor
import com.timushev.sbt.updates.versions.Version
import sbt.{IO, ModuleID, URLRepository}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.Exception.catching

object IvyMetadataLoader {
  val VersionExtractor = new HtmlVersionExtractor
}

class IvyMetadataLoader(repo: URLRepository, downloader: Downloader) extends MetadataLoader {
  def getVersions(module: ModuleID): Future[Seq[Version]] = {
    val prefixes = repo.patterns.artifactPatterns.flatMap(getRevisionPrefix(_, module))
    Future.sequence(prefixes.map(download)).map(_.flatten.flatMap(extractVersions))
  }

  private def getRevisionPrefix(pattern: String, module: ModuleID): Option[String] =
    RepositoryPattern.revisionPrefix(
      pattern,
      module,
      if (repo.patterns.isMavenCompatible) module.organization.replace('.', '/') else module.organization
    )

  private def download(url: String): Future[Option[String]] =
    Future {
      catching(classOf[FileNotFoundException]).opt {
        val is = downloader.startDownload(new URI(url).toURL)
        try IO.readStream(is)
        finally is.close()
      }
    }

  private def extractVersions(data: String): Seq[Version] =
    IvyMetadataLoader.VersionExtractor.applyOrElse(data, (_: String) => Nil)

}
