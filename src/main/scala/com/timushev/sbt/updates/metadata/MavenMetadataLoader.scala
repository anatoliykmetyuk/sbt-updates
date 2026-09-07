package com.timushev.sbt.updates.metadata

import java.net.URI

import com.timushev.sbt.updates.Downloader
import com.timushev.sbt.updates.versions.Version
import sbt.{ModuleID, URLRepository}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.XML

class MavenMetadataLoader(repo: URLRepository, downloader: Downloader) extends MetadataLoader {
  def getVersions(module: ModuleID): Future[Seq[Version]] =
    Future
      .sequence(
        repo.patterns.artifactPatterns
          .flatMap(url(_, module))
          .map(download)
          .map(_.map(extractVersions))
      )
      .map(_.flatten)

  private def url(pattern: String, module: ModuleID): Option[String] =
    RepositoryPattern
      .revisionPrefix(pattern, module, module.organization.split("\\.").mkString("/"))
      .map(_ + "maven-metadata.xml")

  def extractVersions(metadata: xml.Elem): Seq[Version] =
    (metadata \ "versioning" \ "versions" \ "version").map(_.text).map(Version.apply)

  private def download(url: String) =
    Future {
      XML.load(downloader.startDownload(new URI(url).toURL))
    }

}
