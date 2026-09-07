package com.timushev.sbt.updates.metadata

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import sbt.ModuleID

class RepositoryPatternSpec extends AnyFreeSpec with Matchers {
  private val module = ModuleID("org.example", "sample", "1.0")

  "Repository patterns" - {
    "locate revisions in Maven and Ivy layouts with either organization spelling" in {
      for (organization <- Seq("organisation", "organization")) {
        val pattern = s"https://repo/[$organization]/[module]/[revision]/[artifact].[ext]"
        RepositoryPattern.revisionPrefix(pattern, module, "org/example") shouldBe Some(
          "https://repo/org/example/sample/"
        )
        RepositoryPattern.revisionPrefix(pattern, module, module.organization) shouldBe Some(
          "https://repo/org.example/sample/"
        )
      }
    }
    "substitute configuration and extra attributes, including overrides" in {
      val configured = module.withConfigurations(Some("compile")).extra("scalaVersion" -> "3", "module" -> "override")
      RepositoryPattern.revisionPrefix(
        "https://repo/[module]/[conf]/scala_[scalaVersion]/[revision]/[artifact]",
        configured,
        configured.organization
      ) shouldBe Some("https://repo/override/compile/scala_3/")
    }
    "only accept revision as the first unresolved token" in {
      for (pattern <- Seq("[artifact]/[revision]", "[conf]/[revision]", "fixed/path", "([revision])/file"))
        RepositoryPattern.revisionPrefix(pattern, module, module.organization) shouldBe None
      RepositoryPattern.revisionPrefix("release-[revision]/file", module, module.organization) shouldBe Some("release-")
    }
    "preserve the token root before a literal optional section" in {
      RepositoryPattern.revisionPrefix("https://repo/(literal)/[revision]", module, module.organization) shouldBe Some(
        "https://repo/"
      )
    }
    "match Ivy optional token substitution semantics" in {
      val tokens   = Map("module" -> "sample", "empty" -> "")
      val examples = Seq(
        "[module]/[revision]"          -> "sample/[revision]",
        "(scala_[missing]/)[revision]" -> "[revision]",
        "([empty])/x"                  -> "/x",
        "(prefix-[module])/x"          -> "prefix-sample/x",
        "(literal)/x"                  -> "(literal)/x",
        "([missing]-[module])/x"       -> "null-sample/x",
        "([module]-[missing])/x"       -> "/x",
        "([empty][module])/x"          -> "sample/x",
        "([module][empty])/x"          -> "/x",
        "[]/[revision]"                -> "[]/[revision]"
      )
      examples.foreach { case (pattern, expected) =>
        withClue(pattern)(RepositoryPattern.substitute(pattern, tokens) shouldBe expected)
      }
    }
    "leave replacement values literal" in {
      RepositoryPattern.substitute("[module]/[revision]", Map("module" -> "$1\\name")) shouldBe "$1\\name/[revision]"
    }
    "handle token delimiters introduced by replacement values" in {
      RepositoryPattern.revisionPrefix("[module]/fixed", module.withName("a["), module.organization) shouldBe None
      RepositoryPattern.revisionPrefix("[module]/[revision]", module.withName("a("), module.organization) shouldBe Some(
        "a"
      )
      RepositoryPattern.revisionPrefix(
        "[module]/[revision]",
        module.withName("a[unknown]"),
        module.organization
      ) shouldBe None
    }
    "reject malformed patterns" in {
      for (pattern <- Seq("[[module]]", "[module", "module]", "((x))", "(x", "x)"))
        withClue(pattern)(intercept[IllegalArgumentException](RepositoryPattern.substitute(pattern, Map.empty)))
    }
  }
}
