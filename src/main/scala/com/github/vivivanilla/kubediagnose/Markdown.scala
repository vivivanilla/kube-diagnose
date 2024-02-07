package com.github.vivivanilla.kubediagnose

trait Markdown {
  def rendered: String
}

case class MarkdownBlocks(blocks: Seq[Markdown]) extends Markdown {
  def rendered = blocks
    .filter {
      case MarkdownEmpty() => false
      case _               => true
    }
    .map(_.rendered)
    .mkString("\n\n")
}

case class MarkdownTable(rows: Seq[Seq[String]]) extends Markdown {
  val columns = rows.map(_.size).max

  val columnWidths = for {
    col <- 0 until columns
  } yield { rows.map(row => if (col < row.size) row(col).length else 0).max }

  def rendered = if (rows.size > 0) {
    renderRow(0) + "\n" + separatorLine + "\n" +
      (for {
        row <- 1 until rows.size
      } yield renderRow(row)).mkString("\n")
  } else ""

  def renderRow(num: Int): String = "| " + (for {
    col <- 0 until columns
  } yield padRight(
    if (col < rows(num).size) rows(num)(col).replace("\n", "<br>") else "",
    columnWidths(col)
  )).mkString(" | ") + " |"

  def padRight(text: String, width: Int): String =
    text + (for (i <- 0 until width - text.length) yield ' ').mkString("")

  def separatorLine: String = "|" + (for {
    col <- 0 until columns
  } yield (for {
    dash <- 0 until columnWidths(col) + 2
  } yield "-").mkString("")).mkString("|") + "|"
}

case class MarkdownAlert(level: String, content: Markdown) extends Markdown {
  val rendered = s"> [!${level}]\n> " + content.rendered.replace("\n", "\n> ")
}

case class MarkdownParagraph(content: String) extends Markdown {
  val rendered = content
}

case class MarkdownCode(content: String, language: Option[String])
    extends Markdown {
  val rendered = s"```${language.getOrElse("")}\n${content}\n```"
}

case class MarkdownHeading(content: String, level: Int) extends Markdown {
  val rendered =
    (for (i <- 0 until level) yield "#").mkString("") + " " + content
}

case class MarkdownEmpty() extends Markdown {
  val rendered = ""
}

case class MarkdownUnorderedList(items: Seq[Markdown]) {
  val rendered =
    "* " + items.map(_.rendered).map(_.replace("\n", "\n  ")).mkString("\n* ")
}
