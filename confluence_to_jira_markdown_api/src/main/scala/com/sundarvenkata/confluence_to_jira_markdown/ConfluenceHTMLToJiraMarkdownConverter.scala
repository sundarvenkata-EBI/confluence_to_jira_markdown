package com.sundarvenkata.confluence_to_jira_markdown
import java.util.Base64
import zio.json._
import requests.RequestAuth
import pine.{HtmlParser, Node, Tag, TagRef, Text}
import scala.collection.immutable.Map
import org.apache.commons.text.StringEscapeUtils

case class ConfluenceCredentials(username: String, password: String); object ConfluenceCredentials {
  implicit val decoder: JsonDecoder[ConfluenceCredentials] = DeriveJsonDecoder.gen[ConfluenceCredentials]}


object ConfluenceHTMLToJiraMarkdownConverter {
  def getConfluenceCredentials(secrets_file: String): Either[String, ConfluenceCredentials] = {
    val jsonSource = scala.io.Source.fromFile(secrets_file)
    val input = try jsonSource.mkString finally jsonSource.close()
    input.fromJson[ConfluenceCredentials]
  }

  def findNodeWithText(node: Node, textToSearch: String,
                       lastVisitedListNode: Either[Error, Node] = Left(new Error("Could not find node"))):
  Either[Error, Node] = {
    node match {
      //    case Tag("li", _, children) => children.view.map(childNode =>
      //      findNodeWithText(childNode, textToSearch, Right(node))).find(childNode => childNode.fold[Boolean](_ => false, _=> true)).getOrElse(Left(new Error("Could not find node")))
      //
      case listNode @ Tag("li", _, children) =>
        if (listNode.toText.startsWith(textToSearch)) {
          Right(node)
        } else
          children.view.map(childNode => findNodeWithText(childNode, textToSearch, Right(node))).find(childNode => childNode.fold[Boolean](_ => false, _=> true)).getOrElse(Left(new Error("Could not find node")))
      case Tag("span", _, children) => children.view.find(childNode => childNode match {
        case Text(content) => content.startsWith(textToSearch)
        case _ => false
      }).fold[Either[Error, Node]](Left(new Error("Could not find node")))(_ => lastVisitedListNode)
      case a @ Tag(_, _, children) =>
        children.view.map(childNode =>
        findNodeWithText(childNode, textToSearch, lastVisitedListNode)).find(childNode => childNode.fold[Boolean](_ => false, _=> true)).getOrElse(Left(new Error("Could not find node")))
      case _ => Left(new Error("Could not find node"))
    }
  }

  def getHTMLListRepresentationForSelectedNode(node: Node, textToSearch: String): String = {
    findNodeWithText(node, textToSearch)
      .fold[String](_ => "", {
        case a@Tag(_, _, _) => s"<ul>${a.toHtml}</ul>"
        case Text(content) => content
      })
  }

  def getJiraMarkdown(node: Any , level: Int = 0,
                      checkBoxFlag: Boolean = false): String = {
    val getStringFromChildNodes: (List[_] => String) = _.map(childNode => getJiraMarkdown(childNode, level)).mkString("")
    node match {
      case Tag("ul", _, children) =>
        val childString = getStringFromChildNodes(children)
        if (childString.trim != "") "\n" +
          children.map(childNode => getJiraMarkdown(childNode, level + 1))
            .filter(content => !content.trim.equals("") && !content.toLowerCase().contains("ops_note")).mkString("\n")
        else ""
      case Tag("li", attributes, children) =>
        val isChecklist = attributes.contains("data-inline-task-id")
        val isChecked = attributes.contains("class") && attributes.get("class").head == "checked"
        val checkBoxSymbol: String = if(isChecklist) if (isChecked) "(/) " else "(x) " else ""
        val childString = getStringFromChildNodes(children)
        if (childString.trim != "") ("*" * level) + " " + checkBoxSymbol + childString else ""
      case Tag("pre", attributes, children) if (attributes.get("class").head == "syntaxhighlighter-pre") => "{code:bash}" + getStringFromChildNodes(children) + "\n{code}"
      case Tag("strong", _, children) => "*" + getStringFromChildNodes(children).trim.replace("*", "\\*") + "*"
      case Tag("em", _, children) => "_" + getStringFromChildNodes(children).trim.replace("_", "\\_") + "_"
      case Text(content) =>  if (content != "\u00A0" && content.stripLineEnd != "") content else ""
      case Tag("a", attributes, _) if attributes.contains("data-username") => s"[~${attributes.get("data-username").head}]"
      case Tag("a", attributes, List(Text(content))) if attributes.contains("href") => s"[$content|${attributes.get("href").head}]"
      case Tag("img", attributes, _) if attributes.contains("src") => s"!${attributes.getOrElse("src", "")}!"
      
      case Tag("style", _, List(Text(content))) => if (!content.contains(".jira-issue")) content else ""
      case Tag(_, _, children) => getStringFromChildNodes(children)
      case _ => node.toString
    }
  }

  val CONFLUENCE_API_BASE_URL = "https://www.ebi.ac.uk/seqdb/confluence/rest/api/content"

  def getConfluencePageHTMLContentFromID(page_id: Int)(implicit confluenceAuth:RequestAuth.Basic): Either[Error, String] = {
    val confluenceQueryByPageIDURL = CONFLUENCE_API_BASE_URL +  s"/$page_id?expand=body.export_view&status=draft"
    val response = requests.get(url=confluenceQueryByPageIDURL, auth=confluenceAuth,
      headers=Map("Cache-Control" -> "no-cache"))
    if (response.statusCode == 200) {
      if (ujson.read(response.text())("body")("export_view")("value").toString.trim == "")
        Left(new Error(s"Unable to get the content for the Confluence page ID $page_id!"))
      else
        Right(ujson.read(response.text())("body")("export_view")("value").toString)
    }
    else
      Left(new Error(s"Unable to get the content for the Confluence Page because: ${response.statusMessage}"))
  }

  def convertConfluencePageToMarkdown(page_id: Int, textToSearch: Either[None.type, String] = Left(None)): String = {
    val result = for {
      confluenceCredentials <- getConfluenceCredentials(s"${System.getProperty("user.home")}/.confluence_pass")
      confluenceAuthHandle <- Right(RequestAuth.implicitBasic(confluenceCredentials.username, confluenceCredentials.password))
      confluenceContentInHTML <- getConfluencePageHTMLContentFromID(page_id)(confluenceAuthHandle)
      confluenceListNodeToLookFor <- Right(getConfluenceListNodeToLookFor(confluenceContentInHTML, textToSearch))
    } yield getJiraMarkdown(confluenceListNodeToLookFor)
    result.fold[String](_ => "", text =>  text.trim.replaceAll("""\\"""", """""""))
  }

  def getConfluenceListNodeToLookFor(confluenceContentInHTML: String,
                                     textToSearch: Either[None.type, String] = Left(None)): Node = {
      val confluenceContentInHTMLUnescaped = StringEscapeUtils.unescapeJava(confluenceContentInHTML)
      .replaceAll("^\"|\"$", "")
      .replaceAll("\u00A0", " ")
      .replaceAll("\u00C2\u00A0", " ")
      val confluenceContentHTMLHeadNode = HtmlParser.fromString(confluenceContentInHTMLUnescaped)
     textToSearch.fold[Node](_ => confluenceContentHTMLHeadNode,
      text => HtmlParser.fromString(getHTMLListRepresentationForSelectedNode(confluenceContentHTMLHeadNode, text)))
  }
}
