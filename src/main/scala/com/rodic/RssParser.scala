package com.rodic

import scala.xml.{XML, Elem, Node, NodeSeq}

// We parse every RSS item to a simple object with title, text and url
case class RssItem(val title: String, val text: String, val url: String)

trait RssParser {
  def getItems(): NodeSeq
  def getUrl(node: Node): String
  def getText(node: Node): String

  def getTitle(node: Node): String = (node \ "title").text

  def parse(): Seq[RssItem] = {
    for{ item <- getItems() } yield	new RssItem(getTitle(item), getText(item), getUrl(item))
  }
}

class Rss2Parser(val xml: Elem) extends RssParser {
  def getItems = xml \\ "channel" \ "item"
  def getText(node: Node) = (node \ "description").text
  def getUrl(node: Node)  = (node \ "link").text
}

class AtomParser(val xml: Elem) extends RssParser {
  def getItems = xml \\ "feed" \ "entry"
  def getText(node: Node) = (node \ "summary").text
  def getUrl(node: Node)  = (node \ "link" \ "@href").text
}

object RssParser {
  def apply(url: String): RssParser = {
    val xml = XML.load(url)
    parserFactory(xml)
  }

  def apply(xml: Elem): RssParser = parserFactory(xml)

  def parserFactory(xml: Elem): RssParser = {
    if ((xml \\ "rss" \ "@version").text == "2.0")
      new Rss2Parser(xml)
    else if (xml.namespace == "http://www.w3.org/2005/Atom")
      new AtomParser(xml)
    else
      throw new IllegalArgumentException("Unknown RSS format")
  }
}
