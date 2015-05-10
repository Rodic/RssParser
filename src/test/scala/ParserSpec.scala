import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.rodic.{RssItem, RssParser, Rss2Parser, AtomParser}

class RssParserSpec extends FlatSpec with ShouldMatchers {

  val rssFeed = <rss version="2.0">
                  <channel>
                    <title>Example Feed</title>
                    <description>Insert witty or insightful remark here</description>
                    <link>http://example.org/</link>
                    <lastBuildDate>Sat, 13 Dec 2003 18:30:02 GMT</lastBuildDate>
                    <managingEditor>johndoe@example.com (John Doe)</managingEditor>

                    <item>
                      <title>Atom-Powered Robots Run Amok</title>
                      <link>http://example.org/2003/12/13/atom03</link>
                      <guid isPermaLink="false">urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</guid>
                      <pubDate>Sat, 13 Dec 2003 18:30:02 GMT</pubDate>
                      <description>Some text.</description>
                    </item>
                  </channel>
                </rss>

  val atomFeed = <feed xmlns="http://www.w3.org/2005/Atom">
                    <title>Example Feed</title>
                    <subtitle>Insert witty or insightful remark here</subtitle>
                    <link href="http://example.org/"/>
                    <updated>2003-12-13T18:30:02Z</updated>
                    <author>
                      <name>John Doe</name>
                      <email>johndoe@example.com</email>
                    </author>
                    <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>

                    <entry>
                      <title>Atom-Powered Robots Run Amok</title>
                      <link href="http://example.org/2003/12/13/atom03"/>
                      <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
                      <updated>2003-12-13T18:30:02Z</updated>
                      <summary>Some text.</summary>
                    </entry>
                  </feed>

  val unknown = <list>
                  <thing>
                    <title>Title</title>
                    <text>Text</text>
                    <url>www.example.com</url>
                  </thing>
                </list>


  "RssParser" should "instaniate Rss2Parser when RSS 2.0 feed is given" in {
    val parser = RssParser(rssFeed) 
    parser shouldBe a [Rss2Parser]
  }

  "Rss2Parser" should "return sequence of RssItems" in {
    val parser  = RssParser(rssFeed)
    val results = parser.parse()
    results shouldBe a [Seq[RssItem]]
  }

  "Rss2Parser" should "return seq of correct items" in {
    val parser  = RssParser(rssFeed)
    val results = parser.parse()

    results should have length 1

    val result = results.head

    result.title shouldEqual "Atom-Powered Robots Run Amok"
    result.text  shouldEqual "Some text."
    result.url   shouldEqual "http://example.org/2003/12/13/atom03"
  }

  "RssParser" should "instaniate AtomParser when Atom feed is given" in {
    val parser = RssParser(atomFeed)
    parser shouldBe a [AtomParser]
  }

  "AtomParser" should "return sequence of RssItems" in {
    val parser  = RssParser(atomFeed)
    val results = parser.parse()
    results shouldBe a [Seq[RssItem]]
  }

  "AtomParser" should "return seq of correct items" in {
    val parser  = RssParser(atomFeed)
    val results = parser.parse()

    results should have length 1

    val result = results.head

    result.title shouldEqual "Atom-Powered Robots Run Amok"
    result.text  shouldEqual "Some text."
    result.url   shouldEqual "http://example.org/2003/12/13/atom03"
  }

  "RssParser" should "throw an error when unknown rss format is passed" in {
    evaluating { RssParser(unknown) } should produce [IllegalArgumentException]
  }
}
