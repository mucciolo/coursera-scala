package wikipedia

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD

case class WikipediaArticle(title: String, text: String) {
  /**
    * @return Whether the text of this article mentions `lang` or not
    * @param lang Language to look for (e.g. "Scala")
    */
  def mentionsLanguage(lang: String): Boolean = text.split(' ').contains(lang)
}

object WikipediaRanking extends WikipediaRankingInterface {

  val langs = List(
    "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
    "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  val conf: SparkConf = new SparkConf().setAppName("Wikipedia Ranking").setMaster("local[*]")
  val sc: SparkContext = new SparkContext(conf)
  val wikiRdd: RDD[WikipediaArticle] =
    sc.parallelize(WikipediaData.lines).map(WikipediaData.parse)

  /** Returns the number of articles on which the language `lang` occurs.
    */
  def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int =
    rdd.aggregate(0)((acc, article) => if (article.mentionsLanguage(lang)) acc + 1 else acc, _ + _)

  /* (1) Uses `occurrencesOfLang` to compute in decresing order the ranking of the languages
   * (`val langs`) by determining the number of Wikipedia articles that
   * mention each language at least once.
   */
  def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
    langs.map(lang => (lang, occurrencesOfLang(lang, rdd)))
         .sortBy(_._2)(Ordering[Int].reverse)

  /* Compute an inverted index of the set of articles, mapping each language
   * to the Wikipedia pages in which it occurs.
   */
  def makeIndex(langs: List[String], rdd: RDD[WikipediaArticle]): RDD[(String, Iterable[WikipediaArticle])] =
    rdd.flatMap(article => for (lang <- langs if article.mentionsLanguage(lang)) yield (lang, article))
       .groupByKey()

  /* (2) Computes the language ranking again, but now using the inverted index.
   */
  def rankLangsUsingIndex(index: RDD[(String, Iterable[WikipediaArticle])]): List[(String, Int)] =
    index.map { case (lang, articles) => (lang, articles.size) }
         .sortBy(_._2, ascending = false)
         .collect()
         .toList

  /* (3) Uses `reduceByKey` so that the computation of the index and the ranking are combined.
   */
  def rankLangsReduceByKey(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
    rdd.flatMap(article => for (lang <- langs if article.mentionsLanguage(lang)) yield (lang, 1))
       .reduceByKey(_ + _)
       .sortBy(_._2, ascending = false)
       .collect()
       .toList

  def main(args: Array[String]): Unit = {

    /* Languages ranked according to (1) */
    val langsRanked: List[(String, Int)] = timed("Part 1: naive ranking", rankLangs(langs, wikiRdd))

    /* An inverted index mapping languages to wikipedia pages on which they appear */
    def index: RDD[(String, Iterable[WikipediaArticle])] = makeIndex(langs, wikiRdd)

    /* Languages ranked according to (2), using the inverted index */
    val langsRanked2: List[(String, Int)] = timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))

    /* Languages ranked according to (3) */
    val langsRanked3: List[(String, Int)] = timed("Part 3: ranking using reduceByKey", rankLangsReduceByKey(langs, wikiRdd))

    /* Output the speed of each ranking */
    println(timing)
    sc.stop()
  }

  val timing = new StringBuffer

  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    result
  }
}
