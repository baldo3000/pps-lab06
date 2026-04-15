package it.unibo.pps.ex2

trait ConferenceReviewing:

  import ConferenceReviewing.Question

  /**
   * Loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  /**
   * Loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Int, question: Question): List[Int]

  /**
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double

  /**
   * An article is considered accepted if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles: Set[Int]

  /**
   * @return accepted articles as a list of pairs article+averageFinalScore,
   *         ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   */
  def averageWeightedFinalScoreMap: Map[Int, Double]

end ConferenceReviewing

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  /**
   * For each article, the reviewer has to reply to all the following questions
   */
  enum Question:
    case Relevance, // ("È importante per questa conferenza?"),
    Significance, // ("Produce contributo scientifico?"),
    Confidence, // ("Ti senti competente a commentarlo?");
    Final // ("É un articolo da accettare?")

  private val requiredFinal = 5.0
  private val requiredRelevance = 8

  private class ConferenceReviewingImpl extends ConferenceReviewing:
    private var reviews = Map[Int, List[Map[Question, Int]]]().withDefaultValue(List())

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      val updatedReviews = reviews(article) :+ scores
      reviews = reviews + (article -> updatedReviews)

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      val review = Map(
        Question.Relevance -> relevance,
        Question.Significance -> significance,
        Question.Confidence -> confidence,
        Question.Final -> fin
      )
      loadReview(article, review)

    override def orderedScores(article: Int, question: Question): List[Int] = reviews(article).map(_(question)).sorted

    override def averageFinalScore(article: Int): Double =
      val finals = reviews(article).map(_(Question.Final))
      finals.sum.toDouble / finals.size

    private def hasRequiredRelevance(article: Int): Boolean =
      reviews(article).map(_(Question.Relevance)).exists(_ >= requiredRelevance)

    private def isAccepted(article: Int): Boolean =
      averageFinalScore(article) >= requiredFinal && hasRequiredRelevance(article) // Short-circuit

    override def acceptedArticles: Set[Int] = reviews.filter((a, _) => isAccepted(a)).keySet

    // There is an inefficiency given from computing averageFinalScore two times.
    override def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.toList.map(a => (a, averageFinalScore(a))).sortBy(_._2)

    // Efficient version
    def sortedAcceptedArticlesEfficient: List[(Int, Double)] =
      reviews.keys.flatMap(article =>
        val averageFinal = averageFinalScore(article)
        if averageFinal >= requiredFinal && hasRequiredRelevance(article) // Short-circuit
        then Some(article -> averageFinal)
        else None
      ).toList.sortBy(_._2)

    private def averageWeightedFinalScore(article: Int): Double =
      val finalsWeighted = reviews(article).map(review => review(Question.Confidence) * review(Question.Final) / 10.0)
      finalsWeighted.sum / finalsWeighted.size

    override def averageWeightedFinalScoreMap: Map[Int, Double] =
      reviews.map(a => a._1 -> averageWeightedFinalScore(a._1))

  end ConferenceReviewingImpl

end ConferenceReviewing
