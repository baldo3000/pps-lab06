package it.unibo.pps.ex2

import it.unibo.pps.ex2.ConferenceReviewing.Question
import org.junit.*
import org.junit.Assert.*

class ConferenceReviewingTest:
  private val cr: ConferenceReviewing = ConferenceReviewing()

  @Before
  def init(): Unit =
    // Carico una revisione per l'articolo 1:
    // - 8 per relevance, significance e final
    // - 7 per confidence
    // Si ricordi che l'ordine delle domande è: relevance, significance, confidence, final
    cr.loadReview(1, 8, 8, 6, 8) // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
    // E simile per gli altri
    cr.loadReview(1, 9, 9, 6, 9) // 5.4
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6
    val map = collection.mutable.HashMap[Question, Int]()
    map.put(Question.Relevance, 8)
    map.put(Question.Significance, 8)
    map.put(Question.Confidence, 7) // 5.6
    map.put(Question.Final, 8)
    cr.loadReview(4, map.toMap)
    cr.loadReview(5, 6, 6, 6, 10) // 6.0
    cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @Test
  def testOrderedScores(): Unit =
    // L'articolo 2 ha preso su RELEVANCE i due voti 4,9
    assertEquals(List(4, 9), cr.orderedScores(2, Question.Relevance));
    // E simile per gli altri
    assertEquals(List(6, 7, 8), cr.orderedScores(4, Question.Confidence));
    assertEquals(List(10, 10), cr.orderedScores(5, Question.Final));


  @Test
  def testAverageFinalScore(): Unit =
    // L'articolo 1 ha preso voto medio su FINAL pari a 8.5, con scarto massimo 0.01
    assertEquals(8.5, cr.averageFinalScore(1), 0.01)
    // E simile per gli altri
    assertEquals(7.5, cr.averageFinalScore(2), 0.01)
    assertEquals(3.5, cr.averageFinalScore(3), 0.01)
    assertEquals(7.0, cr.averageFinalScore(4), 0.01)
    assertEquals(10.0, cr.averageFinalScore(5), 0.01)

  @Test
  def testAcceptedArticles(): Unit =
    // Solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assertEquals(Set(1, 2, 4), cr.acceptedArticles)

  @Test
  def testSortedAcceptedArticles(): Unit =
    // Articoli accettati, e loro voto finale medio
    assertEquals(List((4, 7.0), (2, 7.5), (1, 8.5)), cr.sortedAcceptedArticles)

  @Test
  def optionalTestAverageWeightedFinalScore(): Unit =
    // L'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals(cr.averageWeightedFinalScoreMap(1), (4.8 + 5.4) / 2, 0.01)
    // E simile per gli altri
    assertEquals(cr.averageWeightedFinalScoreMap(2), (9.0 + 6.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(3), (0.9 + 1.6) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(4), (3.6 + 5.6 + 5.6) / 3, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(5), (6.0 + 7.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.size, 5)
