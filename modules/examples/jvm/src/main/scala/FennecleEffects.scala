import cats.Functor
import cats.effect.Sync
import cats.effect.std.Random
import fennec.examples.FennecleKernel.*
import fennec.UpdateEffect
import cats.syntax.all.*

import java.util.{Locale, UUID}
import scala.collection.MapView
import scala.io.{BufferedSource, Source}

def get[F[_]: Sync](path: String): F[MapView[String, Int]] = Sync[F].delay {
  val source: BufferedSource       = Source.fromFile(path)
  val txt                          = source.getLines().mkString(" ")
  val unfilteredWords: Seq[String] = "(\\w*)\\W".r.findAllIn(txt).matchData.toList.map(_.group(1))
  val filtered                     = unfilteredWords.filter(_.length == wordSize)
  val transformed                  = filtered.map(_.toLowerCase(Locale.US))

  val grouped = transformed.groupBy(identity).view.mapValues(_.length)
  source.close()
  grouped
}

def newGame[F[_]: Functor: Random](s: State): F[Game] =
  val difficultyRange = s.difficulty match {
    case Difficulty.Easy   => 30 until Int.MaxValue
    case Difficulty.Medium => 20 until 30
    case Difficulty.Hard   => 5 until 20
  }
  val relevantWords = s.frequencyDict.collect {
    case (word, freq) if difficultyRange.contains(freq) => word
  }.toArray
  Random[F]
    .nextIntBounded(relevantWords.size)
    .map(index => Game(relevantWords(index), List.fill(wordSize)('_'), List.empty))

def fennecleEffects[F[_]: Sync: Random]: UpdateEffect[F, State, Event, Unit] =
  _ =>
    state =>
      case Event.LoadDictionary =>
        get[F]("src/main/resources/pg69875.txt").map(d => List(Event.DictionaryLoaded(d.toMap)))
      case Event.StartNewGame =>
        for
          id   <- Sync[F].delay(UUID.randomUUID())
          game <- newGame[F](state)
        yield List(Event.NewGame(id, game))
      case _ => List.empty.pure[F]
