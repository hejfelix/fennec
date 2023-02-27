package fennec.examples

import fennec.*

import java.util.UUID
import scala.annotation.nowarn
import io.circe.{KeyDecoder, KeyEncoder}

import scala.util.Try
 import fennec.CodecDerived.given
 import fennec.CodecDefaults.given
//import fennec.circe.CirceSupport.given
//import io.circe.generic.auto.given
import monocle.syntax.all.*

given KeyEncoder[FormId] = _ match
  case FormId.Id(uuid)       => uuid.toString
  case FormId.Name(asString) => asString
given KeyDecoder[FormId] = key =>
  val maybeUuid = Try(UUID.fromString(key))
  Some(maybeUuid.fold(_ => FormId.Name(key), uuid => FormId.Id(uuid)))

object FennecleKernel:

  val wordSize = 6

  @nowarn("msg=unused explicit parameter")
  enum Difficulty:
    case Hard
    case Medium
    case Easy

  case class Game(wordToGuess: String, currentGuess: List[Char], guesses: List[String])
  case class State(frequencyDict: Map[String, Int], games: List[(UUID, Game)], difficulty: Difficulty)

  @nowarn("msg=unused explicit parameter")
  enum Event:
    case ModifyGuess(gameId: UUID, index: Int, char: Char)
    case StartNewGame
    case NewGame(id: UUID, g: Game)
    case SetDifficulty(d: Difficulty)
    case LoadDictionary
    case DictionaryLoaded(frequencyDict: Map[String, Int])
    case SubmitGuess(gameId: UUID)

  val emptyGuess: List[Char] = List.fill(wordSize)('_')

  val update: Update[State, Event] = state =>
    case Event.ModifyGuess(gameId, index, char) =>
      state
        .focus(_.games)
        .each
        .filter((id, _) => id == gameId)
        .refocus(_._2.currentGuess)
        .index(index)
        .replace(char)

    case Event.SubmitGuess(gameId) =>
      state
        .focus(_.games)
        .each
        .filter((id, _) => id == gameId)
        .refocus(_._2)
        .modify(
          game =>
          game
            .focus(_.currentGuess).replace(emptyGuess)
            .focus(_.guesses).modify(_.prepended(game.currentGuess.mkString)),
        )
    case Event.NewGame(id, g)      => state.copy(games = (id -> g) :: state.games)
    case Event.DictionaryLoaded(d) => state.copy(frequencyDict = d)
    case Event.SetDifficulty(d)    => state.copy(difficulty = d)
    case _                         => state

  private[examples] val initState: State =
    State(frequencyDict = Map.empty, games = List.empty, difficulty = Difficulty.Medium)

  val kernel: Kernel[cats.Id, State, Event, Unit] =
    Kernel.init[State, Event]("fennecle", initState).withUpdate(update)

end FennecleKernel

object FormsKernel:

  /** Kernel stuff
    */
  case class UserInfo(name: String, age: Int, adult: Boolean)

  type State = Map[FormId, UserInfo]
  enum Event:
    case UpdateUserForm(formId: FormId, userInfo: UserInfo)
    case AddUser(id: FormId)
    case RemoveUser(id: FormId)

  val update: Update[State, Event] = state =>
    case Event.UpdateUserForm(id, info) => state.updated(id, info)
    case Event.AddUser(id)              => state + (id -> UserInfo("", 0, false))
    case Event.RemoveUser(id)           => state.removed(id)

  val kernel: Kernel[cats.Id, State, Event, Unit] =
    Kernel
      .init[State, Event]("formApp", Map.empty[FormId, UserInfo])
      .withUpdate(update)

end FormsKernel

