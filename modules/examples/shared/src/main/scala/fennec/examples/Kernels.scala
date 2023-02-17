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

object TodoKernel:

  case class Id(id: UUID)
  case class Todo(title: String, message: String, id: Id)
  case class State(todos: List[Todo])

  @nowarn("msg=unused explicit parameter")
  enum Event:
    case CreateTodo
    case NewTodo(id: Id)
    case DeleteTodo(id: Id)
    case UpdateTodo(todo: Todo)

  val initState = State(List(Todo("", "", Id(UUID.randomUUID))))

  @nowarn("msg=unused explicit parameter")
  val update: Update[State, Event] = state =>
    case Event.CreateTodo       => state
    case Event.NewTodo(id)      => state.copy(todos = state.todos.prepended(Todo("", "", id)))
    case Event.DeleteTodo(id)   => state.copy(todos = state.todos.filterNot(_.id == id))
    case Event.UpdateTodo(todo) => state.copy(todos = state.todos.map(t => if t.id == todo.id then todo else t))

  val kernel = Kernel.init[State, Event]("todo", initState).withUpdate(update)

end TodoKernel

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

object LifeKernel:
  import cats.syntax.all.*

  case class Grid(cells: Set[(Int, Int)]):

    def isAlive(x: Int, y: Int) = cells.contains((x, y))

    def kill(x: Int, y: Int): Grid =
      Grid(cells - ((x, y)))

    def resurrect(x: Int, y: Int): Grid =
      Grid(cells + ((x, y)))

    def neighbours(index: (Int, Int)): Set[(Int, Int)] =
      val (x, y) = index
      (for
        dx <- -1 to 1
        dy <- -1 to 1 if (dx, dy) != (0, 0)
      yield (x + dx, y + dy)).toSet

    def liveNeighbours(index: (Int, Int)): Int =
      neighbours(index).count(cells.contains)

    def nextState(index: (Int, Int)): Boolean =
      val alive   = cells.contains(index)
      val liveNbs = liveNeighbours(index)

      val willBecomeAlive = !alive && liveNbs == 3
      val willStayAlive   = alive && (liveNbs == 2 || liveNbs == 3)
      val willDie         = alive && !willStayAlive

      if willDie then false
      else if willBecomeAlive then true
      else cells(index)
    end nextState

    def step: Grid =
      val possibleChanges = cells.flatMap(idx => neighbours(idx).toSet)
      val changes         = possibleChanges.toList.fproduct(nextState)
      changes.foldLeft(this)((grid, change) =>
        val ((x, y), alive) = change
        if alive then grid.resurrect(x, y) else grid.kill(x, y),
      )
  end Grid

  case class State(grid: Grid, updateFrequency: Int)
  object State:
    def empty = State(Grid(Set.empty), 0)

  @nowarn("msg=unused explicit parameter")
  enum Event:
    case Step
    case NoOp
    case Click(x: Int, y: Int)
    case ChangeFrequency(to: Int)
    case Clear
    case Initialize

  @nowarn("msg=unused explicit parameter")
  val update: Update[State, Event] = state =>
    case Event.Initialize          => state
    case Event.Clear               => state.copy(grid = Grid(Set.empty))
    case Event.ChangeFrequency(to) => state.copy(updateFrequency = to)
    case Event.Step                => state.copy(state.grid.step)
    case Event.NoOp                => state
    case Event.Click(x, y) =>
      val maybeCell = state.grid.cells.find(_ == ((x, y)))
      val newGrid   = maybeCell.fold(state.grid.resurrect(x, y))((x, y) => state.grid.kill(x, y))
      state.copy(grid = newGrid)

  val kernel: Kernel[cats.Id, State, Event, Unit] = Kernel
    .init[State, Event]("life", State.empty)
    .withUpdate(update)

end LifeKernel

object CounterKernel:

  case class State(count: Int)

  @nowarn("msg=unused explicit parameter")
  enum Event:
    case Increment
    case Decrement

  val update: Update[State, Event] = state =>
    case Event.Increment => State(state.count + 1)
    case Event.Decrement => State(state.count - 1)

  // We have to specify type her, otherwise circe auto freaks out
  val kernel: Kernel[cats.Id, State, Event, Unit] =
    Kernel
      .init[State, Event]("counter", State(0))
      .withUpdate(update)

end CounterKernel
