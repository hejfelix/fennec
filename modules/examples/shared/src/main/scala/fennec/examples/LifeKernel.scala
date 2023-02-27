package fennec.examples

import fennec.{Kernel, Update}

import scala.annotation.nowarn
import fennec.CodecDerived.given
import fennec.CodecDefaults.given

object LifeKernel:
  import cats.syntax.all.*

  case class Grid(cells: Set[(Int, Int)]):

    def isAlive(x: Int, y: Int): Boolean = cells.contains((x, y))

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
    def empty = State(Grid(Set.empty), 1000)

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
