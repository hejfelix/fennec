package fennec.examples

import weaver.*

object KernelsSuite extends FunSuite:

  val grid = LifeKernel.Grid(
    Set(
      (0, -1),
      (0, 0),
      (0, 1),
    ),
  )

  val expected = Set(
    (1, 0),
    (0, 0),
    (-1, 0),
  )

  test("blinker") {
    val stepped      = grid.step
    val expectedGrid = LifeKernel.Grid(expected)
    expect(stepped == expectedGrid)
  }

end KernelsSuite
