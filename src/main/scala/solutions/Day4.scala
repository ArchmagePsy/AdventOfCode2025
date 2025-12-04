package solutions

import scala.io.Source
import scala.compiletime.ops.int


val PAPER_ROLL: Char = '@'
type Row = Array[Boolean]
type Grid = Array[Row]
type Coordinate = Tuple2[Int, Int]

def getAdjacentCoordinates(coordinate: Coordinate): Array[Coordinate] = {
    Array((coordinate._1 - 1, coordinate._2 - 1), (coordinate._1, coordinate._2 - 1), (coordinate._1 + 1, coordinate._2 - 1),
        (coordinate._1 - 1, coordinate._2), (coordinate._1 + 1, coordinate._2),
        (coordinate._1 - 1, coordinate._2 + 1), (coordinate._1, coordinate._2 + 1), (coordinate._1 + 1, coordinate._2 + 1)
    )
}

def getRemovableRolls(grid: Grid, adjacentRolls: Int = 4): List[Coordinate] = {
    var coords: List[Coordinate] = Nil

    for (y <- 0 until grid.length; x <- 0 until grid(0).length) {
        if grid(y)(x) && getAdjacentCoordinates((x, y)).map { (x, y) =>
            try {
                if grid(y)(x) then 1 else 0
            } catch {
                case aioob: ArrayIndexOutOfBoundsException => 0
            }
        }.sum < adjacentRolls then
            coords = coords :+ (x, y)
    }

    coords
}

def removeRolls(grid: Grid, rolls: List[Coordinate]): Unit = {
    rolls.foreach { (x, y) =>
        grid(y)(x) = false
    }
}

def parseRow(line: String): Row = line.map(_.equals(PAPER_ROLL)).toArray

def loadGrid: Grid = {
    val inputLines: Iterator[String] = Source.fromResource("day4.inputs").getLines()
    inputLines.map(parseRow(_)).toArray
}

@main def day4(): Unit = {
    val grid: Grid = loadGrid

    val countOfRollsLessThan4: Int = getRemovableRolls(grid).size
    println(s"Count of rolls with fewer than 4 adjacent rolls: ${countOfRollsLessThan4}")

    var rollsToRemove: List[Coordinate] = Nil
    var removed: Int = 0

    while 
        rollsToRemove = getRemovableRolls(grid)
        !rollsToRemove.isEmpty
        do 
            removed += rollsToRemove.size
            removeRolls(grid, rollsToRemove)

    println(s"Count of the total rolls removed: ${removed}")
}