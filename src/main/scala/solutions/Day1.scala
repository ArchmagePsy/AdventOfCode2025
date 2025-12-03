package solutions

import scala.io.Source


enum Direction {
    case LEFT, RIGHT
}

case class Rotation(val direction: Direction, distance: Int) {
    override def toString(): String = s"${direction} ${distance}"
}

object Dial {
    val MAX_DIAL: Int = 100
}

class Dial(var value: Int) {

    def turnRight(distance: Int): Int = {
        val temp: Int = distance % Dial.MAX_DIAL
        val quotient: Int = distance / Dial.MAX_DIAL
        var clicks: Int = quotient
        value = value + temp
        
        if value >= Dial.MAX_DIAL then clicks += 1
        
        value = value % Dial.MAX_DIAL
    
        clicks
    }

    def turnLeft(distance: Int): Int = {
        val previous: Int = value
        val temp: Int = distance % Dial.MAX_DIAL
        val quotient: Int = distance / Dial.MAX_DIAL
        var clicks: Int = quotient
        value = value - temp

        if value < 1 && previous > 0 then clicks += 1
        if value < 0 then value = value + Dial.MAX_DIAL

        clicks
    }

    def getValue: Int = value
}

class Combination(val rotations: List[Rotation]) {
    override def toString(): String = s"${rotations.map(_.toString).mkString(", ")}"

    def countZeroes(start: Int = 0): Int = {
        var dial: Dial = Dial(start)
        var count: Int = 0

        rotations.foreach { rot =>
            rot.direction match
                case Direction.LEFT => dial.turnLeft(rot.distance)
                case Direction.RIGHT => dial.turnRight(rot.distance)
            
            if dial.getValue == 0 then count += 1
        }

        count
    }

    def countClicks(start: Int = 0): Int = {
        var dial: Dial = Dial(start)
        var count: Int = 0

        rotations.foreach { rot =>
            rot.direction match
                case Direction.LEFT => count += dial.turnLeft(rot.distance)
                case Direction.RIGHT => count += dial.turnRight(rot.distance)
        }

        count
    }
}


def parseCombination(line: String): Rotation = {
    val direction: Direction = line.charAt(0) match
        case 'L' => Direction.LEFT
        case 'R' => Direction.RIGHT
    
    val distance: Int = line.tail.toInt

    Rotation(direction, distance)
}

def loadCombination: Combination = {
    val inputLines: Iterator[String] = Source.fromResource("day1.inputs").getLines()
    Combination(inputLines.map(parseCombination(_)).toList)
}

@main def day1(): Unit =  {
    val combination: Combination = loadCombination

    println(s"Number of zeroes: ${combination.countZeroes(start = 50)}")
    println(s"Number of clicks: ${combination.countClicks(start = 50)}")
}
