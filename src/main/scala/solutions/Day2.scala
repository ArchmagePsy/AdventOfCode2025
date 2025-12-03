package solutions

import scala.io.Source
import scala.collection.immutable.NumericRange


def invalidID(id: String): Boolean = {
    val idLength: Int = id.length
    val idPivot: Int = idLength / 2
    
    Range.inclusive(1, idPivot).map { size => 
        id.sliding(size, size).forall(_.equals(id.substring(0, size)))
    }.find(identity).isDefined
}

def basicInvalidID(id: String): Boolean = {
    val idLength: Int = id.length
    val idPivot: Int = idLength / 2

    if idLength % 2 != 0 then return false
    else if id.substring(0, idPivot) == id.substring(idPivot) then return true

    return false
}

def parseRange(range: String): NumericRange[BigInt] = {
    val pair: Array[String] = range.split("-")
    NumericRange.inclusive(BigInt(pair(0)), BigInt(pair(1)), BigInt(1))
}

def loadRanges: List[NumericRange[BigInt]] = {
    val inputLines: Iterator[String] = Source.fromResource("day2.inputs").getLines()
    inputLines.flatMap(line => line.split(",")).map(parseRange(_)).toList
}

@main def day2(): Unit = {
    val ranges: List[NumericRange[BigInt]] = loadRanges

    val sumOfBasicInvalidIDs: BigInt = ranges.flatMap(_.filter(id => basicInvalidID(id.toString))).sum
    println(s"Sum of basic invalid IDs: ${sumOfBasicInvalidIDs}")

    val sumOfInvalidIDs: BigInt = ranges.flatMap(_.filter(id => invalidID(id.toString))).sum
    println(s"Sum of invalid IDs: ${sumOfInvalidIDs}")
}