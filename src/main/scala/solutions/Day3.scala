package solutions

import scala.io.Source
import scala.collection.immutable.TreeSeqMap.OrderBy

case class Bank(val cells: List[Int]) {
    def getMaxJoltageFor(numberOfCells: Int): BigInt = {
        var offset: Int = -1
        var end: Int = numberOfCells - 1
        BigInt((0 until numberOfCells).flatMap { _ => 
            val maxCell: Tuple2[Int, Int] = cells.zipWithIndex.slice(offset + 1, cells.length - end).maxBy((value, index) => value)
            
            offset = maxCell._2
            end = math.max(end - 1, 0)
            
            maxCell._1.toString
        }.mkString)
    }
}

def parseBank(line: String): Bank = Bank(line.map(_.asDigit).toList)

def loadBanks: List[Bank] = {
    val inputLines: Iterator[String] = Source.fromResource("day3.inputs").getLines()
    inputLines.map(parseBank(_)).toList
}

@main def day3(): Unit = {
    val banks: List[Bank] = loadBanks

    val totalJoltageFor2Cells = banks.map(_.getMaxJoltageFor(2)).sum
    println(s"Total max joltage for 2 cells in each bank: ${totalJoltageFor2Cells}")

    val totalJoltageFor12Cells = banks.map(_.getMaxJoltageFor(12)).sum
    println(s"Total max joltage for 12 cells in each bank: ${totalJoltageFor12Cells}")
}
