package solutions

import scala.io.Source
import scala.collection.immutable.NumericRange


type Bounds = Tuple2[BigInt, BigInt]

def reduceIdRanges(ranges: List[Bounds]): List[Bounds] = {
    var reducedRanges: List[Bounds] = Nil
    
    ranges.sortBy(_._1).foreach { range =>
        val previous: Option[Bounds] = reducedRanges.headOption

        if previous.isDefined then
            if previous.get._1 <= range._1 && range._1 <= previous.get._2 then
                reducedRanges =  Tuple2(previous.get._1, range._2.max(previous.get._2)) :: reducedRanges.tail
            else
                reducedRanges = range +: reducedRanges
        else
            reducedRanges = range +: reducedRanges
    }

    reducedRanges
}

def isFresh(ingredientId: BigInt, freshRanges: List[Bounds]): Boolean = {
    freshRanges.find(b => b._1 <= ingredientId && ingredientId <= b._2).isDefined
}

def parseIdRanges(line: String): Bounds = {
    val bounds: Array[String] = line.split("-")

    Tuple2(BigInt(bounds(0)), BigInt(bounds(1)))
}

def loadIngredients(): Tuple2[List[Bounds], List[BigInt]] = {
    val inputLines: Iterator[String] = Source.fromResource("day5.inputs").getLines()
    val ingredientRanges: Iterator[Bounds] = inputLines.takeWhile(!_.isBlank()).map(parseIdRanges(_))
    val availableIngredients: Iterator[BigInt] = inputLines.map(BigInt(_))

    (ingredientRanges.toList, availableIngredients.toList)
}

@main def day5(): Unit = {
    val (ingredientRanges: List[Bounds], availableIngredients: List[BigInt]) = loadIngredients()
    val countOfFreshIngredients = availableIngredients.map(isFresh(_, ingredientRanges)).count(identity)

    println(s"Count of the fresh ingredients: ${countOfFreshIngredients}")

    val totalFreshIngredients: BigInt = reduceIdRanges(ingredientRanges).map(b => (b._2 - b._1) + 1).sum

    println(s"Total fresh ingredients: ${totalFreshIngredients}")
}
