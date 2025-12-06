package solutions

import scala.io.Source
import java.util.function.BinaryOperator

object Operator {
    def fromSymbol(op: String): Operator = {
        op match
            case "+" => Operator.ADD
            case "*" => Operator.MULTIPLY
    }
}

enum Operator(val op: String, val operation: BinaryOperator[BigInt]) {
    case ADD extends Operator("+", (a, b) => a + b)
    case MULTIPLY extends Operator("*", (a, b) => a * b)
}

object Problem {
    def fromArray(ops: Array[String]): Problem = {
        Problem(ops.slice(0, ops.length - 1).map(BigInt(_)).toList, Operator.fromSymbol(ops.last))
    }
}

class Problem(operands: List[BigInt], operator: Operator) {
    def compute: BigInt = operands.reduce((accumulator, operand) => operator.operation(accumulator, operand))

    override def toString(): String = s"${operands.mkString(", ")} $operator"
}

def loadCorrectWorksheet: List[Problem] = {
    val inputLines: Iterator[String] = Source.fromResource("day6.inputs").getLines()
    val transposedWorksheet: Iterator[String] = inputLines.map(_.toArray).toArray.transpose.map(_.mkString).iterator

    var columns: List[String] = Nil
    var problems: List[Problem] = Nil
    
    while transposedWorksheet.hasNext do
        columns = transposedWorksheet.takeWhile(!_.isBlank).toList
        val op: Operator = Operator.fromSymbol(columns(0).last.toString)
        problems = problems :+ Problem((columns(0).slice(0, columns(0).length - 1) :: columns.tail).map(operand => BigInt(operand.trim)), op)

    problems
}

def loadWorksheet: List[Problem] = {
    val inputLines: Iterator[String] = Source.fromResource("day6.inputs").getLines()
    inputLines.map(_.trim.split("\\s+")).toArray.transpose.map(Problem.fromArray(_)).toList
}

@main def day6(): Unit = {
    val problems: List[Problem] = loadWorksheet

    val sumOfProblems: BigInt = problems.map(_.compute).sum
    println(s"Sum of the problems: ${sumOfProblems}")


    val correctProblems: List[Problem] = loadCorrectWorksheet
    val correctSumOfProblems: BigInt = correctProblems.map(_.compute).sum
    println(s"Correct sum of the problems: ${correctSumOfProblems}")
}
