package solutions

import scala.io.Source


object ManifoldPart {
    def fromSymbol(symbol: Char): ManifoldPart = {
        symbol match {
            case '^' => ManifoldPart.SPLITTER
            case 'S' => ManifoldPart.EMITTER
            case '|' => ManifoldPart.BEAM
            case _ => ManifoldPart.NONE
        }
    }
}

enum ManifoldPart {
    case EMITTER, SPLITTER, BEAM, NONE
}

class Manifold(manifoldRows: Array[Array[ManifoldPart]]) {

    def countSplit: Int = {
        var beams: List[ManifoldPart] = manifoldRows(0).map(part => if part == ManifoldPart.EMITTER then ManifoldPart.BEAM else ManifoldPart.NONE).toList
        var splits: Int = 0

        manifoldRows.slice(1, manifoldRows.length).foreach { row =>
            row.zipWithIndex.foreach { (part, index) =>
                if part == ManifoldPart.SPLITTER && beams(index) == ManifoldPart.BEAM then
                    beams = beams.updated(index, ManifoldPart.NONE)
                    if index != 0 then beams = beams.updated(index - 1, ManifoldPart.BEAM)
                    if index != beams.length - 1 then beams = beams.updated(index + 1, ManifoldPart.BEAM)

                    splits += 1
            }
        }

        splits
    }

    def countTimelines: BigInt = {
        var beams: Array[ManifoldPart] = manifoldRows.head.map(part => if part == ManifoldPart.EMITTER then ManifoldPart.BEAM else ManifoldPart.NONE)
        var pascalRow: Array[BigInt] = manifoldRows.head.map(part => if part == ManifoldPart.EMITTER then BigInt(1) else BigInt(0))
        
        manifoldRows.tail.foreach { row =>
            var indices: List[Int] = Nil

            row.zipWithIndex.foreach { (part, index) =>
                if part == ManifoldPart.SPLITTER && beams(index) == ManifoldPart.BEAM then
                    val left: Int = index - 1
                    val right: Int = index + 1

                    beams.update(index, ManifoldPart.NONE)
                    if index == 0 then

                        beams.update(right, ManifoldPart.BEAM)

                        pascalRow.update(right, pascalRow(right) + pascalRow(index))

                    else if index == beams.length - 1 then

                        beams.update(left, ManifoldPart.BEAM)
                        
                        pascalRow.update(left, pascalRow(left) + pascalRow(index))

                    else

                        beams.update(left, ManifoldPart.BEAM)
                        beams.update(right, ManifoldPart.BEAM)

                        pascalRow.update(left, pascalRow(left) + pascalRow(index))
                        pascalRow.update(right, pascalRow(right) + pascalRow(index))

                    indices = indices :+ index
            }

            indices.foreach(pascalRow.update(_, 0))
        }

        pascalRow.sum
    }
}

def parseManifoldRow(line: String): Array[ManifoldPart] = {
    line.map(ManifoldPart.fromSymbol(_)).toArray
}

def loadManifold: Manifold = {
    val inputLines: Iterator[String] = Source.fromResource("day7.inputs").getLines()
    Manifold(inputLines.map(parseManifoldRow(_)).toArray)
}

@main def day7(): Unit = {
    val tachyonManifold: Manifold = loadManifold

    val countOfSplits: Int = tachyonManifold.countSplit
    println(s"Count of splits in manifold: ${countOfSplits}")

    val countOfTimelines: BigInt = tachyonManifold.countTimelines
    println(s"Count of timelines in manifold: ${countOfTimelines}")
}
