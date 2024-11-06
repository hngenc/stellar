package stellar

import scala.io.Source._
import scala.collection.mutable.{ArrayBuffer, Map => MutMap, Set => MutSet}
import chisel3.stage.ChiselStage
import firrtl.options.Dependency
import firrtl.passes
import firrtl.stage.RunFirrtlTransformAnnotation

object ElaborateSpatialArray extends App {
  // val exampleArray = new SparseDenseConv1D
  // val exampleArray = new GustavsonsMatmul
  val exampleArray = new OuterMatmul(isLoadBalanced = false, travellingCoords = false)
  // val exampleArray = new DiagonalMatmul
  // val exampleArray = new DenseMatmul
  // val exampleArray = new DenseSparseMatmul
  // val exampleArray = new A100DenseMatmul
  // val exampleArray = new A100TiledMatmul
  // val exampleArray = new A100Test
  // val exampleArray = new KeyMatcher
  // val exampleArray = new SortedCsrMatricesMerger(rows=2, smallCompressedCols=2, largeCompressedCols=4)
  // val exampleArray = new Filter(max_I = 2, max_J = 2, max_K = 32)
  // val exampleArray = new MatrixAdder
  // val exampleArray = new CsrMatrixAdder
  // val exampleArray = new SparseDenseMatmul(true, size = 2, maxJOpt = Some(8), outputStationary = false)
//  val exampleArray = {
//    val size = 2
//    new CsrMatrixSorter(size = size, hasSubArrays = true)
//  }

  val (_, elab_time) = Util.time(exampleArray.elaborate(
    // shouldPrint = false,
    // shouldRender = false,
    // emitVerilog = true,
  ))

  println(s"Elaboration took ${elab_time}s")
}

object ElaborateAccelerator extends App {
  // if (false)
  {
    val extraOptimizations = Seq.fill(2)(Seq(
      RunFirrtlTransformAnnotation(Dependency[firrtl.transforms.ConstantPropagation]),
      RunFirrtlTransformAnnotation(Dependency(passes.CommonSubexpressionElimination)),
      RunFirrtlTransformAnnotation(Dependency[firrtl.transforms.DeadCodeElimination]))
    ).flatten

    val start = System.nanoTime()
    val accelerator = new OuterSpace(size = 8, withAsserts = false)

    (new ChiselStage).emitSystemVerilog(accelerator.toChiselModule(dataWidthBits = 32), args = Array("--target-dir", "verilog/"), annotations = extraOptimizations)
    val end = System.nanoTime()
    println(s"\nTime taken to elaborate: ${(end - start).toDouble / 1e9} seconds")
  }

  case class ModuleStats(name: String, var nLines: Int = 0, ports: MutMap[String, Int] = MutMap.empty.withDefaultValue(0),
                         scalaLines: MutMap[(String, Int), Int] = MutMap.empty.withDefaultValue(0), children: MutSet[String] = MutSet.empty)
  val module_stats = ArrayBuffer.empty[ModuleStats]

  {
    // Read systemverilog code
    val file = fromFile("verilog/ChiselAccelerator.sv")
    // val file = fromFile("test_run_dir/tmp.sv")
    // val file = fromFile("tmp.sv")
    var module_stat: Option[ModuleStats] = None
    for (line <- file.getLines()) {
      if (line.startsWith("module")) {
        assert(module_stat.isEmpty)
        val module_name = line.drop("module ".length).dropRight(1)
        module_stat = Some(ModuleStats(module_name))
      } else if (line.startsWith("endmodule")) {
        module_stats += module_stat.get
        module_stat = None
      } else if (line.trim.startsWith("input") || line.trim.startsWith("output")) {
        val port_name = line.trim.reverse.takeWhile(!_.isWhitespace).reverse.
          filter(!_.isDigit).filter(_ != ',').
          reverse.dropWhile(_ == '_').reverse
        module_stat.get.ports(port_name) += 1
        module_stat.get.nLines += 1
      } else {
        val ScalaFilePattern = ".*// @\\[(\\S*\\.scala) (\\d*):.*]\\s*".r
        line match {
          case ScalaFilePattern(fname, lineno) => module_stat.get.scalaLines((fname, lineno.toInt)) += 1
          case _ =>
        }

        val ChildPattern = "^ *(\\S+) \\S+ \\( // @\\[\\S*\\.scala \\d*:.*]\\s*".r
        line match {
          case ChildPattern(child) => module_stat.get.children += child
          case _ =>
        }

        module_stat.foreach(_.nLines += 1)
      }
    }
    file.close()
  }

  val total_lines = module_stats.map(_.nLines).sum
  println(s"\nTotal lines = $total_lines")

  println("\nSorting by module:");
  {
    val sorted = module_stats.sortBy(_.nLines).reverse
    val cumsum = sorted.scanLeft(0)(_ + _.nLines).tail.map(_.toDouble / total_lines)
    val filtered = sorted.zip(cumsum).takeWhile(_._2 < 0.8).map(_._1)
    for (ModuleStats(name, nLines, ports, scalaLines, child) <- filtered.take(10)) {
      println(s"\t$name has $nLines lines (${nLines.toDouble / total_lines.toDouble * 100}% of total)")
    }
  }

  println("\nSorting by module with children:");
  {
    def linesWithChildren(module: String): Int = {
      val mod = module_stats.find(_.name == module).get
      mod.nLines + mod.children.map(linesWithChildren).sum
    }
    val sorted = module_stats.map(x => x.name -> linesWithChildren(x.name)).sortBy(_._2).reverse
    val filtered = sorted.filter(_._2.toDouble / total_lines > 0.01)
    for ((name, nLines) <- filtered) {
      println(s"\t$name has $nLines lines (${nLines.toDouble / total_lines.toDouble * 100}% of total)")
    }
  }

  println("\nSorting by scala files:");
  {
    val sorted = module_stats.flatMap(_.scalaLines.toSeq).groupBy(_._1._1).view.mapValues(_.map(_._2).sum).toSeq.sortBy(_._2).reverse
    val cumsum = sorted.scanLeft(0)(_ + _._2).tail.map(_.toDouble / total_lines)
    val filtered = sorted.zip(cumsum).takeWhile(_._2 < 0.8).map(_._1)
    for ((fname, nLines) <- filtered.take(10)) {
      println(s"\t$fname has $nLines lines (${nLines.toDouble / total_lines.toDouble * 100}% of total)")
    }
  }

  println("\nSorting by scala lines:");
  {
    val sorted = module_stats.flatMap(_.scalaLines.toSeq).groupBy(_._1).view.mapValues(_.map(_._2).sum).toSeq.sortBy(_._2).reverse
    val cumsum = sorted.scanLeft(0)(_ + _._2).tail.map(_.toDouble / total_lines)
    val filtered = sorted.zip(cumsum).takeWhile(_._2 < 0.8).map(_._1)
    for (((fname, lineno), nLines) <- filtered.take(100)) {
      println(s"\t$fname:$lineno has $nLines lines (${nLines.toDouble / total_lines.toDouble * 100}% of total)")
    }
  }

  println("\nSorting by ports:");
  {
    val sorted = module_stats.flatMap(_.ports.toSeq).groupBy(_._1).view.mapValues(_.map(_._2).sum).toSeq.sortBy(_._2).reverse
    val cumsum = sorted.scanLeft(0)(_ + _._2).tail.map(_.toDouble / total_lines)
    val filtered = sorted.zip(cumsum).takeWhile(_._2 < 0.8).map(_._1)
    for ((port, nLines) <- filtered.take(10)) {
      println(s"\t$port has $nLines lines (${nLines.toDouble / total_lines.toDouble * 100}% of total)")
    }
  }
}

object AnalyzeModule extends App {
  val module_name = "ChiselRegFile_7"

  case class Signal(deps: MutSet[String] = MutSet.empty)
  val signals = MutMap.empty[String, Signal]

  {
    // Read systemverilog code
    import scala.util.control.Breaks._

    val file = fromFile("verilog/ChiselAccelerator.sv")
    // val file = fromFile("test_run_dir/tmp.sv")
    // val file = fromFile("tmp.sv")

    var reached = false
    var full_line = ""
    breakable { for (line <- file.getLines()) {
      if (line.startsWith(s"module $module_name")) {
        assert(!reached)
        reached = true
      } else if (reached && line.startsWith("endmodule")) {
        break()
      } else if (reached) {
        full_line += line
        val is_complete = full_line.contains(";") || full_line.startsWith("`") || full_line.trim.startsWith("input ") ||
          full_line.trim.startsWith("output ")

        if (is_complete) {
          val badChars = ",;!()~"

          def extractDeps(line: String): Seq[String] = {
            val trimmed = line.trim.dropWhile(_ != '=')

            val withoutComment = if (trimmed.contains("//")) {
              trimmed.take(trimmed.indexOf("//"))
            } else trimmed

            val tokens = withoutComment.split("\\s|\\[|,|\\{|]|}").filter(_.exists(_.isLetter))
            val token_names = tokens.map(_.replace("$signed(", "").filterNot(badChars contains _)).filter(_.nonEmpty).filterNot(_.head.isDigit)

            token_names.toSeq
          }

          if (full_line.trim.startsWith("wire") || full_line.trim.startsWith("reg") || full_line.trim.startsWith("assign") || full_line.trim.startsWith("input")) {
            val signal_name = full_line.trim.split("\\s").filter(_.exists(_.isLetter))(1).filterNot(badChars contains _)
            if (!signals.contains(signal_name))
              signals(signal_name) = Signal()

            signals(signal_name).deps ++= extractDeps(full_line)
          } /* else if (full_line.trim.split("\\s").tail.headOption.contains("<=")) {
            val signal_name = full_line.trim.split("\\s")(0)
            signals(signal_name).deps ++= extractDeps(full_line)
          } */

          full_line = ""
        }
      }
    }}
  }

  {
    // Trace dependencies
    val fullyTraced = MutSet.empty[String]
    signals.collect { case (name, signal) if !name.startsWith("_") =>
      var done = false
      while (!done) {
        val oldDeps = signal.deps.toSeq
        val newDeps = MutSet.empty[String]
        oldDeps.foreach {
          case dep if fullyTraced contains dep => signal.deps ++= signals(dep).deps
          case dep => newDeps ++= signals(dep).deps.diff(signal.deps)
        }
        if (newDeps.nonEmpty)
          signal.deps ++= newDeps
        else
          done = true
      }
      fullyTraced += name
    }
  }

  signals.toSeq.sortBy(_._2.deps.size).collect { case (name, Signal(deps)) if !name.startsWith("_") && (name.startsWith("io_outs_0") || name.startsWith("io_outs_1_")) && name.endsWith("found") && !name.endsWith("_") =>
    val deps_ = deps.filterNot(_.startsWith("_")).toSeq.sorted
    println(s"$name\n\t${deps_.mkString(" ")}\n")
  }

  case class OutPort(name: String, found_deps: Seq[String], unavail_deps: Seq[String])
}
