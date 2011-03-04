case class Result(val s: String)
trait Converter {
  implicit def arrayToResult(arr: Array[(String, String)]): Result = {
    println("start[%s]".format(title))
    val result = convert(arr)
    println("end  [%s]".format(title))
    result
  }

  val title: String
  def convert(arr: Array[(String, String)]): Result
}

trait XmlConverter extends Converter {
  val title = "Xml"
  def convert(arr: Array[(String, String)]): Result = {
    Result(arr.map(r => "<%s>%s</%s>".format(r._1, r._2, r._1)).mkString)
  }
}

trait JsonConverter extends Converter {
  val title = "Json"
  def convert(arr: Array[(String, String)]): Result = {
    val json = arr.map(r => """"%s":"%s"""".format(r._1, r._2)).mkString(",")
    Result("{%s}".format(json))  }
}

class ImplicitTemplate { self: Converter =>
  def exec: Result = Array(("aaa", "bbb"), ("ccc", "ddd"))
}

object ImplicitTemplate {
  def main(args: Array[String]) {
    println((new ImplicitTemplate with XmlConverter).exec)
    println((new ImplicitTemplate with JsonConverter).exec)
  }
}







import scala.collection._
import java.nio._
import Profiler._

case class Result(label: String, applyTime: Long, updateTime: Long, appendTime: Long)

object Profile extends Application {
  output(ByteBufferProfile.run)
  //  AnyValMutableCollectionProfile.run
  //  JavaCollectionProfile.run
  //  AnyRefMutableCollectionProfile.run
  //  AnyRefImmutableCollectionProfile.run
}

object Profiler {
  val repeat = 10000
  val maxIndex = repeat - 1
  val trials = 100
  val truncate = trials / 5

  def profile(block: => Unit): Long = {
    val totalTime = (0 to trials).map { i =>
      val start = System.nanoTime
      block
      val end = System.nanoTime
      end - start
    }.sortWith(_ < _).slice(truncate, trials - truncate).reduceLeft(_ + _)
    totalTime / (trials - truncate * 2) / 1000
  }

  def output(list: List[Result]): Unit = {
    println("|*micro sec|*.apply|*.update|*.append")
    list.foreach {
      case Result(l, a, u, -1) => println("|%s|%d|%d|-|".format(l, a, u))
      case Result(l, a, u, app) => println("|%s|%d|%d|%d|".format(l, a, u, app))
    }
  }
}

object ByteBufferProfile {
  import scala.collection.JavaConversions._
  val nondirect = ByteBuffer.allocate(repeat * 4).asIntBuffer
  val direct = ByteBuffer.allocateDirect(repeat * 4).asIntBuffer
  nondirect.limit(nondirect.capacity)
  direct.limit(direct.capacity)

  def run = {
    var field = 1
    Result("java.nio.IntByffer",
      profile { var i = repeat; while ({ i -= 1; i >= 0 }) field = nondirect.get(i) },
      profile { var i = repeat; while ({ i -= 1; i >= 0 }) nondirect.put(i, field) },
      -1) ::
      Result("java.nio.IntByffer",
        profile { var i = repeat; while ({ i -= 1; i >= 0 }) field = direct.get(i) },
        profile { var i = repeat; while ({ i -= 1; i >= 0 }) direct.put(i, field) },
        -1) ::
        Nil
  }
}

