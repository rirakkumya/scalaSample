import scala.collection._
import java.nio._
import Profiler._

case class Result(label: String, applyTime: Long, updateTime: Long, appendTime: Long)

object Profile extends Application {
  output(ByteBufferProfile.run)
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

