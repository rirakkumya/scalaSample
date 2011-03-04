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
