class イカ言語 [侵略](ピコピコ:侵略){
  def の[イカスミ](触手:(侵略)=>イカスミ) = 触手(ピコピコ)
  def に[イカスミ](触手:(侵略)=>イカスミ) = 触手(ピコピコ)
  def を[イカスミ](触手:(侵略)=>イカスミ) = 触手(ピコピコ)
}

object イカ娘 {
  implicit def イカ変換[大好き](エビ:大好き) = new イカ言語(エビ)
  def 料理(祝:String) = "イカスミ" + 祝
  def 計算(侵略:Int) = 侵略
  def 倍(イカ:Int) = イカ * 2
  def しなイカ[でゲソ](娘:でゲソ) = 娘 + "でゲソ"
  def 作らなイカ[でゲソ](二期:でゲソ) =  二期 + "でゲソ"

  def main(ゲソ: Array[String]) {
    val イ = (1 + 1) の 計算 を しなイカ
    val カ = 3 を 倍 に しなイカ
    val 娘 = "スイカ" の 料理 を 作らなイカ
    println(イ)
    println(カ)
    println(娘)
  }
}
