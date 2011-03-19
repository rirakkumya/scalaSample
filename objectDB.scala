trait 流派
trait 亀仙流 extends 流派
trait ナメック extends 流派
trait サイヤ extends 流派

abstract class 生物
abstract class サイヤ人 extends 生物
trait 改造
abstract class 人造人間 extends 生物 with 改造

case class 孫悟空() extends サイヤ人 with 亀仙流
case class 孫悟飯() extends サイヤ人 with 亀仙流 with ナメック
case class セル() extends 人造人間 with 亀仙流 with ナメック with サイヤ
