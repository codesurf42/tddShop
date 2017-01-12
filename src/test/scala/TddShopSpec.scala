import com.example.{Cart, Item}
import org.scalatest._

class TddShopSpec extends FlatSpec with Matchers {

  class Context {
    val or = new Item("Orange")
    val ap = new Item("Apple")
    val items = List(or, or, ap)
    val shop = new Cart
  }

  "TddShop" should "compute price" in new Context {
    true should ===(true)
    shop.totalCost(items) should be(25 + 25 + 60)
    shop.totalCost(new Item("Apple") :: items) should be(25 + 25 + 60 + 60)
  }

  "TddShop" should "format price" in new Context {
    shop.priceInPoundsPences(1230) should be("12.30")
    shop.priceInPoundsPences(12) should be("0.12")
  }
}