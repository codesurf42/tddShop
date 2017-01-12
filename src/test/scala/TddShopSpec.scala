import com.example.{Cart, Item, Promotions}
import org.scalatest._

class TddShopSpec extends FlatSpec with Matchers {

  class Context {
    val or = new Item("Orange")
    val ap = new Item("Apple")
    val items = List(or, or, ap)
    val shop = new Cart
    val items2 = List(or, or, ap, ap, ap)
    val items3 = List(or, or, ap, ap, or, ap)
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

  "TddShop" should "do promo b1g1f on apples" in new Context {
    shop.totalCost(items, List(Promotions.b1g1fApple)) should be(25 + 25 + 60)
    shop.totalCost(items2, List(Promotions.b1g1fApple)) should be(2 * 25 + 2 * 60)
  }

  "TddShop" should "do promo 3f2 on oranges" in new Context {
    shop.totalCost(items, List(Promotions.b3f2Orange)) should be(2 * 25 + 60)
    shop.totalCost(items2, List(Promotions.b3f2Orange)) should be(2 * 25 + 3 * 60)
    shop.totalCost(items3, List(Promotions.b3f2Orange)) should be(2 * 25 + 3 * 60)
    shop.totalCost(items3 ++ items3 ++ items2, List(Promotions.b3f2Orange)) should be((2 * 2 + 2) * 25 + (3 + 2 * 3) * 60)
  }

  "TddShop" should "do promo together" in new Context {
    shop.totalCost(items, List(Promotions.b3f2Orange, Promotions.b1g1fApple)) should be(2 * 25 + 60)
    shop.totalCost(items, List(Promotions.b1g1fApple, Promotions.b3f2Orange)) should be(2 * 25 + 60)
    shop.totalCost(items3 ++ items3 ++ items3, List(Promotions.b3f2Orange, Promotions.b1g1fApple)) should be((3 * 2) * 25  + (4 + 1) * 60)
  }

}