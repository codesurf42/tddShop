import com.example.{Cart, Item, Promotions}
import org.scalatest._

class TddShopSpec extends FlatSpec with Matchers {

  class Context {
    val or = new Item("Orange")
    val ap = new Item("Apple")
    val shop = new Cart
    val items = List(or, or, ap)
    val items1 = List(ap, ap)
    val items2 = List(or, or, ap, ap, ap)
    val items3 = List(or, or, ap, ap, or, ap)
    val items4 = List()
    val items5 = List(or)
    val items6 = List(ap)
    val items7 = List(or, ap, or, ap, or, ap, or, ap, or, ap)
    val items8 = (1 to 499).toList.flatMap(_ => List(ap, or))
    val items9 = (1 to 500).toList.flatMap(_ => List(ap, or))
    val items10 = (1 to 501).toList.flatMap(_ => List(ap, or))
  }

  "TddShop" should "compute price" in new Context {
    true should ===(true)
    shop.totalCost(items) should be(25 + 25 + 60)
    shop.totalCost(new Item("Apple") :: items) should be(25 + 25 + 60 + 60)
  }

  "TddShop" should "format price" in new Context {
    shop.priceInPoundsPences(1230) should be("12.30")
    shop.priceInPoundsPences(12) should be("0.12")
    shop.priceInPoundsPences(0) should be("0.00")
    shop.priceInPoundsPences(1234567) should be("12345.67")
  }

  "TddShop" should "do promo b1g1f on apples" in new Context {
    shop.totalCost(items, List(Promotions.b1g1fApple)) should be(25 + 25 + 60)
    shop.totalCost(items1, List(Promotions.b1g1fApple)) should be(60)
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
    val allPromotions = List(Promotions.b3f2Orange _, Promotions.b1g1fApple _)
    shop.totalCost(items4, allPromotions) should be(0)
    shop.totalCost(items5, allPromotions) should be(25)
    shop.totalCost(items6, allPromotions) should be(60)
    shop.totalCost(items7, allPromotions) should be((2 + 2) * 25 + (2 + 1) * 60)
    shop.totalCost(items8, allPromotions) should be((332 + 1) * 25 + (249 + 1) * 60)
    shop.totalCost(items9, allPromotions) should be((332 + 2) * 25 + (250) * 60)
    shop.totalCost(items10, allPromotions) should be((334) * 25 + (250 + 1) * 60)
  }

}