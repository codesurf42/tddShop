package com.example

object TddShop {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}

class Item(val name: String) extends AnyVal

class Cart {

  type promo = List[Item] => List[Item]
  val prices = Map(
    new Item("Orange") -> 25,
    new Item("Apple") -> 60
  )

  def totalCost(items: List[Item], promos: List[promo] = List.empty): Int = {
    val sorted = items.sortBy(_.name)
    val afterPromotions = applyPromos(items, promos)
    afterPromotions.flatMap(prices.get).sum
  }

  def applyPromos(items: List[Item], promos: List[promo]): List[Item] = promos match {
    case h :: t => applyPromos(h(items), t)
    case Nil => items
  }

  def priceInPoundsPences(priceInPences: Int): String = f"${priceInPences.toFloat / 100}%1.2f"
}

object Promotions {
  def b1g1fApple(l1: List[Item]): List[Item] = {
    val item = new Item("Apple")
    val my = l1.filter(_.name == item.name)
    val other = l1.filterNot(_ == item)
    my.take((my.size - 1) / 2 + 1) ++ other
  }

  def b3f2Orange(l1: List[Item]): List[Item] = {
    val item = new Item("Orange")
    val my = l1.filter(_.name == item.name)
    val other = l1.filterNot(_ == item)
    my.take((my.size - 2) / 3 * 2 + 2) ++ other
  }
}