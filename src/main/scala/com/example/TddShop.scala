package com.example

object TddShop {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}

class Item(val name: String) extends AnyVal

class Cart {

  val prices = Map(
    new Item("Orange") -> 25,
    new Item("Apple") -> 60
  )
  def totalCost(items: List[Item]): Int = {
    items.flatMap(prices.get).sum
  }
  def priceInPoundsPences(priceInPences: Int):String = f"${priceInPences.toFloat / 100}%1.2f"
}