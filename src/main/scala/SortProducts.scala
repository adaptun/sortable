import scala.io.Source
import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSONObject


case class Product(
	product_name: String,
	manufacturer: String,
	family: String,
	model: String,
	announced_date: String
);

case class Listing(
	title: String,
	manufacturer: String,
	currency: String,
	price: String
);

case class Result(
	product_name: String,
	listings: Array[Listing]
);

object SortProducts extends App {
	def readProducts(filename: String): Array[Product] = {
		for(line <- Source.fromFile(filename).getLines()) {
			println(line)
		}
		
	}
	// products = readProducts("products.txt");
	// listings = readLisings();
	// results = new Array[Result]();
	val s:String = """{"title":"LED Flash Macro Ring Light (48 X LED) with 6 Adapter Rings for For Canon/Sony/Nikon/Sigma Lenses","manufacturer":"Neewer Electronics Accessories","currency":"CAD","price":"35.99"}"""
	val res = JSON.parseRaw(s)
	println(res.obj.get("title"))
}