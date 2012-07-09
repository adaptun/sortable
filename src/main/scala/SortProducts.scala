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
	def readJson[A](filename: String, makeObject: Map[String, String] => Option[A]):Iterator[A] = {
		Source.fromFile(filename).getLines.flatMap {
			line => 
			JSON.parseRaw(line) match {
				case Some(jsonType) => {
					makeObject(jsonType.asInstanceOf[JSONObject].obj.asInstanceOf[Map[String, String]])
				}
				case None => {
					println ("parsing error")
					None
				}
			}
		}
	}

	def productFromMap(map: Map[String, String]): Option[Product] = {
		for { product_name <- map.get("product_name")
		  manufacturer <- map.get("manufacturer")
		  family <- map.get("family")
		  model <- map.get("model")
		  announced_date <- map.get("announced-date")
		} return Some(Product(product_name, manufacturer, family, model, announced_date))
		return None
	}

	def listingFromMap(map: Map[String, String]): Option[Listing] = {
		for { title <- map.get("title")
		  manufacturer <- map.get("manufacturer")
		  currency <- map.get("currency")
		  price <- map.get("price")
		} return Some(Listing(title, manufacturer, currency, price))
		return None
	}
	// products = readProducts("products.txt");
	// listings = readLisings();
	// results = new Array[Result]();
	val products = readJson("products.txt", productFromMap)
	println(products.size)
	val listings = readJson("listings.txt", listingFromMap)
	println(listings.size)
	//println(res.obj.get("title"))
}