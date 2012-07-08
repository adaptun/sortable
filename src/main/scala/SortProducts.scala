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
	def readProducts(filename: String) = {
		for (line <- Source.fromFile(filename).getLines) {
			val productMaybe = JSON.parseRaw(line)
			productMaybe match {
				case Some(productJson) => {
					val product = productJson.asInstanceOf[JSONObject].obj.asInstanceOf[Map[String,String]]
					for { product_name <- product.get("product_name")
						  manufacturer <- product.get("manufacturer")
						  family <- product.get("family")
						  model <- product.get("model")
						  announced_date <- product.get("announced-date")
						} {
						val p = Product(product_name, manufacturer, family, model, announced_date)
						println(p.product_name)
					}
				}
				case None => {
					println("bad parsing!");
				}
			}
		}
		
	}
	// products = readProducts("products.txt");
	// listings = readLisings();
	// results = new Array[Result]();
	readProducts("products.txt");
	//println(res.obj.get("title"))
}