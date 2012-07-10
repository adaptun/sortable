import scala.io.Source
import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSONObject
import java.lang.Integer


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
	listings: List[Listing]
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

	def instr(a: String, b: String) = if ( a.toLowerCase().indexOf(b.toLowerCase()) != -1) 1.0 else 0.0

	def weight(listing: Listing, product: Product): Double = {
		//if (instr(product.manufacturer, listing.manufacturer) == 1) {
				val words = (product.product_name + " " + product.family + " " + product.model + " " + product.manufacturer).
							replace("_"," ").replace("-"," ").split(" ")

				words.foldLeft(0.0)( (res, word) => res + instr(listing.title, word) ) / words.size
		//	} else {
		//		-1
		//	}

	}

	def assign(listing: Listing, products: List[Product]): Option[Result] = {
		case class PossibleResult (
			product_name: String,
			weight: Double
		)

		def choseBestProduct(best: PossibleResult, probe: Product): PossibleResult = {
			val probeWeight = weight(listing, probe)
			if (probeWeight > best.weight) {
				println("try " + probe.product_name + " with weight " + probeWeight)
				PossibleResult(probe.product_name, probeWeight)
			} else {
				best
			}
		}

		val default = PossibleResult("", -1)
		val probe = products.foldLeft(default)(choseBestProduct)
		if (probe.weight > 0.7) {
			Some(Result(
				product_name = probe.product_name,
				listings = List(listing)
			))
		} else {
			None
		}
	}
	
	// products = readProducts("products.txt");
	// listings = readLisings();
	// results = new Array[Result]();
	val products = readJson("products.txt", productFromMap).toList
	//println(products.size)
	val results = readJson("listings.txt", listingFromMap).take(20).map(listing => assign(listing, products))
}