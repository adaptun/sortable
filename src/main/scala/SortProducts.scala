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
	)

case class Listing(
	title: String,
	manufacturer: String,
	currency: String,
	price: String
	) {
	override def toString = "qq"
}

case class Result(
	product_name: String,
	listings: Array[Listing]
	)

case class OneResult(
	product_name: String,
	listing: Listing
	)

object SortProducts extends App {

	val ACCEPT_VALUE = 0.7

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
		val words = (product.product_name + " " + product.family + " " + product.model + " " + product.manufacturer).
					replace("_"," ").replace("-"," ").split(" ")

		words.foldLeft(0.0)( (res, word) => res + instr(listing.title, word) ) / words.size
	}

	def assign(listing: Listing, products: List[Product]): Option[OneResult] = {
		case class PossibleResult (
			product_name: String,
			weight: Double
		)

		def choseBestProduct(best: PossibleResult, probe: Product): PossibleResult = {
			val probeWeight = weight(listing, probe)
			if (probeWeight > best.weight) {
				//println("try " + probe.product_name + " with weight " + probeWeight)
				PossibleResult(probe.product_name, probeWeight)
			} else {
				best
			}
		}

		val default = PossibleResult("", -1)
		val probe = products.foldLeft(default)(choseBestProduct)
		if (probe.weight > ACCEPT_VALUE) {
			Some(OneResult(probe.product_name, listing))
		} else {
			None
		}
	}
	
	def extractListings(oneResults: Traversable[OneResult]):Array[Listing] = {
		oneResults.map(_.listing).toArray
	}
	// products = readProducts("products.txt");
	// listings = readLisings();
	// results = new Array[Result]();
	val products = readJson("products.txt", productFromMap).toList
	//println(products.size)
	val results = readJson("listings.txt", listingFromMap)
					.take(20)
					.flatMap(listing => assign(listing, products)) // match listing with some product, or not
					.toTraversable
					.groupBy(_.product_name)
					.mapValues(extractListings)

	for ((product_name, listings) <- results) {
		print("{")
		print(""""product_name":"""" + product_name + """",""")
		print(""""listings":[""")
		listings.mkString(",")
		println("]}")
	}
}