import java.io.PrintWriter
import java.lang.Integer
import scala.io.Source
import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSONObject
import scala.collection.GenTraversable

case class Product(
	product_name: String,
	manufacturer: String,
	family: String,
	model: String,
	announced_date: String,
	words: Array[String]
	)

case class Listing(
	title: String,
	manufacturer: String,
	currency: String,
	price: String
	) {
	override def toString = 
	"""{"title": "%s", "manufacturer": "%s", "currency": "%s", "price": "%s"}""".format(title,manufacturer,currency,price)
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

	val ACCEPT_VALUE = 0.8
	val PRICE_SKIP_RATIO = 0.4 // if price less then PRICE_SKIP_RATIO * average_prices then we skip this listing

	def readJson[A](filename: String, makeObject: Map[String, String] => Option[A]):Iterator[A] = {
		Source.fromFile(filename).getLines.flatMap {
			line => 
			JSON.parseRaw(line) match {
				case Some(jsonType) => {
					makeObject(jsonType.asInstanceOf[JSONObject].obj.asInstanceOf[Map[String, String]])
				}
				case None => {
					System.err.printf ("Cannot parse =" + line + "=")
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
		} return Some(Product(product_name, 
							  manufacturer, 
							  family, 
							  model, 
							  announced_date,
							  (product_name + " " + family + " " + model + " " + manufacturer).toLowerCase()
									.replace("_"," ").replace("-"," ").split(" ")
			))
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

	// b lowercased already
	def instr(a: String, b: String) = if ( (" " + a.toLowerCase() + " ").indexOf(" " + b + " ") != -1) 1.0 else 0.0

	def weight(listing: Listing, product: Product): Double = {
		val words = product.words
		val probeTitle = listing.title.replace("_"," ").replace("-"," ")
		words.foldLeft(0.0)( (res, word) => res + instr(probeTitle, word) ) / words.size
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
	
	

	def extractListings(oneResults: GenTraversable[OneResult]):Array[Listing] = {
		def parseDouble(s: String) = try { Some(s.toDouble) } catch { case _ => None }

		val listings = oneResults.map(_.listing)
		val priceLimit = PRICE_SKIP_RATIO * listings.flatMap( listing => parseDouble(listing.price)).sum / listings.size
		
		//println (priceLimit)

		// we skip listings with price less then some threshold value
		listings.filter( l =>
		  parseDouble(l.price) match  {
			case Some(price) => price > priceLimit
			case _ => false
		}).toArray

	}

	val products = readJson("products.txt", productFromMap).toList
	
	val results = readJson("listings.txt", listingFromMap)
					//.take(1300)
					.toTraversable
					//.par
					.flatMap(listing => assign(listing, products)) // match listing with some product, or not
					.groupBy(_.product_name)
					.map( x => Result(x._1,extractListings(x._2)))
	
	// let's print results
	val out = new PrintWriter("results.txt")

	for (result <- results) {
		out.print("{")
		out.print(""""product_name":"""" + result.product_name + """",""")
		out.print(""""listings":[""")
		out.println(result.listings.mkString(","))
		out.println("]}")
	}
	out.close()
}