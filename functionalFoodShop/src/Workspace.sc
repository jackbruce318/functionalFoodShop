import scala.io.Source

def readFile(filename: String): Map[String, List[Int]] = {
  // create buffer to build up map as we read each line

  var mapBuffer: Map[String, List[Int]] = Map()
  try {
    for (line <- Source.fromFile(filename).getLines()) {
      // for each line
      val splitline = line.split(",").map(_.trim).toList // split line at , and convert to List

      // add element to map buffer
      // splitline is line from file as List, e.g. List(Bayern Munich, 24)
      // use head as key
      // tail is a list, but need just the first (only in this case) element, so use head of tail and convert to int
      mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.map(_.toInt))

    }
  } catch {
    case ex: Exception => println("Sorry, an exception happened.")
  }
  mapBuffer
}



val mapdata = readFile("C:\\Users\\actio\\Documents\\GitHub\\functionalFoodShop\\functionalFoodShop\\data.txt");

mapdata.map(x => x._2.apply(x._2.length / 2))

val newMapData :Map[String, List[Int]]  = mapdata.map(x => x._1 -> List(x._2.min,x._2.max))

newMapData.map(x => println(x._1 + " " + x._2))