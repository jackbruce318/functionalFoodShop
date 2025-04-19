/**
  * Created by jim on 06/11/2016.
  */

import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap

object MyApp extends App {

  // *******************************************************************************************************************
  // application logic

  // read data from file
  val mapdata = readFile("data.txt")
  // print data to check it's been read in correctly
  println(mapdata)

  // define menu options as a Map of actions
  // for each menu item:
  // key is an Int, the value that will be read from the input
  // value is a function () => Boolean, i.e. no params and returns Boolean
  val actionMap = Map[Int, () => Boolean](1 -> handleOne, 2 -> handleTwo, 3 -> handleThree, 4-> handleFour, 6 -> handleSix)

  // loop to read input and invoke menu option
  // uses function readOption to show menu and read input
  // uses function menu to invoke menu action
  // will terminate if menu returns false
    var opt = 0
    do {
      opt = readOption
    } while (menu(opt))


  // *******************************************************************************************************************
  // FUNCTIONS FOR MENU

  // shows menu and reads input
  def readOption: Int = {
    println(
      """|Please select one of the following:
        |  1 - Get Most Recent Prices
        |  2 - View Highest and Lowest Prices
        |  3 - View Median Prices
        |  4 - Compare Two Average Prices
        |  6 - quit""".stripMargin)
    readInt()
  }

  // invokes selected menu option
  // finds corresponding function to invoke in action map using get
  // pattern matching used as get returns an Option
  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  // handlers for menu options
  def handleOne(): Boolean = {
    mnuShowPrices(getCurrentPrice) // calls function mnuShowPoints, which invokes function currentPoints
    true
  }

  def handleTwo(): Boolean = {
    mnuHighestLowest(getHighestLowest)
    true
  }

  def handleSix(): Boolean = {
    println("selected quit") // returns false so loop terminates
    false
  }

  def handleThree(): Boolean = {
    mnuMedian(getMedian)
    true
  }

  def handleFour(): Boolean = {
    mnuAvgs(getAvg)
    true
  }


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS

  // reads data file - comma separated file
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


  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results

  def mnuShowPrices(f: () => Map[String, Int]) = {
        f() map(x => println(x._1 + ": " + x._2 + "p"))
  }

  def mnuHighestLowest(f: () => Map[String,List[Int]]) = {
        f() map(x => println(x._1 + ": Lowest: " + x._2.min + "p - Highest: " + x._2.max + "p"))
  }

  def mnuMedian(f: () => Map[String,Double]) = {
        f() map(x => println(x._1 + ": Median: " + x._2 + "p"))
  }

  def mnuAvgs(f: (String, String) => Option[(Map[String, Int], Int)]) = {

    //get foods from user
    //cast both to upper case since they are all stored that way
    val food1 = readLine("Enter the first food type: ").toUpperCase()
    val food2 = readLine("Enter the second food type: ").toUpperCase()

    val avgTupleOption = f(food1, food2)
    avgTupleOption match {
      case Some((avgMap, result)) =>
        //if both foods exist
        avgMap.map(x => println(x._1 + ": " + x._2 + "p"))
        println("The difference between these two averages was: " + result)

      case None =>
        //If any entered food symbols were invalid
        println("One or both food types could not be found in the data.")
    }

  }

  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user


  def getCurrentPrice(): Map[String, Int] = {
    val currentPriceMap :Map[String, Int]  = mapdata.map(x => x._1 -> x._2.last)

    currentPriceMap
  }

  def getMedian(): Map[String, Double] = {
    val medianMap: Map[String, Double] = mapdata.map(x => x._1 -> median(x._2))
    medianMap
  }


  def getHighestLowest(): Map[String, List[Int]] = {
    val highestLowestMap: Map[String, List[Int]] = mapdata.map(x => x._1 -> List(x._2.min,x._2.max))
    highestLowestMap
  }

  def getAvg(userInput1: String, userInput2: String): Option[(Map[String, Int], Int)] = {
    //Use get to check if keys exist
    val avg1Option = mapdata.get(userInput1).map(data => data.sum / data.length)
    val avg2Option = mapdata.get(userInput2).map(data => data.sum / data.length)

    //Take the avg value from the option and cast it to an integer value
    for {
      avg1 <- avg1Option
      avg2 <- avg2Option
    } yield {
      //This only executes if both keys exist
      val result = math.abs(avg1 - avg2)
      val avgMap = Map(userInput1 -> avg1, userInput2 -> avg2)
      (avgMap, result)
    }
  }



  def median(values: List[Int]): Double = {

    val len = values.length

    //if the length of the list is odd then just return the middle element
    if (len % 2 == 1) {
      values(len / 2).toDouble
    }
      //if the length is even then return the average of the two middle numbers
      //in simpler terms - values(len / 2) will return the number on the "right" half of the list, and
      //values(len / 2 - 1) will return the val on the "left" side of the list
      //the list was not ordered for this operation as it would not accurately reflect the fluctuation of food prices
      //over the time period
    else {
      val (up, down) = (values(len / 2), values(len / 2 - 1))
      (up + down) / 2.0
    }
  }


  // *******************************************************************************************************************

}
