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
  val actionMap = Map[Int, () => Boolean](1 -> handleOne, 2 -> handleTwo, 3 -> handleThree)

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
        |  3 - quit""".stripMargin)
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

  def handleThree(): Boolean = {
    println("selected quit") // returns false so loop terminates
    false
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

  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user


  def getCurrentPrice(): Map[String, Int] = {
    val currentPriceMap :Map[String, Int]  = mapdata.map(x => x._1 -> x._2.last)

    currentPriceMap
  }




  def getHighestLowest(): Map[String, List[Int]] = {
    val highestLowestMap: Map[String, List[Int]] = mapdata.map(x => x._1 -> List(x._2.min,x._2.max))
    highestLowestMap
  }


  // *******************************************************************************************************************

}
