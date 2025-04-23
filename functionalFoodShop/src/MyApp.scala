import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap
import scala.util.{Try, Success, Failure}

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
  val actionMap = Map[Int, () => Boolean](1 -> handleOne, 2 -> handleTwo, 3 -> handleThree, 4-> handleFour, 5 -> handleFive, 6 -> handleSix)

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
        |  5 - Build a Basket
        |  6 - quit""".stripMargin)
    try{
      readInt()
    }
    catch{
      //used recursion here to have the user always returned to the menu
      case _: NumberFormatException =>
        println("Invalid input, please try again")
        readOption
    }
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

  def handleFive(): Boolean = {
    mnuBasket(getBasket)
    true
  }


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS

  // reads data file - comma separated file
  //method has been altered in order to be functional rather than imperative
  def readFile(filename: String): Map[String, List[Int]] = {
    try {
      val lines = Source.fromFile(filename).getLines().toList

      lines.foldLeft(Map.empty[String, List[Int]]) { (acc, line) =>
        val splitline = line.split(",").map(_.trim).toList
        //Use head as key and tail as the list of values
        acc + (splitline.head -> splitline.tail.map(_.toInt))
      }
    } catch {
      case ex: Exception =>
        println("Sorry, an exception happened.")
        Map.empty[String, List[Int]]
    }
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

  def mnuBasket(f: (Map[String, Double]) => Option[(Map[String, Int], List[Double])]) =
  {
    val noOfItems: String = readLine("How many items would you like to add to your basket? ")

    def validateInt(input: String): Int = {
      //Try to convert to int, if it fails then ask again recursively
      try {
        math.abs(input.toInt)
      }
      catch {
        case _: NumberFormatException =>
          val input = readLine("That's not a valid number. Please try again: ")
          validateInt(input)  //Recursive call to try again
      }
    }


    def getItemsFromBasket(noItems: Int, acc: Map[String,Double]): Map[String,Double] = {
      //if the number of items requested have been added, return the map
      if (noItems == 0) {
        acc
      }
      else {
        //take inputs for each item
        val userInput1 = readLine("Which food item do you wish to add? ").toUpperCase()
        val userInput2 = readLine("How many kg you wish to add? ")


        mapdata.get(userInput1) match {
          //if get method returns values
          case Some(prices) => {
            //then test the second input
            Try(math.abs(userInput2.toDouble)) match {
              case Success(value) => {
                //add it to the basket with correct key-value pair
                val newAcc = acc + (userInput1 -> value)
                getItemsFromBasket(noItems - 1, newAcc)
              }
              case Failure(_) => {
                println("Invalid Input, try again")
                getItemsFromBasket(noItems, acc)
              }
            }
          }
          //if None is returned, item is skipped
          case None => {
            println("Invalid Input, try again")
            getItemsFromBasket(noItems, acc)
          }
        }


      }
    }

    val intNoOfItems = validateInt(noOfItems)
    val basketMap: Map[String, Double] = getItemsFromBasket(intNoOfItems, Map.empty)

    val basketTuple = f(basketMap)

    //Process the tuple using pattern matching and recursion
    basketTuple match {
      case Some((items, quantities)) => {

        //recursive function to process elements
        def printTupleEntries(itemEntries: List[(String, Int)], quantities: List[Double],totalCostAcc: Double = 0.0
        ): Double = {
          (itemEntries, quantities) match {
            case (Nil, _) | (_, Nil) =>

              //when either list is empty, stop recursion
              totalCostAcc
            case ((item, price) :: remainingItems, quantity :: remainingQuantities) =>
              //Print current map entry and corresponding quantity
              println(s"Item: $item, Quantity: $quantity, Price: " + price*quantity + "p")
              //Recursive call with remaining entries
              printTupleEntries(remainingItems, remainingQuantities, totalCostAcc + price*quantity)
          }
        }

        // Start the recursive printing with the map entries converted to a list
        val totalCost = printTupleEntries(items.toList, quantities)
        println(s"Total Cost: $totalCost")
      }
      case None =>
        println("No basket information available")
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

  //takes strings inputted by user and returns a tuple containing a Map (Key -> Average) and the difference between both averages (Int)
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

  def getBasket(basket: Map[String, Double]): Option[(Map[String, Int], List[Double])] = {

    //This function uses head recursion to build a new map of the selected items and their most recent prices
    def buildBasketMap(keys: List[String], acc: Map[String, Int]): Map[String, Int] = keys match {
      case Nil => acc //If list is empty, there are no more items to process

      case head :: tail => //if there are items in the list

        //Look up the current key in mapdata
        val newAcc = mapdata.get(head) match {

          case Some(prices) => acc + (head -> prices.last) //if an entry is found then add to map here
          case None => acc //Skip keys not found in mapdata
        }

        //Recursive call with the tail and updated accumulator
        buildBasketMap(tail, newAcc)
    }

    val quantities: List[Double] = basket.values.toList

    // Start recursion with the list of basket keys and an empty map
    val basketTuple = (buildBasketMap(basket.keys.toList, Map.empty), quantities)
    Some(basketTuple)
  }


  // *******************************************************************************************************************

}
