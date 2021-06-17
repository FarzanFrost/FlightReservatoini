import flightquestion.{MakeSeatFree, MakeSeatOccupied}

import scala.io.StdIn.readInt

object flightquestion extends App {
    val noOfRows = 10
    val noOfCols = 10
    val cities = Array("Colombo" , "Galle" , "Batticaloa" , "Trincomalee" , "Jaffna" , "Mannar" , "Vavuniya" , "Anuradhapura" , "Dambulla" , "Kandy" , "Nuwara Eliya")
    var seats = Array.ofDim[Boolean](noOfRows,noOfCols)
    var temp = 0

    while(temp < 12){
        val currentCityPositionInArray = (temp % (cities.size))
        val nextCityPositionInArray = ((temp+1) % (cities.size))
        println("The list of cities the flight will travel to:")
        displayAllCities(cities)
        println(s"The flight is situated in ${cities(currentCityPositionInArray)}")
        println(s"The flight goes next to ${cities(nextCityPositionInArray)}")
        println("Current Status of seats")
        printArray(seats);



        var stopWhileLoop = true

        while(stopWhileLoop){
            println("Options")
            println("1 : Occupy seat")
            println("2 : Free seat")
            println("3 : Number of free seats")
            println("4 : Flight Travels to next city")
            println("Choose option number from the list below")

            val option = readInt()

            option match {
                case 1 => {
                    println("Enter the number of seats to occupy")
                    val noOfSeatsToOccupy = readInt()
                    MakeSeatOccupied(noOfSeatsToOccupy)
                }
                case 2 => {
                    println("Enter the number of seats to free")
                    val noOfSeatsToFree = readInt()
                    MakeSeatFree(noOfSeatsToFree)
                }
                case 3 => {
                    println("No of free seats available " + freeNoOfSeats())
                }
                case 4 => {
                    stopWhileLoop = false
                }
                case _ => println("Invalid input")

            }
            temp = temp +1
        }

    }


    def printArray(a:Array[Array[Boolean]]) = {
        for {
            i <- 0 to noOfRows-1
            j <- 0 to noOfCols-1
        }println("row " + i + " "+ "column " + j + " " +a(i)(j))
    }

    def displayAllCities(x : Array[String]) = {
        for{
            i <- 0 to (x.size-1)
        }{
            println(x(i))
        }
    }

    def MakeSeatFree(noOfSeats : Int): Unit ={
        for {
            i <- 1 to noOfSeats
        }{
            println("Enter row number of the seat")
            var rowNumber = readInt()
            if(rowNumber >= noOfRows){
                println("Invalid row number")
                println("Enter row number of the seat")
                rowNumber = readInt()
            }
            println("Enter column number of the seat")
            var columnNumber = readInt()
            if(columnNumber >= noOfCols){
                println("Invalid column number")
                println("Enter column number of the seat")
                columnNumber = readInt()
            }
            if (seats(rowNumber)(columnNumber)) {
                seats(rowNumber)(columnNumber) = false
                println("Seat successfully Free")
            }
            else {
                println("Seat already Free")
            }
            ()
        }
    }

    def MakeSeatOccupied(noOfSeats : Int): Unit ={
        for {
            i <- 1 to noOfSeats
        }{
            println("Enter row number of the seat")
            var rowNumber = readInt()
            if(rowNumber >= noOfRows){
                println("Invalid row number")
                println("Enter row number of the seat")
                rowNumber = readInt()
            }
            println("Enter column number of the seat")
            var columnNumber = readInt()
            if(columnNumber >= noOfCols){
                println("Invalid column number")
                println("Enter column number of the seat")
                columnNumber = readInt()
            }
            if(!seats(rowNumber)(columnNumber)) {
                seats(rowNumber)(columnNumber) = true
                println("Seat successfully occupied")
            }
            else {
                println("Seat already occupied")

            }
            ()
        }
    }

    def freeNoOfSeats(): Int = {
        var noOfFreeSeats = 0
        for {
            i <- 0 to noOfRows-1
            j <- 0 to noOfCols-1
        }{
            if(!seats(i)(j)) {
                noOfFreeSeats = noOfFreeSeats + 1
            }
        }

        noOfFreeSeats
    }
}
