package simulations

import math.random

class EpidemySimulator extends Simulator {
  

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    
    val incubationPeriod = 6
    val mayDiePeriod = 14
    val sickPeriod = 16
    val immunePeriod = 18
    
    val prevalence = 0.01
    val deathRate = 25
    val transmissionRate = 40
  }

  import SimConfig._
  
  def generatePopulation() : List[Person] = {
    val ids = 0 until population
    val persons : List[Person] = List.range(0, population).map((x: Int) => new Person(x))
    val initially_infected = (prevalence * population).toInt
    for(i <- 0 to initially_infected-1) {
      persons(i) infect
    }
    for(p <- persons) {
      p.move
    }
    persons
  }
  
  val persons : List[Person] = generatePopulation()

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
   
    override def toString() : String = {
      return id+"("+row+","+col+") "
    }
    
    def personsInRoom(r: Int, c: Int) : List[Person] = {
      persons.filter(p => p.row == r && p.col == c && p.id != id)
    }
    
    def inSameRoom() : List[Person] = {
      personsInRoom(row, col)
    }
   
    def infectious() : Boolean = {
      return infected || dead || immune || sick
    }
    
    def visiblyInfectous() : Boolean = {
      return sick || dead
    }
    
    def infect() : Unit = {
      if (infected || dead || immune) return 
      infected = true
      
      afterDelay(incubationPeriod) {
        becomeSick()
      }
      afterDelay(mayDiePeriod) {
    	  mayDie()
      }
      afterDelay(sickPeriod) {
        becomeImmune()
      }
      afterDelay(immunePeriod) {
        becomeHealthy()
      }
    }
    
    def becomeSick() {
      sick = true
    }
    
    def mayDie() {
      if (randomBelow(100)+1 < deathRate) {
        dead = true
      }
    }
    
    def becomeImmune() {
    	if (!dead) {
    	  immune = true
    	  sick = false
    	}
    }
    
    def becomeHealthy() {
      if (!dead) {
        immune = false
        infected = false
      }
    }
    
    def roomAfter(move: (Int, Int)) = {
      var new_r = (row + move._1) % roomRows
      if (new_r < 0) {
        new_r = new_r + roomRows
      }
      var new_c = (col + move._2) % roomColumns
      if (new_c < 0) {
        new_c = new_c + roomColumns
      }
      (new_r, new_c)
    }
    
    def calculateNextRoom() : (Int, Int) = {
      val safeRooms = for {
        move <- (1,0)::(-1,0)::(0,1)::(0,-1)::List()
        (room_r, room_c) = roomAfter(move)
        peopleInRoom = personsInRoom(room_r, room_c)
        if peopleInRoom forall (p => !p.visiblyInfectous)
      } yield (room_r, room_c)
      
      safeRooms match {
        case Nil => (row, col)
        case rooms => {
          safeRooms(randomBelow(rooms.length))
        }
      }
    }
    
    def enter(room: (Int, Int)) = {
      row = room._1
      col = room._2
      if (inSameRoom.exists(p => p.infectious)) {
        if (randomBelow(100)+1 < transmissionRate) infect()
      }
    }
    
    def move() : Unit = {
      afterDelay(randomBelow(5)+1) {
        if (!dead) {
          val nextRoom = calculateNextRoom()
          if (nextRoom != (row, col)) {
            enter(nextRoom)
          }
    	  move()
        }
      }
    }
  }
}
