package philosophers

import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import scala.collection.{immutable, mutable}

object Philosophers {
	
	class PhilosopherActor(val name:String, val leftFork:Int, val rightFork:Int) extends Actor {
	private val random = new Random()
	private var meals = 5
	def act() {
			loop {
				react {
					case "enter_dining_room" =>	{
						waiter ! "guest_arriving"
						println(name + " is thinking.")
						Thread.sleep(random.nextInt(100))
						println(name + " is hungry.")
						waiter ! ("forks_available", leftFork, rightFork, self)
					}
					case "forks_given" => {
						println(name + " is eating meal nr. " + meals)
						Thread.sleep(random.nextInt(100))
						waiter ! ("forks_release", leftFork, rightFork)
						meals -= 1
						if(meals > 0) {
							println(name + " is thinking.")
							Thread.sleep(random.nextInt(100))
							waiter ! ("forks_available", leftFork, rightFork, self)
						} else {
							waiter ! "guest_leaving"
							println(name + " is leaving.")
							exit
						}
					}
					case "forks_busy" => {
						println(name + "'s forks are busy.")
						println(name + " is thinking.")
						Thread.sleep(random.nextInt(100))
						println(name + " is hungry.")
						waiter ! ("forks_available", leftFork, rightFork, self)
					}
				}
			}
		}
	}
	

	class WaiterActor extends Actor {
		private var guests = 0
		private val forks = new mutable.ArrayBuffer[Int]
		forks.appendAll(List(1,2,3,4,5))

		println( "Dining room opens.")

		def act() {
			loop {
				react {
					case ("forks_available", leftFork:Int, rightFork:Int, sender:PhilosopherActor) => {
						if ((forks.contains(leftFork) && forks.contains(rightFork)) == true) {
							forks -= leftFork
							forks -= rightFork
							sender ! "forks_given"
						} else {
							sender ! "forks_busy"
						}
					}
					case ("forks_release", leftFork:Int, rightFork:Int) => {
						forks += leftFork
						forks += rightFork
					}
					case "guest_arriving" => {
						guests += 1
					}
					case "guest_leaving" => {
						guests -= 1
						if(guests == 0)	{
							println( "Waiter is leaving.")
							println( "Dining room closes")
							exit
						}
					}
				}
			}
		}
	}

	val waiter =  new WaiterActor
	val plato =  new PhilosopherActor("Plato", 5, 1)
	val confucius =  new PhilosopherActor("Confucius", 1, 2)
	val socrates =  new PhilosopherActor("Socrates", 2, 3)
	val voltaire =  new PhilosopherActor("Voltaire", 3, 4)
	val descartes =  new PhilosopherActor("Descartes", 4, 5)

	def main(args:Array[String]) {
		waiter.start()
		plato.start()
		confucius.start()
		socrates.start()
		voltaire.start()
		descartes.start()

		plato ! "enter_dining_room"
		confucius ! "enter_dining_room"
		socrates ! "enter_dining_room"
		voltaire ! "enter_dining_room"
		descartes ! "enter_dining_room"
	}
}