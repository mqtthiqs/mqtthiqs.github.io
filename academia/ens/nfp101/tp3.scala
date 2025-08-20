abstract class Simulation {
  
  type Action = () => Unit
  case class WorkItem(time: Int, action: Action)

  private var time = 0
  private var agenda: List[WorkItem] = List()
  
  private def insert(ag: List[WorkItem],
		     item: WorkItem): List[WorkItem] = {
    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)
  }
  
  def afterDelay(delay: Int)(block: => Unit) = {
    val item = WorkItem(time + delay, () => block)
    agenda = insert(agenda, item)
  }
  
  private def next() = agenda match {
    case item :: rest =>
      agenda = rest
      curtime = item.time
      item.action()
    case Nil => ()
  }

  def run() = {
    afterDelay(0) {
      println("*** simulation started, time = "+ time +" ***")
    }
    while (!agenda.isEmpty) next()
  }
}

class Wire {

  private var signalValue = false
  private var actions : List[Action] = List()

  def sig = signalValue
  def sig_=(s:Boolean) =
    if (s != signalValue) {
      signalValue = s
      actions foreach (_ ())
    }

  def >>(a: =>Unit) = {
    actions = (() => a) ::actions
    a
  }
}

abstract class CircuitSimulation extends Simulation {

  val notDelay : Int
  val andDelay : Int
  val orDelay : Int

  def not(input: Wire, output: Wire) =
    input >> { afterDelay(notDelay) { output.sig = !input.sig } }

  def and(in1: Wire, in2: Wire, output: Wire) = {
    in1 >> {afterDelay(andDelay) { output.sig = in1.sig & in2.sig }}
    in2 >> {afterDelay(andDelay) {output.sig = in1.sig & in2.sig }}
  }

  def or(in1: Wire, in2: Wire, output: Wire) = {
    in1 >> {afterDelay(orDelay) {output.sig = in1.sig | in2.sig }}
    in2 >> {afterDelay(orDelay) {output.sig = in1.sig | in2.sig }}
  }

  def probe(s: String, w: Wire) = {
    w >> { println(s+" = "+w.sig+" (time="+time+")") }
  }
}

abstract class ComplexCircuitSimulator extends CircuitSimulation {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {
    val d, e = new Wire
    or(a, b, d)
    and(a, b, c)
    not(c, e)
    and(d, e, s)
  }
}

object MySimulation extends ComplexCircuitSimulator {
  val notDelay = 1
  val andDelay = 3
  val orDelay = 5
}

import MySimulation._

object TP3 {
  def main(args: Array[String]) = {

    val w = new Wire
    w >> (println("w becomes "+w.sig))
    w.sig = true
    run()
    w.sig = false
    run()

    val x, y, z, t = new Wire;
    and(x, y, z)
    not(z, t)
    probe("nand output", t)
    x.sig = true
    run()
    y.sig = true
    run()

    val a, b, s, c = new Wire;
    halfAdder(a, b, s, c);
    probe("sum", s)
    probe("carry", c)

    a.sig = true
    run()
    b.sig = true
    run()
  }
}
