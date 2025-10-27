package TLUtils

/*
 ** follow the Apache-2.0 License.(c), All rights reserved **
 * this file will make a more friendly use fifo and the fifo control is more simple to use *
*/

import spinal.core._
import spinal.lib._
import TLUtils.TLPlugin.TLModule

class SimplifyFIFO[T <: Data](gen: HardType[T], val entries: Int) extends TLModule {

  val io = new Bundle {
    val push = slave(Stream(gen))
    val pop = master(Stream(gen))
  }

  val enq_ptr = Counter(entries).init(0)
  val deq_ptr = Counter(entries).init(0)

  val queue = Mem(gen, entries)
  val full_empty = RegInit(False)
  io.pop.payload := queue.readAsync(deq_ptr)

  when(io.push.fire) {
    /* into the queue */
    queue.write(enq_ptr, io.push.payload)
    enq_ptr.increment()
  }

  when(io.pop.fire) {
    /* out the queue */
    deq_ptr.increment()
  }

  val empty = enq_ptr === deq_ptr && !full_empty
  val full = enq_ptr === deq_ptr && full_empty

  /* the key concept about the difference push and pop */
  when(io.push.fire =/= io.pop.fire) {
    full_empty := io.push.fire
  }

  io.push.ready := !full
  io.pop.valid := !empty
}

/* one cycles late */
object SimplifyFIFO{
  def apply[T <: Data](pop: Stream[T], entries: Int): Stream[T] = {
    val fifo = new SimplifyFIFO(cloneOf(pop.payload), entries)
    fifo.io.pop >> pop
    fifo.io.push
  }
}
