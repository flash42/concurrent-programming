package chapter2

import util.*

def produceAndConsume(times: Int = 15): Int = {
  val syncVar = SyncVar[Int]()

  var cnt = 0
  val producer = thread {
    while cnt < times do
      syncVar.synchronized {
        if syncVar.isEmpty then
          syncVar.put(cnt)
          cnt += 1
      }

    println("producer done")
  }
  val consumer = thread {
    while cnt < times do
      syncVar.synchronized {
        if syncVar.nonEmpty then
          println(syncVar.get())
      }
    println("consumer done")
  }
  producer.join()
  consumer.join()
  cnt
}