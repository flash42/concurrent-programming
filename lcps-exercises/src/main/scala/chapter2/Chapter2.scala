package chapter2

import util.*

def produceAndConsume(): Unit = {
  val syncVar = SyncVar[Int]()

  var cnt = 0
  val producer = thread {

      while cnt < 15 do
        syncVar.synchronized {
          if syncVar.isEmpty then
            syncVar.put(cnt)
            cnt += 1
        }

  }
  val consumer = thread {
      while syncVar.synchronized { syncVar.nonEmpty } do
        println(syncVar.get())

  }
  producer.join()
  consumer.join()
}