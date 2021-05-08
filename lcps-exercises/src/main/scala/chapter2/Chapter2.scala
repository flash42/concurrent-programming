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
          println(syncVar.get)
      }
    println("consumer done")
  }
  producer.join()
  consumer.join()
  cnt
}

def produceAndConsume2(times: Int = 15): Int = {
  val syncVar = SyncVar[Int]()

  var cnt = 0
  val producer = thread {
    while (cnt < times)
      syncVar.synchronized {
        while (syncVar.nonEmpty) syncVar.wait()
        syncVar.put(cnt)
        cnt += 1
        syncVar.notify()
      }


  }
  val consumer = thread {
    while (cnt < times)
      syncVar.synchronized {
        while (syncVar.isEmpty) syncVar.wait()
        println(syncVar.get)
        syncVar.notify()
      }

  }
  producer.join()
  consumer.join()
  cnt
}

def produceAndConsume3(times: Int): Int = {
  val syncVar = SyncVar[Int]()

  var cnt = 0
  val producer = thread {
    while (cnt < times)
      syncVar.synchronized {
        syncVar.putWait(cnt)
        cnt += 1
        syncVar.notify()
      }


  }
  val consumer = thread {
    while (cnt < times)
      syncVar.synchronized {
        println(syncVar.getWait())
        syncVar.notify()
      }

  }
  producer.join()
  consumer.join()
  cnt
}
