package chapter2

import util.*

object Account {
  var uidCount = 0L

  def getUniqueId() = this.synchronized {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }
}

class Account(val name: String, var money: Int) {
  val uid = Account.getUniqueId()
}

import scala.collection._

private val transfers = mutable.ArrayBuffer[String]()

def logTransfer(name: String, n: Int) = transfers.synchronized {
  transfers += s"transfer to account '$name' = $n"
}
def add(account: Account, n: Int) = account.synchronized {
  account.money += n
  if (n > 10) logTransfer(account.name, n)
}
def send_deadlock(a: Account, b: Account, n: Int) = a.synchronized {
  b.synchronized {
    a.money -= n
    b.money += n
  }
}

def send(a1: Account, a2: Account, n: Int): Unit = {
  def adjust() = {
    a1.money -= n
    a2.money += n
  }

  if (a1.uid < a2.uid)
    a1.synchronized {
      a2.synchronized {
        adjust()
      }
    }
  else a2.synchronized {
    a1.synchronized {
      adjust()
    }
  }
}

def sendAll_with_multiple_threads(accounts: Set[Account], target: Account): Unit = {
  // Assumptions:
  // - we shall use send function
  // - all send operations run in their own thread
  // TODO how can a deadlock occur?
  val r = scala.util.Random
  val threads = accounts.map(a => thread {
    Thread.sleep(r.nextLong(100))
    send(a, target, a.money)
  })
  for (t <- threads) t.join()
}

def sendAll(accounts: Set[Account], target: Account): Unit = {
  for (a <- accounts) {
    val m = a.money
    send(a, target, m)
  }
}
