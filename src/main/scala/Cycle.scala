package cycles

import java.util.concurrent.atomic.{ AtomicInteger, AtomicLong }

/**
 * A Cycle is an Iterator that has will never be empty except
 * in cases where Cycle is created with an empty iterable.
 */
trait Cycle[+T] extends Iterator[T]

trait Counter[+T] extends Cycle[T] {
  def until: T
  def zero: T
  def max = until
  def min = zero
  def hasNext = true
}

object Cycle {

  def ints(_until: Int): Cycle[Int] =
    new Counter[Int] {
      val until = _until
      val zero = 0
      val counter = new AtomicInteger(until)
      def turn = {
        val value = counter.getAndIncrement() % until
        if (value == zero) counter.getAndAdd(-until)
        value
      }
      def next = turn
    }

  def longs(_until: Long): Cycle[Long] =
    new Counter[Long] {
      val until = _until
      def zero = 0L
      val counter = new AtomicLong(until)
      def turn = {
        val value = counter.getAndIncrement() % until
        if (value == zero) counter.getAndAdd(-until)
        value
      }
      def next = turn
    }

  def through[T](it: Iterable[T]): Cycle[T] =
    new Cycle[T] {
      @volatile var iter = it.iterator
      def hasNext = it.nonEmpty
      def next = {
        if (!iter.hasNext) {
          iter = it.iterator
        }
        iter.next
      }
    }
}
