/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import generic._

import compat.Platform.{ ConcurrentModificationException => CME }

/** 
 * @define Coll mutable.ConcurrentTreeSet
 * @define coll mutable concurrent tree set
 * @factoryInfo
 *   Companion object of ConcurrentTreeSet providing factory related utilities.
 * 
 * @author Lucien Pereira
 * 
 */
object ConcurrentTreeSet extends MutableSortedSetFactory[ConcurrentTreeSet] {
  /**
   *  The empty set of this type
   */
  def empty[A](implicit ordering: Ordering[A]) = new ConcurrentTreeSet[A]()

}
  
/**
 * A mutable concurrent SortedSet using an immutable AVL Tree as underlying
 * data structure.
 *
 * @author Lucien Pereira
 * 
 */
class ConcurrentTreeSet[A](implicit val ordering: Ordering[A]) extends SortedSet[A]
  with SetLike[A, ConcurrentTreeSet[A]]
  with SortedSetLike[A, ConcurrentTreeSet[A]]
  with Serializable {

  // Projection constructor
  private def this(base: Option[ConcurrentTreeSet[A]], from: Option[A], until: Option[A])(implicit ordering: Ordering[A]) {
    this()
    this.base = base
    this.from = from
    this.until = until
  }
  
  private var stateId = Long.MinValue;

  private var base: Option[ConcurrentTreeSet[A]] = None

  private var from: Option[A] = None

  private var until: Option[A] = None

  private var avl: AVLTree[A] = Leaf

  private var cardinality: Int = 0

  def resolve: ConcurrentTreeSet[A] = base.getOrElse(this)

  private def isLeftAcceptable(from: Option[A], ordering: Ordering[A])(a: A): Boolean =
    from.map(x => ordering.gteq(a, x)).getOrElse(true)

  private def isRightAcceptable(until: Option[A], ordering: Ordering[A])(a: A): Boolean =
    until.map(x => ordering.lt(a, x)).getOrElse(true)

  /**
   * Cardinality store the set size, unfortunately a
   * set view (given by rangeImpl)
   * cannot take advantage of this optimisation
   * 
   */
  override def size: Int = base.map(_ => super.size).getOrElse(cardinality)

  override def stringPrefix = "ConcurrentTreeSet"

  override def empty: ConcurrentTreeSet[A] = ConcurrentTreeSet.empty

  override def rangeImpl(from: Option[A], until: Option[A]): ConcurrentTreeSet[A] = new ConcurrentTreeSet(Some(this), from, until)

  override def -=(elem: A): this.type = {
    try {
      synchronized {
        resolve.avl = resolve.avl.remove(elem, ordering)
        resolve.cardinality = resolve.cardinality - 1
        resolve.stateId = resolve.stateId + 1
      }
    } catch {
      case e: NoSuchElementException => ()
    }
    this
  }

  override def +=(elem: A): this.type = {
    try {
      synchronized {
        resolve.avl = resolve.avl.insert(elem, ordering)
        resolve.cardinality = resolve.cardinality + 1
        resolve.stateId = resolve.stateId + 1
      }
    } catch {
      case e: IllegalArgumentException => ()
    }
    this
  }

  /**
   * Thanks to the immutable nature of the
   * underlying AVL Tree, we can share it with
   * the clone. So clone complexity in time is O(1).
   * 
   */
  override def clone: ConcurrentTreeSet[A] = {
    val clone = new ConcurrentTreeSet[A](base, from, until)
    clone.avl = resolve.avl
    clone.cardinality = resolve.cardinality
    clone
  }

  override def contains(elem: A): Boolean = {
    isLeftAcceptable(from, ordering)(elem) &&
    isRightAcceptable(until, ordering)(elem) &&
    resolve.avl.contains(elem, ordering)
  }
  
  /**
   * Creates a new iterator over all elements contained in this iterable object.
   *
   * @throws scala.compat.Platform.ConcurrentModificationException is a
   * concurrent modification is detected
   */
  override def iterator: Iterator[A] = new Iterator[A] {
    val stateIdAtIteratorCreation = stateId
    val underlying = resolve.avl.iterator
      .dropWhile(e => !isLeftAcceptable(from, ordering)(e))
        .takeWhile(e => isRightAcceptable(until, ordering)(e))
    
    override def hasNext: Boolean = underlying.hasNext
  
    override def next(): A = {
      if(stateIdAtIteratorCreation != stateId)
        throw new CME()
      else
        underlying.next()
    }
  }
  
}

