package aoc2022.util

import javax.swing.tree.TreeNode
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait SimpleTree[A, B] {
  def insert(path: List[A], value: B): Unit
  def get(path: List[A]): B
}

object SimpleTree {
  def apply[A, B](): SimpleTree[A, B] = {
    new SimpleTreeImpl[A, B]
  }
}

class MyTreeNode[C, D](val name: C,
                       val value: D,
                       var children: mutable.ArrayBuffer[MyTreeNode[C, D]]) {}

class SimpleTreeImpl[A, B] extends SimpleTree[A, B] {
  var root: Option[MyTreeNode[A, B]] = None

  override def insert(path: List[A], value: B): Unit = {
    root match {
      case None =>
        root = Option(new MyTreeNode[A, B](path.head, value, ArrayBuffer()))
    }
  }

  override def get(path: List[A]): B = {
    ???
  }
}

