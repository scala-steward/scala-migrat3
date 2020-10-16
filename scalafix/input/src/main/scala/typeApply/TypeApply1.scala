/*
rule = AddTypeApply
*/
package typeApply

object TypeApply1 {
//  val some = Some(1).map(_ + 2).map(_ + 3)
  val people = List(1).map(y => "string")
//
//  def a[A, B](fa: A, fb: B): Tuple2[A, B] = Tuple2(fa, fb)
//  def ok = Some(Test.Test1(""))


}

//object Test {
//  case class Test1(s: String)
//}
//trait Foo {
//  type Inner
//}
//
//object Foo {
//  val foo: Foo { type Inner = String } = ???
//
//  def inner(foo: Foo): foo.Inner = ???
//
//  def bar(f: String => Int): Option[Int] = Some.apply[typeApply.Foo.foo.Inner](inner(foo)).map(f)
//}