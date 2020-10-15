/*
rule = MigrationRule
*/

package synthetic

//trait Context[M[_]]
//
//trait Foo[T]
//trait Bar[T]
//object Fizz extends Foo[Int] with Bar[Int]
//
//object Foo {
//  implicit val ctx: Context[Foo] = ???
//}
//
//object Test {
//  def from[M[_], T](m: M[T])(implicit ctx: Context[M]): Unit = ???
//
//  from(Fizz)
//}
 object Incompat1 {
   trait Foo {
     type Inner
   }

   object Foo {
     val foo: Foo { type Inner = String } = ???

     def inner(foo: Foo): foo.Inner = ???

     def bar(f: String => Int): Option[Int] = Some.apply[Foo.foo.Inner](inner(foo)).map(f)
   }
 }