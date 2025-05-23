//// [tests/cases/conformance/types/typeRelationships/assignmentCompatibility/assignmentCompatWithNumericIndexer.ts] ////

//// [assignmentCompatWithNumericIndexer.ts]
// Derived type indexer must be subtype of base type indexer

interface Base { foo: string; }
interface Derived extends Base { bar: string; }
interface Derived2 extends Derived { baz: string; }

class A {
    [x: number]: Base;
}

var a: A;
var b: { [x: number]: Derived; }
a = b;
b = a; // error

var b2: { [x: number]: Derived2; }
a = b2;
b2 = a; // error

module Generics {
    class A<T extends Base> {
        [x: number]: T;
    }

    class B extends A<Base> {
        [x: number]: Derived; // ok
    }

    function foo<T extends Base>() {
        var a: A<T>;
        var b: { [x: number]: Derived; }
        a = b; // error
        b = a; // error

        var b2: { [x: number]: Derived2; }
        a = b2; // error
        b2 = a; // error

        var b3: { [x: number]: T; }
        a = b3; // ok
        b3 = a; // ok
    }
}

//// [assignmentCompatWithNumericIndexer.js]
class A {
}
var a;
var b;
a = b;
b = a; // error
var b2;
a = b2;
b2 = a; // error
var Generics;
(function (Generics) {
    class A {
    }
    class B extends A {
    }
    function foo() {
        var a;
        var b;
        a = b; // error
        b = a; // error
        var b2;
        a = b2; // error
        b2 = a; // error
        var b3;
        a = b3; // ok
        b3 = a; // ok
    }
})(Generics || (Generics = {}));
