//// [tests/cases/compiler/requiredMappedTypeModifierTrumpsVariance.ts] ////

//// [requiredMappedTypeModifierTrumpsVariance.ts]
const a: Required<{ a?: 1; x: 1 }> = { a: 1, x: 1 };
const b: Required<{ b?: 1; x: 1 }> = { b: 1, x: 1 };
export let A = a;
export let B = b;
A = b; // Should Error
B = a; // Should Error

a.b; // Property 'b' does not exist on type 'Required<{ a?: 1; x: 1; }>'.
b.a; // Property 'a' does not exist on type 'Required<{ b?: 1; x: 1; }>'.

interface Foo<T> {
    a: Required<T>;
}
const aa: Foo<{ a?: 1; x: 1 }> = { a: { a: 1, x: 1 } };
const bb: Foo<{ b?: 1; x: 1 }> = { a: { b: 1, x: 1 } };
export let AA = aa;
export let BB = bb;
AA = bb; // Should Error
BB = aa; // Should Error

aa.a.b; // Property 'b' does not exist on type 'Required<{ a?: 1; x: 1; }>'.
bb.a.a; // Property 'a' does not exist on type 'Required<{ b?: 1; x: 1; }>'.

//// [requiredMappedTypeModifierTrumpsVariance.js]
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.BB = exports.AA = exports.B = exports.A = void 0;
const a = { a: 1, x: 1 };
const b = { b: 1, x: 1 };
exports.A = a;
exports.B = b;
exports.A = b; // Should Error
exports. // Should Error
B = a; // Should Error
a.b; // Property 'b' does not exist on type 'Required<{ a?: 1; x: 1; }>'.
b.a; // Property 'a' does not exist on type 'Required<{ b?: 1; x: 1; }>'.
const aa = { a: { a: 1, x: 1 } };
const bb = { a: { b: 1, x: 1 } };
exports.AA = aa;
exports.BB = bb;
exports.AA = bb; // Should Error
exports. // Should Error
BB = aa; // Should Error
aa.a.b; // Property 'b' does not exist on type 'Required<{ a?: 1; x: 1; }>'.
bb.a.a; // Property 'a' does not exist on type 'Required<{ b?: 1; x: 1; }>'.
