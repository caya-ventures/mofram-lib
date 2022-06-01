import M "mo:matchers/Matchers";
import Array "../src/Array";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";
import Nat "mo:base/Nat";

let equals1 = M.equals(T.nat(1));
let equals2 = M.equals(T.nat(2));
let equals3 = M.equals(T.nat(3));
let equals4 = M.equals(T.nat(4));
let equals5 = M.equals(T.nat(5));
let inArray = S.suite("inArray", [
    S.test("Value is in array",
      Array.inArray([1, 2, 3, 4], 2, Nat.equal),
      M.equals(T.bool(true))),
    S.test("Value is not in array",
      Array.inArray([1, 2, 3, 4], 5, Nat.equal),
      M.equals(T.bool(false))),
]);

let combine = S.suite("combine", [
    S.test("Correct resulting array", 
    Array.combine([1, 2],[3, 4]),
    M.array([equals1,equals2,equals3,equals4])),
]);

let sum = S.suite("sum", [
    S.test("Correct sum of elements in array", 
    Array.sum([1, 2, 3, 4], 0, Nat.add),
    M.equals(T.nat(10))),
]);

let product = S.suite("product", [
    S.test("Correct product of elements in array", 
    Array.product([1, 2, 3, 4], 1, Nat.mul),
    M.equals(T.nat(24))),
]);

let unique = S.suite("unique", [
    S.test("Array without duplicate values", 
    Array.unique([1, 1, 2, 3, 4, 4, 1 ,2], Nat.equal),
    M.array([equals1,equals2,equals3,equals4])),
]);

let diff = S.suite("diff", [
    S.test("Difference of arrays", 
    Array.diff([1, 1, 2, 3, 4, 4, 1 ,2], [1, 3, 7], Nat.equal),
    M.array([equals2,equals4,equals4,equals2])),
]);

let intersect = S.suite("intersect", [
    S.test("Intersection of arrays", 
    Array.intersect([1, 1, 2, 3, 4, 4, 1 ,2, 5], [1, 3, 5], Nat.equal),
    M.array([equals1,equals1,equals3,equals1,equals5])),
]);

let push = S.suite("push", [
    S.test("Push elements into array", 
    Array.push([1, 2, 3], [3, 4, 5]),
    M.array([equals1,equals2,equals3,equals3,equals4,equals5])),
]);

let pop = S.suite("pop", [
    S.test("Last element of the array", 
    Array.pop([1, 2, 3, 4, 5]),
    M.equals(T.nat(5))),
]);

let slice = S.suite("slice", [
    S.test("Slice of the array", 
    Array.slice([0, 1, 2, 3, 4, 5, 6], 3, 5),
    M.array([equals2,equals3,equals4])),
]);

let splice = S.suite("splice", [
    S.test("Replace values in the array", 
    Array.splice([0, 1, 2, 3, 4], [1,1], 0, 1),
    M.array([equals1,equals1,equals2,equals3,equals4])),
]);

let unshift = S.suite("unshift", [
    S.test("Prepend elements to the beginning of array", 
    Array.unshift([1, 2, 3], [3, 4, 5]),
    M.array([equals3,equals4,equals5,equals1,equals2,equals3])),
]);

S.run(inArray);
S.run(combine);
S.run(sum);
S.run(product);
S.run(unique);
S.run(diff);
S.run(intersect);
S.run(push);
S.run(pop);
S.run(slice);
S.run(splice);
S.run(unshift);