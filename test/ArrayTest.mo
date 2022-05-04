import M "mo:matchers/Matchers";
import Array "../src/Array";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";
import Nat "mo:base/Nat";

let equals1 = M.equals(T.nat(1));
let equals2 = M.equals(T.nat(2));
let equals3 = M.equals(T.nat(3));
let equals4 = M.equals(T.nat(4));
let inArray = S.suite("inArray", [
    S.test("Value is in array",
      Array.inArray([1, 2, 3, 4], 2, Nat.equal),
      M.equals(T.bool(true))),
    S.test("Value is not in array",
      Array.inArray([1, 2, 3, 4], 5, Nat.equal),
      M.equals(T.bool(false))),
]);

let combine = S.suite("combine", [
    S.test("Value is in array", 
    Array.combine([1, 2],[3, 4]),
    M.array([equals1,equals2,equals3,equals4])),
]);

S.run(inArray);
