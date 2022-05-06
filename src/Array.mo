/// To start using this library, import it like in example below
/// ```motoko
/// import Array "mo:mofram/Array";
/// ```
import Prim "mo:⛔";
import Buffer "mo:base/Buffer";

module {
    /// Searches for needle in haystack. Return true if found.
    public func inArray<T>(haystack : [T], needle : T, equalChecker : (T, T) -> Bool) : Bool {
        for (value in haystack.vals()) {
            if (equalChecker(value, needle)) {
                return true;
            }
        }; 
        return false;
    };
    /// Combine two arrays into one 
    public func combine<T>(x_arr : [T], y_arr : [T]) : [T] {
        let ys : Buffer.Buffer<T> = Buffer.Buffer(x_arr.size());
        for (x in x_arr.vals()) {
            ys.add(x);
        };
        for (y in y_arr.vals()) {
            ys.add(y);
        };
        ys.toArray();
    };

    /// Summarise values in the array. 
    public func sum<A>(a : [A], initial : A, addFunc : (A, A) -> A) : A {
        var res = initial;
        for (val in a.vals() ) {
            res := addFunc(res, val);
        };
        res;
    };
}
