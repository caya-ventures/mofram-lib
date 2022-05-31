/// To start using this library, import it like in example below
/// ```motoko
/// import Array "mo:mofram/Array";
/// ```
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
import Prim "mo:⛔";

module {
    // Determine whether an array includes a certain value.
    public func inArray<T>(haystack : [T], needle : T, equalChecker : (T, T) -> Bool) : Bool {
        for (value in haystack.vals()) {
            if (equalChecker(value, needle)) {
                return true;
            }
        }; 
        return false;
    };

    // Combine two arrays into one.
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

    // Calculate the sum of values in an array.
    public func sum<A>(a : [A], initial : A, addFunc : (A, A) -> A) : A {
        var res = initial;
        for (val in a.vals() ) {
            res := addFunc(res, val);
        };
        res;
    };

    // Calculate the product of values in an array.
    public func product<A>(a : [A], initial : A, mulFunc : (A, A) -> A) : A {
        var res = initial;
        for (val in a.vals() ) {
            res := mulFunc(res, val);
        };
        res;
    };

    // Split an array into chunks
    public func chunk<T> (array : [T], size : Nat) : [[T]] {
        if (size == 0) {
            return [[]];
        };

        let chunks = Buffer.Buffer<[T]>(array.size());
        let tmp = Buffer.Buffer<T>(size);
        var i : Nat = 0;

        for (val in array.vals()) {
            if (i < size and tmp.size() < size) {
                tmp.add(val);
                i += 1;
            } else {
                chunks.add(tmp.toArray());
                tmp.clear();
                i := 0;
                tmp.add(val);
            };
        };

        if (tmp.size() > 0) {
            chunks.add(tmp.toArray());
        };

        var result = chunks.toArray();

        return result;
    };

    // Remove duplicate values from an array.
    public func unique<T> (array : [T], equalChecker : (T, T) -> Bool) : [T] {
        let tmp = Buffer.Buffer<T>(array.size());
        var i : Nat = 0;

        for (val in array.vals()) {
            var flag : Nat = 1;
            label innerLoop for (j in Iter.range(0, i)) {
                if (j < i) {
                    if (equalChecker(array[j], val)) {
                        flag := 0;
                        break innerLoop;
                    };
                };
            };

            if (flag == 1) {
                tmp.add(array[i]);
            };

            i += 1;
        };

        var result = tmp.toArray();

        return result;
    };

    // Computes the difference of arrays.
    // Returns the values in array a that are not present in array b.
    public func diff<T> (a : [T], b : [T], equalChecker : (T, T) -> Bool) : [T] {
        let tmp = Buffer.Buffer<T>(a.size());
        var i : Nat = 0;

        for (va in a.vals()) {
            var flag : Nat = 1;
            tmp.add(va);
            label innerLoop for (vb in b.vals()) {
                if (equalChecker(va, vb) == true) {
                    flag := 0;
                    break innerLoop;
                };
            };
            if (flag == 0) {
                var t = tmp.removeLast();
            };
        };

        var result = tmp.toArray();

        return result;
    };

    // Computes the intersection of arrays.
    // Returns an array containing all the values of array a that are present in array b.
    public func intersect<T> (a : [T], b : [T], equalChecker : (T, T) -> Bool) : [T] {
        let tmp = Buffer.Buffer<T>(a.size());

        for (va in a.vals()) {
            label innerLoop for (vb in b.vals()) {
                if (equalChecker(va, vb) == true) {
                    tmp.add(va);
                    break innerLoop;
                };
            };
        };

        var result = tmp.toArray();

        return result;
    };

    // Push elements of array b onto the end of array a.
    public func push<T>(a : [T], b : [T]) : [T] {
        let tmp = Buffer.Buffer<T>(a.size() + b.size());

        for (aval in a.vals()) {
            tmp.add(aval);
        };
        for (bval in b.vals()) {
            tmp.add(bval);
        };

        return tmp.toArray();
    };

    // Returns the value of the last element of an array.
    public func pop<T>(a : [T]) : T {
        let i : Nat = a.size() - 1;
        return a[i];
    };

    // Extract a slice of the array.
    public func slice<T>(a : [T], s : Nat, e: Nat) : [T] {
        if (e < s or s <= 0 or e <= 0 or e > a.size()) return [];
        let tmp = Buffer.Buffer<T>(e-s);

        for (i in Iter.range(s - 1, e - 1)) {
            tmp.add(a[i]);
        };

        return tmp.toArray();
    };

    // Remove a portion of the array and replace it with something else.
    public func splice<T>(a : [T], b : [T], s : Nat, e: Nat) : [T] {
        var k : Nat = 0;
        var na : [var T] = Array.thaw(a);
        for (nv in Iter.range(s, e)) {
            na[nv] := b[k];
            k+=1;
        };

        return Array.freeze(na);
    };

    // Shift an element off the beginning of array.
    // Returns a new array without a first element and the first element of the initial array.
    public func shift<T>(a : [T]) : ([T], T) {
        let na = Array.tabulate<T>(a.size()-1, func (i : Nat) : T {
            a[i+1];
        });

        return (na, a[0]);
    };

    // Prepend elements of array b to the beginning of array a.
    public func unshift<T>(a : [T], b : [T]) : [T] {
        let na = Array.tabulate<T>(a.size() + b.size(), func (i : Nat) : T {
            if (i < b.size()) {
                b[i];
            } else {
                a[i - b.size()];
            };
        });

        return na;
    };    
}
