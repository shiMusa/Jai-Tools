#import "Basic";

NewResizableArray :: (count: s64, $T: Type, $initialized := true, alignment: s32 = -1) -> [..]T {
    arr := NewArray(count, T, initialized, alignment); // ! alloc
    res := resizable(arr);
    res.count = 0;
    return res;
}

TempNewArray :: (count: s64, $T: Type, $initialized := true, alignment: s32 = -1) -> []T #expand {
    res := NewArray(count, T, initialized, alignment);
    `defer array_free(res);
    return res;
}
TempNewResizableArray :: (count: s64, $T: Type, $initialized := true, alignment: s32 = -1) -> [..]T #expand {
    res := NewResizableArray(count, T, initialized, alignment);
    `defer array_free(res);
    return res;
}


// ######## ##     ## ##    ##  ######  ######## ####  #######  ##    ##  ######
// ##       ##     ## ###   ## ##    ##    ##     ##  ##     ## ###   ## ##    ##
// ##       ##     ## ####  ## ##          ##     ##  ##     ## ####  ## ##
// ######   ##     ## ## ## ## ##          ##     ##  ##     ## ## ## ##  ######
// ##       ##     ## ##  #### ##          ##     ##  ##     ## ##  ####       ##
// ##       ##     ## ##   ### ##    ##    ##     ##  ##     ## ##   ### ##    ##
// ##        #######  ##    ##  ######     ##    ####  #######  ##    ##  ######

inverse_permutation :: (order: []int) -> []int {
    res := NewArray(order.count, int);
    for i: 0..order.count-1 {
        res[order[i]] = i;
    }
    return res;
}

array_permute :: (a: []$T, order: []int) -> []T {
    assert(a.count == order.count);

    res := NewArray(a.count, T);
    for order {
        res[it_index] = a[it];
    }

    return res;
}

array_fill :: inline (a: []$T, value: T) {
    for *a it.* = value;
}

array_equal :: (a: []$T, b: []T) -> bool {
    if a.count != b.count return false;
    for a if it != b[it_index] return false;
    return true;
}

minmax :: (arr: []$T) -> T, T {
    assert(arr.count > 0, "Cannot find min or max value of empty array.");
    min := arr[0];
    max := arr[0];
    for i: 1..arr.count-1 {
        min = ifx arr[i] < min then arr[i] else min;
        max = ifx arr[i] > max then arr[i] else max;
    }
    return min, max;
}

array_reverse :: (arr: []$T) {
    n := arr.count;
    for 0..n/2-1 {  // n=3 => 0, n=4 => 1, n=5 => 1 [0, 1, 2, 3, 4], no need to swap middle
        tmp := arr[it];
        arr[it] = arr[n-1-it];
        arr[n-1-it] = tmp;
    }
}