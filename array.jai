#import "Basic";

NewResizableArray :: (count: s64, $T: Type, $initialized := true, allocator: Allocator = .{}, alignment: s32 = -1) -> [..]T {
    arr := NewArray(count, T, initialized, allocator, alignment); // ! alloc
    res := resizable(arr);
    res.count = 0;
    return res;
}

TempNewArray :: (count: s64, $T: Type, $initialized := true, allocator : Allocator = .{}, alignment: s32 = -1) -> []T #expand {
    res := NewArray(count, T, initialized, allocator, alignment);
    `defer array_free(res);
    return res;
}
TempNewResizableArray :: (count: s64, $T: Type, $initialized := true, allocator : Allocator = .{}, alignment: s32 = -1) -> [..]T #expand {
    res := NewResizableArray(count, T, initialized, allocator, alignment);
    `defer array_free(res);
    return res;
}

array_equal :: (a: []$T, b: []T) -> bool {
    if a.count != b.count return false;
    for a if it != b[it_index] return false;
    return true;
}


minmax :: (arr: []$T) -> T, T {
    assert(arr.count > 0);
    min := arr[0];
    max := arr[0];
    for i: 1..arr.count-1 {
        min = ifx arr[i] < min then arr[i] else min;
        max = ifx arr[i] > max then arr[i] else max;
    }
    return min, max;
}