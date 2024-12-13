#import,file "debug.jai";
#import,file "bitfield.jai";
#import "Basic";





HolyArray :: struct(T: Type, N: u32) {
    Entry :: union {
        value: T;
        next_free: u32;
    }
    data: [N+1]Entry = #run ()->[N+1]Entry {
        res : [N+1]Entry;
        for i: 0..N-1 res[i].next_free = cast(u32)(i+1);
        res[N].next_free = 0;
        return res;
    }();
    occupied: [(N >> 5) + 1]u32;
}

#scope_file
is_occupied :: inline (arr: HolyArray, index: int) -> bool {
    occ_idx := cast(u32)index >> 5;
    bit_idx := cast(u32)index & 31;
    return (arr.occupied[occ_idx] & (cast(u32)1 << bit_idx)) > 0;
}
set_occupied :: inline (arr: *HolyArray, index: int, occupied: bool) {
    occ_idx := cast(u32)index >> 5;
    bit_idx := cast(u32)index & 31;
    if occupied
        arr.occupied[occ_idx] |= cast(u32)1 << bit_idx;
    else
        arr.occupied[occ_idx] &= ~(cast(u32)1 << bit_idx);
}
#scope_export

array_add :: (arr: *$H/HolyArray, value: H.T) -> int {
    assert(arr.data[0].next_free != 0, "Array full.");
    next_free := arr.data[0].next_free;
    next_next_free := arr.data[next_free].next_free;
    arr.data[next_free].value = value;
    arr.data[0].next_free = next_next_free;
    set_occupied(arr, next_free-1, true);
    return next_free-1;
}

array_remove_at :: (arr: *$H/HolyArray, index: int) -> H.T {
    Debug(false);
    assert(index < H.N, "Index % out of range %.\n", index, H.N);
    assert(is_occupied(arr, index), "Cannot remove from unoccupied spot at index %.\n", index);
    res := arr.data[index+1].value;
    arr.data[index+1].next_free = arr.data[0].next_free;
    arr.data[0].next_free = cast(u32)(index+1);

    set_occupied(arr, index, false);

    return res;
}


operator [] :: (arr: $H/HolyArray, index: int) -> H.Entry, bool {
    assert(index < H.N, "Index % out of range %.\n", index, H.N);
    return arr.data[index+1], is_occupied(arr, index);
}
operator []= :: (arr: *$H/HolyArray, index: int, value: H.T) {
    assert(index < H.N, "Index % out of range %.\n", index, H.N);
    assert(is_occupied(arr, index), "Cannot set value at unoccupied index %", index);
    arr.data[index+1].value = value;
}
operator *[] :: (arr: *$H/HolyArray, index: int) -> *H.Entry, bool {
    assert(index < H.N, "Index % out of range %.\n", index, H.N);
    return *arr.data[index+1], is_occupied(arr, index);
}

for_expansion :: (arr: $H/HolyArray, body: Code, flags: For_Flags) #expand {
    Debug(false);
    #if flags & For_Flags.REVERSE {
        debugln("reverse");
        for <i: H.N-1..0 {
            #if flags & For_Flags.POINTER val, occ := *arr[i];
            else                          val, occ :=  arr[i];
            
            if !occ continue;
            
            #if flags & For_Flags.POINTER `it := *val.value;
            else                          `it :=  val.value;
            // `it := val.value;
            `it_index := i;
            
            #insert body;
        }
    } else {
        debugln("forward");
        for i: 0..H.N-1 {
            #if flags & For_Flags.POINTER val, occ := *arr[i];
            else                          val, occ :=  arr[i];
            debugln("%: %, %", i, val, occ);
            
            if !occ continue;
            
            #if flags & For_Flags.POINTER `it := *val.value;
            else                          `it := val.value;
            // `it := val.value;
            `it_index := i;
            
            #insert body;
        }
    }
}

debug_print :: (arr: $H/HolyArray) {
    Debug(false);
    print("HolyArray(%, %)\n", H.T, H.N);
    
    for i: 0..H.N {
        if i == 0 {
            print("   [0: -> %]\n", arr.data[0].next_free);
            continue;
        }
        
        val, occ := arr[i-1];
        if occ print("    %: %\n", i, val.value);
        else   print("    %: -> %\n", i, val.next_free);
    }
}



// Foo :: struct {
//     v: float64;
// }

// main :: () {
//     arr : HolyArray(Foo, 100);
//     debug_print(arr);
    
//     array_add(*arr, .{42.0});
//     array_add(*arr, .{313.0});
//     array_add(*arr, .{3.141});
//     array_add(*arr, .{6.9});
//     in1 := array_add(*arr, .{-1.0});
//     print("added -1 at position %\n", in1);
//     debug_print(arr);
    

//     v := array_remove_at(*arr, 3);
//     print("removed value %\n", v);
//     debug_print(arr);

//     array_add(*arr, .{2077.0});
//     array_remove_at(*arr, 2);
//     debug_print(arr);
//     for   arr print("%: %\n", it_index, it);
//     for < arr print("%: %\n", it_index, it);
//     for  *arr print("%: <<% = %\n", it_index, it, <<it);
//     for <*arr print("%: <<% = %\n", it_index, it, <<it);
//     print("%\n", arr);
// }














ResizableHolyArray :: struct(T: Type) {
    Entry :: union {
        value: T;
        next_free: u32;
    }

    data: [..]Entry;
    occupied: [..]u32;
}

NewResizableHolyArray :: ($T: Type) -> ResizableHolyArray(T) {
    res : ResizableHolyArray(T);
    e := array_add(*res.data);
    e.next_free = cast(u32)res.data.count;
    array_add(*res.occupied, 0);
    return res;
}

#scope_file
is_occupied :: inline (arr: ResizableHolyArray, index: int) -> bool {
    occ_idx := cast(u32)index >> 5;
    bit_idx := cast(u32)index & 31;
    return (arr.occupied[occ_idx] & (cast(u32)1 << bit_idx)) > 0;
}
set_occupied :: inline (arr: *ResizableHolyArray, index: int, occupied: bool) {
    occ_idx := cast(u32)index >> 5;
    bit_idx := cast(u32)index & 31;
    if occupied
        arr.occupied[occ_idx] |= cast(u32)1 << bit_idx;
    else
        arr.occupied[occ_idx] &= ~(cast(u32)1 << bit_idx);
}
#scope_export

array_add :: (arr: *ResizableHolyArray($T), value: T) -> int {
    Debug(false);

    if arr.data[0].next_free == arr.data.count {
        debugln("No hole, so need to append to the end.");
        // There is no hole in the array and so we have to append the new value to the end
        index := arr.data.count;

        new_entry := array_add(*arr.data);
        new_entry.value = value;
        arr.data[0].next_free = cast(u32)arr.data.count;
        
        
        occ_idx := cast(u32)(index-1) >> 5;
        bit_idx := cast(u32)(index-1) & 31;
        if occ_idx >= arr.occupied.count
            array_add(*arr.occupied, cast(u32)1 << bit_idx);
        else
            arr.occupied[occ_idx] |= cast(u32)1 << bit_idx;

        return index-1;
    }

    debugln("hole exists, so fill it.");
    // There is a hole in the array, so fill it
    hole := arr.data[0].next_free;
    next_hole := arr.data[hole].next_free;

    arr.data[hole].value = value;
    arr.data[0].next_free = next_hole;

    set_occupied(arr, hole-1, true);

    return hole-1; // "-1" because the first valid position to put a value is at 1 instead of 0 as usual
}

array_remove_at :: (arr: *ResizableHolyArray($T), index: int) -> T {
    Debug(false);
    assert(index < arr.data.count-1, "Cannot remove element at index % from an array of length %.", index, arr.data.count-1);
    res := arr.data[index+1].value;
    arr.data[index+1].next_free = arr.data[0].next_free;
    arr.data[0].next_free = cast(u32)(index+1);

    set_occupied(arr, index, false);

    return res;
}


operator [] :: (arr: ResizableHolyArray($T), index: int) -> T, bool {
    return arr.data[index+1].value, is_occupied(arr, index);
}
operator []= :: (arr: *ResizableHolyArray($T), index: int, value: T) {
    assert(!is_occupied(arr, index), "Cannot set value at unoccupied index %", index);
    arr.data[index+1] = value;
}
operator *[] :: (arr: *ResizableHolyArray($T), index: int) -> *T, bool {
    return *arr.data[index+1].value, is_occupied(arr, index);
}

for_expansion :: (arr: ResizableHolyArray, body: Code, flags: For_Flags) #expand {
    #if flags & For_Flags.REVERSE {
        for #v2 <i: 0..arr.data.count-2 {
            #if flags & For_Flags.POINTER val, occ := *arr[i];
            else                          val, occ :=  arr[i];
            
            if !occ continue;
            
            `it := val;
            `it_index := i;
        }
    } else {
        for i: 0..arr.data.count-2 {
            #if flags & For_Flags.POINTER val, occ := *arr[i];
            else                          val, occ :=  arr[i];
            
            if !occ continue;
            
            `it := val;
            `it_index := i;
        }
    }
}

debug_print :: (arr: ResizableHolyArray) {
    Debug(false);
    print("ResizableHolyArray(%)\n", arr.T);
    
    for i: 0..arr.data.count-1 {
        if i == 0 {
            print("    0: -> %\n", arr.data[0].next_free);
            continue;
        }
        
        val, occ := arr[i-1];
        if occ print("    %: %\n", i, val);
        else   print("    %: -> %\n", i, val);
    }
}


// #scope_export

// Foo :: struct {
//     v: float64;
// }

// main :: () {
//     arr := NewResizableHolyArray(Foo);

//     array_add(*arr, .{42.0});
//     i313 := array_add(*arr, .{313.0});
//     print("added 313 at index %\n", i313);
//     array_add(*arr, .{777.0});

//     print("ResizableHolyArray: %\n", arr);
//     for arr print("%: %\n", it_index, it);

//     v := array_remove_at(*arr, 1);
//     print("removed value %\n", v);
//     print("ResizableHolyArray: %\n", arr);
//     print("next hole: %\n", arr.data[0].next_free);
//     for arr print("%: %\n", it_index, it);
//     debug_print(arr);

//     array_add(*arr, .{2077.0});
//     print("ResizableHolyArray: %\n", arr);
//     print("next hole: %\n", arr.data[0].next_free);
//     for   arr print("%: %\n", it_index, it);
//     for < arr print("%: %\n", it_index, it);
//     for  *arr print("%: <<% = %\n", it_index, it, <<it);
//     for <*arr print("%: <<% = %\n", it_index, it, <<it);
//     debug_print(arr);
// }