#import,file "debug.jai";
#import,file "bitfield.jai";
#import "Basic";





HolyArray :: struct(T: Type, N: u32, ABC:=true) {
    P :: #run () -> Type {
        if N < (1 <<  8) then return u8;
        if N < (1 << 16) then return u16;
        if N < (1 << 32) then return u32;
        return u64;
    }();
    Entry :: union {
        value: T;
        next_free: P;
    }

    occupied: BitField(N,ABC=ABC);
    data: [N+1]Entry = #run ()->[N+1]Entry {
        res : [N+1]Entry;
        for i: 0..N-1 res[i].next_free = cast(P)(i+1);
        res[N].next_free = 0;
        return res;
    }();
}

array_add :: (arr: *$H/HolyArray, value: H.T) -> int {
    assert(arr.data[0].next_free != 0, "Array full.");
    next_free := arr.data[0].next_free;
    next_next_free := arr.data[next_free].next_free;
    arr.data[next_free].value = value;
    arr.data[0].next_free = next_next_free;
    arr.occupied[next_free-1] = true;
    return next_free-1;
}

array_remove_at :: (arr: *$H/HolyArray, index: int) -> H.T {
    Debug(false);
    #if arr.ABC assert(index < H.N, "Index % out of range %.\n", index, H.N);
    #if arr.ABC assert(arr.occupied[index], "Cannot remove from unoccupied spot at index %.\n", index);
    res := arr.data[index+1].value;
    arr.data[index+1].next_free = arr.data[0].next_free;
    arr.data[0].next_free = cast(arr.P)(index+1);
    arr.occupied[index] = false;
    return res;
}


operator [] :: (arr: $H/HolyArray, index: int) -> H.Entry, bool {
    #if arr.ABC assert(index < H.N, "Index % out of range %.\n", index, H.N);
    return arr.data[index+1], arr.occupied[index];
}
operator []= :: (arr: *$H/HolyArray, index: int, value: H.T) {
    #if arr.ABC assert(index < H.N, "Index % out of range %.\n", index, H.N);
    #if arr.ABC assert(arr.occupied[index], "Cannot set value at unoccupied index %", index);
    arr.data[index+1].value = value;
}
operator *[] :: (arr: *$H/HolyArray, index: int) -> *H.Entry, bool {
    #if arr.ABC assert(index < H.N, "Index % out of range %.\n", index, H.N);
    return *arr.data[index+1], arr.occupied[index];
}

for_expansion :: (arr: $H/HolyArray, body: Code, flags: For_Flags) #expand {
    Debug(false);
    #if flags & For_Flags.REVERSE {
        debugln("reverse");
        for #v2 <i: 0..H.N-1 {
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
//     for        arr print("%: %\n", it_index, it);
//     for #v2 <  arr print("%: %\n", it_index, it);
//     for       *arr print("%: <<% = %\n", it_index, it, <<it);
//     for #v2 < *arr print("%: <<% = %\n", it_index, it, <<it);
//     print("%\n", arr);
// }














ResizableHolyArray :: struct(T: Type, ABC:=true) {
    Entry :: union {
        value: T;
        next_free: u32;
    }

    occupied: ResizableBitField;
    data: [..]Entry;
}

NewResizableHolyArray :: ($T: Type) -> ResizableHolyArray(T) {
    res : ResizableHolyArray(T);
    e := array_add(*res.data);
    e.next_free = cast(u32)res.data.count;
    bitfield_add(*res.occupied, false);
    return res;
}

array_add :: (arr: *ResizableHolyArray($T), value: T) -> int {
    Debug(false);

    if arr.data[0].next_free == arr.data.count {
        debugln("No hole, so need to append to the end.");
        // There is no hole in the array and so we have to append the new value to the end
        index := arr.data.count;

        array_add(*arr.data, arr.Entry.{value=value});
        bitfield_add(*arr.occupied, true);

        arr.data[0].next_free = cast(u32)arr.data.count;

        return index-1;
    }

    debugln("hole exists, so fill it.");
    // There is a hole in the array, so fill it
    hole := arr.data[0].next_free;
    next_hole := arr.data[hole].next_free;

    arr.data[hole].value = value;
    arr.data[0].next_free = next_hole;

    arr.occupied[hole] = true;

    return hole-1; // "-1" because the first valid position to put a value is at 1 instead of 0 as usual
}

array_remove_at :: (arr: *ResizableHolyArray($T), index: int) -> T {
    Debug(false);
    assert(index < arr.data.count-1, "Cannot remove element at index % from an array of length %.", index, arr.data.count-1);
    res := arr.data[index+1].value;
    arr.data[index+1].next_free = arr.data[0].next_free;
    arr.data[0].next_free = cast(u32)(index+1);

    arr.occupied[index+1] = false;

    return res;
}


operator [] :: (arr: ResizableHolyArray($T), index: int) -> T, bool {
    return arr.data[index+1].value, arr.occupied[index+1];
}
operator []= :: (arr: *ResizableHolyArray($T), index: int, value: T) {
    assert(!arr.occupied[index], "Cannot set value at unoccupied index %", index);
    arr.data[index+1].value = value;
}
operator *[] :: (arr: *ResizableHolyArray($T), index: int) -> *T, bool {
    return *arr.data[index+1].value, arr.occupied[index+1];
}

for_expansion :: (arr: ResizableHolyArray, body: Code, flags: For_Flags) #expand {
    #if flags & For_Flags.REVERSE {
        for #v2 <i: 0..arr.data.count-2 {
            #if flags & For_Flags.POINTER val, occ := *arr[i];
            else                          val, occ :=  arr[i];
            
            if !occ continue;
            
            `it := val;
            `it_index := i;

            #insert body;
        }
    } else {
        for i: 0..arr.data.count-2 {
            #if flags & For_Flags.POINTER val, occ := *arr[i];
            else                          val, occ :=  arr[i];
            
            if !occ continue;
            
            `it := val;
            `it_index := i;

            #insert body;
        }
    }
}

debug_print :: (arr: ResizableHolyArray) {
    Debug(false);
    print("ResizableHolyArray(%)\n", arr.T);
    
    for i: 0..arr.data.count-1 {
        if i == 0 {
            print("    0 -> %\n", formatInt(arr.data[0].next_free, minimum_digits=4, padding=#char " "));
            continue;
        }
        
        val, occ := arr[i-1];
        if occ print(" % : %\n"   , formatInt(i, minimum_digits=4, padding=#char " "), val);
        else   print(" % -> %\n", 
            formatInt(i, minimum_digits=4, padding=#char " "), 
            formatInt(arr.data[i].next_free, minimum_digits=4, padding=#char " ")
        );
    }
}


#scope_export

Foo :: struct {
    v: float64;
}

main :: () {
    arr := NewResizableHolyArray(Foo);
    debug_print(arr);

    array_add(*arr, .{42.0});
    debug_print(arr);
    debug_print(arr.occupied);
    i313 := array_add(*arr, .{313.0});
    debug_print(arr);
    print("added 313 at index %\n", i313);
    array_add(*arr, .{777.0});
    debug_print(arr);

    print("ResizableHolyArray: %\n", arr);

    v := array_remove_at(*arr, 1);
    print("removed value %\n", v);
    print("ResizableHolyArray: %\n", arr);
    print("next hole: %\n", arr.data[0].next_free);
    for arr print("%: %\n", it_index, it);
    debug_print(arr);

    array_add(*arr, .{2077.0});
    array_add(*arr, .{1989.0});
    print("ResizableHolyArray: %\n", arr);
    print("next hole: %\n", arr.data[0].next_free);
    for   arr print("%: %\n", it_index, it);
    for < arr print("%: %\n", it_index, it);
    for  *arr print("%: <<% = %\n", it_index, it, <<it);
    for <*arr print("%: <<% = %\n", it_index, it, <<it);
    
    debug_print(arr);
}