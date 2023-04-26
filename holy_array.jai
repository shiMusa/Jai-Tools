#import,file "debug.jai";
#import "Basic";

HolyArray :: struct(T: Type) {
    Entry :: union {
        value: T;
        next_free: u32;
    }

    data: [..]Entry;
    occupied: [..]u32;
}

#scope_file
calc_occupied_indices :: (index: u32) -> u32, u32 {
    bit_idx := index % 32;
    occ_idx := (index - bit_idx)/32;
    return occ_idx, bit_idx;
}
#scope_export

NewHolyArray :: ($T: Type) -> HolyArray(T) {
    res : HolyArray(T);
    e := array_add(*res.data);
    e.next_free = cast(u32)res.data.count;
    array_add(*res.occupied, 0);
    return res;
}

array_add :: (arr: *HolyArray($T), value: T) -> int {
    Debug(false);

    if arr.data[0].next_free == arr.data.count {
        debugln("No hole, so need to append to the end.");
        // There is no hole in the array and so we have to append the new value to the end
        index := arr.data.count;

        new_entry := array_add(*arr.data);
        new_entry.value = value;
        arr.data[0].next_free = cast(u32)arr.data.count;

        occ_idx, bit_idx := calc_occupied_indices(cast(u32)(index-1));

        debugln("% corresponds to %, % indices in occupied array.", index-1, occ_idx, bit_idx);

        if occ_idx >= arr.occupied.count {
            array_add(*arr.occupied, cast(u32)1 << bit_idx);
        } else {
            arr.occupied[occ_idx] |= cast(u32)1 << bit_idx;
        }

        return arr.data.count - 1;
    }

    debugln("hole exists, so fill it.");
    // There is a hole in the array, so fill it
    hole := arr.data[0].next_free;
    next_hole := arr.data[hole].next_free;

    arr.data[hole].value = value;
    arr.data[0].next_free = next_hole;

    occ_idx, bit_idx := calc_occupied_indices(cast(u32)(hole-1));
    arr.occupied[occ_idx] |= cast(u32)1 << bit_idx;

    return hole-1; // "-1" because the first valid position to put a value is at 1 instead of 0 as usual
}

array_remove_at :: (arr: *HolyArray($T), index: int) -> T {
    Debug(false);
    assert(index < arr.data.count, "Cannot remove element at index % from an array of length %.", index, arr.data.count-1);
    res := arr.data[index+1].value;
    arr.data[index+1].next_free = arr.data[0].next_free;
    arr.data[0].next_free = cast(u32)(index+1);

    occ_idx, bit_idx := calc_occupied_indices(cast(u32)index);
    debugln("% corresponds to %, % indices in occupied array.", index-1, occ_idx, bit_idx);

    arr.occupied[occ_idx] &= ~(cast(u32)1 << bit_idx);
    return res;
}

operator [] :: (arr: HolyArray($T), index: int) -> T {
    assert(arr.occupied[index], "Cannot retrieve value at unoccupied index %", index);
    return arr.data[index+1].value;
}
operator []= :: (arr: *HolyArray($T), index: int, value: T) {
    assert(arr.occupied[index], "Cannot set value at unoccupied index %. Add this value instead.", index);
    arr.data[index+1] = value;
}
operator *[] :: (arr: HolyArray($T), index: int) -> *T {
    assert(arr.occupied[index], "Cannot access value at unoccupied index %.", index);
    return *arr.data[index+1].value;
}

for_expansion :: (arr: HolyArray, body: Code, flags: For_Flags) #expand {
    #if flags & For_Flags.REVERSE {
        for <i: arr.data.count-1..1 {
            occ_idx, bit_idx := calc_occupied_indices(cast(u32)(i-1));
            if !(arr.occupied[occ_idx] & 1 << bit_idx) continue;

            `it_index := i-1;
            #if flags & For_Flags.POINTER `it := *arr.data[i].value;
            else                          `it :=  arr.data[i].value;

            #insert body;
        }
    } else {
        for i: 1..arr.data.count-1 {
            occ_idx, bit_idx := calc_occupied_indices(cast(u32)(i-1));
            if !(arr.occupied[occ_idx] & 1 << bit_idx) continue;

            `it_index := i-1;
            #if flags & For_Flags.POINTER `it := *arr.data[i].value;
            else                          `it :=  arr.data[i].value;

            #insert body;
        }
    }
}

debug_print :: (arr: HolyArray) {
    Debug(false);
    print("HolyArray(%)\n", arr.T);
    for 0..arr.data.count-1 {
        if it == 0 {
            print("    0: -> %\n", arr.data[0].next_free);
            continue;
        }

        occ_idx, bit_idx := calc_occupied_indices(cast(u32)(it-1));
        debugln("% corresponds to %, % indices in occupied array.", it-1, occ_idx, bit_idx);
        debugln("occupied[%] = %, 1 << % = %", occ_idx, arr.occupied[occ_idx], bit_idx, 1<<bit_idx);
        if arr.occupied[occ_idx] & (1 << bit_idx) {
            debugln("occupied");
            print("    %: %\n", it, arr.data[it].value);
        } else {
            debugln("not occupied");
            print("    %: -> %\n", it, arr.data[it].next_free);
        }
    }
}


// #scope_export

// main :: () {
//     arr := NewHolyArray(float);

//     array_add(*arr, 42.0);
//     i313 := array_add(*arr, 313.0);
//     print("added 313 at index %\n", i313);
//     array_add(*arr, 777.0);

//     print("HolyArray: %\n", arr);
//     for arr print("%: %\n", it_index, it);

//     v := array_remove_at(*arr, 1);
//     print("removed value %\n", v);
//     print("HolyArray: %\n", arr);
//     print("next hole: %\n", arr.data[0].next_free);
//     for arr print("%: %\n", it_index, it);
//     debug_print(arr);

//     array_add(*arr, 2077.0);
//     print("HolyArray: %\n", arr);
//     print("next hole: %\n", arr.data[0].next_free);
//     for   arr print("%: %\n", it_index, it);
//     for < arr print("%: %\n", it_index, it);
//     for  *arr print("%: <<% = %\n", it_index, it, <<it);
//     for <*arr print("%: <<% = %\n", it_index, it, <<it);
//     debug_print(arr);
// }