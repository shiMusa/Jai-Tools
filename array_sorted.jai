#import "Basic";
#import,file "debug.jai";

SortedArray :: struct(T: Type) {
    array: [..]T;
#place array;
    count: s64;
    data: *void;
}

operator [] :: inline (using sorted_array: SortedArray($T), index: int) -> T {
    return array[index];
}

free :: inline (using sorted_array: *SortedArray) {
    array_free(*array);
}

sorted_array :: (
    $T: Type,
    count:=0,
    allocator: Allocator = .{}, 
    alignment: s32 = -1
) -> SortedArray(T) {
    return .{
        NewResizableArray(count, T, false, allocator, alignment)
    };
}

FindClosest :: enum u8 {
    NONE;
    PREVIOUS;
    NEXT;
}

find :: (using sorted_array: SortedArray($T), val: T, $closest:=FindClosest.NONE) -> index: int, found: bool {
    Debug(false);

    if !array then return -1, false;

    min := 0;
    max := array.count-1;

    if val < array[min] #if closest == .NEXT     { return   0, false; } else { return -1, false; };
    if val > array[max] #if closest == .PREVIOUS { return max, false; } else { return -1, false; };

    while true {
        debugln("search in (%,%):[%,%]\n", min, max, array[min], array[max]);
        if val == array[min] return min, true;
        if val == array[max] return max, true;

        mid := (max+min)/2;
        debugln("mid (%)[%]\n", mid, array[mid    ]);
        if mid == min || mid == max {
            #if closest == .PREVIOUS  return min, false;
            else #if closest == .NEXT return max, false;
            else                      return  -1, false;
        }

        if val == array[mid] return mid, true;

        if val < array[mid] then max = mid;
        else                     min = mid;
    }

    assert(false, "cannot be reached");
    return -1, false;
}

insert :: (sorted_array: *SortedArray($T), val: T, $unique:=false) -> index:int, new_entry:bool {
    Debug(false);
    array := *sorted_array.array;

    debugln("array_insert_sorted: array %, val %, unique %\n", <<array, val, unique);

    if array.count == 0 {
        debugln("empty array, just add first value\n");
        array_add(array, val);
        return 0, true;
    }

    min := 0;
    max := array.count-1;

    if val < array.data[min] {
        array_insert_at(array, val, 0);
        return 0, true;
    }
    if val > array.data[max] {
        array_add(array, val);
        return max+1, true;
    }

    while true {
        debugln("search in (%,%):[%,%]\n", min, max, array.data[min], array.data[max]);
        if val == array.data[min] {
            #if !unique array_insert_at(array, val, min);
            return min, false;
        }
        if val == array.data[max] {
            #if !unique array_insert_at(array, val, max);
            return max, false;
        }

        mid := (max+min)/2;
        debugln("mid (%)[%]\n", mid, array.data[mid]);
        
        if val == array.data[mid] {
            #if !unique array_insert_at(array, val, mid);
            return mid, true;
        }
        
        if mid == min {
            array_insert_at(array, val, min+1);
            return min+1, true;
        }

        if val < array.data[mid] then max = mid;
        else                          min = mid;
    }

    assert(false, "cannot be reached");
    return -1, false;
}

remove_by_index :: inline (using sorted_array: *SortedArray, index: s64) {
    array_ordered_remove_by_index(*array, index);
}