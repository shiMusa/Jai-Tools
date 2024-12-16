#import "Basic";
#import,file "debug.jai";
#import,file "println.jai";



#scope_file
calc_indices :: (index: u32) -> u32, u32 #expand {
    return index >> 5, index & 31;
}
#scope_export





ResizableBitField :: struct(ABC:=true) {
    count: int;
    bits : [..]u32;
}

set_all :: (using bf: *ResizableBitField, value: bool) {
    val := cast(u32)value * 0xffffffff;
    for *bits it.* = val;
}

bitfield_add :: (bf: *ResizableBitField, value: bool) {
    Debug(false);
    if !(bf.count & 31) {
        debugln("new bits, % & 31 = %", bf.count, bf.count & 31);
        array_add(*bf.bits, cast(u32)value);
        bf.count += 1;
        return;
    }
    
    bl, bi := calc_indices(cast(u32)bf.count);
    debugln("% corresponds to %, %", bf.count, bl, bi);
    if value then bf.bits[bl] |=  (cast(u32)1 << bi);
    else          bf.bits[bl] &= ~(cast(u32)1 << bi);
    bf.count += 1;
}
bitfield_pop :: (bf: *ResizableBitField) -> bool {
    if !((bf.count-1) & 31) {
        res := cast(bool)pop(*bf.bits);
        bf.count -= 1;
        return res;
    }
    
    bl, bi := calc_indices(cast(u32)(bf.count-1));
    bf.count -= 1;
    val := cast(bool)( bf.bits[bl] & (cast(u32)1 << bi) );
    bf.bits[bl] &= cast(u32)((cast(u32)1 << bi) - 1);
    return val;
}
bitfield_unordered_remove_at :: (bf: *ResizableBitField, index: int) -> bool {
    Debug(false);
    #if bf.ABC assert(index < bf.count, "Cannot remove bit at index % when only % bits are available.", index, bf.count);
    val := (<<bf)[index];
    (<<bf)[index] = (<<bf)[bf.count-1];
    debugln("retrieving (<<bf)[%] = %", bf.count-1, (<<bf)[bf.count-1]);
    
    if !((bf.count-1) & 31) pop(*bf.bits);
    else (<<bf)[bf.count-1] = false;
    
    bf.count -= 1;
    
    return val;
}
bitfield_ordered_remove_at :: (bf: *ResizableBitField, index: int) -> bool {
    Debug(false);
    #if bf.ABC assert(index < bf.count, "Cannot remove bit at index % when only % bits are available.", index, bf.count);
    
    val := bf[0][index];
    
    // last element
    if index == bf.count-1 return bitfield_pop(bf);
    
    bl, bi := calc_indices(cast(u32)index);
    
    if bi == {
        case 0;
            debugln("case bi == 0");
            // imagine the array of bits going from right to left (as the bits are)
            left := bf.bits[bl] & ~cast(u32)1;
            debugln("left : %", bit_string(left, 32));
            bf.bits[bl] = (bf.bits[bl] >> 1) & ~cast(u32)(1 << 31); // shift all the bits 1 right in the block
            
            if index + 32 < bf.count-1
                bf[0][index + 31] = bf[0][index + 32]; // assign last bit the first bit from the next block
        case 31;
            debugln("case bi == 31");
            bf[0][index] = bf[0][index+1]; // assign first bit from next block
        case;
            debugln("case bi = %", bi);
            left  := bf.bits[bl] & ~cast(u32)((cast(u32)1 << (bi+1)) - 1);
            right := bf.bits[bl] & cast(u32)(cast(u32)(1 << bi)-1);
            debugln("left  : %", bit_string(left , 32));
            debugln("right : %", bit_string(right, 32));
            debugln("left | right : %", bit_string( (left>>1) | right, 32));
            left = left >> 1;
            bf.bits[bl] = left | right;
    }
    
    // assign first bit from next block to last bit of current block
    while (bl+1) << 5 < bf.count {
        bf.bits[bl] |= (bf.bits[bl+1] & cast(u32)1) << 31;
        
        bl += 1;
        bf.bits[bl] >>= 1;
    }
    
    bf.count -= 1;
    
    // left  := bf[0].bits[bl] & ~cast(u32)((1 << bi)-1);
    // right := bf[0].bits[bl] &  cast(u32)((1 << bi)-1);
    return val;
}

operator [] :: (bf: ResizableBitField, index: int) -> bool {
    Debug(false);
    #if bf.ABC assert(index < bf.count, "Cannot access index {} out of bounds where count %.", index, bf.count);
    // if index >= bf.count then return false;
    
    bl, bi := calc_indices(cast(u32)index);
    debugln("% corresponds to %, %", index, bl, bi);
    debugln("1 << bi = %", 1 << bi);
    return cast(bool)(bf.bits[bl] & (1 << bi));
}
operator []= :: (bf: *ResizableBitField, index: int, value: bool) {
    #if bf.ABC assert(index < bf.count, "Cannot access bit at index % when count is %.", index, bf.count);
    bl, bi := calc_indices(cast(u32)index);
    if value then bf.bits[bl] |=  (cast(u32)1 << bi);
    else          bf.bits[bl] &= ~(cast(u32)1 << bi);
}

for_expansion :: (bf: ResizableBitField, body: Code, flags: For_Flags) #expand {
    #run assert(flags & For_Flags.POINTER == 0, "ResizableBitField cannot be iterated over by pointer.");
    #if flags & For_Flags.REVERSE {
        for #v2 <i: 0..bf.N-1 {
            bl, bi := calc_indices(cast(u32)i);
            
            `it_index := i;
            `it := bf.bits[bl] & (cast(u32)1 << bi) > 0;

            #insert body;
        }
    } else {
        for i: 0..bf.N-1 {
            bl, bi := calc_indices(cast(u32)i);
            
            `it_index := i;
            `it := bf.bits[bl] & (cast(u32)1 << bi) > 0;

            #insert body;
        }
    }
}

debug_print :: (bf: ResizableBitField) {
    sb : String_Builder;
    print_to_builder(*sb, "ResizableBitField, count = %\n", bf.count);
    for bf.bits {
        print_to_builder(*sb, "    %: ", it_index);
        print_integer(*sb, formatInt(it, base=2, minimum_digits=32));
        append(*sb, "\n");
    }
    print(builder_to_string(*sb));
}


// #import "Random";
// main :: () {
//     bf : ResizableBitField;
    
//     debug_print(bf);
    
//     bitfield_add(*bf, true );
//     bitfield_add(*bf, false);
//     bitfield_add(*bf, true);
//     bitfield_add(*bf, true);
//     bitfield_add(*bf, false);
    
//     println("added 1,0,1,1,0");
//     debug_print(bf);
    
//     bf[0] = false;
//     bf[4] = true;
    
//     debug_print(bf);
    
//     rem := bitfield_unordered_remove_at(*bf, 1);
//     println("removed @ %: %", 1, rem);
//     debug_print(bf);
    
//     rem = bitfield_pop(*bf);
//     println("popped %", rem);
//     debug_print(bf);
    
    
//     for 0..70 bitfield_add(*bf, cast(bool)(random_get() & cast(u64)1));
//     debug_print(bf);
    
//     for 0..70 {
//         if bf.count <= 12 break;
//         r := bitfield_ordered_remove_at(*bf, 12);
//         println("removed : %", r);
//         debug_print(bf);
//     }
// }


















BitField :: struct(N: int, ABC:=true) {
    bits : [(N >> 5) + 1]u32;
}

set_all :: (using bf: *BitField, value: bool) {
    val := ifx value then 0xffffffff else 0;
    for *bits it.* = val;
}

operator [] :: (bf: BitField, index: int) -> bool {
    #if bf.ABC assert(index < bf.N, "Index % out of bounds %.", index, bf.N);
    bl, bi := calc_indices(cast(u32)index);
    return cast(bool)(bf.bits[bl] & (1 << bi));
}
operator []= :: (bf: *BitField, index: int, value: bool) {
    #if bf.ABC assert(index < bf.N, "Index % out of bounds %.", index, bf.N);
    bl, bi := calc_indices(cast(u32)index);
    if value then bf.bits[bl] |=  (cast(u32)1 << bi);
    else          bf.bits[bl] &= ~(cast(u32)1 << bi);
}

for_expansion :: (bf: BitField, body: Code, flags: For_Flags) #expand {
    #run assert(flags & For_Flags.POINTER == 0, "BitField cannot be iterated over by pointer.");
    #if flags & For_Flags.REVERSE {
        for #v2 <i: 0..bf.N-1 {
            bl, bi := calc_indices(cast(u32)i);
            
            `it_index := i;
            `it := bf.bits[bl] & (cast(u32)1 << bi) > 0;

            #insert body;
        }
    } else {
        for i: 0..bf.N-1 {
            bl, bi := calc_indices(cast(u32)i);
            
            `it_index := i;
            `it := bf.bits[bl] & (cast(u32)1 << bi) > 0;

            #insert body;
        }
    }
}

debug_print :: (bf: BitField) {
    sb : String_Builder;
    print_to_builder(*sb, "BitField(%)\n", bf.N);
    for bf.bits {
        print_to_builder(*sb, "    %: ", it_index);
        print_integer(*sb, formatInt(it, base=2, minimum_digits=32));
        append(*sb, "\n");
    }
    print(builder_to_string(*sb));
}


// main :: () {
//     bf : BitField(40);
    
//     debug_print(bf);
    
//     bf[3] = true;
//     bf[38] = true;
    
//     debug_print(bf);
    
//     print("bf[38] = %\n", bf[38]);
//     print("bf[39] = %\n", bf[39]);
// }