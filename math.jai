#import,file "debug.jai";

array_round_to :: (x: [$N]$T, $type: Type) -> [N]type {
    res : [N]type = ---;
    for x res[it_index] = round_to(it, type);
    return res;
}

round_to :: inline (x: $T, $type: Type) -> type {
    Debug(false);
    // return cast(type)Math.round(x);
    res : type = ---;
    if x >= 0
        res = cast(type)( (cast(s64)(2*x + 1)) >> 1);
    else
        res = cast(type)( (cast(s64)(2*x)) >> 1);
    debugln("round_to | x = %, type = %, res = %", x, type, res);
    return res;
}
round :: inline (x: $T) -> T {
    return round_to(x, T);
}

floor_to :: inline (x: $T, $type: Type) -> type {
    return cast(type)(cast(s64)x);
}
floor :: inline (x: $T) -> T {
    return floor_to(x, T);
}
