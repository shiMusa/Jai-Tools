#import,file "debug.jai";
#import "Math";
#import "Basic";
#import,file "array.jai";

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


interpolate :: (from: $T, to: T, val: float) -> T {
    return from * (1.0 - val) + to * val;
}


// clamp :: (min: $MIN, x: $X, max: $MAX) -> X {
//     if x < min return cast(X)min;
//     if x > max return cast(X)max;
//     return x;
// }


// assuming x and at are sorted in increasing order
interpolate_linear :: (x: []$X, y: []$Y, at: []float) -> []float {
    assert(x.count == y.count, "x (count %) and y (count %) must be of same length!", x.count, y.count);

    res : []float;
    if at.count == 0 return res;

    array_resize(*res, at.count);

    if x[0] > at[at.count-1] {
        array_fill(res, cast(float)x[0]);
        return res;
    }
    if x[x.count-1] < at[0] {
        array_fill(res, cast(float)x[x.count-1]);
        return res;
    }

    i := -1;
    // find first index
    for x if it > at[0] {
        i = it_index-1;
        break;
    }

    j := 0;

    if i == -1 {
        at[0] = cast(float)y[0];
        i = 0;
        j = 1;
    }

    while j < at.count-1 && i < x.count-2 {
        // linear interpolation between two datapoints
        dx := at[j] - cast(float)x[i];
        Dx := cast(float, x[i+1] - x[i]);
        Dy := cast(float, y[i+1] - y[i]);
        res[j] = dx/Dx * Dy + cast(float)y[i];
        j += 1;
        while i < x.count-2 && at[j] > cast(float)x[i+1] {
            i += 1;
        }
    }

    while j < at.count-1 {
        res[j] = cast(float)y[y.count-1];
        j += 1;
    }

    return res;
}