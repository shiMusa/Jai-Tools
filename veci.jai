#import "Math";

Vec2 :: Vector2;
Vec3 :: Vector3;
Vec4 :: Vector4;

Mat4 :: Matrix4;
Mat4_ID :: #run identity_of(Mat4);

// vec2 :: make_vector2;
// vec3 :: make_vector3;
// vec4 :: make_vector4;

vec2 :: inline (x: $X, y: $Y) -> Vec2 {
    return Vec2.{cast(float)x, cast(float)y};
}
vec3 :: inline (x: $X, y: $Y, z: $Z) -> Vec3 {
    return Vec3.{cast(float)x, cast(float)y, cast(float)z};
}
vec4 :: inline (x: $X, y: $Y, z: $Z, w: $W) -> Vec4 {
    return Vec4.{cast(float)x, cast(float)y, cast(float)z, cast(float)w};
}


color4 :: ($$rgba: u32) -> Vec4 {
    #if is_constant(rgba) {
        a :: #run cast(float)(rgba       & 0xff)/255.0;
        b :: #run cast(float)(rgba >>  8 & 0xff)/255.0;
        g :: #run cast(float)(rgba >> 16 & 0xff)/255.0;
        r :: #run cast(float)(rgba >> 24 & 0xff)/255.0;
        return Vec4.{r,g,b,a};
    } else {
        a := cast(float)(rgba       & 0xff)/255.0;
        b := cast(float)(rgba >>  8 & 0xff)/255.0;
        g := cast(float)(rgba >> 16 & 0xff)/255.0;
        r := cast(float)(rgba >> 24 & 0xff)/255.0;
        return Vec4.{r,g,b,a};
    }
}

color_u8 :: ($$rgba: u32) -> [4]u8 {
    #if is_constant(rgba) {
        a :: #run cast(u8)(rgba       & 0xff);
        b :: #run cast(u8)(rgba >>  8 & 0xff);
        g :: #run cast(u8)(rgba >> 16 & 0xff);
        r :: #run cast(u8)(rgba >> 24 & 0xff);
        return u8.[r,g,b,a];
    } else {
        a := cast(u8)(rgba       & 0xff);
        b := cast(u8)(rgba >>  8 & 0xff);
        g := cast(u8)(rgba >> 16 & 0xff);
        r := cast(u8)(rgba >> 24 & 0xff);
        return u8.[r,g,b,a];
    }
}




norm_1 :: inline (v: Vec2) -> float {
    return abs(v.x) + abs(v.y);
}
norm_2 :: inline (v: Vec2) -> float {
    return length(v);
}
norm_l :: (v: Vec2, $$l: float) -> float {
    #if is_constant(l) {
        powl :: #bake_arguments pow(power=l);
        sqrtl :: #bake_arguments pow(power=1.0/l);
        return sqrtl(powl(abs(v.x)) + powl(abs(v.y)));
    } else {
        return pow(pow(abs(v.x), l) + pow(abs(v.y), l), 1.0/l);
    }
}

norm_1 :: inline (v: Vec3) -> float {
    return abs(v.x) + abs(v.y) + abs(v.z);
}
norm_2 :: inline (v: Vec3) -> float {
    return length(v);
}
norm_l :: (v: Vec3, $$l: float) -> float {
    #if is_constant(l) {
        powl :: #bake_arguments pow(power=l);
        sqrtl :: #bake_arguments pow(power=1.0/l);
        return sqrtl(powl(abs(v.x)) + powl(abs(v.y)) + powl(abs(v.z)));
    } else {
        return pow(pow(abs(v.x), l) + pow(abs(v.y), l) + pow(abs(v.z), l), 1.0/l);
    }
}

norm_1 :: inline (v: Vec4) -> float {
    return abs(v.x) + abs(v.y) + abs(v.z) + abs(v.w);
}
norm_2 :: inline (v: Vec4) -> float {
    return length(v);
}
norm_l :: (v: Vec4, $$l: float) -> float {
    #if is_constant(l) {
        powl :: #bake_arguments pow(power=l);
        sqrtl :: #bake_arguments pow(power=1.0/l);
        return sqrtl(powl(abs(v.x)) + powl(abs(v.y)) + powl(abs(v.z)) + powl(abs(v.w)));
    } else {
        return pow(pow(abs(v.x), l) + pow(abs(v.y), l) + pow(abs(v.z), l) + pow(abs(v.w), l), 1.0/l);
    }
}



// operator * :: inline (a: Vec4, b: float32) -> Vec4 #symmetric {
//     return Vec4.{a.x * b, a.y * b, a.z * b, a.w * b};
// }
// operator / :: inline (a: Vec4, b: float32) -> Vec4 {
//     return Vec4.{a.x / b, a.y / b, a.z / b, a.w / b};
// }










// ##     ## ########  ######   #######  ####
// ##     ## ##       ##    ## ##     ##  ##
// ##     ## ##       ##              ##  ##
// ##     ## ######   ##        #######   ##
//  ##   ##  ##       ##       ##         ##
//   ## ##   ##       ##    ## ##         ##
//    ###    ########  ######  ######### ####

Vec2i :: struct {
    x: s32;
    y: s32;
#place x;
    c: s32;
    r: s32;
#place x;
    width: s32;
    height: s32;
#place x;  
    cols: s32;
    rows: s32;
}


for_expansion :: (v: Vec2i, body: Code, flags: For_Flags) #expand {
    for r: 0..v.rows-1 for c: 0..v.cols-1 {
        `it := Vec2i.{c,r};
        `it_index := r*v.cols + c;
        #insert body;
    }
}


vec2i :: inline (x: $X, y: $Y) -> Vec2i {
    return .{cast(s32)x, cast(s32)y};
}
vec2i :: inline (v: Vec2, $round_values:=false) -> Vec2i {
    #if !round_values
        return vec2i(cast(s32)v.x, cast(s32)v.y);
    else
        return vec2i(round(v.x), round(v.y));
}
vec2 :: inline (v: Vec2i) -> Vec2 {
    return vec2(v.x, v.y);
}

operator == :: inline (a: Vec2i, b: Vec2i) -> bool {
    return a.x == b.x && a.y == b.y;
}

operator + :: inline (a: Vec2i, b: Vec2i) -> Vec2i {
    return vec2i(a.x+b.x, a.y+b.y);
}
operator - :: inline (a: Vec2i, b: Vec2i) -> Vec2i {
    return vec2i(a.x - b.x, a.y - b.y);
}
operator * :: inline (a: Vec2i, b: int) -> Vec2i #symmetric {
    return vec2i(a.x * cast(s32)b, a.y * cast(s32)b);
}
operator / :: inline (a: Vec2i, b: int) -> Vec2i {
    return vec2i(a.x / cast(s32)b, a.y / cast(s32)b);
}

operator * :: inline (a: Vec2i, b: float32) -> Vec2 #symmetric {
    return vec2(a.x * b, a.y * b);
}
operator / :: inline (a: Vec2i, b: float32) -> Vec2 {
    return vec2(a.x / b, a.y / b);
}
operator + :: inline (a: Vec2i, b: Vec2) -> Vec2 #symmetric {
    return vec2(cast(float32)a.x+b.x, cast(float32)a.y+b.y);
}
operator - :: inline (a: Vec2i, b: Vec2) -> Vec2 {
    return vec2(cast(float32)a.x - b.x, cast(float32)a.y - b.y);
}
operator - :: inline (a: Vec2, b: Vec2i) -> Vec2 {
    return vec2(a.x - cast(float32)b.x, a.y - cast(float32)b.y);
}

operator * :: inline (a: Vec2i, b: Vec2i) -> Vec2i {
    return vec2i(a.x * b.x, a.y * b.y);
}
operator * :: inline (a: Vec2i, b: Vec2) -> Vec2 #symmetric {
    return vec2(a.x * b.x, a.y * b.y);
}


Vec4i :: struct {
    x: s32;
    y: s32;
    z: s32;
    w: s32;
#place z;
    width: s32;
    height: s32;
#place z;
    size: Vec2i;
#place x;
    pos: Vec2i;
#place z;
    cols: s32;
    rows : s32;
#place x;
    c: s32;
    r: s32;
}
operator + :: inline (a: Vec4i, b: Vec4i) -> Vec4i {
    return Vec4i.{a.x+b.x, a.y+b.y, a.z+b.z, a.w+b.w};
}
operator - :: inline (a: Vec4i, b: Vec4i) -> Vec4i {
    return Vec4i.{a.x-b.x, a.y-b.y, a.z-b.z, a.w-b.w};
}
