#import "Math";

Vec2 :: Vector2;
Vec3 :: Vector3;
Vec4 :: Vector4;

Mat4 :: Matrix4;
Mat4_ID :: #run identity_of(Mat4);

vec2 :: make_vector2;
vec3 :: make_vector3;
vec4 :: make_vector4;


// ##     ## ########  ######   #######  ####
// ##     ## ##       ##    ## ##     ##  ##
// ##     ## ##       ##              ##  ##
// ##     ## ######   ##        #######   ##
//  ##   ##  ##       ##       ##         ##
//   ## ##   ##       ##    ## ##         ##
//    ###    ########  ######  ######### ####

Vec2i :: struct {
    x: s16;
    y: s16;
#place x;
    c: s16;
    r: s16;
#place x;
    width: s16;
    height: s16;
#place x;  
    cols: s16;
    rows: s16;
}


for_expansion :: (v: Vec2i, body: Code, flags: For_Flags) #expand {
    for r: 0..v.rows-1 for c: 0..v.cols-1 {
        `it := Vec2i.{c,r};
        `it_index := r*v.cols + c;
        #insert body;
    }
}


vec2i :: inline (x: $X, y: $Y) -> Vec2i {
    return .{cast(s16)x, cast(s16)y};
}
vec2i :: inline (v: Vec2, $round_values:=false) -> Vec2i {
    #if !round_values
        return vec2i(cast(s16)v.x, cast(s16)v.y);
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
    return vec2i(a.x * cast(s16)b, a.y * cast(s16)b);
}
operator / :: inline (a: Vec2i, b: int) -> Vec2i {
    return vec2i(a.x / cast(s16)b, a.y / cast(s16)b);
}

operator * :: inline (a: Vec2i, b: float32) -> Vec2 #symmetric {
    return vec2(a.x * b, a.y * b);
}
operator / :: inline (a: Vec2i, b: float32) -> Vec2 {
    return vec2(a.x * b, a.y * b);
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
    x: s16;
    y: s16;
    z: s16;
    w: s16;
#place z;
    width: s16;
    height: s16;
#place z;
    size: Vec2i;
#place x;
    pos: Vec2i;
#place z;
    cols: s16;
    rows : s16;
#place x;
    c: s16;
    r: s16;
}
operator + :: inline (a: Vec4i, b: Vec4i) -> Vec4i {
    return Vec4i.{a.x+b.x, a.y+b.y, a.z+b.z, a.w+b.w};
}
operator - :: inline (a: Vec4i, b: Vec4i) -> Vec4i {
    return Vec4i.{a.x-b.x, a.y-b.y, a.z-b.z, a.w-b.w};
}
