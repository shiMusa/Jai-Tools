#import "Basic";

println :: inline (msg: string, args: ..Any) {
    print(msg, ..args);
    print("\n");
}

println :: inline (arg: Any) {
    print("%\n", arg);
}

bit_string :: (x: $X, num_bits:=64) -> string {
    b: String_Builder;
    print_integer(*b, formatInt(x, base=2, minimum_digits=num_bits));
    return builder_to_string(*b);
}