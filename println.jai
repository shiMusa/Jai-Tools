#import "Basic";

println :: inline (msg: string, args: ..Any) {
    print(msg, ..args);
    print("\n");
}

println :: inline (arg: Any) {
    print("%\n", arg);
}