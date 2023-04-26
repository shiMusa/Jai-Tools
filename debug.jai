#import "Basic";

Debug :: ($value: bool, $msg: string = "") #expand {
    `DEBUG :: value;
    #if msg != "" `DEBUG_HEAD :: msg;
    else          `DEBUG_HEAD :: #procedure_name();
}

debugln :: inline (msg: string, args: ..Any, $print_head:=true) #expand {
    #if `DEBUG {
        #if print_head print("% | ", `DEBUG_HEAD);
        print(msg, ..args);
        print("\n");
    }
}

debugln :: inline (arg: Any, $print_head:=true) #expand {
    #if `DEBUG {
        #if print_head print("% | ", `DEBUG_HEAD);
        print("%\n", arg);
    }
}

debug :: inline (msg: string, args: ..Any, $print_head:=true) #expand {
    #if `DEBUG {
        #if print_head print("% | ", `DEBUG_HEAD);
        print(msg, ..args);
    }
}