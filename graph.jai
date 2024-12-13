#import "Basic";
#import,file "debug.jai";
#import,file "array.jai";
#import,file "println.jai";
#import,file "console.jai";




print_graph :: (graph: [][]int, $with_layers:=false) {
    Debug(true);
    N := graph.count;
    debugln("N = %", N);
    debugln(graph);

    #if with_layers {
        layers := execution_layers(graph);
        layer_idx := 0;
        debugln("layers = %", layers);
    }

    rows := NewArray(N, []string);
    for *rows {
        it.* = NewArray(N, string);
        array_fill(it.*, "  ");
        it.*[it_index] = ""; //"██";//╲";

        #if with_layers {
            layer := layers[layer_idx];
            if it_index > layer[layer.count-1] {
                layer_idx += 1;
            }
            if layer_idx % 2 == 1 
                it.*[it_index] = "";
        }
    }


    wires := NewArray(N, []int);
    for 0..N-1 wires[it] = NewArray(N, int);
    defer {
        for 0..N-1 array_free(wires[it]);
        array_free(wires);
    }

    LR :: 1 << 0;
    TD :: 1 << 1;
    TR :: 1 << 2;
    LD :: 1 << 3;

    for node, i: graph {
        for dep: node {
            if dep > i {
                for c: i+1..dep-1 wires[i][c] |= LR;
                wires[i][dep] |= LD;
                for r: i+1..dep-1 wires[r][dep] |= TD;
            } else {
                for r: dep+1..i-1 wires[r][dep] |= TD;  
                wires[i][dep] |= TR;
                for c: dep+1..i-1 wires[i][c] |= LR;
            }
        }
    }

    wire_to_glyph :: (r: int, c: int) -> string #expand {
        if c > r { // top right triangle
            if wires[r][c] == { // LR, LD, TD
                case LR;           return "──";
                case TD;           return " │";
                case LD;           return "─╮"; // "─┐";
                case LR | TD;      return "╴│";
                case LR | LD;      return "─┬";
                case TD | LD;      return "─┤";
                case LR | TD | LD; return "─┤";
            }
        } else { // bottom left triangle
            if wires[r][c] == { // LR, TR, TD
                case LR;           return "──";
                case TD;           return "│ ";
                case TR;           return "╰─"; // "└─";
                case LR | TD;      return "│╶";
                case LR | TR;      return "┴─";
                case TD | TR;      return "├─";
                case LR | TD | TR; return "├─";
            }
        }
        return "  ";
    }

    for r: 0..N-1 for c: 0..N-1 {
        if r == c continue;
        rows[r][c] = wire_to_glyph(r, c);
    }

    for row: rows {
        for c: row print(c);
        print("\n");
    }
}

print_execution_graph_mermaid :: (graph: [][]int) {
    Debug(true);
    exec_graph := execution_graph(graph);
    flattened := graph_flatten(exec_graph);
    exec_sorted_graph := graph_permute(graph_copy(graph), flattened);

    is_initial_layer := true;
    last_layer_index := 0;
    layer_index := 0;
    println("flowchart RL");
    println("subgraph l0");
    for node, i: exec_sorted_graph {
        if node.count == 0 {
            println(i);
            last_layer_index = i;
            continue;
        }
        if is_initial_layer {
            is_initial_layer = false;
            layer_index += 1;
            println("end");
            println("subgraph l%", layer_index);
        }

        for dep, j: node {
            if dep > last_layer_index {
                println("end");
                layer_index += 1;
                println("subgraph l%", layer_index);
                last_layer_index = i-1;
                break;
            }
        }
        for dep: node 
            println("%-->%", i, dep);
    }
    println("end");

    
    // layers : [..][..]int;
    // current_layer := array_add(*layers);
    // last_index := 0;
    // initial_layer := true;
    // for node, i: exec_sorted_graph {
    //     // nodes with no dependencies
    //     if node.count == 0 {
    //         array_add(current_layer, cast(int)i);
    //         last_index = i;
    //         continue;
    //     }

    //     // not initial layer anymore, so make new layer
    //     if initial_layer {
    //         current_layer = array_add(*layers);
    //         initial_layer = false;
    //     }

    //     // check if node only depends on previous layer
    //     for dep, j: node {
    //         if dep > last_index { // depends on current layer
    //             last_index = i;
    //             current_layer = array_add(*layers);
    //             array_add(current_layer, i);
    //             continue i;
    //         }
    //     }

    //     array_add(current_layer, i);
    // }
    // debugln("%", layers);

    // for layer, i: layers {
    //     for node, j: layer {
    //         // print(" (%) ", formatInt(node, minimum_digits=2, padding = #char " "));
    //         println("%", node);
    //     }
    //     if i>0 {
    //         deps = exec_sorted_graph[i];
    //         for dep
    //     }
    //     print("\n");
    // }
}




graph_permute :: (graph: [][]int, order: []int) -> [][]int {
    assert(graph.count == order.count);
    inv_ord := inverse_permutation(order);
    sorted_graph := array_permute(graph, order);
    for sorted_graph for *dep: it 
        dep.* = inv_ord[dep.*];
    return sorted_graph;
}

graph_copy :: (graph: [][]int) -> [][]int {
    res := NewArray(graph.count, []int);
    for *res it.* = array_copy(graph[it_index]);
    return res;
}

graph_flatten :: (graph: [][]int) -> []int {
    sum := 0;
    for graph sum += it.count;

    res := NewArray(sum, int);
    indx := 0;
    for graph for val: it {
        res[indx] = val;
        indx += 1;
    }

    return res;
}




topological_sort :: (dependency_graph: [][]int) -> []int {
    Debug(false);
    first := 0;
    for dependency_graph if it.count == 0 { first = it_index; break; }

    order : [..]int;
    array_reserve(*order, dependency_graph.count);
    visited := NewArray(dependency_graph.count, bool);
    defer array_free(visited);

    for i: 0..dependency_graph.count-1 {
        ii := i;
        if i == 0    then ii = first;
        if i == first then ii = 0;

        _topo_sort(dependency_graph, ii, *order, visited);
    }

    return order;
}

#scope_file
_topo_sort :: (graph: [][]int, index: int, order: *[..]int, visited: []bool) {
    Debug(false);

    debugln("index %", index);
    debugln("order %", order.*);
    debugln("visited %", visited);

    if visited[index] return;
    visited[index] = true;

    for link: graph[index]
        _topo_sort(graph, link, order, visited);

    array_add(order, index);
}

#scope_export

execution_graph :: (dependency_graph: [][]int) -> [][]int {
    Debug(false);

    copy := NewArray(dependency_graph.count, [..]int);
    for *copy {
        it.* = resizable(array_copy(dependency_graph[it_index]));
    }

    collected := NewArray(dependency_graph.count, bool);
    num_collected := 0;

    res: [..][]int;

    while num_collected < dependency_graph.count && res.count < dependency_graph.count {
        debugln("level %, res = %", res.count, res);
        debugln("copy %", copy);
        debugln("collected %", collected);
        level : [..]int;

        for #v2 <i: 0..copy.count-1 {
            if collected[i] continue;
            debugln("i = %", i);

            if copy[i].count == 0 {
                debugln("no dependencies");
                array_add(*level, i);

                collected[i] = true;
                num_collected += 1;

                // for *n, k: copy {
                //     if k == i continue;
                //     array_unordered_remove_by_value(n, i);
                // }
            }
        }

        debugln("collected level %", level);
        for *deps: copy {
            for coll: level for #v2 <i: 0..deps.count-1 {
                debugln("checking deps.*[%] = % against %", i, deps.*[i], coll);
                if deps.*[i] == coll array_ordered_remove_by_index(deps, i);
            }
        }

        array_add(*res, cast([]int)level);
    }

    return cast,force([][]int)res;
}


execution_layers :: (execution_graph: [][]int) -> [][]int {
    layers : [..][..]int;
    current_layer := array_add(*layers);
    last_index := 0;
    initial_layer := true;

    for node, i: execution_graph {
        // nodes with no dependencies
        if node.count == 0 {
            array_add(current_layer, cast(int)i);
            last_index = i;
            continue;
        }

        // not initial layer anymore, so make new layer
        if initial_layer {
            current_layer = array_add(*layers);
            initial_layer = false;
        }

        // check if node only depends on previous layer
        for dep, j: node {
            if dep > last_index { // depends on current layer
                last_index = i-1;
                current_layer = array_add(*layers);
                array_add(current_layer, i);
                continue i;
            }
        }

        array_add(current_layer, i);
    }

    res := NewArray(layers.count, []int);
    for layers res[it_index] = cast,force([]int)it;
    return res;
}