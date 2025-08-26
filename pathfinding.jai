#module_parameters(RELEASE:=false);

#import "Basic";
#import "Hash_Table";
#import "Math";
#import,file "min_max_heap.jai"(RELEASE=RELEASE);
#import,file "debug.jai";
#import,file "array.jai";


/*

https://en.wikipedia.org/wiki/A*_search_algorithm:
---------------------------------------------------

function reconstruct_path(cameFrom, current)
    total_path := {current}
    while current in cameFrom.Keys:
        current := cameFrom[current]
        total_path.prepend(current)
    return total_path

// A* finds a path from start to goal.
// h is the heuristic function. h(n) estimates the cost to reach goal from node n.
function A_Star(start, goal, h)
    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    // This is usually implemented as a min-heap or priority queue rather than a hash-set.
    openSet := {start}

    // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from the start
    // to n currently known.
    cameFrom := an empty map

    // For node n, gScore[n] is the currently known cost of the cheapest path from start to n.
    gScore := map with default value of Infinity
    gScore[start] := 0

    // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
    // how cheap a path could be from start to finish if it goes through n.
    fScore := map with default value of Infinity
    fScore[start] := h(start)

    while openSet is not empty
        // This operation can occur in O(Log(N)) time if openSet is a min-heap or a priority queue
        current := the node in openSet having the lowest fScore[] value
        if current = goal
            return reconstruct_path(cameFrom, current)

        openSet.Remove(current)
        for each neighbor of current
            // d(current,neighbor) is the weight of the edge from current to neighbor
            // tentative_gScore is the distance from start to the neighbor through current
            tentative_gScore := gScore[current] + d(current, neighbor)
            if tentative_gScore < gScore[neighbor]
                // This path to neighbor is better than any previous one. Record it!
                cameFrom[neighbor] := current
                gScore[neighbor] := tentative_gScore
                fScore[neighbor] := tentative_gScore + h(neighbor)
                if neighbor not in openSet
                    openSet.add(neighbor)

    // Open set is empty but goal was never reached
    return failure

*/




a_star :: (
	start: int, 
	goal: int,
	data: $T, 
	$heuristic: (from: int, to: int, data: T) -> float, 
	$weight : (from: int, to: int, data: T) -> float,
	$neighbors : (node: int, data: T) -> []int,
	$reversed := false
) -> []int {
	Debug(false);

	simple_hash_fct :: inline (n: int) -> u32 { return cast(u32, n); }
	FTable :: Table(int, float, simple_hash_fct);

	previous_of           : Table(int, int, simple_hash_fct);
	score_until           : FTable;  // gScore, default int
	estimated_total_score : FTable;  // fScore, default int

	larger_than :: inline (a: int, b: int, ets: *FTable) -> bool { 
		a_ok, av := table_find_new(ets, a);
		if !a_ok av = FLOAT32_INFINITY;
		b_ok, bv := table_find_new(ets, b);
		if !b_ok bv = FLOAT32_INFINITY;
		return av > bv; 
	}
	Empty :: struct {}
	open_set : MMH(int, larger_than);
	open_set.data = *estimated_total_score;
	open_set_map : Table(int, Empty, simple_hash_fct);

	reconstruct_path :: () -> []int #expand {
		res : [..]int;
		array_add(*res, `current);
		
		ok, prev := table_find_new(*`previous_of, `current);
		while ok {
			`current = prev;
			array_add(*res, `current);
			ok, prev = table_find_new(*`previous_of, `current);
		}

		// reverse such that start is at index 0
		#if !reversed array_reverse(res);

		return res;
	}
	
	append(*open_set, start);
	table_add(*score_until, start, 0.0);
	table_add(*estimated_total_score, start, heuristic(start, goal, data));

	while open_set.items.count != 0 {
		current := pop_min(*open_set);
		table_remove(*open_set_map, current);

		debugln("current = %", current);

		if current == goal {
			res := reconstruct_path();

			deinit(*previous_of);
			deinit(*score_until);
			deinit(*estimated_total_score);
			deinit(*open_set_map);
			mmh_free(*open_set);

			return res;
		}

		neigh := neighbors(current, data);
		defer array_free(neigh);

		debugln("neighbours: %", neigh);

		for n: neigh {
			ok, score := table_find_new(*score_until, current);
			tentative_score : float;
			if !ok tentative_score = FLOAT32_INFINITY;
			else   tentative_score = score + weight(n, current, data);

			debugln("    %: tentative_score = %", n, tentative_score);

			ok, score = table_find_new(*score_until, n);
			if !ok score = FLOAT32_INFINITY;
			
			debugln("    %: score_until = %", n, score);

			if tentative_score < score {
				table_set(*previous_of, n, current);
				table_set(*score_until, n, tentative_score);
				ets := tentative_score + heuristic(n, goal, data);
				table_set(*estimated_total_score, n, ets);
				debugln("     > added with estimated_total_score = %", ets);
				if !table_contains(*open_set_map, n) {
					append(*open_set, n);
					table_add(*open_set_map, n, .{});
				}
			}
		}
	}


	deinit(*previous_of);
	deinit(*score_until);
	deinit(*estimated_total_score);
	deinit(*open_set_map);
	mmh_free(*open_set);
	return .[];
}