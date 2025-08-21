#import "Basic";
#import "Hash_Table";


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
	$heuristic: (node: int) -> float, 
	$weight : (from: int, to: int) -> float
) -> []int {
	
	Entry :: struct {
		node: int;  // neg values for next free node in array
		next_higher_index: int;
	}
	open_set : [..]Entry;


	// double-linked list removal
	remove :: (index: int) {
		n := open_set[index];
		n.node = open_set[0].node;
		open_set[0].node = index;

		higher := n.next_higher_index;

		open_set[lower ].next_higher_index = higher;
		open_set[higher].next_lower_index  = lower;
	}

	add :: (node: int) {
		free := -open_set[0].node;

		go_lower := 


		if free == open_set.count {
			// new entry to array
			array_add(*open_set, .{})
		}
	}


	reconstruct_path :: (node: int) -> []int {
		res : [..]int;
		// TODO
		return res;
	}
	
	array_add(*open_set, .{   -2, 1, 1});  // will always just point to the lowest fscore entry
	array_add(*open_set, .{start, 0, 0});

	simple_hash_fct :: inline (n: int) -> u32 { return cast(u32, n); }
	previous_of           : Table(int,   int, simple_hash_fct);
	score_until           : Table(int, float, simple_hash_fct);
	estimated_total_score : Table(int, float, simple_hash_fct);

	table_add(*score_until, start, 0.0);
	table_add(*estimated_total_score, start, heuristic(start));

	while open_set.count != 0 {
		ic := open_set[0].next_higher_index;
		current := open_set[ic].node;

		if current == goal return reconstruct_path(current);

		array_remove()
	}

	return .[];
}




main :: () {
	print("Pathfinding\n");
}