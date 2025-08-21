#import "Basic";


/*
https://en.wikipedia.org/wiki/Min-max_heap 
*/




#scope_file

swap :: (arr: []MMH_Node, i: int, j: int) #expand {
	// swap everything
	tmp := arr[i];
	arr[i] = arr[j];
	arr[j] = tmp;
	// un-swap parents and levels
	arr[j].parent = arr[i].parent;
	arr[i].parent = tmp.parent;
	arr[j].level = arr[i].level;
	arr[i].level = tmp.level;
}

#scope_export

MMH_Node :: struct(T: Type, cmp: (a: T, b: T) -> bool) {
	level  : int =  0;
	parent : int = -1;
	left   : int = -1;
	right  : int = -1;
	value: T;
}

mmh_is_min_level :: inline (arr: []MMH_Node, i: int) -> bool {
	return arr[i].level % 2 == 0;
}

mmh_has_children :: inline (arr: []MMH_Node, i: int) -> bool {
	return arr[i] != -1 || arr[i] != -1;
}

mmh_smallest_child_or_grandchild :: (arr: []MMH_Node($T, $cmp), i: int) -> int, T, bool {
	i := -1;
	v : T; 
	is_child := true;

	i_l  := arr[i].left;
	i_r := arr[i].right;

	if i_l  != -1 { 
		n := arr[i_l];

		i = i_l ; 
		v = n.value; 

		i_ll := n.left;
		i_lr := n.right;

		if i_ll != -1 && cmp(arr[i_ll].value, v) { i = i_ll; v = arr[i_ll].value; is_child = false; }
		if i_lr != -1 && cmp(arr[i_lr].value, v) { i = i_lr; v = arr[i_lr].value; is_child = false; }
	}
	if i_r != -1 {
		n := arr[i_r];

		if cmp(n.value, v) { i = i_r; v = n.value; is_child = true; };

		i_rl := n.left;
		i_rr := n.right;

		if i_rl != -1 && cmp(arr[i_rl].value, v) { i = i_rl; v = arr[i_rl].value; is_child = false; }
		if i_rr != -1 && cmp(arr[i_rr].value, v) { i = i_rr; v = arr[i_rr].value; is_child = false; }
	}

	return i, v, is_child;
}

mmh_largest_child_or_grandchild :: (arr: []MMH_Node($T, $cmp), i: int) -> int, T, bool {
	i := -1;
	v : T; 
	is_child := true;

	i_l  := arr[i].left;
	i_r := arr[i].right;

	if i_l  != -1 { 
		n := arr[i_l];

		i = i_l ; 
		v = n.value; 

		i_ll := n.left;
		i_lr := n.right;

		if i_ll != -1 && !cmp(arr[i_ll].value, v) { i = i_ll; v = arr[i_ll].value; is_child = false; }
		if i_lr != -1 && !cmp(arr[i_lr].value, v) { i = i_lr; v = arr[i_lr].value; is_child = false; }
	}
	if i_r != -1 {
		n := arr[i_r];

		if !cmp(n.value, v) { i = i_r; v = n.value; is_child = true; };

		i_rl := n.left;
		i_rr := n.right;

		if i_rl != -1 && !cmp(arr[i_rl].value, v) { i = i_rl; v = arr[i_rl].value; is_child = false; }
		if i_rr != -1 && !cmp(arr[i_rr].value, v) { i = i_rr; v = arr[i_rr].value; is_child = false; }
	}

	return i, v, is_child;
}

mmh_grandparent_of :: (arr: []MMH_Node, i: int) -> int {
	p := arr[i].parent;
	return ifx p == -1 then -1 else arr[p].parent;
}

mmh_push_down :: (arr: []MMH_Node($T, $cmp), index: int) {
	m := index;

	while mmh_has_children(arr, m) {
		i := m;
		if mmh_is_min_level(arr, i) {
			m, v, is_child := mmh_smallest_child_or_grandchild(arr, i);
			if !cmp(v, arr[i].value) {
				swap(arr, m, i);
				if !is_child { 
					if cmp(v, arr[arr[m].parent].value) then swap(arr, m, arr[m].parent);
				} else break;
			} else break;
		} else {
			m, v, is_child := mmh_largest_child_or_grandchild(arr, i);
			if cmp(v, arr[i].value) {
				swap(arr, m, i);
				if !is_child {
					if !cmp(v, arr[arr[m].parent].value) then swap(arr, m, arr[m].parent);
				} else break;
			} else break;
		}
	}
}


mmh_push_up_min :: (arr: []MMH_Node($T, $cmp), index: int) {
	i := index;
	gp := mmh_grandparent_of(arr, i);
	while gp != -1 && !cmp(arr[i].value, arr[gp].value) {
		swap(arr, i, gp);
		i = gp;
	}
}
mmh_push_up_max :: (arr: []MMH_Node($T, $cmp), index: int) {
	i := index;
	gp := mmh_grandparent_of(arr, i);
	while gp != -1 && cmp(arr[i].value, arr[gp].value) {
		swap(arr, i, gp);
		i = gp;
	}
}

mmh_push_up :: (arr: []MMH_Node($T, $cmp), i: int) {
	if i == 0 return;

	if mmh_is_min_level(arr, i) {
		p := arr[i].parent;
		if cmp(arr[i].value, arr[p].value) {
			swap(arr, i, p);
			mmh_push_up_max(arr, p);
		} else {
			mmh_push_up_min(arr, i);
		}
	} else {
		p := arr[i].parent;
		if !cmp(arr[i].value, arr[p].value) {
			swap(arr, i, p);
			mmh_push_up_min(arr, p);
		} else {
			mmh_push_up_max(arr, i);
		}
	}
}

mmh_build :: (arr: *[]MMH_Node) {
	for <i: arr.count/2..1 {
		mmh_push_down(arr, i);
	}
}

// mmh_min :: (arr: []MMH_Node($T)) -> T {

// }


mmh_append :: (arr: *[..]MMH_Node($T, $cmp), value: T) {
	n : MMH_Node(T, cmp);
	n.value = value;
	index := arr.count;
	array_add(arr, n);
	mmh_push_up(arr.*, index);
}




main :: () {
	cmp :: (a: int, b: int) -> bool { return a >= b; }
	arr : [..]MMH_Node(int, cmp);

	values :: int.[46, 51, 31, 21, 13, 31, 10, 11, 16, 71, 41, 8];

	for v: values {
		mmh_append(*arr, v);

		print("added % -----------------\n", v);
		for arr print("%\n", it);
	}
}