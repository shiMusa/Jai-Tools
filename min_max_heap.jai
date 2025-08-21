#import "Basic";
#import,file "debug.jai";


/*
https://en.wikipedia.org/wiki/Min-max_heap 
*/

#scope_export

MMH :: struct(T: Type, larger_than: (a: T, b: T) -> bool) {
	arr: [..]T;
}

operator *[] :: (heap: *MMH($T, $larger_than), index: u64) -> *T {
    return *heap.arr[index];
}


#scope_file

NO_PARENT : u64 : 0xFFFFFFFFFFFFFFFF;
parent :: (i: u64) -> u64 #expand {
	return ifx i == 0 || i == NO_PARENT then NO_PARENT else (i-1)/2;
}

left :: (i: u64) -> u64 #expand {
	return 2*i+1;
}
right :: (i: u64) -> u64 #expand {
	return 2*i+2;
}

swap :: inline (heap: MMH, i: u64, j: u64) {
	tmp := heap[i];
	heap[i] = heap[j];
	heap[j] = tmp;
}





// Basic/Int128.jai
count_leading_zero_bits :: inline (x: u64) -> u8 {
    // Maybe Basic should have a Simple_Math component with stuff like this in it? - IK
    n: u8 = ---;
    #if CPU == .X64 {
        #asm {
            lzcnt n, x;
        }
    } else {
        if x == 0 return(64);
        n = 0;
        if x <= 0x00000000FFFFFFFF { n = n + 32; x = x << 32; }
        if x <= 0x0000FFFFFFFFFFFF { n = n + 16; x = x << 16; }
        if x <= 0x00FFFFFFFFFFFFFF { n = n +  8; x = x <<  8; }
        if x <= 0x0FFFFFFFFFFFFFFF { n = n +  4; x = x <<  4; }
        if x <= 0x3FFFFFFFFFFFFFFF { n = n +  2; x = x <<  2; }
        if x <= 0x7FFFFFFFFFFFFFFF { n = n +  1; }
    }
    return n;
}



is_min_level :: (i: u64) -> bool #expand {
	return (63 - count_leading_zero_bits(i + 1)) & 1 == 0;
}

has_children :: inline (heap: MMH, i: u64) #expand {
	n := heap.count;
	return right(i) < n || left(i) < n;
}

smallest_child_or_grandchild :: (heap: MMH($T, $larger_than), i: u64) -> u64, T, bool {
	index := -1;
	value : T; 
	is_child := true;
	n := heap.count;

	l := left(i);
	if l < n {
		index = l;
		value = heap[l];

		ll := left(l);
		lr := right(l);

		if ll < n && !larger_than(heap[ll], value) then { index = ll; value = heap[ll]; is_child = false; }
		if lr < n && !larger_than(heap[lr], value) then { index = lr; value = heap[lr]; is_child = false; }
	}
	r := right(i);
	if r < n {
		if !larger_than(heap[r], value) then { index = r; value = heap[r]; is_child = true; }
		rl := left(l);
		rr := right(l);
		if rl < n && !larger_than(heap[rl], value) then { index = rl; value = heap[rl]; is_child = false; }
		if rr < n && !larger_than(heap[rr], value) then { index = rr; value = heap[rr]; is_child = false; }
	}
	return index, value, is_child;
}

largest_child_or_grandchild :: (heap: MMH($T, $larger_than), i: u64) -> u64, T, bool {
	index := -1;
	value : T; 
	is_child := true;
	n := heap.count;

	l := left(i);
	if l < n {
		index = l;
		value = heap[l];

		ll := left(l);
		lr := right(l);

		if ll < n && larger_than(heap[ll], value) then { index = ll; value = heap[ll]; is_child = false; }
		if lr < n && larger_than(heap[lr], value) then { index = lr; value = heap[lr]; is_child = false; }
	}
	r := right(i);
	if r < n {
		if larger_than(heap[r], value) then { index = r; value = heap[r]; is_child = true; }
		rl := left(l);
		rr := right(l);
		if rl < n && larger_than(heap[rl], value) then { index = rl; value = heap[rl]; is_child = false; }
		if rr < n && larger_than(heap[rr], value) then { index = rr; value = heap[rr]; is_child = false; }
	}
	return index, value, is_child;
}

push_down :: (heap: MMH($T, $larger_than), index: u64) {
	m := index;

	while has_children(heap, m) {
		i := m;
		if is_min_level(i) {
			m, v, is_child := smallest_child_or_grandchild(heap, i);
			if !larger_than(v, heap[i]) {
				swap(heap, m, i);
				if !is_child { 
					if larger_than(v, heap[parent(m)]) then swap(heap, m, parent(m));
				} else break;
			} else break;
		} else {
			m, v, is_child := mmh_largest_child_or_grandchild(heap, i);
			if larger_than(v, heap[i]) {
				swap(heap, m, i);
				if !is_child {
					if !larger_than(v, heap[parent(m)]) then swap(heap, m, parent(m));
				} else break;
			} else break;
		}
	}
}


push_up_min :: (heap: MMH($T, $larger_than), index: u64) {
	Debug(true);
	i := index;
	gp := parent(parent(i));
	debugln("gp = %", gp);
	while gp != NO_PARENT && !larger_than(heap[i], heap[gp]) {
		swap(heap, i, gp);
		i = gp;
		gp = parent(parent(i));
	}
}
push_up_max :: (heap: MMH($T, $larger_than), index: u64) {
	Debug(true);
	i := index;
	gp := parent(parent(i));
	debugln("gp = %", gp);
	while gp != NO_PARENT && larger_than(heap[i], heap[gp]) {
		debugln("% @ % > % @ %, so swap", heap[i], i, heap[gp], gp);
		swap(heap, i, gp);
		i = gp;
		gp = parent(parent(i));
	}
}

push_up :: (heap: MMH($T, $larger_than), i: u64) {
	Debug(true);
	debugln("i = %", i);
	if i == 0 return;

	if is_min_level(i) {
		debugln("is min level");
		p := parent(i);
		if larger_than(heap[i], heap[p]) {
			swap(heap, i, p);
			push_up_max(heap, p);
		} else {
			push_up_min(heap, i);
		}
	} else {
		debugln("is max level");
		p := parent(i);
		if !larger_than(heap[i], heap[p]) {
			swap(heap, i, p);
			push_up_min(heap, p);
		} else {
			push_up_max(heap, i);
		}
	}
}

#scope_export

mmh_append :: (heap: *MMH($T, $larger_than), value: T) {
	Debug(true);
	index := heap.arr.count;
	array_add(*heap.arr, value);
	debugln("added % @ %", value, index);
	push_up(heap, cast(u64, index));
}

mmh_min :: inline (heap: MMH($T, $larger_than)) -> T {
	return heap.arr[0];
}

mmh_max :: inline (heap: MMH($T, $larger_than)) -> T {
	if heap.arr.count == 1 return heap.arr[0];
	return ifx heap.arr[1] > heap.arr[2] then heap.arr[1] else heap.arr[2];
}

main :: () {
	larger_than :: (a: u64, b: u64) -> bool { return a >= b; }
	heap : MMH(u64, larger_than);

	values :: u64.[46, 51, 31, 21, 13, 31, 10, 11, 16, 71, 41, 8];

	for v: values {
		print("adding % -----------------\n", v);
		mmh_append(*heap, v);
		for heap.arr print("%\n", it);
	}
}