#module_parameters(RELEASE := false);

#import "Basic";
#import,file "debug.jai";


/*
https://en.wikipedia.org/wiki/Min-max_heap 
*/

#scope_export

MMH :: struct(T: Type, larger_than: (a: T, b: T, data: $D) -> bool) {
	items: [..]T;
	data: D;
}

operator *[] :: (heap: *MMH, index: u64) -> *heap.T {
	Check(index);
    return *heap.items[index];
}


#scope_file

Check :: (i: u64) #expand {
	#if !RELEASE assert(cast(u64, `heap.items.count) > i, "index % out of range < %.", i, `heap.items.count);
}


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

swap :: inline (i: u64, j: u64) #expand {
	Debug(false && !RELEASE);
	#if !RELEASE heap := `heap;
	Check(i); Check(j);
	debugln("swapping % & %", i, j);
	
	tmp := `heap[i];
	`heap[i] = `heap[j];
	`heap[j] = tmp;
	
	#if DEBUG {
		debugln("after swapping:");
		for `heap.items debugln("%: %", it_index, it);
	}
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

has_children :: inline (heap: MMH, i: u64) -> bool #expand {
	Check(i);
	n := cast(u64)heap.items.count;
	return right(i) < n || left(i) < n;
}


NO_CHILDREN : u64 : 0xFFFFFFFFFFFFFFFF;
smallest_child_or_grandchild :: (heap: MMH($T, $larger_than), i: u64) -> u64, bool {
	Check(i);
	index := NO_CHILDREN;
	is_child := true;
	n := cast(u64)heap.items.count;

	l := left(i);
	if l < n {
		index = l;

		ll :=  left(l);
		lr := right(l);

		if ll < n && !larger_than(heap[ll], heap[index], heap.data) then { index = ll; is_child = false; }
		if lr < n && !larger_than(heap[lr], heap[index], heap.data) then { index = lr; is_child = false; }
	}
	r := right(i);
	if r < n {
		if !larger_than(heap[r], heap[index], heap.data) then { index = r; is_child = true; }
		rl :=  left(r);
		rr := right(r);
		if rl < n && !larger_than(heap[rl], heap[index], heap.data) then { index = rl; is_child = false; }
		if rr < n && !larger_than(heap[rr], heap[index], heap.data) then { index = rr; is_child = false; }
	}
	return index, is_child;
}

largest_child_or_grandchild :: (heap: MMH($T, $larger_than), i: u64) -> u64, bool {
	Debug(false && !RELEASE);
	Check(i);
	index := NO_CHILDREN;
	is_child := true;
	n := cast(u64)heap.items.count;

	l := left(i);
	debugln("l: % @ %", heap[l], l);
	if l < n {
		index = l;

		ll :=  left(l);
		lr := right(l);
		debugln("ll: % @ %, lr: % @ %", heap[ll], ll, heap[lr], lr);

		if ll < n && larger_than(heap[ll], heap[index], heap.data) then { index = ll; is_child = false; }
		if lr < n && larger_than(heap[lr], heap[index], heap.data) then { index = lr; is_child = false; }
	}
	r := right(i);
	debugln("r: % @ %", heap[r], r);
	if r < n {
		if larger_than(heap[r], heap[index], heap.data) then { index = r; is_child = true; }
		rl :=  left(r);
		rr := right(r);
		debugln("rl: % @ %, rr: % @ %", heap[rl], rl, heap[rr], rr);
		if rl < n && larger_than(heap[rl], heap[index], heap.data) then { index = rl; is_child = false; }
		if rr < n && larger_than(heap[rr], heap[index], heap.data) then { index = rr; is_child = false; }
	}
	return index, is_child;
}

push_down :: (heap: MMH($T, $larger_than), index: u64) {
	Debug(false && !RELEASE);
	Check(index);
	m := index;

	while has_children(heap, m) {
		debugln("% has children", m);
		i := m;
		if is_min_level(i) {
			debugln("% is min level", i);
			_m, is_child := smallest_child_or_grandchild(heap, i);
			m = _m;
			debugln("smallest child or grandchild: % @ %, is child? %", heap[m], m, is_child);
			if !larger_than(heap[m], heap[i], heap.data) {
				debugln("% @ % is smaller than % @ %", heap[m], m, heap[i], i);
				swap(m, i);
				if !is_child { 
					if larger_than(heap[m], heap[parent(m)], heap.data) then swap(m, parent(m));
				} else break;
			} else break;
		} else {
			debugln("% is max level", i);
			_m, is_child := largest_child_or_grandchild(heap, i);
			m = _m;
			debugln("largest child or grandchild: % @ %, is child? %", heap[m], m, is_child);
			if larger_than(heap[m], heap[i], heap.data) {
				debugln("% @ % is larger than % @ %", heap[m], m, heap[i], i);
				swap(m, i);
				if !is_child {
					if !larger_than(heap[m], heap[parent(m)], heap.data) then swap(m, parent(m));
				} else break;
			} else break;
		}
	}
}


push_up_min :: (heap: MMH($T, $larger_than), index: u64) {
	Debug(false && !RELEASE);
	Check(index);
	i := index;
	gp := parent(parent(i));
	debugln("gp = %", gp);
	while gp != NO_PARENT && !larger_than(heap[i], heap[gp], heap.data) {
		swap(i, gp);
		i = gp;
		gp = parent(parent(i));
	}
}
push_up_max :: (heap: MMH($T, $larger_than), index: u64) {
	Debug(false && !RELEASE);
	Check(index);
	i := index;
	gp := parent(parent(i));
	debugln("gp = %", gp);
	while gp != NO_PARENT && larger_than(heap[i], heap[gp], heap.data) {
		debugln("% @ % > % @ %, so swap", heap[i], i, heap[gp], gp);
		swap(i, gp);
		i = gp;
		gp = parent(parent(i));
	}
}

push_up :: (heap: MMH($T, $larger_than), i: u64) {
	Debug(false && !RELEASE);
	Check(i);
	debugln("i = %", i);
	if i == 0 return;

	if is_min_level(i) {
		debugln("is min level");
		p := parent(i);
		if larger_than(heap[i], heap[p], heap.data) {
			swap(i, p);
			push_up_max(heap, p);
		} else {
			push_up_min(heap, i);
		}
	} else {
		debugln("is max level");
		p := parent(i);
		if !larger_than(heap[i], heap[p], heap.data) {
			swap(i, p);
			push_up_min(heap, p);
		} else {
			push_up_max(heap, i);
		}
	}
}

#scope_export

mmh_free :: (heap: *MMH) {
	array_free(heap.items);
}

mmh_reset_keeping_memory :: (heap: *MMH) {
	array_reset_keeping_memory(*heap.items);
}

append :: (heap: *MMH($T, $larger_than), value: T) {
	Debug(false && !RELEASE);
	index := heap.items.count;
	array_add(*heap.items, value);
	debugln("added % @ %", value, index);
	push_up(heap, cast(u64, index));
}

min :: inline (heap: MMH($T, $larger_than)) -> T {
	#if !RELEASE assert(heap.items.count > 0, "heap empty.");
	return heap.items[0];
}

max :: inline (heap: MMH($T, $larger_than)) -> T {
	#if !RELEASE assert(heap.items.count > 0, "heap empty.");
	if heap.items.count == 1 return heap.items[0];
	return ifx larger_than(heap.items[1], heap.items[2], heap.data) then heap.items[1] else heap.items[2];
}

remove_at :: (heap_p: *MMH($T, $larger_than), index: u64) -> T {
	#if !RELEASE assert(cast(u64)heap_p.items.count > index, "cannot remove at index % when heap has size %.", index, heap_p.items.count);
	heap := heap_p.*;
	
	res := heap[index];
	heap[index] = heap[cast(u64, heap.items.count - 1)];
	heap_p.items.count -= 1;  // keep memory
	push_down(heap, index);
	return res;
}

pop_min :: (heap_p: *MMH($T, $larger_than)) -> T {
	#if !RELEASE assert(heap_p.items.count > 0, "cannot pop from empty heap.");
	return inline remove_at(heap_p, 0);
}

pop_max :: (heap_p: *MMH($T, $larger_than)) -> T {
	Debug(false && !RELEASE);
	#if !RELEASE assert(heap_p.items.count > 0, "cannot pop from empty heap.");
	heap := heap_p.*;
	n := heap_p.items.count;

	if n == 1 {
		res := heap[0];
		heap_p.items.count -= 1;
		debugln("max % @ 0", res);
		return res;
	}
	if n == 2 {
		res := heap[1];
		heap_p.items.count -=1 ;
		debugln("max % @ 1", res);
		return res;
	}

	i := cast(u64)( ifx larger_than(heap[1], heap[2], heap.data) then 1 else 2 );
	return inline remove_at(heap_p, i);
}