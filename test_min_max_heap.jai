#import "Basic";
#import,file "min_max_heap.jai"(RELEASE=false);


main :: () {
	larger_than :: (a: u64, b: u64, data: *void) -> bool { return a >= b; }
	heap : MMH(u64, larger_than);

	values :: u64.[46, 51, 31, 21, 13, 31, 10, 11, 16, 71, 41, 8];

	for v: values {
		print("adding % -----------------\n", v);
		append(*heap, v);
		for heap.items print("%\n", it);
	}

	print("min = %\n", min(heap));
	print("max = %\n", max(heap));

	print("removed min = %\n", pop_min(*heap));
	for heap.items print("%\n", it);
	
	print("removed max = %\n", pop_max(*heap));
	for heap.items print("%\n", it);

	print("min = %\n", min(heap));
	print("max = %\n", max(heap));
}