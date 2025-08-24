
#import "Basic";
#import "Math";
#import "Random";
#import,file "pathfinding.jai";


DIAGONALS :: false;
N :: 32;
map : [N][N]float = ---;

rc :: (index: int) -> int, int {
	c := index % N;
	r := (index - c) / N;
	return r, c;
}

index :: (r: int, c: int) -> int #expand {
	return r * N + c;
}

weight :: (from: int, to: int) -> float {
	f :: #run sqrt(2.0);
	fr, fc := rc(from);
	tr, tc := rc(to);

	#if DIAGONALS
		c := ifx fr != tr && fc != tc then f else 1.0;
	else 
		c :: 1.0;

	return 0.5 * (map[fr][fc] + map[tr][tc]) * c;
	// return map[tr][tc];
}

neighbors :: (node: int) -> []int {
	res : [..]int;
	r, c := rc(node);

	if r > 0   if map[r-1][c  ] != FLOAT32_INFINITY array_add(*res, index(r-1, c  ));
	if c > 0   if map[r  ][c-1] != FLOAT32_INFINITY array_add(*res, index(r  , c-1));
	if c < N-1 if map[r  ][c+1] != FLOAT32_INFINITY array_add(*res, index(r  , c+1));
	if r < N-1 if map[r+1][c  ] != FLOAT32_INFINITY array_add(*res, index(r+1, c  ));
	#if DIAGONALS {
		if r > 0 && c > 0     if map[r-1][c-1] != FLOAT32_INFINITY array_add(*res, index(r-1, c-1));
		if r > 0 && c < N-1   if map[r-1][c+1] != FLOAT32_INFINITY array_add(*res, index(r-1, c+1));
		if r < N-1 && c > 0   if map[r+1][c-1] != FLOAT32_INFINITY array_add(*res, index(r+1, c-1));
		if r < N-1 && c < N-1 if map[r+1][c+1] != FLOAT32_INFINITY array_add(*res, index(r+1, c+1));
	}

	return res;
}

heuristic :: (from: int, to: int) -> float {
	fr, fc := rc(from);
	tr, tc := rc(to);
	dx := cast(float, tc - fc);
	dy := cast(float, tr - fr);
	return sqrt(dx*dx + dy*dy);
}

main :: () {
	time := current_time_monotonic();
	random_seed(cast,force(Some_Big_Time_Value)time);

	for r: 0..N-1 for c: 0..N-1 {
		map[r][c] = ifx random_get_zero_to_one() < 0.3 then FLOAT32_INFINITY else 1.0;
	}


	path := a_star(0, N*N-1, heuristic, weight, neighbors);


	builder: String_Builder;
	for r: 0..N-1 {
		for c: 0..N-1 {
			found, where := array_find(path, index(r,c));
			if found {
				fmt := formatInt(where, minimum_digits=2);
				print_to_builder(*builder, "%", fmt);
			} else {
				if map[r][c] == FLOAT32_INFINITY append(*builder, "██");
				else                             append(*builder, "  ");
			}
		}
		append(*builder, "\n");
	}
	print("%", builder_to_string(*builder));
}