#import "Basic";
#import,file "perf.jai";

main :: () {
	Perf(true);

	perf_start();
	perf_end("end");

	print("%\n", perf_report());
}