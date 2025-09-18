#module_parameters(PERF_MASTER_ENABLED := true);
#import "Basic";
#import "Hash_Table";
#import "Math";
#import,file "array.jai";

#scope_file

_perf_initialized := false;
_perf_data : Table(string, [..]float64);

#scope_export


Perf :: ($enabled: bool, $id: string = "") #expand {
	`PERF :: enabled && PERF_MASTER_ENABLED;
	#if id != "" `PERF_HEAD :: id;
	else         `PERF_HEAD :: #procedure_name();

	#if `PERF {
		`perf_times : [..]Apollo_Time;
		`defer array_free(perf_times);

		if !_perf_initialized {
			init(*_perf_data);
			_perf_initialized = true;
		}
	}
}

perf_start :: () #expand {
	#if `PERF {
		array_add(*`perf_times, current_time_monotonic());
	}
}

perf_end :: ($id: string = "") #expand {
	#if `PERF {
		now := current_time_monotonic();
		start := `perf_times[`perf_times.count-1];
		`perf_times.count -= 1;

		diff := to_float64_seconds(now-start);

		table_id := sprint("%-%", `PERF_HEAD, id);

		record := table_find_pointer(*_perf_data, table_id);
		if record == null {
			rec : [..]float64;
			record = table_add(*_perf_data, table_id, rec);
		} else {
			free(table_id);
		}
		array_add(record, diff);
	}
}

perf_report :: () -> string #expand {
	#if `PERF {
		builder : String_Builder;

		for value, key: _perf_data {

			tot : float64 = 0.0;
			avg : float64 = 0.0;
			err : float64 = 0.0;
			
			for value tot += it;
			avg = tot/value.count;

			for value err += (it - avg)*(it - avg);
			err = sqrt(err/(value.count-1));

			mi, ma := minmax(value);

			print_to_builder(*builder, "% : % datapoints\n", key, value.count);
			print_to_builder(*builder, "    total : % s\n", tot);
			print_to_builder(*builder, "    avg : % s +- % s\n", avg, err);
			print_to_builder(*builder, "    min : % s, max : % s\n", mi, ma);
		}

		return builder_to_string(*builder);
	} else return "Perf not activated";
}