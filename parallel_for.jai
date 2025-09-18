#import "Basic";
#import "System";
#import "Thread";
#import "Pool";


// ########     ###    ########     ########  #######  ########
// ##     ##   ## ##   ##     ##    ##       ##     ## ##     ##
// ##     ##  ##   ##  ##     ##    ##       ##     ## ##     ##
// ########  ##     ## ########     ######   ##     ## ########
// ##        ######### ##   ##      ##       ##     ## ##   ##
// ##        ##     ## ##    ##     ##       ##     ## ##    ##
// ##        ##     ## ##     ##    ##        #######  ##     ##

#scope_file
ParallelForWork :: struct(T: Type, proc: (*T, int)->()) {
    data: *T;
    index: int;
}
#scope_export

parallel_for :: inline (elements: []$T, $proc: (*T)->(), $logging: bool = false) {
    p :: inline (x: *T, ix: int) {
        proc(x);
    }
    parallel_for(elements, p, logging);
}

parallel_for :: (elements: []$T, $proc: (*T, int) -> (), $logging: bool = false) {
    // initialize
    num_threads := get_number_of_processors();
    #if logging print("initializing % threads\n", num_threads);

    parallel_for_proc :: (group: *Thread_Group, thread: *Thread, work: *void) -> Thread_Continue_Status {
        wk := cast(*ParallelForWork(T, proc))work;
        wk.proc(wk.data, wk.index);
        return .CONTINUE;
    }

    thread_group: Thread_Group;
    init(*thread_group, num_threads, parallel_for_proc);
    thread_group.name = "parallel_for";
    thread_group.logging = false;
    #if logging print("thread group initialized\n");

    start(*thread_group);

    pool: Pool;
    set_allocators(*pool);
    allocator: Allocator;
    allocator.proc = pool_allocator_proc;
    allocator.data = *pool;

    #if logging print("adding work to thread group\n");
    for *elements {
        work := New(ParallelForWork(T, proc), false);
        work.data = it;
        work.index = it_index;
        add_work(*thread_group, work);
    }

    sleep_milliseconds(10);
    #if logging print("retrieving data\n");
    work_remaining := elements.count;
    while work_remaining > 0 {
        results := get_completed_work(*thread_group);
        #if logging print("retrieved % results\n", results.count);

        work_remaining -= results.count;

        sleep_milliseconds(10);
    }

    #if logging print("shutting down thread group\n");
    shutdown(*thread_group);
    #if logging print("releasing pool\n");
    release(*pool);
    #if logging print("Done!\n");
}


// ########     ###    ########     ########  #######  ########      ######   ##        #######  ########
// ##     ##   ## ##   ##     ##    ##       ##     ## ##     ##    ##    ##  ##       ##     ## ##     ##
// ##     ##  ##   ##  ##     ##    ##       ##     ## ##     ##    ##        ##       ##     ## ##     ##
// ########  ##     ## ########     ######   ##     ## ########     ##   #### ##       ##     ## ########
// ##        ######### ##   ##      ##       ##     ## ##   ##      ##    ##  ##       ##     ## ##     ##
// ##        ##     ## ##    ##     ##       ##     ## ##    ##     ##    ##  ##       ##     ## ##     ##
// ##        ##     ## ##     ##    ##        #######  ##     ##     ######   ########  #######  ########

#scope_file
ParallelForWorkGlobal :: struct(T: Type, G: Type, proc: (*T, int, *G)->()) {
    data: *T;
    index: int;
    global: *G;
}
#scope_export

parallel_for :: inline (elements: []$T, global: *$G, $proc: (*T, *G) -> (), $logging: bool = false) {
    p :: inline (x: *T, ix: int, g: *G) {
        proc(x,g);
    }
    parallel_for(elements, global, p, logging);
}



parallel_for :: (elements: []$T, global: *$G, $proc: (*T, int, *G) -> (), $logging: bool = false, $use_pool:=true) {
    // initialize
    num_threads := get_number_of_processors();
    #if logging print("initializing % threads\n", num_threads);
    #if logging && !use_pool print("NOT using pool allocator.\n");

    parallel_for_proc :: (group: *Thread_Group, thread: *Thread, work: *void) -> Thread_Continue_Status {
        wk := cast(*ParallelForWorkGlobal(T, G, proc))work;
        wk.proc(wk.data, wk.index, wk.global);
        return .CONTINUE;
    }

    thread_group: Thread_Group;
    init(*thread_group, num_threads, parallel_for_proc);
    thread_group.name = "parallel_for";
    thread_group.logging = false;
    #if logging print("thread group initialized\n");

    start(*thread_group);

    #if use_pool {
        pool: Pool;
        set_allocators(*pool);
        allocator: Allocator;
        allocator.proc = pool_allocator_proc;
        allocator.data = *pool;
    }

    #if logging print("adding work to thread group\n");
    for *elements {
        work := New(ParallelForWorkGlobal(T, G, proc), false);
        work.data = it;
        work.index = it_index;
        work.global = global;
        add_work(*thread_group, work);
    }

    sleep_milliseconds(10);
    #if logging print("retrieving data\n");
    work_remaining := elements.count;
    while work_remaining > 0 {
        results := get_completed_work(*thread_group);
        #if logging print("retrieved % results\n", results.count);

        work_remaining -= results.count;

        sleep_milliseconds(10);
    }

    #if logging print("shutting down thread group\n");
    shutdown(*thread_group);
    #if logging print("releasing pool\n");
    #if use_pool release(*pool);
    #if logging print("Done!\n");
}



// ########     ###    ########     ########  #######  ########      ######  ##     ## ##    ##
// ##     ##   ## ##   ##     ##    ##       ##     ## ##     ##    ##    ## ##     ## ##   ##
// ##     ##  ##   ##  ##     ##    ##       ##     ## ##     ##    ##       ##     ## ##  ##
// ########  ##     ## ########     ######   ##     ## ########     ##       ######### #####
// ##        ######### ##   ##      ##       ##     ## ##   ##      ##       ##     ## ##  ##
// ##        ##     ## ##    ##     ##       ##     ## ##    ##     ##    ## ##     ## ##   ##
// ##        ##     ## ##     ##    ##        #######  ##     ##     ######  ##     ## ##    ##

#scope_file
ParallelForChunkedWork :: struct(T: Type, proc: (*T, int)->()) {
    data: []T;
    index: int;
}
#scope_export

parallel_for_chunked :: inline (elements: []$T, chunksize: int, $proc: (*T)->(), $logging: bool = false) {
    p :: inline (x: *T, ix: int) {
        proc(x);
    }
    parallel_for_chunked(elements, chunksize, p, logging);
}

parallel_for_chunked :: (elements: []$T, chunksize: int, $proc: (*T, int) -> (), $logging: bool = false) {
    // initialize
    num_threads := get_number_of_processors();
    #if logging print("initializing % threads\n", num_threads);

    parallel_for_proc :: (group: *Thread_Group, thread: *Thread, work: *void) -> Thread_Continue_Status {
        wk := cast(*ParallelForChunkedWork(T, proc))work;
        // #if logging print("calculating chunk of size %\n", wk.data.count);
        for *wk.data {
            wk.proc(it, wk.index + it_index);
        }
        return .CONTINUE;
    }

    thread_group: Thread_Group;
    init(*thread_group, num_threads, parallel_for_proc);
    thread_group.name = "parallel_for";
    thread_group.logging = false;
    #if logging print("thread group initialized\n");

    start(*thread_group);

    pool: Pool;
    set_allocators(*pool);
    allocator: Allocator;
    allocator.proc = pool_allocator_proc;
    allocator.data = *pool;

    #if logging print("adding work to thread group\n");

    n_chunks := elements.count / chunksize;
    #if logging print("generating % chunks\n", n_chunks);
    for 0..n_chunks-1 {
        rest := ifx it == n_chunks-1 then elements.count % chunksize else chunksize;

        view := array_view(elements, it * chunksize, rest);
        work := New(ParallelForChunkedWork(T, proc), false);
        work.data = view;
        work.index = it * chunksize;
        // #if logging print("sending chunk %/% off to thread group\n", it, n_chunks);
        add_work(*thread_group, work);
    }

    sleep_milliseconds(10);
    #if logging print("retrieving data\n");
    work_remaining := n_chunks;
    while work_remaining > 0 {
        results := get_completed_work(*thread_group);
        #if logging print("retrieved % results\n", results.count);

        work_remaining -= results.count;

        sleep_milliseconds(10);
    }

    #if logging print("shutting down thread group\n");
    shutdown(*thread_group);
    #if logging print("releasing pool\n");
    release(*pool);
    #if logging print("Done!\n");
}

// ########     ###    ########     ########  #######  ########      ######  ##     ## ##    ##     ######   ##        #######  ########
// ##     ##   ## ##   ##     ##    ##       ##     ## ##     ##    ##    ## ##     ## ##   ##     ##    ##  ##       ##     ## ##     ##
// ##     ##  ##   ##  ##     ##    ##       ##     ## ##     ##    ##       ##     ## ##  ##      ##        ##       ##     ## ##     ##
// ########  ##     ## ########     ######   ##     ## ########     ##       ######### #####       ##   #### ##       ##     ## ########
// ##        ######### ##   ##      ##       ##     ## ##   ##      ##       ##     ## ##  ##      ##    ##  ##       ##     ## ##     ##
// ##        ##     ## ##    ##     ##       ##     ## ##    ##     ##    ## ##     ## ##   ##     ##    ##  ##       ##     ## ##     ##
// ##        ##     ## ##     ##    ##        #######  ##     ##     ######  ##     ## ##    ##     ######   ########  #######  ########


#scope_file
ParallelForChunkedWorkGlobal :: struct(T: Type, G: Type, proc: (*T, int, *G) -> ()) {
    data: []T;
    index: int;
    global: *G;
}
#scope_export

parallel_for_chunked :: inline (elements: []$T, global: *$G, chunksize: int, $proc: (*T, *G) -> (), $logging: bool = false) {
    p :: inline (x: *T, ix: int, g: *G) {
        proc(x,g);
    }
    parallel_for_chunked(elements, global, chunksize, p, logging);
}

parallel_for_chunked :: (elements: []$T, global: *$G, chunksize: int, $proc: (*T, int, *G) -> (), $logging: bool = false) {
    // initialize
    num_threads := get_number_of_processors();
    #if logging print("initializing % threads\n", num_threads);

    parallel_for_proc :: (group: *Thread_Group, thread: *Thread, work: *void) -> Thread_Continue_Status {
        wk := cast(*ParallelForChunkedWorkGlobal(T, G, proc))work;
        // #if logging print("calculating chunk of size %\n", wk.data.count);
        for *wk.data {
            wk.proc(it, wk.index + it_index, wk.global);
        }
        return .CONTINUE;
    }

    thread_group: Thread_Group;
    init(*thread_group, num_threads, parallel_for_proc);
    thread_group.name = "parallel_for";
    thread_group.logging = false;
    #if logging print("thread group initialized\n");

    start(*thread_group);

    pool: Pool;
    set_allocators(*pool);
    allocator: Allocator;
    allocator.proc = pool_allocator_proc;
    allocator.data = *pool;

    #if logging print("adding work to thread group\n");

    n_chunks := elements.count / chunksize;
    #if logging print("generating % chunks\n", n_chunks);
    for 0..n_chunks-1 {
        rest := ifx it == n_chunks-1 then elements.count % chunksize else chunksize;

        view := array_view(elements, it * chunksize, rest);
        work := New(ParallelForChunkedWorkGlobal(T, G, proc), false);
        work.data = view;
        work.global = global;
        work.index = it * chunksize;
        // #if logging print("sending chunk %/% off to thread group\n", it, n_chunks);
        add_work(*thread_group, work);
    }

    sleep_milliseconds(10);
    #if logging print("retrieving data\n");
    work_remaining := n_chunks;
    while work_remaining > 0 {
        results := get_completed_work(*thread_group);
        #if logging print("retrieved % results\n", results.count);

        work_remaining -= results.count;

        sleep_milliseconds(10);
    }

    #if logging print("shutting down thread group\n");
    shutdown(*thread_group);
    #if logging print("releasing pool\n");
    release(*pool);
    #if logging print("Done!\n");
}




// ########     ###    ########     ########  #######  ########     ########     ###    ##      ##
// ##     ##   ## ##   ##     ##    ##       ##     ## ##     ##    ##     ##   ## ##   ##  ##  ##
// ##     ##  ##   ##  ##     ##    ##       ##     ## ##     ##    ##     ##  ##   ##  ##  ##  ##
// ########  ##     ## ########     ######   ##     ## ########     ########  ##     ## ##  ##  ##
// ##        ######### ##   ##      ##       ##     ## ##   ##      ##   ##   ######### ##  ##  ##
// ##        ##     ## ##    ##     ##       ##     ## ##    ##     ##    ##  ##     ## ##  ##  ##
// ##        ##     ## ##     ##    ##        #######  ##     ##    ##     ## ##     ##  ###  ###



RawParallelForWork :: struct {
    data: *void;
    proc: (*void) -> ();
}


parallel_for_init :: ($logging:=false) -> *Thread_Group, Allocator {
    num_threads : s32 = get_number_of_processors(type=.ALL_LOGICAL);
    #if logging print("initializing % threads\n", num_threads);

    parallel_for_proc :: (group: *Thread_Group, thread: *Thread, work: *void) -> Thread_Continue_Status {
        wk := cast(*RawParallelForWork)work;
        wk.proc(wk.data);
        return .CONTINUE;
    }

    thread_group := New(Thread_Group);
    init(thread_group, num_threads, parallel_for_proc);
    thread_group.name = "parallel_for";
    thread_group.logging = false;
    #if logging print("thread group initialized\n");

    start(thread_group);

    pool := New(Pool);
    set_allocators(pool);
    allocator: Allocator;
    allocator.proc = pool_allocator_proc;
    allocator.data = pool;

    return thread_group, allocator;
}

parallel_for_raw :: (thread_group: *Thread_Group, pool_allocator: Allocator, $chunk_size: int, elements: []$T, global: *$G, $proc: (*T, int, *G) -> (), $logging:=false) {

    Work :: struct {
        data: []T;
        cursor: int;
        global: *G;
    }

    p :: (work: *void) {
        wk := cast(*Work)work;
        for *wk.data {
            proc(it, wk.cursor + it_index, wk.global);
        }
    }


    cursor := 0;
    n_chunks := 0;
    while cursor < elements.count {
        diff := elements.count - cursor;
        rest := ifx diff > chunk_size then chunk_size else diff;
        defer cursor += chunk_size;

        work_data := New(Work,, pool_allocator);
        work_data.data = array_view(elements, cursor, rest);
        work_data.cursor = cursor;
        work_data.global = global;
        
        work := New(RawParallelForWork,, pool_allocator);
        work.data = work_data;
        work.proc = p;
        
        add_work(thread_group, work);
        n_chunks += 1;
    }

    #if logging println("% chunks submitted to thread group.", n_chunks);

    // sleep_milliseconds(10);
    work_remaining := n_chunks;
    while work_remaining > 0 {
        results := get_completed_work(thread_group);
        work_remaining -= results.count;
        // sleep_milliseconds(10);
    }

    reset(cast(*Pool)pool_allocator.data);
}

parallel_for_raw :: (thread_group: *Thread_Group, pool_allocator: Allocator, $chunk_size: int, elements: []$T, $proc: (*T, int) -> (), $logging:=false) {

    Work :: struct {
        data: []T;
        cursor: int;
    }

    p :: (work: *void) {
        wk := cast(*Work)work;
        for *wk.data {
            proc(it, wk.cursor + it_index);
        }
    }


    cursor := 0;
    n_chunks := 0;
    while cursor < elements.count {
        diff := elements.count - cursor;
        rest := ifx diff > chunk_size then chunk_size else diff;
        defer cursor += chunk_size;

        work_data := New(Work,,pool_allocator);
        work_data.data = array_view(elements, cursor, rest);
        work_data.cursor = cursor;
        
        work := New(RawParallelForWork,,pool_allocator);
        work.data = work_data; 
        work.proc = p;
        
        add_work(thread_group, work);
        n_chunks += 1;
    }

    #if logging println("% chunks submitted to thread group.", n_chunks);

    // sleep_milliseconds(10);
    work_remaining := n_chunks;
    while work_remaining > 0 {
        results := get_completed_work(thread_group);
        work_remaining -= results.count;
        // sleep_milliseconds(10);
    }

    reset(cast(*Pool)pool_allocator.data);
}

parallel_for_stop :: (thread_group: *Thread_Group, pool_allocator: Allocator) {
    shutdown(thread_group);
    release(cast(*Pool)pool_allocator.data);
}