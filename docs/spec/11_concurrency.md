<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>11 — Concurrency</h1>
</div>

## 1. Overview

Kraken provides concurrency at two levels: **async/await** for cooperative concurrency and **OS threads** for true parallelism. Both models share synchronization primitives (mutexes, channels, atomics).

## 2. Async Functions

Functions declared with `async fn` return a future. They can use `await` to suspend until a sub-operation completes.

```kraken
async fn fetch_data(url: string) -> string {
    let response = await http_get(url);
    return response;
}
```

### Await

`await` suspends the current async function and yields control to the executor until the awaited future resolves.

```kraken
let data = await fetch_data("https://example.com");
```

`await` can only appear inside `async fn` bodies.

## 3. Spawn

`spawn` creates a new concurrent task:

```kraken
let handle = spawn {
    heavy_computation();
};
```

The spawned block runs concurrently. The returned handle can be joined to wait for completion.

## 4. Thread Primitives

OS-level threading via pthreads:

| Function | Description |
|----------|-------------|
| `thread_spawn(fn) -> ThreadHandle` | Spawn OS thread |
| `thread_join(handle) -> int` | Wait for thread completion |
| `thread_detach(handle)` | Detach thread |
| `sleep_ms(ms: int)` | Sleep current thread |

```kraken
fn worker() -> void {
    printf("running in thread\n");
}

fn main() -> int {
    let handle = thread_spawn(&worker);
    thread_join(handle);
    return 0;
}
```

### Function Pointers

The `&fn_name` syntax creates a C-callable function pointer for thread spawning:

```kraken
let fp = &my_function;
thread_spawn(fp);
```

## 5. Synchronization Primitives

### Mutex

```kraken
let m = mutex_create();
mutex_lock(m);
// critical section
mutex_unlock(m);
mutex_destroy(m);
```

### Condition Variables

```kraken
let cv = condvar_create();
condvar_wait(cv, mutex);     // wait (releases mutex, re-acquires on wake)
condvar_signal(cv);           // wake one waiter
condvar_broadcast(cv);        // wake all waiters
condvar_destroy(cv);
```

### Channels

Bounded channels for inter-thread communication:

```kraken
let ch = channel_create();
channel_send(ch, 42);         // blocking send
let val = channel_recv(ch);   // blocking receive
channel_close(ch);
```

Non-blocking variants:

```kraken
let ok = channel_try_send(ch, 42);   // returns bool
let val = channel_try_recv(ch);       // returns 0 if empty
```

### Atomics

Lock-free atomic operations:

```kraken
let a = atomic_new(0);
atomic_store(a, 42);
let v = atomic_load(a);
atomic_add(a, 1);
atomic_sub(a, 1);
let old = atomic_cas(a, 42, 100);   // compare-and-swap
```

## 6. Thread Pool

```kraken
let pool = pool_new(4);       // 4 worker threads
pool_spawn(pool, &task_fn);   // submit work
pool_shutdown(pool);           // graceful shutdown
```

## 7. Async Runtime

### Executor

```kraken
let exec = executor_new();
executor_spawn(exec, async_task);
executor_run(exec);
executor_shutdown(exec);
```

### Block On

Bridge from synchronous to asynchronous code:

```kraken
let result = block_on(async_operation());
```

### Cancellation

```kraken
let token = cancel_token_new();
cancel_token_cancel(token);
let cancelled = cancel_token_is_cancelled(token);
```

## 8. Thread Safety Markers

- **`Send`** — a type that can be safely transferred between threads.
- **`Sync`** — a type that can be safely shared (by reference) between threads.

These are marker traits used in generic bounds:

```kraken
fn send_to_thread<T>(value: T) -> void where T: Send {
    // value can be sent to another thread
}
```
