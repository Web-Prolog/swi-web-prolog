# Timeout handling
# Monitor				[OK]
# Implement more options to spawn/3 
  - [X] src_predicates [OK]
  - [X] src_list [OK]
  
# Handle syntax errors in src_* predicates

# Cleaning up after actor termination
  
# Link
  - What about running children?
  - What about propagating?

# Crash in engine_destroy:
  - Create engine
  - catch(thread_signal(Pid, abort), _, true),
  - engine_destroy(Pid).

# Engine aliases are not alias:
  - engine_create(x, true, Id, [alias(xx)]),
    engine_next(xx, N).
    --> Engine is not current, but recreating fails as the alias
    is not deleted.

