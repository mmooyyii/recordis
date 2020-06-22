-author("yimo").


-record(redis_cmd, {
    cmd :: list(),
    transfer :: function() | {module(), function()}
}).