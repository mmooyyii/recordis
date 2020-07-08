-author("yimo").


-record(redis_cmd, {
    cmd :: list(),
    formatter :: function() | {module(), function()}
}).