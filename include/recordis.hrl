-author("yimo").

-define(Delimiter, <<":">>).

-record(redis_cmd, {
    cmd :: list(),
    formatter :: function() | {module(), function()}
}).

-record(recordis_callback, {
    before_new = [],
    after_new = [],
    before_update = [],
    after_update = [],
    before_delete = [],
    after_delete = []
}).

-record(recordis_where, {
    column :: atom(),
    expr = []
}).