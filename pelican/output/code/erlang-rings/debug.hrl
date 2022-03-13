-ifdef(debug).
-define(DEBUG(Format, Args),
        io:format("~p: " ++ Format, [self()] ++ Args)).
-else.
-define(DEBUG(Format, Args), true).
-endif.
