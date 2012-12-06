-define(ERROR(Message),
    io:format("ERROR:~s:Mod-~w:Line-~w~n", [Message, ?MODULE, ?LINE])).
-define(INFO(Message),
    io:format("INFO :~s:Mod>~w:Line>~w~n", [Message, ?MODULE, ?LINE])).
-define(DEBUG(Message),
    io:format("DEBUG:~s:Mod>~w:Line>~w~n", [Message, ?MODULE, ?LINE])).
