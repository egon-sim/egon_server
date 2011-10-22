-module(es_releasing).
-export([create_release/0]).

create_release() ->
    create_release("egon_server").

create_release(Name) ->
    code:add_patha("./ebin"),
    systools:make_script(Name, []),
    delete(Name ++ ".tar.gz"),
    systools:make_tar(Name, [{erts, code:root_dir()}]),
    delete("tmp_release"),
    erl_tar:extract(Name ++ ".tar.gz", [{cwd, "tmp_release"}, compressed]).

delete(File) ->
    ok.