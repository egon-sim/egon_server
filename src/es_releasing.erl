-module(es_releasing).
-export([create_release/0, clear_files/0]).

create_release() ->
    create_release("release_files", "egon_server", "egon_release").

create_release(Release_dir, Rel_file_name, Release_name) ->
    Full_name = Release_dir ++ "/" ++ Rel_file_name,
    Full_release_name = Release_dir ++ "/" ++ Release_name,
    code:add_patha("./ebin"),
    clear_files(Release_dir, Rel_file_name, Release_name),
    systools:make_script(Full_name, []),
    systools:make_tar(Full_name, [{erts, code:root_dir()}]),
    erl_tar:extract(Full_name ++ ".tar.gz", [{cwd, Full_release_name}, compressed]),
    generate_ini(Release_dir, Release_name),
    generate_install(Release_dir, Release_name),
    generate_start(Release_dir, Rel_file_name, Release_name),
    copy_files(Release_dir, Rel_file_name, Release_name),
    cleanup(Release_dir, Rel_file_name, Release_name),
    ok.

generate_ini(Release_dir, Release_name) ->
    {ok, Path} = file:get_cwd(),
    Filename = Release_dir ++ "/" ++ Release_name ++ "/erl.ini",
    Content = "[erlang]\nBindir=" ++ Path ++ "/" ++ Release_dir ++ "/" ++ Release_name ++ "/erts-5.6.4/bin\nProgname=erl\nRootdir=" ++ Path ++ "/" ++ Release_dir ++ "/" ++ Release_name,
    file:write_file(Filename, Content).
    
generate_install(Release_dir, Release_name) ->
    Filename = Release_dir ++ "/" ++ Release_name ++ "/install.bat",
    Content = "move erl.ini erts-5.6.4/bin/\n",
    file:write_file(Filename, Content).    

generate_start(Release_dir, Rel_file_name, Release_name) ->
    Filename = Release_dir ++ "/" ++ Release_name ++ "/start.bat",
    Content = "erts-5.6.4\\bin\\erl -sname " ++ Rel_file_name ++ " -boot " ++ Rel_file_name ++"\n",
    file:write_file(Filename, Content).    

clear_files() ->
    clear_files("release_files", "egon_server", "egon_release").

clear_files(Release_dir, Rel_file_name, Release_name) ->
    Full_name = Release_dir ++ "/" ++ Rel_file_name,
    Full_release_name = Release_dir ++ "/" ++ Release_name,
    delete(Full_name ++ ".boot"),
    delete(Full_name ++ ".script"),
    delete(Full_name ++ ".tar.gz"),
    delete(Full_release_name).

copy_files(Release_dir, Rel_file_name, Release_name) ->
    Full_name = Release_dir ++ "/" ++ Rel_file_name,
    Full_release_name = Release_dir ++ "/" ++ Release_name,
    file:rename(Full_name ++ ".boot", Full_release_name ++ "/" ++ Rel_file_name ++ ".boot").

cleanup(Release_dir, Rel_file_name, _Release_name) ->
    Full_name = Release_dir ++ "/" ++ Rel_file_name,
    delete(Full_name ++ ".tar.gz"),
    delete(Full_name ++ ".script").

delete(File) ->
    Is_dir = filelib:is_dir(File),
    Is_file = filelib:is_regular(File),
    if
       Is_dir  ->
    	    {ok, Files} = file:list_dir(File),
    	    lists:map(fun(F) -> delete(File ++ "/" ++ F) end, Files),
    	    file:del_dir(File);
       Is_file  ->
	    file:delete(File);
       true ->
	    error
    end.
