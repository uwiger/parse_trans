-module(make_plt).
-export([add/2]).



add(Apps, Plt) ->
    Dirs = [filename:join(code:lib_dir(App), "ebin") || App <- Apps],
    io:fwrite("generating custom plt file ~s~n", [Plt]),
    dialyzer:run([{output_plt, Plt},
                  {analysis_type, plt_add},
                  {from, byte_code},
                  {erlang_mode, true},
                  {files_rec, Dirs}]),
    io:fwrite("...done~n", []),
    init:stop().
