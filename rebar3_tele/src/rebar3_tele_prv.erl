-module(rebar3_tele_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, false},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 tele"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {namespace, tele},
            {short_desc, "A rebar plugin for compiling tele files"},
            {desc, "A rebar plugin for compiling tele files"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running tele...", []),
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    lists:foreach(fun compile_app/1, Apps),
    {ok, State}.

compile_app(AppInfo) ->
    Opts = rebar_app_info:opts(AppInfo),
    Dir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),

    filelib:ensure_dir(filename:join(EbinDir, "dummy.beam")),
    filelib:ensure_dir(filename:join(OutDir, "dummy.erl")),
    SrcPath = filename:join(OutDir, "src"),
    filelib:ensure_dir(filename:join(SrcPath, "dummy.erl")),

    TargetExt = ".erl",
    SourceExt = ".tl",
    rebar_base_compiler:run(Opts,
                            [],
                            Dir,
                            SourceExt,
                            OutDir,
                            TargetExt,
                            fun(S, T, _C) -> do_compile(S, T, Dir, SrcPath, EbinDir) end).


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_compile(Source, _Target, SourceDir, SrcPath, EbinDir) ->
    InputFile = filename:join(SourceDir, Source),
    Cmd = io_lib:format("tele ~s ~s", [InputFile, SrcPath]),
    Result = os:cmd(Cmd),
    OutputFile = filename:join(SrcPath, filename:rootname(filename:basename(InputFile))),
    case compile:file(OutputFile, {outdir, EbinDir}) of
       {ok, _ModuleName} ->
            ok;
        error ->
            error;
        {error, _Errors, _Warnings} ->
            error
    end.
