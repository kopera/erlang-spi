-module(spi).
-export([
    open/1,
    open/2,
    close/1,
    transfer/2,
    write/2,
    write/3,
    read/2,
    read/3
]).
-export_type([
    device/0
]).

-on_load(init/0).

-type device() :: reference().


-spec open(Filename) -> {ok, device()} | {error, term()} when
    Filename :: string().
open(Filename) ->
    open(Filename, #{}).


-spec open(Filename, Options) -> {ok, device()} | {error, term()} when
    Filename :: string(),
    Options :: #{
        mode => 0 | 1 | 2 | 3,
        speed_hz => pos_integer(),
        bits_per_word => 8 | 16
    }.
open(_Filename, _Options) ->
    {error, unsupported}.


-spec close(device()) -> ok | {error, term()}.
close(_Device) ->
    {error, unsupported}.


-spec transfer(device(), [transfer()]) -> {ok, [binary()]} | {error, term()}.
-type transfer() :: binary() | {binary(), transfer_options()}.
-type transfer_options() :: #{
    speed_hz => pos_integer(),
    bits_per_word => 8 | 16,
    delay_usecs => pos_integer(),
    cs_change => boolean()
}.
transfer(_Device, _Transfers) ->
    {error, unsupported}.


-spec write(device(), binary()) -> ok | {error, term()}.
write(Device, Data) ->
    write(Device, Data, #{}).


-spec write(device(), binary(), transfer_options()) -> ok | {error, term()}.
write(Device, Data, Options) ->
    case transfer(Device, [{Data, Options}]) of
        {ok, [Reply]} when byte_size(Reply) =:= byte_size(Reply) ->
            ok;
        {error, _} = Error ->
            Error
    end.


-spec read(device(), pos_integer()) -> {ok, binary()} | {error, term()}.
read(Device, Size) ->
    read(Device, Size).


-spec read(device(), pos_integer(), transfer_options()) -> {ok, binary()} | {error, term()}.
read(Device, Size, Options) ->
    case transfer(Device, [{<<0:Size/binary>>, Options}]) of
        {ok, [Reply]} when byte_size(Reply) =:= Size ->
            {ok, Reply};
        {error, _} = Error ->
            Error
    end.


init() ->
    case nif_path() of
        undefined ->
            ok;
        Path ->
            ok = erlang:load_nif(Path, 0)
    end.


-spec nif_path() -> string() | binary() | undefined.
nif_path() ->
    Priv = case code:priv_dir(spi) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                File when is_list(File) ->
                    filename:join([filename:dirname(File), "../priv"]);
                _ ->
                    "../priv"
            end;
        Dir ->
            Dir
    end,
    nif_path(os:type(), Priv).


nif_path({unix, linux}, Dir) ->
    filename:join([Dir, "port"]);

nif_path(_, _Dir) ->
    undefined.
