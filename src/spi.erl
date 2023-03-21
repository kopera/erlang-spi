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
-opaque device() :: reference().

%% @equiv open(Path, #{})
-spec open(Path) -> {ok, device()} | {error, term()} when
    Path :: file:filename_all().
open(Path) ->
    open(Path, #{}).

%% @doc Open an SPI device given its full path. The path should normally look like
%% "/dev/spidevX.Y".
%%
%% Once a device is open, transfers can be done using {@link transfer/2}.
%% {@link read/2}, {@link read/3}, {@link write/2} and {@link write/3} are
%% convenience helpers implemented on top of the {@link transfer/2} function
%% that ignore the reveived data in case of `write' or write a continuous stream
%% of 0s in the case of `read'.
%%
%% The {@link device(). handle} to the SPI device is automatically closed when the
%% process that opened it terminates, it can also be closed explicitely
%% using {@link close/1}.
%%
%% Returns a {@link device(). handle} to communite with the SPI device in case of success.
-spec open(Path, Options) -> {ok, device()} | {error, term()} when
    Path :: file:filename_all(),
    Options :: #{
        mode => 0 | 1 | 2 | 3,
        speed_hz => pos_integer(),
        bits_per_word => 8 | 16
    }.
open(Path, Options) ->
    open_nif(unicode:characters_to_list(Path), maps:merge(#{
        mode => 0,
        speed_hz => 1000,
        bits_per_word => 8
    }, Options)).

%% nif
open_nif(_Filename, _Options) ->
    erlang:nif_error(not_loaded).

%% @doc Close a previously open SPI device.
-spec close(device()) -> ok | {error, term()}.
close(Device) ->
    close_nif(Device).

%% nif
close_nif(_Device) ->
    erlang:nif_error(not_loaded).

%% @doc Initiate an SPI transfer.
-spec transfer(device(), [transfer()]) -> {ok, [binary()]} | {error, term()}.
-type transfer() :: binary() | {binary(), transfer_options()}.
-type transfer_options() :: #{
    speed_hz => pos_integer(),
    bits_per_word => 8 | 16,
    delay_usecs => pos_integer(),
    cs_change => boolean()
}.
transfer(Device, Transfers) ->
    transfer_nif(Device, Transfers).

%% nif
transfer_nif(_Device, _Transfers) ->
    erlang:nif_error(not_loaded).

%% @equiv write(Device, Data, #{})
-spec write(device(), binary()) -> ok | {error, term()}.
write(Device, Data) ->
    write(Device, Data, #{}).

%% @doc Initiate a simple write transfer.
-spec write(device(), binary(), transfer_options()) -> ok | {error, term()}.
write(Device, Data, Options) ->
    case transfer(Device, [{Data, Options}]) of
        {ok, [Reply]} when byte_size(Reply) =:= byte_size(Reply) ->
            ok;
        {error, _} = Error ->
            Error
    end.

%% @equiv read(Device, Size, #{})
-spec read(device(), pos_integer()) -> {ok, binary()} | {error, term()}.
read(Device, Size) ->
    read(Device, Size, #{}).

%% @doc Initiate a simple read transfer.
-spec read(device(), pos_integer(), transfer_options()) -> {ok, binary()} | {error, term()}.
read(Device, Size, Options) ->
    case transfer(Device, [{<<0:(Size * 8)>>, Options}]) of
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
    filename:join([Dir, "spidev_nif"]);
nif_path(_, _Dir) ->
    undefined.
