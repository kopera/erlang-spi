# spi

An Erlang application for interfacing with SPI devices on Linux systems.

# Setup

You need to add `spi` as a dependency to your project. If you are using
`rebar3`, you can add the following to your `rebar.config`:

```erlang
{deps, [
    {spi, "0.5.0"}
]}.
```

Also ensure that `spi` is added as a dependency to your application, by
updating your `.app.src` file:

```erlang
{application, my_app, [

    {applications, [
        kernel,
        stdlib,

        spi  % <- You need this in your applications list
    ]}
]}.
```

# Usage

The following will open /dev/spidev0.0 using mode 0, at 10MHz using 8 bits per word:

```erlang
> {ok, Device} = spi:open("/dev/spidev0.0", #{
    mode => 0,
    speed_hz => 10000000,
    bits_per_word => 8
}).
{ok, #Ref<0.2893647232.3229876230.113792>}
```

Once the device is open you need to use `spi:transfer/2` to initiate a transfer.
Alternatively `spi:read/2`, `spi:read/3`, `spi:write/2` and `spi:write/3` can
also be used for simple transfers.