#include <stdbool.h>
#include <stdint.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <limits.h>
#include <errno.h>

#include <linux/spi/spidev.h>

#include <erl_nif.h>


typedef struct {
    int fd;
    bool closed;
    ErlNifPid owner;
    ErlNifMonitor owner_monitor;
} spidev_data_t;

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_undefined;

static ERL_NIF_TERM am_cs_change;
static ERL_NIF_TERM am_speed_hz;
static ERL_NIF_TERM am_delay_usecs;
static ERL_NIF_TERM am_bits_per_word;

static ErlNifResourceType* spidev_resource_type;


static char* posix_error_to_string(posix_errno_t error) {
    switch (error) {
        case EACCES: return "eacces";
        case EINVAL: return "einval";
        case EISDIR: return "eisdir";
        case EMFILE: return "emfile";
        case ENFILE: return "enfile";
        case ENOENT: return "enoent";
        case ENOTDIR: return "enotdir";
        default: return "unknown";
    }
}

static ERL_NIF_TERM posix_error_to_tuple(ErlNifEnv *env, posix_errno_t posix_errno) {
    ERL_NIF_TERM error = enif_make_atom(env, posix_error_to_string(posix_errno));
    return enif_make_tuple2(env, am_error, error);
}

static uint8_t cast_spi_mode(uint value) {
    switch (value) {
        case 0:
        default:
            return SPI_MODE_0;
        case 1:
            return SPI_MODE_1;
        case 2:
            return SPI_MODE_2;
        case 3:
            return SPI_MODE_3;
    }
}

static int set_spi_mode(int fd, uint value) {
    uint8_t mode = cast_spi_mode(value);
    return ioctl(fd, SPI_IOC_WR_MODE, &mode);
}

static uint8_t cast_spi_bits_per_word(uint value) {
    switch (value) {
        case 8:
        default:
            return 8;
        case 16:
            return 16;
    }
}

static int set_spi_bits_per_word(int fd, uint value) {
    uint8_t bits_per_word = cast_spi_bits_per_word(value);
    return ioctl(fd, SPI_IOC_WR_BITS_PER_WORD, &bits_per_word);
}

static uint32_t cast_spi_speed(uint value) {
    return (uint32_t) value;
}

static int set_spi_speed(int fd, uint value) {
    uint32_t speed = cast_spi_speed(value);
    return ioctl(fd, SPI_IOC_WR_MAX_SPEED_HZ, &speed);
}

static uint32_t cast_spi_delay(uint value) {
    return (uint32_t) value;
}


static ERL_NIF_TERM open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char filename[PATH_MAX];

    if (!enif_get_string(env, argv[0], filename, sizeof(filename), ERL_NIF_LATIN1)
        || !enif_is_map(env, argv[1])) {
        return enif_make_badarg(env);
    }

    int fd;
    do {
        fd = open(filename, O_RDWR);
    } while(fd == -1 && errno == EINTR);

    if (fd < 0) {
        return posix_error_to_tuple(env, errno);
    }

    ERL_NIF_TERM options = argv[1];
    ERL_NIF_TERM option;
    if (enif_get_map_value(env, options, am_mode, &option)) { // mode
        unsigned int mode;
        if (!enif_get_uint(option, &mode) || set_spi_mode(fd, mode) < 0) {
            close(fd);
            return enif_make_badarg(env);
        }
    } else if (enif_get_map_value(env, options, am_speed_hz, &option)) { // speed_hz
        unsigned int speed_hz;
        if (!enif_get_uint(option, &speed_hz) || set_spi_speed(fd, speed_hz) < 0) {
            close(fd);
            return enif_make_badarg(env);
        }
    } else if (enif_get_map_value(env, options, am_bits_per_word, &option) { // bits_per_word
        unsigned int bits_per_word;
        if (!enif_get_uint(option, &bits_per_word) || set_spi_bits_per_word(bits_per_word) < 0) {
            close(fd);
            return enif_make_badarg(env);
        }
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    spidev_data_t* d = (spidev_data_t*) enif_alloc_resource(spidev_resource_type, sizeof(spidev_data_t));
    d->fd = fd;
    d->closed = false;
    d->owner = owner;

    if (enif_monitor_process(env, d, &owner, &d->owner_monitor)) {
        close(fd);

        return posix_error_to_tuple(env, EINVAL);
    }

    result = enif_make_resource(env, d);
    enif_release_resource(result);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM transfer_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    spidev_data_t* d;

    if (!enif_get_resource(env, argv[0], spidev_resource_type, (void**) &d)
            || !enif_is_list(env, argv[1])) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM transfer_list = argv[1];
    unsigned transfer_list_length = 0;
    enif_get_list_length(env, transfer_list, &transfer_list_length);

    struct spi_ioc_transfer transfer_buffer[transfer_list_length] = {0};

    ERL_NIF_TERM transfer;
    ERL_NIF_TERM rest;
    ERL_NIF_TERM rx_list = enif_make_list(env, 0);
    unsigned index = 0;
    while (enif_get_list_cell(env, transfer_list, &transfer, &rest)) {
        ErlNifBinary tx;
        ErlNifBinary rx;

        if (enif_is_tuple(env, transfer)) {
            int arity;
            ERL_NIF_TERM fields[];

            enif_get_tuple(env, transfer, &arity, &fields);
            if (arity == 2
                    && enif_inspect_binary(env, fields[0], &tx)
                    && enif_is_map(env, fields[1])) {

                unsigned char* rx_buffer = enif_make_new_binary(env, tx->size, &rx);
                transfer_buffer[index].tx_buf = (unsigned long)tx->data;
                transfer_buffer[index].rx_buf = (unsigned long)rx_buffer;
                transfer_buffer[index].len = tx->size;

                ERL_NIF_TERM option;
                if (enif_get_map_value(env, fields[1], am_cs_change, &option)) { // cs_change
                    transfer_buffer[index].cs_change = (enif_compare(option, am_true) == 0);
                } else if (enif_get_map_value(env, fields[1], am_speed_hz, &option)) { // speed_hz
                    unsigned int speed_hz;
                    if (enif_get_uint(option, &speed_hz)) {
                        transfer_buffer[index].speed_hz = cast_spi_speed(speed_hz);
                    } else {
                        return enif_make_badarg(env);
                    }
                } else if (enif_get_map_value(env, fields[1], am_delay_usecs, &option) { // delay_usecs
                    unsigned int delay_usecs;
                    if (enif_get_uint(option, &delay_usecs)) {
                        transfer_buffer[index].delay_usecs = cast_spi_delay(delay_usecs);
                    } else {
                        return enif_make_badarg(env);
                    }
                } else if (enif_get_map_value(env, fields[1], am_bits_per_word, &option) { // bits_per_word
                    unsigned int bits_per_word;
                    if (enif_get_uint(option, &bits_per_word)) {
                        transfer_buffer[index].bits_per_word = cast_spi_bits_per_word(bits_per_word);
                    } else {
                        return enif_make_badarg(env);
                    }
                }

                rx_list = enif_make_list_cell(env, rx, rx_list);
            } else {
                return enif_make_badarg(env);
            }
        } else if (enif_inspect_binary(env, transfer, &tx)) {
            unsigned char* rx_buffer = enif_make_new_binary(env, tx->size, &rx);

            transfer_buffer[index].tx_buf = (unsigned long)tx->data;
            transfer_buffer[index].rx_buf = (unsigned long)rx_buffer;
            transfer_buffer[index].len = tx->size;

            rx_list = enif_make_list_cell(env, rx, rx_list);
        } else {
            return enif_make_badarg(env);
        }

        index++;
        transfer_list = rest;
    }

    if (ioctl(d->fd, SPI_IOC_MESSAGE(transfer_list_length), transfer_buffer) < 0) {
        return posix_error_to_tuple(env, errno);
    }

    return enif_make_tuple2(env, am_ok, rx_list);
}

static ERL_NIF_TERM close_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    spidev_data_t* d;

    if (!enif_get_resource(env, argv[0], spidev_resource_type, (void**) &d)) {
        return enif_make_badarg(env);
    }

    if (!d->closed) {
        if (enif_select(env, d->fd, ERL_NIF_SELECT_STOP, d, NULL, am_undefined) >= 0) {
            return am_ok;
        }
    }

    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = {
    {"open", 2, open_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"transfer", 2, transfer_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close", 1, close_nif, ERL_NIF_DIRTY_JOB_IO_BOUND}
};

static void on_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
    spidev_data_t* d = (spidev_data_t*) obj;

    if (d->open) {
        close(d->fd);
        d->open = false;
    }
}

static void on_owner_down(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    spidev_data_t* d = (spidev_data_t*) obj;

    if (d->open) {
        enif_select(env, d->fd, ERL_NIF_SELECT_STOP, d, NULL, am_undefined);
    }
}

static int on_load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceTypeInit callbacks;

    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_undefined = enif_make_atom(env, "undefined");

    am_cs_change = enif_make_atom(env, "cs_change");
    am_speed_hz = enif_make_atom(env, "speed_hz");
    am_delay_usecs = enif_make_atom(env, "delay_usecs");
    am_bits_per_word = enif_make_atom(env, "bits_per_word");

    ErlNifResourceTypeInit spidev_resource_type_callbacks = {
        .dtor = NULL,
        .stop = on_stop,
        .down = on_owner_down,
    };
    spidev_resource_type = enif_open_resource_type_x(env, "spidev",
        spidev_resource_type_callbacks, ERL_NIF_RT_CREATE, NULL);

    *priv_data = NULL;

    return 0;
}

static void on_unload(ErlNifEnv *env, void* priv_data)
{

}

static int on_upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}

ERL_NIF_INIT(spidev, nif_funcs, on_load, NULL, on_upgrade, on_unload);
