#include <stdbool.h>
#include <stdint.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>

#include <linux/spi/spidev.h>

#include <erl_nif.h>


typedef struct {
    int fd;
    bool open;
    ErlNifPid owner;
    ErlNifMonitor owner_monitor;
} spidev_data_t;

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_undefined;
static ERL_NIF_TERM am_true;

static ERL_NIF_TERM am_mode;
static ERL_NIF_TERM am_cs_change;
static ERL_NIF_TERM am_speed_hz;
static ERL_NIF_TERM am_delay_usecs;
static ERL_NIF_TERM am_bits_per_word;

static ErlNifResourceType* spidev_resource_type;


static char* posix_error_to_string(int error) {
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

static ERL_NIF_TERM posix_error_to_tuple(ErlNifEnv *env, int posix_errno)
{
    ERL_NIF_TERM error = enif_make_atom(env, posix_error_to_string(posix_errno));
    return enif_make_tuple2(env, am_error, error);
}

static bool cast_spi_mode(unsigned int value, uint8_t* mode)
{
    switch (value) {
        case 0:
            *mode = SPI_MODE_0;
            return true;
        case 1:
            *mode = SPI_MODE_1;
            return true;
        case 2:
            *mode = SPI_MODE_2;
            return true;
        case 3:
            *mode = SPI_MODE_3;
            return true;
        default:
            return false;
    }
}

static bool set_spi_mode(int fd, unsigned int value)
{
    uint8_t mode;
    return cast_spi_mode(value, &mode)
        && ioctl(fd, SPI_IOC_WR_MODE, &mode) == 0;
}

static bool cast_spi_bits_per_word(unsigned int value, uint8_t* bits_per_word)
{
    switch (value) {
        case 8:
            *bits_per_word = 8;
            return true;
        case 16:
            *bits_per_word = 16;
            return true;
        default:
            return false;
    }
}

static bool set_spi_bits_per_word(int fd, unsigned int value)
{
    uint8_t bits_per_word;
    return cast_spi_bits_per_word(value, &bits_per_word)
        && ioctl(fd, SPI_IOC_WR_BITS_PER_WORD, &bits_per_word) == 0;
}

static bool cast_spi_speed(unsigned int value, uint32_t* speed)
{
    *speed = (uint32_t) value;
    return true;
}

static bool set_spi_speed(int fd, unsigned int value)
{
    uint32_t speed;
    return cast_spi_speed(value, &speed)
        && ioctl(fd, SPI_IOC_WR_MAX_SPEED_HZ, &speed) == 0;
}

static bool cast_spi_delay(unsigned int value, uint32_t* delay)
{
    *delay = (uint32_t) value;
    return true;
}


static ERL_NIF_TERM open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char filename[4096];

    if (enif_get_string(env, argv[0], filename, sizeof(filename), ERL_NIF_LATIN1) <= 0
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
        if (!enif_get_uint(env, option, &mode) || !set_spi_mode(fd, mode)) {
            close(fd);
            return enif_make_badarg(env);
        }
    }

    if (enif_get_map_value(env, options, am_speed_hz, &option)) { // speed_hz
        unsigned int speed_hz;
        if (!enif_get_uint(env, option, &speed_hz) || !set_spi_speed(fd, speed_hz)) {
            close(fd);
            return enif_make_badarg(env);
        }
    }

    if (enif_get_map_value(env, options, am_bits_per_word, &option)) { // bits_per_word
        unsigned int bits_per_word;
        if (!enif_get_uint(env, option, &bits_per_word) || !set_spi_bits_per_word(fd, bits_per_word)) {
            close(fd);
            return enif_make_badarg(env);
        }
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    spidev_data_t* spidev = (spidev_data_t*) enif_alloc_resource(spidev_resource_type, sizeof(spidev_data_t));
    spidev->fd = fd;
    spidev->open = true;
    spidev->owner = owner;

    if (enif_monitor_process(env, spidev, &owner, &spidev->owner_monitor)) {
        enif_release_resource(spidev);
        close(fd);

        return posix_error_to_tuple(env, EINVAL);
    }

    ERL_NIF_TERM result = enif_make_resource(env, spidev);
    enif_release_resource(spidev);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM transfer_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    spidev_data_t* spidev;

    if (!enif_get_resource(env, argv[0], spidev_resource_type, (void**) &spidev)
            || !enif_is_list(env, argv[1])) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM transfer_list = argv[1];
    unsigned transfer_list_length = 0;
    enif_get_list_length(env, transfer_list, &transfer_list_length);

    struct spi_ioc_transfer transfer_buffer[transfer_list_length];
    memset(transfer_buffer, 0, transfer_list_length * sizeof(struct spi_ioc_transfer));

    ERL_NIF_TERM transfer;
    ERL_NIF_TERM rest;
    ERL_NIF_TERM rx_list = enif_make_list(env, 0);
    unsigned index = 0;
    while (enif_get_list_cell(env, transfer_list, &transfer, &rest)) {
        ErlNifBinary tx;
        ERL_NIF_TERM rx;

        if (enif_is_tuple(env, transfer)) {
            int arity;
            const ERL_NIF_TERM* fields;

            enif_get_tuple(env, transfer, &arity, &fields);
            if (arity == 2
                    && enif_inspect_binary(env, fields[0], &tx)
                    && enif_is_map(env, fields[1])) {

                unsigned char* rx_buffer = enif_make_new_binary(env, tx.size, &rx);
                transfer_buffer[index].tx_buf = (unsigned long)tx.data;
                transfer_buffer[index].rx_buf = (unsigned long)rx_buffer;
                transfer_buffer[index].len = tx.size;

                ERL_NIF_TERM option;
                if (enif_get_map_value(env, fields[1], am_cs_change, &option)) { // cs_change
                    transfer_buffer[index].cs_change = (enif_compare(option, am_true) == 0);
                }

                if (enif_get_map_value(env, fields[1], am_speed_hz, &option)) { // speed_hz
                    unsigned int value;
                    uint32_t speed_hz;
                    if (enif_get_uint(env, option, &value) && cast_spi_speed(value, &speed_hz)) {
                        transfer_buffer[index].speed_hz = speed_hz;
                    } else {
                        return enif_make_badarg(env);
                    }
                }

                if (enif_get_map_value(env, fields[1], am_delay_usecs, &option)) { // delay_usecs
                    unsigned int value;
                    uint32_t delay_usecs;
                    if (enif_get_uint(env, option, &value) && cast_spi_delay(value, &delay_usecs)) {
                        transfer_buffer[index].delay_usecs = delay_usecs;
                    } else {
                        return enif_make_badarg(env);
                    }
                }

                if (enif_get_map_value(env, fields[1], am_bits_per_word, &option)) { // bits_per_word
                    unsigned int value;
                    uint8_t bits_per_word;
                    if (enif_get_uint(env, option, &value) && cast_spi_bits_per_word(value, &bits_per_word)) {
                        transfer_buffer[index].bits_per_word = bits_per_word;
                    } else {
                        return enif_make_badarg(env);
                    }
                }

                rx_list = enif_make_list_cell(env, rx, rx_list);
            } else {
                return enif_make_badarg(env);
            }
        } else if (enif_inspect_binary(env, transfer, &tx)) {
            unsigned char* rx_buffer = enif_make_new_binary(env, tx.size, &rx);

            transfer_buffer[index].tx_buf = (unsigned long)tx.data;
            transfer_buffer[index].rx_buf = (unsigned long)rx_buffer;
            transfer_buffer[index].len = tx.size;

            rx_list = enif_make_list_cell(env, rx, rx_list);
        } else {
            return enif_make_badarg(env);
        }

        index++;
        transfer_list = rest;
    }

    if (ioctl(spidev->fd, SPI_IOC_MESSAGE(transfer_list_length), transfer_buffer) < 0) {
        return posix_error_to_tuple(env, errno);
    }

    ERL_NIF_TERM result;
    enif_make_reverse_list(env, rx_list, &result);
    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM close_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    spidev_data_t* spidev;

    if (!enif_get_resource(env, argv[0], spidev_resource_type, (void**) &spidev)) {
        return enif_make_badarg(env);
    }

    if (spidev->open) {
        if (enif_select(env, spidev->fd, ERL_NIF_SELECT_STOP, spidev, NULL, am_undefined) >= 0) {
            return am_ok;
        }
    }

    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = {
    {"open_nif", 2, open_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"transfer_nif", 2, transfer_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_nif", 1, close_nif}
};

static void on_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
    spidev_data_t* spidev = (spidev_data_t*) obj;

    if (spidev->open) {
        close(spidev->fd);
        spidev->open = false;
    }
}

static void on_owner_down(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    spidev_data_t* spidev = (spidev_data_t*) obj;

    if (spidev->open) {
        enif_select(env, spidev->fd, ERL_NIF_SELECT_STOP, spidev, NULL, am_undefined);
    }
}

static int on_load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_undefined = enif_make_atom(env, "undefined");
    am_true = enif_make_atom(env, "true");

    am_mode = enif_make_atom(env, "mode");
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
        &spidev_resource_type_callbacks, ERL_NIF_RT_CREATE, NULL);

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
    if (on_load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}

ERL_NIF_INIT(spi, nif_funcs, on_load, NULL, on_upgrade, on_unload);
