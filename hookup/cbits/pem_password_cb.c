#include <stdlib.h>
#include <string.h>

struct CStringLen
{
    char const* ptr;
    int len;
};

void *
hookup_new_userdata(char const* ptr, int len)
{
    struct CStringLen *result = malloc(sizeof *result);

    // The null/error case is handled in hookup_pem_passwd_cb
    if (NULL != result) {
        result->ptr = ptr;
        result->len = len;
    }

    return result;
}

void
hookup_free_userdata(void *ud)
{
        free(ud);
}

int
hookup_pem_passwd_cb(char *buf, int size, int rwflag, void *userdata)
{
    struct CStringLen *password = userdata;

    // hookup_new_userdata failed, so we fail.
    if (NULL == password) { return -1; }

    // password requested when none was provided, so we fail.
    if (0 > password->len) { return -1; }

    // OpenSSL says to truncate the password if it's too long
    if (password->len < size) {
        size = password->len;
    }

    memcpy(buf, password->ptr, size);

    return size;
}
