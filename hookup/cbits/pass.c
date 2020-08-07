#include <string.h>

int hookup_pem_passwd_cb(char *buf, int size, int rwflag, void* userdata) {
        char const* const password = userdata;
        if (!password) { return 0; }
        size_t password_len = strlen(password);
        if (size < password_len) { password_len = size; }
        strncpy(buf, password, password_len);
        return password_len;
}
