#ifndef GLIRC_API
#define GLIRC_API

#include <stdlib.h>

struct glirc_string {
        const char *str;
        size_t len;
};

struct glirc_message {
        struct glirc_string network;
        struct glirc_string prefix;
        struct glirc_string command;
        struct glirc_string *params;
        size_t params_n;
        struct glirc_string *tagkeys;
        struct glirc_string *tagvals;
        size_t tags_n;
};

typedef void * start_type(void *glirc);
typedef void stop_type(void *glirc, void *S);
typedef void process_message_type(void *glirc, void *S, const struct glirc_message *);

struct glirc_extension {
        char *name;
        int major_version, minor_version;
        start_type *start;
        stop_type  *stop;
        process_message_type *process_message;
};

int glirc_send_message(void *glirc, const char *net, size_t netlen, const struct glirc_message *);
int glirc_report_error(void *glirc, const char *str, size_t len);

#endif
