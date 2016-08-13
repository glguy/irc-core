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
};

typedef void * start_type(void);
typedef void stop_type(void *);
typedef void process_message_type(void *, const struct glirc_message *);

struct glirc_extension {
        start_type *start;
        stop_type  *stop;
        process_message_type *process_message;
};



#endif
