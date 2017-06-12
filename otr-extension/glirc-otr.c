#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>
#include <libotr/proto.h>
#include <libotr/message.h>
#include <libotr/privkey.h>
#include <libgen.h>


#include "glirc-api.h"

#define NAME "OTR"
#define MAJOR 1
#define MINOR 0


static void print_status(struct glirc *G, ConnContext *context, const char *msg)
{
    const char *net = context->protocol;
    const char *src = "* OTR *";
    const char *tgt = context->username;

    glirc_inject_chat
      (G, net, strlen(net),
          src, strlen(src),
          tgt, strlen(tgt),
          msg, strlen(msg));
}

static char * keyfile_path(void)
{
    const char *home = getenv("HOME");
    char *res = NULL;
    if (home) {
        asprintf(&res, "%s/.config/glirc/otr-keys.txt", home);
    }
    return res;
}


static OtrlPolicy op_policy(void *opdata, ConnContext *context)
{
    return OTRL_POLICY_DEFAULT & ~OTRL_POLICY_SEND_WHITESPACE_TAG;
}

static void handle_smp_event
  (void *opdata, OtrlSMPEvent smp_event, ConnContext *context,
   unsigned short progress_percent, char *question)
{
    struct glirc *G = opdata;
    char buffer[512];

    if (smp_event >= 9) return;

    const char *messages[9] = {
      [OTRL_SMPEVENT_NONE          ] = "None"       ,
      [OTRL_SMPEVENT_ERROR         ] = "Error"      ,
      [OTRL_SMPEVENT_ABORT         ] = "Abort"      ,
      [OTRL_SMPEVENT_CHEATED       ] = "Cheated"    ,
      [OTRL_SMPEVENT_ASK_FOR_ANSWER] = "Question:"  ,
      [OTRL_SMPEVENT_ASK_FOR_SECRET] = "Secret?"    ,
      [OTRL_SMPEVENT_IN_PROGRESS   ] = "In progress",
      [OTRL_SMPEVENT_SUCCESS       ] = "Success"    ,
      [OTRL_SMPEVENT_FAILURE       ] = "Failure"    ,
      };

    const char *message = messages[smp_event];

    if (question) {
        snprintf(buffer, sizeof(buffer), "SMP %s [%s]", message, question);
    } else {
        snprintf(buffer, sizeof(buffer), "SMP %s", message);
    }

    print_status(G, context, buffer);
}

static void
op_inject
  (void *opdata, const char *accountname,
  const char *protocol, const char *recipient, const char *message)
{
    char *message1 = strdup(message);
    bool nonempty = 0;
    for (char *cursor = message1; *cursor; cursor++) {
        if (*cursor == '\n') *cursor = ' ';
        if (*cursor != ' ') nonempty = true;
    }

    struct glirc *G = opdata;
    struct glirc_string params[2] =
    { { .str = recipient, .len = strlen(recipient) },
      { .str = message1 , .len = strlen(message1)  },
    };
    struct glirc_message m = {
      .network  = { .str = protocol, .len  = strlen(protocol) } ,
      .command  = { .str = "PRIVMSG", .len = strlen("PRIVMSG") },
      .params   = params,
      .params_n = 2
    };
    if (nonempty) {
        glirc_send_message(G, &m);
    }
    free(message1);
}

static void
handle_msg_event
  (void *opdata, OtrlMessageEvent msg_event, ConnContext *context,
   const char *message, gcry_error_t err)
{
    struct glirc *G = opdata;
    char buffer[512];

    const char *messages[16] = {
      [OTRL_MSGEVENT_NONE                      ] = "None",
      [OTRL_MSGEVENT_ENCRYPTION_REQUIRED       ] = "Encryption required",
      [OTRL_MSGEVENT_ENCRYPTION_ERROR          ] = "Encryption error",
      [OTRL_MSGEVENT_CONNECTION_ENDED          ] = "Connection ended",
      [OTRL_MSGEVENT_SETUP_ERROR               ] = "Setup error",
      [OTRL_MSGEVENT_MSG_REFLECTED             ] = "Message reflected",
      [OTRL_MSGEVENT_MSG_RESENT                ] = "Message reset",
      [OTRL_MSGEVENT_RCVDMSG_NOT_IN_PRIVATE    ] = "Received message not private",
      [OTRL_MSGEVENT_RCVDMSG_UNREADABLE        ] = "Received message unreadable",
      [OTRL_MSGEVENT_RCVDMSG_MALFORMED         ] = "Received message malformed",
      [OTRL_MSGEVENT_LOG_HEARTBEAT_RCVD        ] = "Log heartbeat received",
      [OTRL_MSGEVENT_LOG_HEARTBEAT_SENT        ] = "Log heartbeat sent",
      [OTRL_MSGEVENT_RCVDMSG_GENERAL_ERR       ] = "Received general error:",
      [OTRL_MSGEVENT_RCVDMSG_UNENCRYPTED       ] = "Received message unencrypted",
      [OTRL_MSGEVENT_RCVDMSG_UNRECOGNIZED      ] = "Received message unrecognizable",
      [OTRL_MSGEVENT_RCVDMSG_FOR_OTHER_INSTANCE] = "Received message for other instance",
    };

    const char *desc = messages[msg_event];

    if (message) {
        snprintf(buffer, sizeof(buffer), "%s [%s]", desc, message);
    } else {
        snprintf(buffer, sizeof(buffer), "%s", desc);
    }

    print_status(G, context, buffer);
}

static int op_max_message(void *opdata, ConnContext *context)
{
    return 400; // pessmistic
}

static void
op_create_privkey(void *opdata, const char *accountname, const char *protocol)
{
    const char *txt = "No private key [/extension " NAME " keygen]";
    struct glirc *G = opdata;
    struct glirc_string m = { .str = txt, .len = strlen(txt) };
    glirc_print(G, ERROR_MESSAGE, m);
}

static int is_logged_in
  (void *opdata, const char *accountname, const char *protocol,
   const char *recipient)
{
    return 1; // TODO: ask glirc if we share a channel, look in csUsers
}

static
const char *
account_name(void *opdata, const char *account, const char *protocol)
{
    const char *fmt = "%s:%s";
    char *res = NULL;
    asprintf(&res, fmt, protocol, account);
    return res;
}

static void account_name_free(void *opdata, const char *account_name)
{
    free((char*)account_name);
}

static void gone_secure(void *G, ConnContext *context)
{
    print_status(G, context, "SECURE");
}

static void gone_insecure(void *G, ConnContext *context)
{
    print_status(G, context, "INSECURE");
}

static void still_secure(void *G, ConnContext *context, int is_reply)
{
    if (is_reply) {
        print_status(G, context, "STILL SECURE (reply)");
    } else {
        print_status(G, context, "STILL SECURE");
    }
}



static OtrlMessageAppOps ops = {
    .policy            = op_policy,
    .create_privkey    = op_create_privkey,
    .inject_message    = op_inject,
    .max_message_size  = op_max_message,
    .handle_msg_event  = handle_msg_event,
    .is_logged_in      = is_logged_in,
    .account_name      = account_name,
    .account_name_free = account_name_free,

    .gone_secure       = gone_secure,
    .gone_insecure     = gone_insecure,
    .still_secure      = still_secure,
    .handle_smp_event  = handle_smp_event,
};

static void *start_entrypoint(struct glirc *G, const char *libpath)
{
        OTRL_INIT;
        OtrlUserState us = otrl_userstate_create();

        char *path = keyfile_path();
        if (path) otrl_privkey_read(us, path);
        free(path);

        return us;
}

static void stop_entrypoint(struct glirc *G, void *L)
{
        OtrlUserState us = L;
        otrl_userstate_free(us);
}

static enum process_result
message_entrypoint(struct glirc *G, void *L, const struct glirc_message *msg)
{
    OtrlUserState us = L;

    if (0 == strncmp("PRIVMSG", msg->command.str, msg->command.len) && msg->params_n == 2) {

        char *newmessage = NULL;
        OtrlTLV *tlvs = NULL;

        const char *sender  = msg->prefix_nick.str;
        const char *target  = msg->params[0].str;
        const char *message = msg->params[1].str;
        const char *net     = msg->network.str;

        int drop = otrl_message_receiving(us, &ops, G, target, net, sender,
                          message, &newmessage, &tlvs, NULL, NULL, NULL);

        if (newmessage) {
            glirc_inject_chat(G, msg->network.str, msg->network.len,
                                 msg->prefix_nick.str, msg->prefix_nick.len,
                                 msg->prefix_nick.str, msg->prefix_nick.len,
                                 newmessage, strlen(newmessage));
        }

        otrl_tlv_free(tlvs); // ignoring this for now
        otrl_message_free(newmessage);

        return (newmessage || drop) ? DROP_MESSAGE : PASS_MESSAGE;
    }

    return PASS_MESSAGE;
}


static enum process_result chat_entrypoint(struct glirc *G, void *L, const struct glirc_chat *chat)
{
        OtrlUserState us = L;
        const char * net = chat->network.str;
        const char * tgt = chat->target.str;
        const char * msg = chat->message.str;

        char * me = glirc_my_nick(G, chat->network);
        char *newmsg = NULL;

        int err = otrl_message_sending
          (us, &ops, G, me, net, tgt, OTRL_INSTAG_BEST, msg,
           NULL, &newmsg, OTRL_FRAGMENT_SEND_ALL, NULL, NULL, NULL);

        glirc_free_string(me);

        if (newmsg) {
                otrl_message_free(newmsg);
                return DROP_MESSAGE;
        }

        if (err) {
                const char *errTxt = "PANIC: OTR encryption error";
                struct glirc_string m = { .str = errTxt, .len = strlen(errTxt) };
                glirc_print(G, ERROR_MESSAGE, m);
                return DROP_MESSAGE;
        }

        return PASS_MESSAGE;

}

typedef void (*smp_func)
  (OtrlUserState, const OtrlMessageAppOps *, void *, ConnContext *, const unsigned char *, size_t);

static void cmd_end
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    char *net = NULL; size_t netlen = 0;
    char *tgt = NULL; size_t tgtlen = 0;
    char *me  = NULL;
    glirc_current_focus(G, &net, &netlen, &tgt, &tgtlen);

    if (net && tgt) {
        struct glirc_string mynet = { .str = net, .len = netlen };
        me = glirc_my_nick(G, mynet);

        otrl_message_disconnect_all_instances(us, &ops, G, me, net, tgt);

        const char *src = "* OTR *";
        const char *msg = "Session Terminated";

        glirc_inject_chat
          (G, net, strlen(net),
              src, strlen(src),
              tgt, strlen(tgt),
              msg, strlen(msg));
    }

    glirc_free_string(me);
    glirc_free_string(net);
    glirc_free_string(tgt);
}

static void do_smp
  (struct glirc *G, OtrlUserState us,
   const unsigned char *secret, size_t secretlen,
   smp_func func)
{
    char *net = NULL; size_t netlen = 0;
    char *tgt = NULL; size_t tgtlen = 0;
    char *me  = NULL;
    glirc_current_focus(G, &net, &netlen, &tgt, &tgtlen);

    if (net && tgt) {
        struct glirc_string mynet = { .str = net, .len = netlen };
        me = glirc_my_nick(G, mynet);

        ConnContext *context = otrl_context_find(us, tgt, me, net, OTRL_INSTAG_BEST, 0, NULL, NULL, NULL);

        if (context) {
                func(us, &ops, G, context, secret, secretlen);
        }
    }

    glirc_free_string(me);
    glirc_free_string(net);
    glirc_free_string(tgt);
}

static void cmd_ask
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    if (params_n >= 2) {
            do_smp(G, us, (unsigned char *)params[1].str,
                                           params[1].len,
            otrl_message_initiate_smp);
    }
}

static void cmd_secret
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    if (params_n >= 2) {
        do_smp(G, us, (unsigned char *)params[1].str,
                                       params[1].len,
        otrl_message_respond_smp);
    }
}

static void cmd_poll
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    otrl_message_poll(us, &ops, G);
}

static void cmd_keygen
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    char *path = keyfile_path(), *net = NULL, *me = NULL;
    size_t netlen = 0;
    if (path) {
        glirc_current_focus(G, &net, &netlen, NULL, NULL);
        if (net) {
            struct glirc_string netstr = {.str=net, .len=netlen};
            char *me = glirc_my_nick(G, netstr);
            otrl_privkey_generate(us, path, me, net);
        }
    }
    free(path);
    glirc_free_string(me);
    glirc_free_string(net);
}

struct cmd_impl {
    const char *name;
    void (*func)(struct glirc *, OtrlUserState, const struct glirc_string *, size_t);
};

struct cmd_impl cmd_impls[] = {
   { .name = "secret", .func = cmd_secret },
   { .name = "ask",    .func = cmd_ask    },
   { .name = "poll",   .func = cmd_poll   },
   { .name = "end",    .func = cmd_end    },
   { .name = "keygen", .func = cmd_keygen },
   { .name = NULL,     .func = NULL       },
};

static void command_entrypoint
  (struct glirc *G, void *L, const struct glirc_command *cmd)
{
    OtrlUserState us = L;

    if (cmd->params_n > 0) {
        for (struct cmd_impl *c = cmd_impls; c->name; c++) {
            if (0 == strncmp(cmd->params[0].str, c->name, cmd->params[0].len)) {
                c->func(G, us, cmd->params, cmd->params_n);
                break;
            }
        }
    }
}

struct glirc_extension extension = {
        .name            = NAME,
        .major_version   = MAJOR,
        .minor_version   = MINOR,
        .start           = start_entrypoint,
        .stop            = stop_entrypoint,
        .process_message = message_entrypoint,
        .process_chat    = chat_entrypoint,
        .process_command = command_entrypoint,
};
