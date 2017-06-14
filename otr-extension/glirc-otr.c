#define _GNU_SOURCE
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <limits.h>
#include <libotr/proto.h>
#include <libotr/message.h>
#include <libotr/privkey.h>
#include <libgen.h>


#include "glirc-api.h"

#define NAME "OTR"
#define MAJOR 1
#define MINOR 0
#define PLUGIN_USER "* OTR *"
#define PLAIN "\x0f"
#define BOLD(x) "\x02" x "\x02"
#define GREEN(x) "\x03" "03" x PLAIN
#define RED(x)   "\x03" "04" x PLAIN

static void
glirc_printf
  (struct glirc *G, const char *net, const char *src, const char *tgt, const char *fmt, ...)
  __attribute__ ((format (printf, 5, 6)));

static void print_status(struct glirc *G, ConnContext *context, const char *fmt, ...)
  __attribute__ ((format (printf, 3, 4)));



static void
glirc_vprintf
  (struct glirc *G, const char *net, const char *src, const char *tgt, const char *fmt, va_list ap)
{
    char *msg = NULL;

    int res = vasprintf(&msg, fmt, ap);

    if (res < 0 || !msg) abort();

    glirc_inject_chat
      (G, net, strlen(net),
          src, strlen(src),
          tgt, strlen(tgt),
          msg, strlen(msg));
}

static void
glirc_printf
  (struct glirc *G, const char *net, const char *src, const char *tgt, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    glirc_vprintf(G, net, src, tgt, fmt, ap);
    va_end(ap);
}

static void print_status(struct glirc *G, ConnContext *context, const char *fmt, ...)
{
    const char *net = context->protocol;
    const char *src = PLUGIN_USER;
    const char *tgt = context->username;

    va_list ap;
    va_start(ap, fmt);
    glirc_vprintf(G, net, src, tgt, fmt, ap);
    va_end(ap);
}

static char * state_path(const char *what)
{
    const char *home = getenv("HOME");
    char *res = NULL;
    if (home) {
        asprintf(&res, "%s/.config/glirc/otr-%s.txt", home, what);
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

    if (smp_event >= 9) return;

    const char *messages[9] = {
      [OTRL_SMPEVENT_NONE          ] = "None",
      [OTRL_SMPEVENT_ERROR         ] = "Error",
      [OTRL_SMPEVENT_ABORT         ] = "Abort",
      [OTRL_SMPEVENT_CHEATED       ] = "Cheated",
      [OTRL_SMPEVENT_ASK_FOR_ANSWER] = "Question: [/extension OTR secret <response>]"  ,
      [OTRL_SMPEVENT_ASK_FOR_SECRET] = "Secret? [/extension OTR secret <response>]",
      [OTRL_SMPEVENT_IN_PROGRESS   ] = "In progress",
      [OTRL_SMPEVENT_SUCCESS       ] = "Success",
      [OTRL_SMPEVENT_FAILURE       ] = "Failure",
      };

    const char *message = messages[smp_event];

    if (question) {
        print_status(G, context, "SMP %s [%s]", message, question);
    } else {
        print_status(G, context, "SMP %s", message);
    }
}

static void
inject_message
  (void *opdata, const char *accountname,
  const char *protocol, const char *recipient, const char *message)
{
    OtrlMessageType msgtype = otrl_proto_message_type(message);

    // The default query message contains HTML and newlines!
    if (msgtype == OTRL_MSGTYPE_QUERY) {
        message = "?OTRv23? This message is attempting to initiate an encrypted"
                  " session, but your client doesn't support this protocol.";
    }

    struct glirc *G = opdata;
    struct glirc_string params[2] =
    { { .str = recipient, .len = strlen(recipient) },
      { .str = message  , .len = strlen(message)   },
    };
    struct glirc_message m = {
      .network  = { .str = protocol, .len  = strlen(protocol) } ,
      .command  = { .str = "PRIVMSG", .len = strlen("PRIVMSG") },
      .params   = params,
      .params_n = 2
    };
    glirc_send_message(G, &m);
}

static void
handle_msg_event
  (void *opdata, OtrlMessageEvent msg_event, ConnContext *context,
   const char *message, gcry_error_t err)
{
    struct glirc *G = opdata;

    const char *messages[16] = {
      [OTRL_MSGEVENT_NONE                      ] = "None",
      [OTRL_MSGEVENT_ENCRYPTION_REQUIRED       ] = "Encryption required, message not sent",
      [OTRL_MSGEVENT_ENCRYPTION_ERROR          ] = "Encryption error, message not sent",
      [OTRL_MSGEVENT_CONNECTION_ENDED          ] = "Connection ended, message not sent",
      [OTRL_MSGEVENT_SETUP_ERROR               ] = "Setup error",
      [OTRL_MSGEVENT_MSG_REFLECTED             ] = "Message reflected",
      [OTRL_MSGEVENT_MSG_RESENT                ] = "Previous message resent",
      [OTRL_MSGEVENT_RCVDMSG_NOT_IN_PRIVATE    ] = "Received unexpected encrypted message",
      [OTRL_MSGEVENT_RCVDMSG_UNREADABLE        ] = "Received message unreadable",
      [OTRL_MSGEVENT_RCVDMSG_MALFORMED         ] = "Received message malformed",
      [OTRL_MSGEVENT_LOG_HEARTBEAT_RCVD        ] = "Heartbeat received",
      [OTRL_MSGEVENT_LOG_HEARTBEAT_SENT        ] = "Heartbeat sent",
      [OTRL_MSGEVENT_RCVDMSG_GENERAL_ERR       ] = "Received general error",
      [OTRL_MSGEVENT_RCVDMSG_UNENCRYPTED       ] = "Received message unencrypted",
      [OTRL_MSGEVENT_RCVDMSG_UNRECOGNIZED      ] = "Received message unrecognizable",
      [OTRL_MSGEVENT_RCVDMSG_FOR_OTHER_INSTANCE] = "Received message for other instance",
    };

    const char *desc = messages[msg_event];

    if (message) {
        print_status(G, context, "%s [%s]", desc, message);
    } else {
        print_status(G, context, "%s", desc);
    }
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
    glirc_print(G, ERROR_MESSAGE, txt, strlen(txt));
}

static int is_logged_in
  (void *opdata, const char *accountname, const char *protocol,
   const char *recipient)
{
    struct glirc *G = opdata;
    int seen = glirc_is_logged_on(G, protocol, strlen(protocol),
                                     recipient, strlen(recipient));

    return seen ? 1 : -1; // not seen just means we might not share a channel
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
    if (otrl_context_is_fingerprint_trusted(context->active_fingerprint)) {
        print_status(G, context, "Connection secured [" GREEN("trusted") "]");
    } else {
        print_status(G, context, "Connection secured [" RED("untrusted") "]");
    }
}

static void gone_insecure(void *G, ConnContext *context)
{
    print_status(G, context, "Secure session terminated");
}

static void still_secure(void *G, ConnContext *context, int is_reply)
{
    if (is_reply) {
        print_status(G, context, "Connection refreshed (by remote)");
    } else {
        print_status(G, context, "Connection refreshed (by local)");
    }
}

static void
new_fingerprint
  (void *opdata, OtrlUserState us,
   const char *accountname, const char *net,
   const char *tgt, unsigned char fp[20])
{

    char *path = state_path("fingerprints");
    otrl_privkey_write_fingerprints(us, path);
    free(path);

    char human[OTRL_PRIVKEY_FPRINT_HUMAN_LEN];
    otrl_privkey_hash_to_human(human, fp);

    struct glirc *G = opdata;
    const char *src = PLUGIN_USER;

    glirc_printf(G, net, src, tgt, "New fingerprint: %s", human);
}

static OtrlMessageAppOps ops = {
    .policy            = op_policy,
    .create_privkey    = op_create_privkey,
    .inject_message    = inject_message,
    .max_message_size  = op_max_message,
    .handle_msg_event  = handle_msg_event,
    .is_logged_in      = is_logged_in,
    .account_name      = account_name,
    .account_name_free = account_name_free,

    .gone_secure       = gone_secure,
    .gone_insecure     = gone_insecure,
    .still_secure      = still_secure,
    .handle_smp_event  = handle_smp_event,
    .new_fingerprint   = new_fingerprint,
};

static void *start_entrypoint(struct glirc *G, const char *libpath)
{
    OTRL_INIT;
    OtrlUserState us = otrl_userstate_create();

    char *path = state_path("keys");
    if (path) otrl_privkey_read(us, path);
    free(path);

    path = state_path("fingerprints");
    if (path) otrl_privkey_read_fingerprints(us, path, NULL, NULL);
    free(path);

    path = state_path("instags");
    if (path) otrl_instag_read(us, path);
    free(path);

    return us;
}

static void stop_entrypoint(struct glirc *G, void *L)
{
    OtrlUserState us = L;

    char *path = state_path("fingerprints");
    if (path) otrl_privkey_write_fingerprints(us, path);
    free(path);

    otrl_userstate_free(us);
}


static char *
rebuild_userinfo(const struct glirc_message *msg)
{
    char *userinfo = malloc(msg->prefix_nick.len +
                            msg->prefix_user.len +
                            msg->prefix_host.len +
                            3);
    if (!userinfo) abort();

    strcpy(userinfo, msg->prefix_nick.str);
    if (msg->prefix_user.len > 0) {
        strcat(userinfo, "!");
        strcat(userinfo, msg->prefix_user.str);
    }
    if (msg->prefix_host.len > 0) {
        strcat(userinfo, "@");
        strcat(userinfo, msg->prefix_host.str);
    }

    return userinfo;
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

        if (glirc_is_channel(G, net, msg->network.len, target, msg->params[0].len)) return PASS_MESSAGE;

        int drop = otrl_message_receiving(us, &ops, G, target, net, sender,
                          message, &newmessage, &tlvs, NULL, NULL, NULL);

        if (newmessage) {
            char *userinfo = rebuild_userinfo(msg);
            glirc_inject_chat(G, msg->network.str    , msg->network.len,
                                 userinfo            , strlen(userinfo),
                                 msg->prefix_nick.str, msg->prefix_nick.len,
                                 newmessage          , strlen(newmessage));
            free(userinfo);
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
    char * newmsg = NULL;
    char * me = NULL;
    int err = 1; // default to error unless sending runs and succeeds
    const char * net = chat->network.str;
    const char * tgt = chat->target.str;
    const char * msg = chat->message.str;

    if (glirc_is_channel(G, net, chat->network.len, tgt, chat->target.len)) return PASS_MESSAGE;

    me = glirc_my_nick(G, chat->network.str, chat->network.len);
    if (!me) goto chat_entrypoint_done;

    err = otrl_message_sending
      (us, &ops, G, me, net, tgt, OTRL_INSTAG_BEST, msg,
       NULL, &newmsg, OTRL_FRAGMENT_SEND_ALL, NULL, NULL, NULL);

    if (err) {
        const char *errTxt = "PANIC: OTR encryption error";
        glirc_print(G, ERROR_MESSAGE, errTxt, strlen(errTxt));
    }

chat_entrypoint_done:
    otrl_message_free(newmsg);
    glirc_free_string(me);
    return err || newmsg ? DROP_MESSAGE : PASS_MESSAGE;
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
    if (!net || !tgt) goto cmd_end_done;

    me = glirc_my_nick(G, net, netlen);
    if (!me) goto cmd_end_done;

    otrl_message_disconnect_all_instances(us, &ops, G, me, net, tgt);

    const char *src = PLUGIN_USER;
    const char *msg = "Session Terminated";

    glirc_inject_chat
      (G, net, strlen(net),
          src, strlen(src),
          tgt, strlen(tgt),
          msg, strlen(msg));

cmd_end_done:
    glirc_free_string(me);
    glirc_free_string(net);
    glirc_free_string(tgt);
}


static ConnContext *
get_current_context(struct glirc *G, OtrlUserState us)
{
    char *net = NULL; size_t netlen = 0;
    char *tgt = NULL; size_t tgtlen = 0;
    char *me  = NULL;
    ConnContext *context = NULL;

    glirc_current_focus(G, &net, &netlen, &tgt, &tgtlen);
    if (!net || !tgt) goto get_current_context_done;

    me = glirc_my_nick(G, net, netlen);
    if (!me) goto get_current_context_done;

    context = otrl_context_find(us, tgt, me, net, OTRL_INSTAG_BEST, 0, NULL, NULL, NULL);

get_current_context_done:
    glirc_free_string(me);
    glirc_free_string(net);
    glirc_free_string(tgt);

    return context;
}

static inline void do_smp
  (struct glirc *G, OtrlUserState us,
   const unsigned char *secret, size_t secretlen,
   smp_func func)
{
    ConnContext *context = get_current_context(G, us);

    if (context) {
        func(us, &ops, G, context, secret, secretlen);
    }

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

/*
 * Generate keypair and instance tag for the current network and nick.
 */
static void cmd_keygen
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    char *net     = NULL;
    char *me      = NULL;
    char *path    = NULL;
    size_t netlen = 0;

    glirc_current_focus(G, &net, &netlen, NULL, NULL);
    if (!net) goto cmd_keygen_done;

    me = glirc_my_nick(G, net, netlen);
    if (!me) goto cmd_keygen_done;

    path = state_path("keys");
    if (path) otrl_privkey_generate(us, path, me, net);
    free(path);

    path = state_path("instags");
    if (path) otrl_instag_generate(us, path, me, net);
    free(path);

cmd_keygen_done:
    glirc_free_string(me);
    glirc_free_string(net);
}

/*
 * Manually mark the fingerprint associated with the current window trusted.
 */
static void cmd_trust
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    ConnContext *context = get_current_context(G, us);
    if (!context) return;

    otrl_context_set_trust(context->active_fingerprint, "manual");

    char *path = state_path("fingerprints");
    if (path) otrl_privkey_write_fingerprints(us, path);
    free(path);

    char human[OTRL_PRIVKEY_FPRINT_HUMAN_LEN];
    otrl_privkey_hash_to_human(human, context->active_fingerprint->fingerprint);
    print_status(G, context, "Fingerprint trusted: " BOLD("%s"), human);
}

/*
 * Manually mark the fingerprint associated with the current window untrusted.
 */
static void cmd_untrust
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    ConnContext *context = get_current_context(G, us);
    if (!context) return;

    otrl_context_set_trust(context->active_fingerprint, NULL);

    char *path = state_path("fingerprints");
    if (path) otrl_privkey_write_fingerprints(us, path);
    free(path);

    char human[OTRL_PRIVKEY_FPRINT_HUMAN_LEN];
    otrl_privkey_hash_to_human(human, context->active_fingerprint->fingerprint);
    print_status(G, context, "Fingerprint untrusted: " BOLD("%s"), human);
}

/*
 * Print status information for the current context to the chat window
 */
static void cmd_status
  (struct glirc *G, OtrlUserState us,
   const struct glirc_string *params, size_t params_n)
{
    ConnContext *context = get_current_context(G, us);
    if (!context) return;

    char human[OTRL_PRIVKEY_FPRINT_HUMAN_LEN] = {0};

    otrl_privkey_fingerprint(us, human, context->accountname, context->protocol);
    print_status(G, context, "Local  fingerprint: " BOLD("%s"), human);

    const Fingerprint *fp = context->active_fingerprint;
    if (fp) {
        otrl_privkey_hash_to_human(human, fp->fingerprint);

        const char *trust = fp->trust;
        if (trust) {
            print_status(G, context, "Remote fingerprint: " BOLD("%s") " [" GREEN("%s") "]", human, trust);
        } else {
            print_status(G, context, "Remote fingerprint: " BOLD("%s") " [" RED("untrusted") "]", human);
        }
    }

    print_status(G, context,
      "Local instance [" BOLD("%08X") "] Remote instance [" BOLD("%08X") "] Protocol [" BOLD("%u") "]",
      context->our_instance, context->their_instance, context->protocol_version);

    const char *statuses[] = {
      [OTRL_MSGSTATE_PLAINTEXT] = RED  ("Plaintext"),
      [OTRL_MSGSTATE_ENCRYPTED] = GREEN("Encrypted"),
      [OTRL_MSGSTATE_FINISHED ] = RED  ("Finished" ),
    };

    print_status(G, context, "Connection state: %s", statuses[context->msgstate]);
}

struct cmd_impl {
    const char *name;
    void (*func)(struct glirc *, OtrlUserState, const struct glirc_string *, size_t);
};

static struct cmd_impl cmd_impls[] = {
   { .name = "secret",  .func = cmd_secret  },
   { .name = "ask",     .func = cmd_ask     },
   { .name = "poll",    .func = cmd_poll    },
   { .name = "end",     .func = cmd_end     },
   { .name = "keygen",  .func = cmd_keygen  },
   { .name = "trust",   .func = cmd_trust   },
   { .name = "untrust", .func = cmd_untrust },
   { .name = "status",  .func = cmd_status  },
   { .name = NULL,      .func = NULL        },
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
