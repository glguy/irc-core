#pragma once
#ifndef OTR_HPP
#define OTR_HPP

#include <string>

extern "C" {
    #include <libotr/proto.h>
    #include <libotr/message.h>
    #include <libotr/privkey.h>
}

class OTR {
public:
        OtrlUserState us;

        OTR();
        ~OTR();
        OTR(const OTR &) = delete;
        OTR &operator=(const OTR &) = delete;

        ConnContext * context_find
          (const std::string &username, const std::string &accountname, const std::string &protocol) const;

        void message_disconnect_all_instances(const OtrlMessageAppOps *ops, void *opdata,
                        const std::string &accountname, const std::string &protocol,
                        const std::string &username) const;

        gcry_error_t privkey_write_fingerprints(const char *path) const;
        gcry_error_t privkey_generate(const char *path, const char *accountname, const char *protocol) const;
        gcry_error_t instag_generate(const char *path, const char *accountname, const char *protocol) const;
        gcry_error_t privkey_read(const char *path) const;
        gcry_error_t instag_read(const char *path) const;
        gcry_error_t privkey_read_fingerprints(const char *path) const;


        void message_initiate_smp
          (const OtrlMessageAppOps *ops, void *opdata, ConnContext *context,
           const std::string &secret) const;

        void message_respond_smp
          (const OtrlMessageAppOps *ops, void *opdata, ConnContext *context,
           const std::string &secret) const;

        void message_poll(const OtrlMessageAppOps *ops);

        std::tuple<gcry_error_t, bool>
        message_sending(const OtrlMessageAppOps *ops, void *opdata,
                        const std::string &accountname,
                        const std::string &protocol,
                        const std::string &username,
                        const std::string &message) const;

        std::tuple<int, bool>
        message_receiving(const OtrlMessageAppOps *ops, void *opdata,
                          const std::string &accountname,
                          const std::string &protocol,
                          const std::string &username,
                          const std::string &message,
                          std::string *newmessage) const;
};

#endif
