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
        const OtrlMessageAppOps *ops;
        void *opdata;

public:
        OtrlUserState us;

        OTR(const OtrlMessageAppOps *, void *);
        ~OTR();
        OTR(const OTR &) = delete;
        OTR &operator=(const OTR &) = delete;

        ConnContext * context_find
          (const std::string &username, const std::string &accountname, const std::string &protocol) const;

        void message_disconnect_all_instances(
                        const std::string &accountname, const std::string &protocol,
                        const std::string &username) const;

        gcry_error_t privkey_write_fingerprints(const char *path) const;
        gcry_error_t privkey_generate(const char *path, const char *accountname, const char *protocol) const;
        gcry_error_t instag_generate(const char *path, const char *accountname, const char *protocol) const;
        gcry_error_t privkey_read(const char *path) const;
        gcry_error_t instag_read(const char *path) const;
        gcry_error_t privkey_read_fingerprints(const char *path) const;


        void message_initiate_smp (ConnContext *context, const std::string &secret) const;

        void message_respond_smp (ConnContext *context, const std::string &secret) const;

        void message_poll();

        std::tuple<gcry_error_t, bool>
        message_sending(const std::string &accountname,
                        const std::string &protocol,
                        const std::string &username,
                        const std::string &message) const;

        std::tuple<int, bool, std::string>
        message_receiving(const std::string &accountname,
                          const std::string &protocol,
                          const std::string &username,
                          const std::string &message) const;
};

#endif
