#include "OTR.hpp"

OTR::OTR() : us(otrl_userstate_create()) {}
OTR::~OTR() { otrl_userstate_free(us); }

ConnContext *
OTR::context_find
  (const std::string &username, const std::string &accountname, const std::string &protocol) const
{
    return otrl_context_find
                (us, username.c_str(), accountname.c_str(), protocol.c_str(),
                 OTRL_INSTAG_BEST, 0, NULL, NULL, NULL);
}

void
OTR::message_disconnect_all_instances(const OtrlMessageAppOps *ops, void *opdata,
                const std::string &accountname, const std::string &protocol,
                const std::string &username) const
{
  otrl_message_disconnect_all_instances
          (us, ops, opdata, accountname.c_str(), protocol.c_str(), username.c_str());
}


gcry_error_t
OTR::privkey_write_fingerprints(const char *path) const
{
    return otrl_privkey_write_fingerprints(us, path);
}


gcry_error_t
OTR::privkey_generate(const char *path, const char *accountname, const char *protocol) const
{
    return otrl_privkey_generate(us, path, accountname, protocol);
}

gcry_error_t
OTR::instag_generate(const char *path, const char *accountname, const char *protocol) const
{
    return otrl_instag_generate(us, path, accountname, protocol);
}

gcry_error_t
OTR::privkey_read(const char *path) const
{
    return otrl_privkey_read(us, path);
}

gcry_error_t
OTR::instag_read(const char *path) const
{
    return otrl_instag_read(us, path);
}

gcry_error_t
OTR::privkey_read_fingerprints(const char *path) const
{
    return otrl_privkey_read_fingerprints(us, path, nullptr, nullptr);
}

void
OTR::message_initiate_smp
  (const OtrlMessageAppOps *ops, void *opdata, ConnContext *context,
   const std::string &secret) const
{
    otrl_message_initiate_smp
        (us, ops, opdata, context,
         reinterpret_cast<const unsigned char *>(secret.c_str()),
         secret.length());
}

void
OTR::message_respond_smp
  (const OtrlMessageAppOps *ops, void *opdata, ConnContext *context,
   const std::string &secret) const
{
    otrl_message_respond_smp
        (us, ops, opdata, context,
         reinterpret_cast<const unsigned char *>(secret.c_str()),
         secret.length());
}

void
OTR::message_poll(const OtrlMessageAppOps *ops) {
    otrl_message_poll(us, ops, static_cast<void*>(this));
}