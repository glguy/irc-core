#define HSOPENSSL_NEED_NOT_INCLUDE_CABAL_MACROS_H 1
#include "HsOpenSSL.h"
#include <stdint.h>
#include "mutex.h"

/* OpenSSL ********************************************************************/
void HsOpenSSL_OpenSSL_add_all_algorithms() {
    OpenSSL_add_all_algorithms();
}

void HsOpenSSL_OPENSSL_free(void* ptr) {
    OPENSSL_free(ptr);
}

/* BIO ************************************************************************/
void HsOpenSSL_BIO_set_flags(BIO* bio, int flags) {
    BIO_set_flags(bio, flags);
}

int HsOpenSSL_BIO_flush(BIO* bio) {
    return BIO_flush(bio);
}

int HsOpenSSL_BIO_reset(BIO* bio) {
    return BIO_reset(bio);
}

int HsOpenSSL_BIO_eof(BIO* bio) {
    return BIO_eof(bio);
}

int HsOpenSSL_BIO_set_md(BIO* bio, EVP_MD* md) {
    return BIO_set_md(bio, md);
}

int HsOpenSSL_BIO_set_buffer_size(BIO* bio, int bufSize) {
    return BIO_set_buffer_size(bio, bufSize);
}

int HsOpenSSL_BIO_should_retry(BIO* bio) {
    return BIO_should_retry(bio);
}

int HsOpenSSL_BIO_FLAGS_BASE64_NO_NL() {
    return BIO_FLAGS_BASE64_NO_NL;
}

/* DH *************************************************************************/
DH* HsOpenSSL_DHparams_dup(DH* dh) {
    return DHparams_dup(dh);
}

/* EVP ************************************************************************/
int HsOpenSSL_EVP_MD_size(EVP_MD* md) {
    return EVP_MD_size(md);
}

int HsOpenSSL_EVP_CIPHER_CTX_block_size(EVP_CIPHER_CTX* ctx) {
    return EVP_CIPHER_CTX_block_size(ctx);
}

int HsOpenSSL_EVP_CIPHER_iv_length(EVP_CIPHER* cipher) {
    return EVP_CIPHER_iv_length(cipher);
}

/* X509 ***********************************************************************/
long HsOpenSSL_X509_get_version(X509* x509) {
    return X509_get_version(x509);
}

ASN1_TIME* HsOpenSSL_X509_get_notBefore(X509* x509) {
    return X509_get_notBefore(x509);
}

ASN1_TIME* HsOpenSSL_X509_get_notAfter(X509* x509) {
    return X509_get_notAfter(x509);
}

long HsOpenSSL_X509_REQ_get_version(X509_REQ* req) {
    return X509_REQ_get_version(req);
}

X509_NAME* HsOpenSSL_X509_REQ_get_subject_name(X509_REQ* req) {
    return X509_REQ_get_subject_name(req);
}

long HsOpenSSL_X509_CRL_get_version(X509_CRL* crl) {
    return X509_CRL_get_version(crl);
}

ASN1_TIME* HsOpenSSL_X509_CRL_get_lastUpdate(X509_CRL* crl) {
    return X509_CRL_get_lastUpdate(crl);
}

ASN1_TIME* HsOpenSSL_X509_CRL_get_nextUpdate(X509_CRL* crl) {
    return X509_CRL_get_nextUpdate(crl);
}

X509_NAME* HsOpenSSL_X509_CRL_get_issuer(X509_CRL* crl) {
    return X509_CRL_get_issuer(crl);
}

STACK_OF(X509_REVOKED)* HsOpenSSL_X509_CRL_get_REVOKED(X509_CRL* crl) {
    return X509_CRL_get_REVOKED(crl);
}

void HsOpenSSL_X509_ref(X509* x509) {
    CRYPTO_add(&x509->references, 1, CRYPTO_LOCK_X509);
}

void HsOpenSSL_X509_CRL_ref(X509_CRL* crl) {
    CRYPTO_add(&crl->references, 1, CRYPTO_LOCK_X509_CRL);
}

X509* HsOpenSSL_X509_STORE_CTX_get0_current_issuer(X509_STORE_CTX *ctx) {
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
    return X509_STORE_CTX_get0_current_issuer(ctx);
#else
    return ctx->current_issuer;
#endif
}

X509_CRL* HsOpenSSL_X509_STORE_CTX_get0_current_crl(X509_STORE_CTX *ctx) {
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
    return X509_STORE_CTX_get0_current_crl(ctx);
#else
    return ctx->current_crl;
#endif
}

/* PKCS#7 *********************************************************************/
long HsOpenSSL_PKCS7_is_detached(PKCS7* pkcs7) {
    return PKCS7_is_detached(pkcs7);
}


/* DH *************************************************************************/
BIGNUM *HsOpenSSL_DH_get_pub_key(DH *dh) {
    return dh->pub_key;
}

int HsOpenSSL_DH_length(DH *dh) {
    return BN_num_bits(dh->p);
}


/* ASN1 ***********************************************************************/
ASN1_INTEGER* HsOpenSSL_M_ASN1_INTEGER_new() {
    return M_ASN1_INTEGER_new();
}

void HsOpenSSL_M_ASN1_INTEGER_free(ASN1_INTEGER* intPtr) {
    M_ASN1_INTEGER_free(intPtr);
}

ASN1_INTEGER* HsOpenSSL_M_ASN1_TIME_new() {
    return M_ASN1_TIME_new();
}

void HsOpenSSL_M_ASN1_TIME_free(ASN1_TIME* timePtr) {
    M_ASN1_TIME_free(timePtr);
}

/* Threads ********************************************************************/
static mutex_t* mutex_at;

struct CRYPTO_dynlock_value {
    mutex_t mutex;
};

static void HsOpenSSL_lockingCallback(int mode, int n, const char* file, int line) {
    if (mode & CRYPTO_LOCK) {
        mutex_lock(&mutex_at[n]);
    }
    else {
        mutex_unlock(&mutex_at[n]);
    }
}

static unsigned long HsOpenSSL_idCallback() {
    return (unsigned long)self();
}

static struct CRYPTO_dynlock_value* HsOpenSSL_dynlockCreateCallback(const char* file, int line) {
    struct CRYPTO_dynlock_value* val;

    val = OPENSSL_malloc(sizeof(struct CRYPTO_dynlock_value));
    mutex_init(&val->mutex);

    return val;
}

static void HsOpenSSL_dynlockLockCallback(int mode, struct CRYPTO_dynlock_value* val, const char* file, int line) {
    if (mode & CRYPTO_LOCK) {
        mutex_lock(&val->mutex);
    }
    else {
        mutex_unlock(&val->mutex);
    }
}

static void HsOpenSSL_dynlockDestroyCallback(struct CRYPTO_dynlock_value* val, const char* file, int line) {
    mutex_destroy(&val->mutex);
    OPENSSL_free(val);
}

void HsOpenSSL_setupMutex() {
    int i;

    mutex_at = OPENSSL_malloc(CRYPTO_num_locks() * sizeof(*mutex_at));

    for (i = 0; i < CRYPTO_num_locks(); i++) {
        mutex_init(&mutex_at[i]);
    }

    CRYPTO_set_locking_callback(HsOpenSSL_lockingCallback);
    CRYPTO_set_id_callback(HsOpenSSL_idCallback);

    CRYPTO_set_dynlock_create_callback(HsOpenSSL_dynlockCreateCallback);
    CRYPTO_set_dynlock_lock_callback(HsOpenSSL_dynlockLockCallback);
    CRYPTO_set_dynlock_destroy_callback(HsOpenSSL_dynlockDestroyCallback);
}

/* DSA ************************************************************************/

/* OpenSSL sadly wants to ASN1 encode the resulting bignums so we use this
 * function to skip that. Returns > 0 on success */
int HsOpenSSL_dsa_sign(DSA *dsa, const unsigned char *ddata, int dlen,
                       BIGNUM **r, BIGNUM **s) {
  DSA_SIG *const sig = dsa->meth->dsa_do_sign(ddata, dlen, dsa);
  if (!sig) return 0;
  *r = sig->r;
  *s = sig->s;
  free(sig);
  return 1;
}

int HsOpenSSL_dsa_verify(DSA *dsa, const unsigned char *ddata, int dlen,
                         BIGNUM *r, BIGNUM *s) {
  DSA_SIG sig;
  sig.r = r;
  sig.s = s;
  return dsa->meth->dsa_do_verify(ddata, dlen, &sig, dsa);
}

#if !defined(DSAPublicKey_dup)
# define DSAPublicKey_dup(dsa)                                      \
    (DSA *)ASN1_dup((i2d_of_void *)i2d_DSAPublicKey,                \
                    (d2i_of_void *)d2i_DSAPublicKey,(char *)dsa)
#endif

#if !defined(DSAPrivateKey_dup)
#define DSAPrivateKey_dup(dsa)                                      \
    (DSA *)ASN1_dup((i2d_of_void *)i2d_DSAPrivateKey,               \
                    (d2i_of_void *)d2i_DSAPrivateKey,(char *)dsa)
#endif

DSA* HsOpenSSL_DSAPublicKey_dup(const DSA* dsa) {
    return DSAPublicKey_dup(dsa);
}

DSA* HsOpenSSL_DSAPrivateKey_dup(const DSA* dsa) {
    return DSAPrivateKey_dup(dsa);
}

/* SSL ************************************************************************/
long HsOpenSSL_SSL_CTX_set_options(SSL_CTX* ctx, long options) {
    return SSL_CTX_set_options(ctx, options);
}

/* OpenSSL < 0.9.8m does not have SSL_CTX_clear_options() */
long HsOpenSSL_SSL_CTX_clear_options(SSL_CTX* ctx, long options) {
#if defined(SSL_CTX_clear_options)
    return SSL_CTX_clear_options(ctx, options);
#else
    long tmp = SSL_CTX_get_options(ctx);
    return SSL_CTX_set_options(ctx, tmp & ~options);
#endif
}

long HsOpenSSL_SSL_set_options(SSL* ssl, long options) {
    return SSL_set_options(ssl, options);
}

/* OpenSSL < 0.9.8m does not have SSL_clear_options() */
long HsOpenSSL_SSL_clear_options(SSL* ssl, long options) {
#if defined(SSL_clear_options)
    return SSL_clear_options(ssl, options);
#else
    long tmp = SSL_get_options(ssl);
    return SSL_set_options(ssl, tmp & ~options);
#endif
}
