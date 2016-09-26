{-# LANGUAGE DeriveDataTypeable #-}
-- | See https://www.openssl.org/docs/ssl/SSL_CTX_set_options.html
module OpenSSL.SSL.Option
    ( SSLOption(..)
    , optionToIntegral
    )
    where
import Data.Typeable

#include <openssl/ssl.h>

-- | The behaviour of the SSL library can be changed by setting
-- several options. During a handshake, the option settings of the
-- 'OpenSSL.Session.SSL' object are used. When a new
-- 'OpenSSL.Session.SSL' object is created from a
-- 'OpenSSL.Session.SSLContext', the current option setting is
-- copied. Changes to 'OpenSSL.Session.SSLContext' do not affect
-- already created 'OpenSSL.Session.SSL' objects.
data SSLOption
    = -- | As of OpenSSL 1.0.0 this option has no effect.
      SSL_OP_MICROSOFT_SESS_ID_BUG
      -- | As of OpenSSL 1.0.0 this option has no effect.
    | SSL_OP_NETSCAPE_CHALLENGE_BUG
      -- | As of OpenSSL 0.9.8q and 1.0.0c, this option has no effect.
    | SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG
    | SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG
    | SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER
#if defined(SSL_OP_SAFARI_ECDHE_ECDSA_BUG)
      -- | Don't prefer ECDHE-ECDSA ciphers when the client appears to
      -- be Safari on OS X. OS X 10.8..10.8.3 has broken support for
      -- ECDHE-ECDSA ciphers.
    | SSL_OP_SAFARI_ECDHE_ECDSA_BUG
#endif
    | SSL_OP_SSLEAY_080_CLIENT_DH_BUG
    | SSL_OP_TLS_D5_BUG
    | SSL_OP_TLS_BLOCK_PADDING_BUG
#if defined(SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS)
      -- | Disables a countermeasure against a SSL 3.0/TLS 1.0
      -- protocol vulnerability affecting CBC ciphers, which cannot be
      -- handled by some broken SSL implementations. This option has
      -- no effect for connections using other ciphers.
    | SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS
#endif
#if defined(SSL_OP_TLSEXT_PADDING)
      -- | Adds a padding extension to ensure the ClientHello size is
      -- never between 256 and 511 bytes in length. This is needed as
      -- a workaround for some implementations.
    | SSL_OP_TLSEXT_PADDING
#endif
      -- | All of the above bug workarounds.
    | SSL_OP_ALL
#if defined(SSL_OP_TLS_ROLLBACK_BUG)
      -- | Disable version rollback attack detection.
      --
      -- During the client key exchange, the client must send the same
      -- information about acceptable SSL/TLS protocol levels as
      -- during the first hello. Some clients violate this rule by
      -- adapting to the server's answer. (Example: the client sends a
      -- SSLv2 hello and accepts up to SSLv3.1=TLSv1, the server only
      -- understands up to SSLv3. In this case the client must still
      -- use the same SSLv3.1=TLSv1 announcement. Some clients step
      -- down to SSLv3 with respect to the server's answer and violate
      -- the version rollback protection.)
    | SSL_OP_TLS_ROLLBACK_BUG
#endif
      -- | Always create a new key when using temporary/ephemeral DH
      -- parameters. This option must be used to prevent small
      -- subgroup attacks, when the DH parameters were not generated
      -- using \"strong\" primes (e.g. when using DSA-parameters). If
      -- \"strong\" primes were used, it is not strictly necessary to
      -- generate a new DH key during each handshake but it is also
      -- recommended. 'SSL_OP_SINGLE_DH_USE' should therefore be enabled
      -- whenever temporary/ephemeral DH parameters are used.
    | SSL_OP_SINGLE_DH_USE
      -- | Always use ephemeral (temporary) RSA key when doing RSA
      -- operations. According to the specifications this is only
      -- done, when a RSA key can only be used for signature
      -- operations (namely under export ciphers with restricted RSA
      -- keylength). By setting this option, ephemeral RSA keys are
      -- always used. This option breaks compatibility with the
      -- SSL/TLS specifications and may lead to interoperability
      -- problems with clients and should therefore never be
      -- used. Ciphers with DHE (ephemeral Diffie-Hellman) key
      -- exchange should be used instead.
    | SSL_OP_EPHEMERAL_RSA
#if defined(SSL_OP_CIPHER_SERVER_PREFERENCE)
      -- | When choosing a cipher, use the server's preferences
      -- instead of the client preferences. When not set, the SSL
      -- server will always follow the clients preferences. When set,
      -- the SSLv3/TLSv1 server will choose following its own
      -- preferences. Because of the different protocol, for SSLv2 the
      -- server will send its list of preferences to the client and
      -- the client chooses.
    | SSL_OP_CIPHER_SERVER_PREFERENCE
#endif
    | SSL_OP_PKCS1_CHECK_1
    | SSL_OP_PKCS1_CHECK_2
      -- | If we accept a netscape connection, demand a client cert,
      -- have a non-self-signed CA which does not have its CA in
      -- netscape, and the browser has a cert, it will
      -- crash/hang. Works for 3.x and 4.xbeta
    | SSL_OP_NETSCAPE_CA_DN_BUG
    | SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG
      -- | Do not use the SSLv2 protocol.
    | SSL_OP_NO_SSLv2
      -- | Do not use the SSLv3 protocol.
    | SSL_OP_NO_SSLv3
      -- | Do not use the TLSv1 protocol.
    | SSL_OP_NO_TLSv1
#if defined(SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION)
      -- | When performing renegotiation as a server, always start a
      -- new session (i.e., session resumption requests are only
      -- accepted in the initial handshake). This option is not needed
      -- for clients.
    | SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION
#endif
      -- | Normally clients and servers will, where possible,
      -- transparently make use of
      -- <http://tools.ietf.org/html/rfc4507 RFC 4507> tickets for
      -- stateless session resumption.
      --
      -- If this option is set this functionality is disabled and
      -- tickets will not be used by clients or servers.
    | SSL_OP_NO_TICKET
#if defined(SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION)
      -- | Allow legacy insecure renegotiation between OpenSSL and
      -- unpatched clients or servers. See
      -- <https://www.openssl.org/docs/ssl/SSL_CTX_set_options.html#secure_renegotiation SECURE RENEGOTIATION>
      -- for more details.
    | SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION
#endif
#if defined(SSL_OP_LEGACY_SERVER_CONNECT)
      -- | Allow legacy insecure renegotiation between OpenSSL and
      -- unpatched servers _only_. See
      -- <https://www.openssl.org/docs/ssl/SSL_CTX_set_options.html#secure_renegotiation SECURE RENEGOTIATION>
      -- for more details.
    | SSL_OP_LEGACY_SERVER_CONNECT
#endif
      deriving (Eq, Ord, Show, Typeable)

optionToIntegral :: Integral a => SSLOption -> a
optionToIntegral SSL_OP_MICROSOFT_SESS_ID_BUG                  = #const SSL_OP_MICROSOFT_SESS_ID_BUG
optionToIntegral SSL_OP_NETSCAPE_CHALLENGE_BUG                 = #const SSL_OP_NETSCAPE_CHALLENGE_BUG
optionToIntegral SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG       = #const SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG
optionToIntegral SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG            = #const SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG
optionToIntegral SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER             = #const SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER
#if defined(SSL_OP_SAFARI_ECDHE_ECDSA_BUG)
optionToIntegral SSL_OP_SAFARI_ECDHE_ECDSA_BUG                 = #const SSL_OP_SAFARI_ECDHE_ECDSA_BUG
#endif
optionToIntegral SSL_OP_SSLEAY_080_CLIENT_DH_BUG               = #const SSL_OP_SSLEAY_080_CLIENT_DH_BUG
optionToIntegral SSL_OP_TLS_D5_BUG                             = #const SSL_OP_TLS_D5_BUG
optionToIntegral SSL_OP_TLS_BLOCK_PADDING_BUG                  = #const SSL_OP_TLS_BLOCK_PADDING_BUG
#if defined(SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS)
optionToIntegral SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS            = #const SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS
#endif
#if defined(SSL_OP_TLSEXT_PADDING)
optionToIntegral SSL_OP_TLSEXT_PADDING                         = #const SSL_OP_TLSEXT_PADDING
#endif
optionToIntegral SSL_OP_ALL                                    = #const SSL_OP_ALL
#if defined(SSL_OP_TLS_ROLLBACK_BUG)
optionToIntegral SSL_OP_TLS_ROLLBACK_BUG                       = #const SSL_OP_TLS_ROLLBACK_BUG
#endif
optionToIntegral SSL_OP_SINGLE_DH_USE                          = #const SSL_OP_SINGLE_DH_USE
optionToIntegral SSL_OP_EPHEMERAL_RSA                          = #const SSL_OP_EPHEMERAL_RSA
#if defined(SSL_OP_CIPHER_SERVER_PREFERENCE)
optionToIntegral SSL_OP_CIPHER_SERVER_PREFERENCE               = #const SSL_OP_CIPHER_SERVER_PREFERENCE
#endif
optionToIntegral SSL_OP_PKCS1_CHECK_1                          = #const SSL_OP_PKCS1_CHECK_1
optionToIntegral SSL_OP_PKCS1_CHECK_2                          = #const SSL_OP_PKCS1_CHECK_2
optionToIntegral SSL_OP_NETSCAPE_CA_DN_BUG                     = #const SSL_OP_NETSCAPE_CA_DN_BUG
optionToIntegral SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG        = #const SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG
optionToIntegral SSL_OP_NO_SSLv2                               = #const SSL_OP_NO_SSLv2
optionToIntegral SSL_OP_NO_SSLv3                               = #const SSL_OP_NO_SSLv3
optionToIntegral SSL_OP_NO_TLSv1                               = #const SSL_OP_NO_TLSv1
#if defined(SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION)
optionToIntegral SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION = #const SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION
#endif
optionToIntegral SSL_OP_NO_TICKET                              = #const SSL_OP_NO_TICKET
#if defined(SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION)
optionToIntegral SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION      = #const SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION
#endif
#if defined(SSL_OP_LEGACY_SERVER_CONNECT)
optionToIntegral SSL_OP_LEGACY_SERVER_CONNECT                  = #const SSL_OP_LEGACY_SERVER_CONNECT
#endif
