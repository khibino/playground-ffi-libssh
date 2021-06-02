
module Libssh where

import Foreign (Ptr)
import Foreign.C (CInt (..))


#include <libssh/libssh.h>

data ISshSession
type SshSession = Ptr ISshSession

-- ssh_session ssh_new(void);
foreign import ccall unsafe "libssh/libssh.h ssh_new"
  c'ssh_new :: IO SshSession

-- void ssh_free(ssh_session session);
foreign import ccall unsafe "libssh/libssh.h ssh_free"
  c'ssh_free :: SshSession -> IO ()


newtype SshOptions = SshOptions { codeOptions :: CInt }

#{enum SshOptions, SshOptions,
  SSH_OPTIONS_HOST,
  SSH_OPTIONS_PORT,
  SSH_OPTIONS_PORT_STR,
  SSH_OPTIONS_FD,
  SSH_OPTIONS_USER,
  SSH_OPTIONS_SSH_DIR,
  SSH_OPTIONS_IDENTITY,
  SSH_OPTIONS_ADD_IDENTITY,
  SSH_OPTIONS_KNOWNHOSTS,
  SSH_OPTIONS_TIMEOUT,
  SSH_OPTIONS_TIMEOUT_USEC,
  SSH_OPTIONS_SSH1,
  SSH_OPTIONS_SSH2,
  SSH_OPTIONS_LOG_VERBOSITY,
  SSH_OPTIONS_LOG_VERBOSITY_STR,
  SSH_OPTIONS_CIPHERS_C_S,
  SSH_OPTIONS_CIPHERS_S_C,
  SSH_OPTIONS_COMPRESSION_C_S,
  SSH_OPTIONS_COMPRESSION_S_C,
  SSH_OPTIONS_PROXYCOMMAND,
  SSH_OPTIONS_BINDADDR,
  SSH_OPTIONS_STRICTHOSTKEYCHECK,
  SSH_OPTIONS_COMPRESSION,
  SSH_OPTIONS_COMPRESSION_LEVEL,
  SSH_OPTIONS_KEY_EXCHANGE,
  SSH_OPTIONS_HOSTKEYS,
  SSH_OPTIONS_GSSAPI_SERVER_IDENTITY,
  SSH_OPTIONS_GSSAPI_CLIENT_IDENTITY,
  SSH_OPTIONS_GSSAPI_DELEGATE_CREDENTIALS,
  SSH_OPTIONS_HMAC_C_S,
  SSH_OPTIONS_HMAC_S_C,
  SSH_OPTIONS_PASSWORD_AUTH,
  SSH_OPTIONS_PUBKEY_AUTH,
  SSH_OPTIONS_KBDINT_AUTH,
  SSH_OPTIONS_GSSAPI_AUTH,
  SSH_OPTIONS_GLOBAL_KNOWNHOSTS,
  SSH_OPTIONS_NODELAY,
  SSH_OPTIONS_PUBLICKEY_ACCEPTED_TYPES,
  SSH_OPTIONS_PROCESS_CONFIG,
  SSH_OPTIONS_REKEY_DATA,
  SSH_OPTIONS_REKEY_TIME}

#{enum CInt, ,
  SSH_LOG_NOLOG,
  SSH_LOG_WARNING,
  SSH_LOG_PROTOCOL,
  SSH_LOG_PACKET,
  SSH_LOG_FUNCTIONS}


-- int ssh_options_set(ssh_session session, enum ssh_options_e type, const void *value);
foreign import ccall unsafe "libssh/libssh.h ssh_options_set"
  c'ssh_options_set :: SshSession -> SshOptions -> Ptr a -> IO CInt


-- int ssh_connect(ssh_session session);
foreign import ccall unsafe "libssh/libssh.h ssh_connect"
  c'ssh_connect :: SshSession -> IO CInt
