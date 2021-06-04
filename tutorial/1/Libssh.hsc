
module Libssh where

import Foreign (Ptr)
import Foreign.C (CInt (..), CUInt (..), CUChar (..), CString, peekCString, Errno (..), getErrno)

import String (c'strerror)

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


sshRcOK :: CInt
sshRcOK = #const SSH_OK

sshRcError :: CInt
sshRcError = #const SSH_ERROR

sshRcAgain :: CInt
sshRcAgain = #const SSH_AGAIN

sshRcEOF :: CInt
sshRcEOF = #const SSH_EOF

-- tricky handling `#define`s with hsc-enum
#{enum CInt, ,
  SSH_OK,
  SSH_ERROR,
  SSH_AGAIN,
  SSH_EOF}


-- int ssh_connect(ssh_session session);
-- for blocking case, `safe` is better?
foreign import ccall unsafe "libssh/libssh.h ssh_connect"
  c'ssh_connect :: SshSession -> IO CInt

-- const char *ssh_get_error(void *error);
foreign import ccall unsafe "libssh/libssh.h ssh_get_error"
  c'ssh_get_error :: Ptr a -> IO CString

sshGetError :: Ptr a -> IO String
sshGetError p = peekCString =<< c'ssh_get_error p

sshSessionGetError :: SshSession -> IO String
sshSessionGetError = sshGetError

-- void ssh_disconnect(ssh_session session);
-- for blocking case, `safe` is better?
foreign import ccall unsafe "libssh/libssh.h ssh_disconnect"
  c'ssh_disconnect :: SshSession -> IO ()

-- socket_t ssh_get_fd(ssh_session session);
foreign import ccall unsafe "libssh/libssh.h ssh_get_fd"
  c'ssh_get_fd :: SshSession -> IO CInt

data ISshKey
type SshKey = Ptr ISshKey

-- int ssh_get_server_publickey(ssh_session session, ssh_key *key);
foreign import ccall unsafe "libssh/libssh.h ssh_get_server_publickey"
  c'ssh_get_server_publickey :: SshSession -> Ptr SshKey -> IO CInt


newtype SshPublickeyHashType = SshPublickeyHashType { codePublickeyHashType :: CInt }

#{enum SshPublickeyHashType, SshPublickeyHashType,
  SSH_PUBLICKEY_HASH_SHA1,
  SSH_PUBLICKEY_HASH_MD5,
  SSH_PUBLICKEY_HASH_SHA256}

type Hash = Ptr CUChar

-- int ssh_get_publickey_hash(const ssh_key key,
--                            enum ssh_publickey_hash_type type,
--                            unsigned char **hash,
--                            size_t *hlen);
foreign import ccall unsafe "libssh/libssh.h ssh_get_publickey_hash"
  c'ssh_get_publickey_hash :: SshKey -> SshPublickeyHashType -> Ptr Hash -> Ptr CUInt -> IO CInt

-- void ssh_key_free (ssh_key key);
foreign import ccall unsafe "libssh/libssh.h ssh_key_free"
  c'ssh_key_free :: SshKey -> IO ()


newtype SshKnownHosts = SshKnownHosts { codeKnownHosts :: CInt } deriving (Eq, Show)

#{enum SshKnownHosts, SshKnownHosts,
  SSH_KNOWN_HOSTS_ERROR,
  SSH_KNOWN_HOSTS_NOT_FOUND,
  SSH_KNOWN_HOSTS_UNKNOWN,
  SSH_KNOWN_HOSTS_OK,
  SSH_KNOWN_HOSTS_CHANGED,
  SSH_KNOWN_HOSTS_OTHER}

-- enum ssh_known_hosts_e ssh_session_is_known_server(ssh_session session);
foreign import ccall unsafe "libssh/libssh.h ssh_session_is_known_server"
  c'ssh_session_is_known_server :: SshSession -> IO SshKnownHosts

-- void ssh_clean_pubkey_hash(unsigned char **hash);
foreign import ccall unsafe "libssh/libssh.h ssh_clean_pubkey_hash"
  c'ssh_clean_pubkey_hash :: Ptr Hash -> IO ()

-- void ssh_print_hexa(const char *descr, const unsigned char *what, size_t len);
foreign import ccall unsafe "libssh/libssh.h ssh_print_hexa"
  c'ssh_print_hexa :: CString -> Hash -> CUInt -> IO ()

-- char *ssh_get_hexa(const unsigned char *what, size_t len);
foreign import ccall unsafe "libssh/libssh.h ssh_get_hexa"
  c'ssh_get_hexa :: Hash -> CUInt -> IO CString

-- void ssh_string_free_char(char *s);
foreign import ccall unsafe "libssh/libssh.h ssh_string_free_char"
  c'ssh_string_free_char :: CString -> IO ()

sshGetHexA :: Hash -> CUInt -> IO String
sshGetHexA hash len = do
  hexa <- c'ssh_get_hexa hash len
  x <- peekCString hexa
  c'ssh_string_free_char hexa
  return x

-- int ssh_session_update_known_hosts(ssh_session session);
foreign import ccall unsafe "libssh/libssh.h ssh_session_update_known_hosts"
  c'ssh_session_update_known_hosts :: SshSession -> IO CInt

-- on success, Nothing.
-- on error, Just error message string.
sshSessionUpdateKnownHosts :: SshSession -> IO (Maybe String)
sshSessionUpdateKnownHosts session = do
  rc <- c'ssh_session_update_known_hosts session
  Errno eno <- getErrno

  if rc < 0
    then fmap Just $ peekCString =<< c'strerror eno
    else return Nothing

newtype SshAuth = SshAuth { codeAuth :: CInt } deriving (Eq, Ord)

#{enum SshAuth, SshAuth,
  SSH_AUTH_SUCCESS,
  SSH_AUTH_DENIED,
  SSH_AUTH_PARTIAL,
  SSH_AUTH_INFO,
  SSH_AUTH_AGAIN,
  SSH_AUTH_ERROR}

-- int ssh_userauth_password(ssh_session session,
--                           const char *username,
--                           const char *password);
-- username SHOULD be NULL.
-- Most server implementations do not permit changing the username.
foreign import ccall unsafe "libssh/libssh.h ssh_userauth_password"
  c'ssh_userauth_password :: SshSession -> CString -> CString -> IO SshAuth

data ISshChannel
type SshChannel = Ptr ISshChannel

-- ssh_channel ssh_channel_new(ssh_session session);
foreign import ccall unsafe "libssh/libssh.h ssh_channel_new"
  c'ssh_channel_new :: SshSession -> IO SshChannel

-- int ssh_channel_close(ssh_channel channel);
foreign import ccall unsafe "libssh/libssh.h ssh_channel_close"
  c'ssh_channel_close :: SshChannel -> IO CInt

-- void ssh_channel_free(ssh_channel channel);
foreign import ccall unsafe "libssh/libssh.h ssh_channel_free"
  c'ssh_channel_free :: SshChannel -> IO ()

-- int ssh_channel_open_session(ssh_channel channel);
foreign import ccall unsafe "libssh/libssh.h ssh_channel_open_session"
  c'ssh_channel_open_session :: SshChannel -> IO CInt

-- int ssh_channel_request_exec(ssh_channel channel, const char *cmd);
foreign import ccall unsafe "libssh/libssh.h ssh_channel_request_exec"
  c'ssh_channel_request_exec :: SshChannel -> CString -> IO CInt

-- int ssh_channel_read(ssh_channel channel, void *dest, uint32_t count, int is_stderr);
foreign import ccall unsafe "libssh/libssh.h ssh_channel_read"
  c'ssh_channel_read :: SshChannel -> Ptr a -> CUInt -> CInt -> IO CInt

-- int ssh_channel_send_eof(ssh_channel channel);
foreign import ccall unsafe "libssh/libssh.h ssh_channel_send_eof"
  c'ssh_channel_send_eof :: SshChannel -> IO CInt
