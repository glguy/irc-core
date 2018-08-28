{-|
Module      : Exports
Description : C API function exports
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Entry point into glirc2 from the C API

-}
module Exports () where

import Client.CApi.Exports
import Client.CApi.Types
import Foreign.C

foreign export ccall glirc_send_message       :: Glirc_send_message
foreign export ccall glirc_print              :: Glirc_print
foreign export ccall glirc_inject_chat        :: Glirc_inject_chat
foreign export ccall glirc_list_networks      :: Glirc_list_networks
foreign export ccall glirc_identifier_cmp     :: Glirc_identifier_cmp
foreign export ccall glirc_is_channel         :: Glirc_is_channel
foreign export ccall glirc_is_logged_on       :: Glirc_is_logged_on
foreign export ccall glirc_list_channels      :: Glirc_list_channels
foreign export ccall glirc_list_channel_users :: Glirc_list_channel_users
foreign export ccall glirc_my_nick            :: Glirc_my_nick
foreign export ccall glirc_user_account       :: Glirc_user_account
foreign export ccall glirc_user_channel_modes :: Glirc_user_channel_modes
foreign export ccall glirc_channel_modes      :: Glirc_channel_modes
foreign export ccall glirc_channel_masks      :: Glirc_channel_masks
foreign export ccall glirc_mark_seen          :: Glirc_mark_seen
foreign export ccall glirc_clear_window       :: Glirc_clear_window
foreign export ccall glirc_free_string        :: Glirc_free_string
foreign export ccall glirc_free_strings       :: Glirc_free_strings
foreign export ccall glirc_current_focus      :: Glirc_current_focus
foreign export ccall glirc_set_focus          :: Glirc_set_focus
foreign export ccall glirc_resolve_path       :: Glirc_resolve_path
foreign export ccall glirc_set_timer          :: Glirc_set_timer
foreign export ccall glirc_cancel_timer       :: Glirc_cancel_timer
