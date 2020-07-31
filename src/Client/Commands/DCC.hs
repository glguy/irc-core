{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.DCC
Description : DCC command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.DCC (dccCommands) where

import           Client.Commands.Arguments.Spec
import           Client.Commands.Chat (cmdCtcp)
import           Client.Commands.TabCompletion
import           Client.Commands.Types
import           Client.Configuration
import           Client.State
import           Client.State.DCC
import           Client.State.Focus
import           Control.Applicative
import qualified Control.Concurrent.Async as Async
import           Control.Lens
import           System.Directory (doesDirectoryExist)
import           System.FilePath ((</>))

dccCommands :: CommandSection
dccCommands = CommandSection "DCC"

  [ Command
      (pure "dcc")
      (liftA2 (,) (optionalArg (simpleToken "[accept|cancel|clear|resume]"))
                               optionalNumberArg)
      "Main access to the DCC subsystem with the following subcommands:\n\n\
       \  /dcc           : Access to a list of pending offer and downloads\n\
       \  /dcc accept #n : start downloading the #n pending offer\n\
       \  /dcc resume #n : same as accept but appending to the file on `download-dir`\n\
       \  /dcc clear  #n : remove the #n offer from the list \n\
       \  /dcc cancel #n : cancel the download #n \n\n"
    $ ClientCommand cmdDcc noClientTab
  ]

-- | Implementation of @/dcc [(cancel|accept|resume)] [key]
cmdDcc :: ClientCommand (Maybe String, Maybe Int)
cmdDcc st (Nothing, Nothing) = commandSuccess (changeSubfocus FocusDCC st)
cmdDcc st (Just cmd, Just key) = checkAndBranch st cmd key
cmdDcc st _ = commandFailureMsg "Invalid syntax" st

checkAndBranch :: ClientState -> String -> Int -> IO CommandResult
checkAndBranch st cmd key
  | isCancel, NotExist <- curKeyStatus
      = commandFailureMsg "No such DCC entry" st
  | isCancel, curKeyStatus == Pending
      = commandSuccess
      $ set (clientDCC . dsOffers . ix key . dccStatus) UserKilled st
  | isCancel, curKeyStatus /= Downloading
      = commandFailureMsg "Transfer already stopped" st
  | isCancel = Async.cancel threadId *> commandSuccess st

  | isClear, NotExist <- curKeyStatus
      = commandFailureMsg "No such DCC entry" st
  | isClear, curKeyStatus `elem` [Downloading, Pending]
      = commandFailureMsg "Cancel the download first" st
  | isClear = commandSuccess
            $ set (clientDCC . dsOffers    . at key) Nothing
            $ set (clientDCC . dsTransfers . at key) Nothing st

  | isAcceptOrResume, curKeyStatus `elem` alreadyAcceptedSet
      = commandFailureMsg "Offer already accepted" st
  | isAcceptOrResume, NotExist <- curKeyStatus
      = commandFailureMsg "No such DCC entry" st
  | isAcceptOrResume
      = do isDirectory <- doesDirectoryExist downloadPath
           msize       <- getFileOffset downloadPath
           case (isDirectory, msize, cmd, mcs) of
             (True, _, _, _)      -> commandFailureMsg "DCC transfer would overwrite a directory" st
             (_, Nothing, _, _)   -> acceptOffer -- resume from 0 is accept
             (_, _, "accept", _)  -> acceptOffer -- overwrite file
             (_, Just size, "resume", Just cs) -> resumeOffer size cs
             _ -> commandFailureMsg "Unknown case" st

  | otherwise = commandFailureMsg "Invalid syntax" st
  where
    -- General
    isAcceptOrResume = cmd `elem` ["accept", "resume"]
    isCancel         = cmd == "cancel"
    isClear          = cmd == "clear"
    dccState         = view clientDCC st
    curKeyStatus     = statusAtKey key dccState
    alreadyAcceptedSet = [ CorrectlyFinished, UserKilled, LostConnection
                         , Downloading]

    -- For cancel, other cases handled on the guards
    threadId = st ^?! clientDCC . dsTransfers . ix key . dtThread . _Just

    -- Common values for resume or accept
    Just offer   = view (clientDCC . dsOffers . at key) st -- guarded exist
    updChan      = view clientDCCUpdates st
    downloadDir  = view (clientConfig . configDownloadDir) st
    downloadPath = downloadDir </> _dccFileName offer
    mcs          = preview (clientConnection (_dccNetwork offer)) st

    -- Actual workhorses for the commands
    acceptOffer =
        do newDCCState <- supervisedDownload downloadDir key updChan dccState
           commandSuccess (set clientDCC newDCCState st)

    resumeOffer size cs =
        let newOffer = offer { _dccOffset = size }
            (target, txt) = resumeMsg size newOffer
            st' = set (clientDCC . dsOffers . at key) (Just newOffer) st
        in cmdCtcp cs st' (target, "DCC", txt)
