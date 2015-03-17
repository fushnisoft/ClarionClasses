  MEMBER()

  INCLUDE('EQUATES.CLW'),ONCE

  MAP
  END

  INCLUDE('SqlCommand.inc'),ONCE
thisOwnerString CSTRING(1024)
thisSqlOptions CSTRING(1024)

SqlCommand.Init            PROCEDURE(STRING pOptions, STRING pOwner)
  CODE
  thisSqlOptions = pOptions
  thisOwnerString = pOwner

SqlCommand.ExecuteReader     PROCEDURE() !,STRING,PROC
ThisTempTable                FILE,DRIVER('MSSQL', thisSqlOptions),OWNER(thisOwnerString),NAME('dbo.TempTable'),PRE(TEMP),CREATE,BINDABLE,THREAD
Record                         RECORD,PRE()
Result                           CSTRING(1024)
                               END
                             END
RV CStringClass
  CODE

  SELF.SetLastError()
  Open(ThisTempTable)
  SELF.CheckError('Open(ThisTempTable)')

  ThisTempTable{PROP:SQL} = SELF.Str()
  SELF.SetLastError(FileError(), FileErrorCode())

  SELF.CheckError('ThisTempTable{{PROP:SQL}||SQL: ' & SELF.Str())

  Next(ThisTempTable)
  IF ErrorCode()
    Clear(TEMP:Record)
  END
  RV.Str(TEMP:Result)
  Close(ThisTempTable)

  RETURN RV.Str()

SqlCommand.ExecuteNonQuery   PROCEDURE(BYTE pSilent=TRUE) !,BYTE,PROC
ThisTempTable                FILE,DRIVER('MSSQL', thisSqlOptions),OWNER(thisOwnerString),NAME('dbo.TempTable'),PRE(TEMP),CREATE,BINDABLE,THREAD
Record                         RECORD,PRE()
Result                           CSTRING(1024)
                               END
                             END
  CODE

  SELF.SetLastError()
  Open(ThisTempTable)
  IF pSilent=FALSE
    SELF.CheckError('Open(ThisTempTable)')
  END

  ThisTempTable{PROP:SQL} = SELF.Str()
  SELF.SetLastError(FileError(), FileErrorCode())

  IF pSilent=FALSE
    SELF.CheckError('ThisTempTable{{PROP:SQL}||SQL: ' & SELF.Str())
  END

  Close(ThisTempTable)
  IF pSilent=FALSE
    SELF.CheckError('Close(ThisTempTable)')
  END

  RETURN Choose(SELF.lastFileErrorCode>0, level:notify, level:benign)

SqlCommand.SetLastError        PROCEDURE(<STRING pFileError>, <STRING pFileErrorCode>)
  CODE
  SELF.lastFileError       = Choose(Omitted(pFileError),'',pFileError)
  SELF.lastFileErrorCode   = Choose(Omitted(pFileErrorCode),'',pFileErrorCode)


SqlCommand.CheckError PROCEDURE  (<STRING pMessage>) !,BYTE,PROC
errCode                      LONG
err                          CSTRING(255)
errorString                  CSTRING(255)
MessageCS                    CStringClass
  CODE
  errCode     = ErrorCode()
  err         = Choose(ErrorCode()=90, FileErrorCode(), ErrorCode())
  errorString = Choose(ErrorCode()=90, FileError(), Error())
  MessageCS.Str(Clip(pMessage))
  IF MessageCS.Len() > 512
    ! If the message provided is too long then truncate it so that it will hopefully at least fit on the screen!
    MessageCS.Str(Sub(MessageCS.Str(), 1, 255) & |
                  ' <13,10,13,10>        *** message truncated because it was too long ***<13,10,13,10>' & |
                  Sub(MessageCS.Str(), MessageCS.Len()-255, 255))
  END

  IF ~Omitted(pMessage) AND errCode <> NoError
    IF Message(MessageCS.Str() & '||' & |
              'Error: ' & err & '|' & |
              'ErrorCode: ' & errorString, 'An Error Occured!',ICON:Exclamation,BUTTON:IGNORE+BUTTON:ABORT) = BUTTON:ABORT
      Halt()
    END
  END

  RETURN errCode
