  MEMBER()
  MAP
  END
    INCLUDE('CheckErrorClass.inc'),ONCE

CheckErrorClass.CheckError PROCEDURE  (<STRING pMessage>) !,BYTE,PROC
errCode                      LONG
err                          CSTRING(255)
errorString                  CSTRING(255)
MessageCS                    cstringClass
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