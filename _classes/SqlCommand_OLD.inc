  INCLUDE('CStringClass.inc'),ONCE

SqlCommand  CLASS(CStringClass),TYPE,MODULE('SqlCommand.Clw'),LINK('SqlCommand.Clw',1)
lastFileError       CSTRING(255)
lastFileErrorCode   CSTRING(255)
Init                PROCEDURE(STRING pOptions, STRING pOwner)
ExecuteReader       PROCEDURE() ,STRING,PROC
ExecuteNonQuery     PROCEDURE(BYTE pSilent=TRUE) ,BYTE,PROC
SetLastError        PROCEDURE(<STRING pFileError> , <STRING pFileErrorCode>)
CheckError          PROCEDURE(<STRING pMessage>) ,BYTE,PROC
  END
