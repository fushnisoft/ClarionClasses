  INCLUDE('CheckErrorClass.inc'),ONCE
  PRAGMA('project(#pragma link(C%V%DF%X%.LIB))'
  INCLUDE('DynFile.inc'),ONCE

SqlCommand  CLASS(CStringClass),TYPE,MODULE('SqlCommand.Clw'),LINK('SqlCommand.Clw',1)
lastFileError               CSTRING(255)
lastFileErrorCode           CSTRING(255)
isManualConnection          BYTE
DynFile                     &DynFile,PRIVATE
TheFile                     &FILE,PRIVATE
Errors                      &CheckErrorClass
Init                        PROCEDURE(STRING pOptions, STRING pOwner)
ExecuteReader               PROCEDURE() ,STRING,PROC
ExecuteNonQuery             PROCEDURE(BYTE pSilent=TRUE) ,BYTE,PROC
SetLastError                PROCEDURE(<STRING pFileError>, <STRING pFileErrorCode>)
Open                        PROCEDURE(BYTE pIsManualConnect=TRUE)
Close                       PROCEDURE()
Read                        PROCEDURE() ,STRING
Construct                   PROCEDURE()
Destruct                    PROCEDURE() ,VIRTUAL
  END
