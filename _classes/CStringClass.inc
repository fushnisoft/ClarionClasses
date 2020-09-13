! vim: set filetype=clarion:
NOT_CASE_SENSITIVE           EQUATE(FALSE)
DEFAULT_CS_BUFFER_SIZE       EQUATE(1024)

core_TextToQueueType         QUEUE,TYPE
text                           cstring(255)
                             END
CStringClass     Class(),Type,Module('CStringClass.Clw'),LINK('CStringClass.Clw',1)
! Properties
cs                           &CSTRING, PROTECTED
bufferSize                   LONG
newStrSize                   LONG ,PROTECTED
strLength                    LONG ,PROTECTED
nextPointer                  LONG
textToQueueQ                 &core_TextToQueueType
! Methods
Cat                    PROCEDURE(STRING pStr) ,*CSTRING,PROC ,VIRTUAL 
CatWithPrefix          PROCEDURE(STRING pStr, STRING pPrefix)
Construct              PROCEDURE()
Contains               PROCEDURE(STRING pFind, BYTE pCaseSensitive=TRUE) ,BYTE ,VIRTUAL 
Destruct               PROCEDURE() ,VIRTUAL 
Len                    PROCEDURE(),LONG ,VIRTUAL 
Lower                  PROCEDURE() ,STRING ,VIRTUAL 
Replace                PROCEDURE(STRING pFind, STRING pReplace) ,*CSTRING,PROC ,VIRTUAL 
Str                    PROCEDURE(STRING pStr) ,*CSTRING, PROC   ,VIRTUAL 
Str                    PROCEDURE() ,*CSTRING ,VIRTUAL 
SubString              PROCEDURE(LONG pPosition, LONG pLength) ,STRING,PROC ,VIRTUAL 
ToLower                PROCEDURE() ,*CSTRING,PROC ,VIRTUAL 
ToUpper                PROCEDURE() ,*CSTRING,PROC ,VIRTUAL 
Trim                   PROCEDURE(<STRING pPicture>) ,*CSTRING,PROC ,VIRTUAL 
Upper                  PROCEDURE() ,STRING ,VIRTUAL 
IndexOf                PROCEDURE(STRING pValue, BYTE pCaseSensitive=FALSE) ,LONG ,VIRTUAL 
FoundIn                PROCEDURE(STRING pValue, BYTE pCaseSensitive=FALSE) ,BYTE ,VIRTUAL 
SetBuffer              PROCEDURE(LONG pNewBuffer)  ,VIRTUAL 
EscapeXml              PROCEDURE(<STRING pStr>) ,STRING ,VIRTUAL 
Set                    PROCEDURE() ,VIRTUAL 
Next                   PROCEDURE(<STRING pDelimiter>) ,STRING ,VIRTUAL 
TextToQueue            PROCEDURE(<STRING pDefaultIfEmpty>, <STRING pDelimiter>) ,VIRTUAL
JoinQField             PROCEDURE(*QUEUE pQ, *? pQueueField, STRING pDelimiter, <STRING pQuotes>)
CountOccurences        PROCEDURE(STRING pFind) ,LONG
                     END 
