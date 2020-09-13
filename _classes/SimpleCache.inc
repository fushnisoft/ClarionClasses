  Include('CStringClass.inc'),ONCE
  Include('QueueManager.inc'),ONCE
cacheQ_Type QUEUE,TYPE
key           CSTRING(255)
value         &CStringClass
encodingType  BYTE
  END

ENCODING_TYPE ITEMIZE,PRE()
NONE    EQUATE(0)
BASE64  EQUATE
  END

SimpleCache  CLASS(),TYPE,MODULE('SimpleCache.Clw'),LINK('SimpleCache.Clw',1)
! Properties
cacheQ                         &cacheQ_Type
! Methods
Construct                      PROCEDURE()
Destruct                       PROCEDURE() ,VIRTUAL
Contains                       PROCEDURE(STRING pKey) ,BYTE
Add                            PROCEDURE(STRING pKey, STRING pValue, BYTE pEncoding=ENCODING_TYPE:NONE)
AddOrUpdate                    PROCEDURE(STRING pKey, STRING pValue, BYTE pEncoding=ENCODING_TYPE:NONE)
Appendx                         PROCEDURE(STRING pKey, STRING pValue, BYTE pEncoding=ENCODING_TYPE:NONE, <STRING pDelimiter>)
Get                            PROCEDURE(STRING pKey) ,STRING
Delete                         PROCEDURE(STRING pKey)
ClearCache                     PROCEDURE() ,VIRTUAL
Set                            PROCEDURE()
Next                           PROCEDURE() ,BYTE
CurrentValue                   PROCEDURE(STRING pKey) ,STRING
CloneTo                        PROCEDURE(*SimpleCache pTargetCache)
  END
