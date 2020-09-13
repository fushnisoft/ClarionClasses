      MEMBER()
      MAP
      END
      INCLUDE('CStringClass.inc'),ONCE

CStringClass.Construct PROCEDURE
  CODE
  SELF.bufferSize  = DEFAULT_CS_BUFFER_SIZE
  SELF.CS         &= New(CSTRING(SELF.bufferSize))
CStringClass.Destruct PROCEDURE
  CODE
  Dispose(SELF.cs)
  Dispose(SELF.textToQueueQ)

CStringClass.Cat                PROCEDURE(STRING pStr) !,*CSTRING,PROC
newLen                       LONG,AUTO
oldCS                        &CSTRING
  CODE
  newLen = Len(pStr)
  IF (newLen+SELF.strLength+2) > SELF.newStrSize
    ! Only grow the internal string if the result of the cat will be larger than the string currently is.
    ! The reason for the "+2" is because this is used in the string slicing outside this IF. Without this matching +2 there is potential for an out of bounds slice which would be bad!

    ! Save a temporary copy of the old string so we can us it in the concatination after we have grown it!
    ! Note the commented out bits? This is a minor optimisation and seems to be memory safe!
    !oldCS &= New(CSTRING(SELF.strLength+1))
    oldCS &= (Address(SELF.CS))
    !Dispose(SELF.CS)

    SELF.newStrSize = newLen + SELF.strLength + 1 + SELF.bufferSize
    SELF.CS &= New(CSTRING(SELF.newStrSize))
    SELF.CS = oldCS
    Dispose(oldCS)
  END

  ! Append the new string directly to the end of the old one.
  ! And terminate the CSTRING manually (that's what the +1 is for... to include the <0> in the end of the slice!)
  SELF.CS[SELF.strLength+1 : SELF.strLength+newLen+1] = pStr & '<0>'

  ! This is the same as doing "SELF.strLength = Len(SELF.CS)" but the Len() is _really_ slow on large strings. This is much faster!
  SELF.strLength += newLen

  ! This is what it used to be:
  ! SELF.Str(SELF.Str() & s)
  ! It is a nice and neat solution but performance, especially on large strings was terrible!

  RETURN SELF.Str()

CStringClass.CatWithPrefix          PROCEDURE(STRING pStr, STRING pPrefix)
  CODE

  ! loc:LocationList.Cat(Choose(loc:LocationList.Len()>0, ',', '') & LocationCache.cacheQ.key)
  IF SELF.Len()>0
    SELF.Cat(pPrefix & pStr)
  ELSE
    SELF.Cat(pStr)
  END

CStringClass.Str PROCEDURE  (STRING pStr) !,*CSTRING, PROC
  CODE
  IF Len(pStr) > SELF.newStrSize
    ! Only Dispose/New the internal string if the new one requires it.
    ! This might be slightly innefficient in terms of memory usage when the string gets smaller
    ! But it is _vasty_ better for performance when the string gets added to a lot.
    Dispose(SELF.CS)
    SELF.newStrSize = Len(pStr) + 1 + SELF.bufferSize
    SELF.CS &= New(CSTRING(SELF.newStrSize))
  END

  SELF.CS        = pStr
  SELF.strLength = Len(SELF.CS)

  RETURN SELF.CS
CStringClass.Len PROCEDURE  !,LONG
  CODE
  RETURN SELF.strLength
CStringClass.Replace PROCEDURE  (STRING pFind, STRING pReplace) !,*CSTRING,PROC ! Declare Procedure
! FindString , ReplaceWith
locate                       LONG,AUTO
lastLocate                   LONG
  CODE
  LOOP
    locate = InString(Upper(pFind), Upper(SELF.Str()), 1, lastLocate+1)
    IF ~locate
      BREAK
    END

    ! So we dont end up having recursive replacement.
    lastLocate = locate + Len(pReplace)-1

    SELF.Str(Sub(SELF.Str(), 1, locate-1)                  & |
             pReplace                                      & |
             Sub(SELF.Str(), locate+Len(pFind), SELF.Len())    |
             )
  END

  RETURN SELF.Str()
CStringClass.Str PROCEDURE  () !,*CSTRING            3
  CODE
  RETURN SELF.CS
!------------------------------------------------------------------------------
CStringClass.Contains PROCEDURE  (STRING pFind, BYTE pCaseSensitive=TRUE) !,BYTE ! Declare Procedure
! Returns a value (TRUE) indicating whether the specified String occurs within this string.
! Second parameter defaults to a case sensitive search.
  CODE
  IF pCaseSensitive = TRUE
    IF InString(pFind, SELF.Str(), 1 , 1) > 0
      RETURN TRUE
    END
  ELSE
    IF InString(Lower(pFind), SELF.Lower(), 1 , 1) > 0
      RETURN TRUE
    END
  END

  RETURN FALSE
CStringClass.Lower PROCEDURE  () !,STRING
! Returns a "Lowered" version of the self.cs doesnt change the self.cs
  CODE
  RETURN Lower(SELF.CS)
CStringClass.SubString PROCEDURE  (LONG pPosition, LONG pLength) !,STRING,PROC ! Declare Procedure
  CODE
  RETURN Sub(SELF.Str(), pPosition, pLength)
CStringClass.ToLower PROCEDURE  () !,*CSTRING,PROC
! Converts this string to lowercase and returns the converted string

  CODE
  RETURN SELF.Str(SELF.Lower())
CStringClass.ToUpper PROCEDURE  () !,*CSTRING,PROC
! Converts this string to uppercase and returns the converted string

  CODE
  RETURN SELF.Str(SELF.Upper())

CStringClass.Trim                   PROCEDURE (<STRING pPicture>) !,*CSTRING,PROC ,VIRTUAL
  CODE
  IF Omitted(pPicture) = FALSE AND pPicture <> ''
    SELF.Str(Clip(Left(Format(SELF.Str(), pPicture))))
  ELSE
    SELF.Str(Clip(Left(SELF.Str())))
  END
  RETURN SELF.Str()

CStringClass.Upper PROCEDURE  () !,STRING
  CODE
  RETURN Upper(SELF.Str())
CStringClass.IndexOf PROCEDURE  (STRING pValue, BYTE pCaseSensitive=FALSE) !,LONG ! Declare Procedure
! Returns the index of the first occurence of the parameter (pValue) found within the SELF.CS
! zero if it is not found
  CODE
  IF pCaseSensitive = TRUE
    RETURN InString(pValue, SELF.Str(), 1 , 1)
  ELSE
    RETURN InString(Lower(pValue), SELF.Lower(), 1 , 1)
  END
CStringClass.FoundIn PROCEDURE  (STRING pValue, BYTE pCaseSensitive=FALSE) !,BYTE ! Declare Procedure
! Returns TRUE if the first parameter (pValue) is found within the SELF.CS
! FALSE if it is no
  CODE
  IF SELF.IndexOf(pValue, pCaseSensitive) > 0
    RETURN TRUE
  ELSE
    RETURN FALSE
  END
CStringClass.SetBuffer PROCEDURE  (LONG pNewBuffer)
  CODE
  SELF.bufferSize = pNewBuffer
CStringClass.EscapeXml PROCEDURE  (<STRING pStr>) !,STRING ! Declare Procedure
CS CStringClass
  CODE
  IF Omitted(pStr)=FALSE
    CS.Str(pStr)
  ELSE
    ! Make a copy so we don't alter the original
    CS.Str(SELF.Str())
  END
  CS.Replace('&', '&amp;')
  CS.Replace('<', '&lt;')
  CS.Replace('>', '&gt;')
  CS.Replace('"', '&quot;')
  CS.Replace('''', '&apos;')

  RETURN CS.Str()

CStringClass.Set PROCEDURE

  CODE
  Clear(SELF.nextPointer)
!!! <summary>
!!! Generated from procedure template - Source
!!! Returns the "next" item in a delimited list
!!! </summary>
CStringClass.Next PROCEDURE  (<STRING pDelimiter>) !,STRING ! Declare Procedure
delimiter          CSTRING(21)
lastPointer        LONG
  CODE
  IF SELF.nextPointer = SELF.Len()
    RETURN ''
  END

  IF Omitted(pDelimiter) OR pDelimiter = ''
    delimiter = '<32>'
  ELSE
    delimiter = pDelimiter
  END

  lastPointer = Choose(SELF.nextPointer=0, 1, SELF.nextPointer+Len(delimiter))

  SELF.nextPointer = Instring(delimiter, SELF.cs , 1 , lastPointer+Len(delimiter))

  IF SELF.nextPointer = 0
    SELF.nextPointer = SELF.Len()+Len(delimiter)
  END

  RETURN Sub(SELF.cs, lastPointer, SELF.nextPointer-lastPointer)

CStringClass.TextToQueue            PROCEDURE(<STRING pDefaultIfEmpty>, <STRING pDelimiter>) !,VIRTUAL
ptr                          LONG
delimiter                    CSTRING(255)
  CODE
  ! Taken from vsa_str.clw (ABCFree classes)
  ! I have added an optional parameter of pDelimiter to allow for different kinds of text.
  IF Omitted(pDelimiter) OR pDelimiter = ''
    delimiter = '|'
  ELSE
    delimiter = pDelimiter
  END
  IF SELF.textToQueueQ &= NULL
    SELF.textToQueueQ &= New(core_TextToQueueType)
  END
  Free(SELF.textToQueueQ)
  LOOP
    IF ~SELF.cs
      BREAK
    END
    ptr=InString(delimiter, SELF.cs, 1)
    IF ptr
      SELF.textToQueueQ.text = Sub(SELF.cs, 1, ptr-1)
      SELF.cs = Sub(SELF.cs, ptr+Len(delimiter), Len(Clip(SELF.cs))-ptr)
    ELSE
      SELF.textToQueueQ.text = Clip(SELF.cs)
      SELF.cs = ''
    END
    Add(SELF.textToQueueQ)
  END
  IF Omitted(pDefaultIfEmpty) = FALSE
    IF ~Records(SELF.textToQueueQ)
      SELF.textToQueueQ.text=pDefaultIfEmpty
      Add(SELF.textToQueueQ)
    END
  END

CStringClass.JoinQField             PROCEDURE (*QUEUE pQ, *? pQueueField, STRING pDelimiter, <STRING pQuotes>)
  CODE
  SELF.Str('')
  Get(pQ, 0)
  LOOP
    Get(pQ, Pointer(pQ)+1)
    IF ErrorCode()
      BREAK
    END
    SELF.Cat(|
      Choose(SELF.Len()>0, pDelimiter, '') & |
      Choose(Omitted(pQuotes)=FALSE, pQuotes,'') & |
      pQueueField & |
      Choose(Omitted(pQuotes)=FALSE, pQuotes,''))
  END

CStringClass.CountOccurences        PROCEDURE(STRING pFind) !,LONG
TempCS CStringClass
  CODE
  TempCS.Str(SELF.Str())
  TempCS.Replace(pFind, '')
  RETURN (SELF.Len() - TempCS.Len())/Len(pFind)