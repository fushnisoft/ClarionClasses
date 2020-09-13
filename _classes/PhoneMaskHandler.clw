  MEMBER()
  MAP
  END
    INCLUDE('PhoneMaskHandler.inc'),ONCE

PhoneMaskHandler.Construct                      PROCEDURE()
  CODE
  SELF.FieldCache &= New(SimpleCache)

PhoneMaskHandler.Destruct                       PROCEDURE()
  CODE
  Dispose(SELF.FieldCache)

PhoneMaskHandler.Init                           PROCEDURE(WindowManager pWM, BYTE pEnabled)
thisField       LONG(0)
nextField       LONG(0)
CS              CstringClass
  CODE
  IF NOT pEnabled
    RETURN
  END

  SELF.WM &= pWM
  pWM.AddItem(SELF.WindowComponent)
  ! Find all possible phone entry controls
  LOOP
    thisField=0{Prop:NextField,thisField}
    IF thisField = 0
      BREAK
    END
    IF thisField < 0
      CYCLE
    END
    IF thisField{PROP:Type} <> CREATE:prompt
      CYCLE
    END

    CS.Str(thisField{PROP:Text})
    IF CS.Contains('phone', NOT_CASE_SENSITIVE) OR |
       CS.Contains('mobile', NOT_CASE_SENSITIVE) OR |
       CS.Contains('fax', NOT_CASE_SENSITIVE)

      nextField = (thisField+1)
      IF (nextField){PROP:Type} = CREATE:Entry
        SELF.FieldCache.Add(nextField, '@' & nextField{PROP:Text})
        nextField{PROP:Text} = Choose(CS.Contains('mobile', NOT_CASE_SENSITIVE) , '@K#### ### ###KB','@K## #### ####KB')
        nextField{PROP:IMM} = TRUE ! This enables NewSelection which lets us capture a click on an already focussed entry.
      END
    END
  END

PhoneMaskHandler.TakeFieldEvent                 PROCEDURE(SIGNED pField)
savedPicture        CSTRING(21)
thisEvent           USHORT
  CODE
  IF SELF.FieldCache.Contains(pField)
    ! If this is one of our managed fields and the ctrl key is down...
    IF (Event() = EVENT:NewSelection OR Event() = EVENT:Selected) AND BAND(KEYSTATE(),0200h)
      ! Toggle the input picture
      thisEvent = Event()
      savedPicture = SELF.FieldCache.CurrentValue(pField)
      SELF.FieldCache.AddOrUpdate(pField, pField{PROP:Text})
      pField{PROP:Text} = savedPicture
    END
  END

PhoneMaskHandler.WindowComponent.TakeEvent PROCEDURE() ! ,BYTE
rv              BYTE
  CODE
  rv = PARENT.WindowComponent.TakeEvent()
  SELF.TakeFieldEvent(Field())

  RETURN rv
