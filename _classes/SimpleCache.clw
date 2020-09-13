  MEMBER()

  MAP
ToBase64              PROCEDURE(string in), string
FromBase64            PROCEDURE(string in), string
Take24                PROCEDURE(byte h,byte m,byte l,*string Into)
Take32                PROCEDURE(*byte h,*byte m,*byte l,*string SFrom),proc,byte
  END
  INCLUDE('EQUATES.CLW'),ONCE
  INCLUDE('SimpleCache.inc'),ONCE

Encode STRING('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=')

SimpleCache.Construct                      PROCEDURE()
  CODE
  SELF.cacheQ &= NEW(cacheQ_Type)

SimpleCache.Destruct                       PROCEDURE()

  CODE
  SELF.ClearCache()
  Dispose(SELF.cacheQ)

SimpleCache.Contains                       PROCEDURE(STRING pKey) !,BYTE
  CODE

  SELF.cacheQ.key = pKey
  Get(SELF.cacheQ, SELF.cacheQ.key)
  IF ErrorCode()
    RETURN FALSE
  ELSE
    RETURN TRUE
  END

SimpleCache.Add                            PROCEDURE(STRING pKey, STRING pValue, BYTE pEncoding=ENCODING_TYPE:NONE)
  CODE

  SELF.cacheQ.key = pKey
  SELF.cacheQ.value &= New(CStringClass)
  CASE pEncoding
  OF ENCODING_TYPE:NONE
    SELF.cacheQ.value.Str(pValue)
  OF ENCODING_TYPE:BASE64
    SELF.cacheQ.value.Str(ToBase64(pValue))
  END
  SELF.cacheQ.encodingType = pEncoding
  Add(SELF.cacheQ)

SimpleCache.AddOrUpdate                    PROCEDURE(STRING pKey, STRING pValue, BYTE pEncoding=ENCODING_TYPE:NONE)
 CODE
 IF SELF.Contains(pKey)

  SELF.cacheQ.key = pKey
  CASE pEncoding
  OF ENCODING_TYPE:NONE
    SELF.cacheQ.value.Str(pValue)
  OF ENCODING_TYPE:BASE64
    SELF.cacheQ.value.Str(ToBase64(pValue))
  END
  SELF.cacheQ.encodingType = pEncoding

  Put(SELF.cacheQ)
 ELSE
   SELF.Add(pKey, pValue, pEncoding)
 END

SimpleCache.Appendx                         PROCEDURE(STRING pKey, STRING pValue, BYTE pEncoding=ENCODING_TYPE:NONE, <STRING pDelimiter>)
  CODE

  SELF.AddOrUpdate(pKey|
    , SELF.Get(pKey) & |
      Choose(Omitted(pDelimiter), '<13,10>', pDelimiter) & |
      pValue|
    , pEncoding)

SimpleCache.Get                            PROCEDURE(STRING pKey) !,STRING
  CODE

  SELF.cacheQ.key = pKey
  Get(SELF.cacheQ, SELF.cacheQ.key)
  IF ErrorCode()
    Clear(SELF.cacheQ)
    RETURN ''
  ELSE
    RETURN SELF.CurrentValue(SELF.cacheQ.key)
  END

SimpleCache.ClearCache                      PROCEDURE() !,VIRTUAL
QMgr QueueManager
  CODE

  QMgr.Set(SELF.cacheQ)
  LOOP UNTIL QMgr.Next()
    Dispose(SELF.cacheQ.value)
    SELF.cacheQ.value &= NULL
  END
  Free(SELF.cacheQ)
  Clear(SELf.cacheQ)

SimpleCache.Set                            PROCEDURE()
  CODE
  Get(SELF.cacheQ, 0)

SimpleCache.Next                           PROCEDURE() !,BYTE
  CODE
  Get(SELF.cacheQ, Pointer(SELF.cacheQ)+1)
  IF ErrorCode()
    Clear(SELF.cacheQ)
    RETURN Level:Notify
  ELSE
    RETURN Level:Benign
  END
  
SimpleCache.CurrentValue                   PROCEDURE(STRING pKey) !,STRING
  CODE

  IF SELF.cacheQ.key = pKey
    CASE SELF.cacheQ.encodingType
    OF ENCODING_TYPE:NONE
      RETURN SELF.cacheQ.value.Str()
    OF ENCODING_TYPE:BASE64
      RETURN FromBase64(SELF.cacheQ.value.Str())
    END
  END

  ! Default return path
  RETURN ''

SimpleCache.Delete                         PROCEDURE(STRING pKey)
  CODE
  IF SELF.Contains(pKey)
    Dispose(SELF.cacheQ.value)
    SELF.cacheQ.value &= NULL
    Delete(SELF.cacheQ)
  END

SimpleCache.CloneTo                        PROCEDURE(*SimpleCache pTargetCache)
  CODE

  ! It's slow and ugly but should work?
  SELF.Set()
  LOOP UNTIL SELF.Next() <> Level:Benign
    pTargetCache.Add(SELF.cacheQ.key, SELF.CurrentValue(SELF.cacheQ.key), SELF.cacheQ.encodingType)
  END
  RETURN

! Lifted direct from cpxml
ToBase64 procedure(string inv)
i signed,auto
bLK signed,auto
outv &string, auto
base64String any
sz  UNSIGNED
  code
  sz=LEN(inv)
  IF(sz=0)
    RETURN ''
  END
  outv &= new string(size(inv) * 3)
  blk = len(inv)/3
  loop i = 1 to blk
    Take24(val(inv[I*3-2]),val(inv[I*3-1]),val(inv[I*3]),outv[I*4-3:I*4])
  end
  if blk * 3 < len(inv)
    if blk *3 + 1 = len(inv)
      Take24(val(inv[len(inv)]),0,0,outv[blk*4+1:blk*4+4])
      outv[blk*4+3] = '='
      outv[blk*4+4] = '='
    else
      Take24(val(inv[len(inv)-1]),val(inv[len(inv)]),0,outv[blk*4+1:blk*4+4])
      outv[Blk*4+4] = '='
    end
    base64String = outv[1 : blk * 4 + 4]
  else
    base64String = outv[1 : blk * 4]
  end
  dispose(outv)
  return base64String
 
FromBase64 procedure(string inv)

f signed(1)
store string(4)
sh byte(0)
outf signed(1)
b  byte,dim(3)
n  byte,auto
outv &string, auto
base64String any

  code
  if inv = ''
    return ''
  end
  outv &= new string(size(inv) * 3)
  loop while f <= len(inv)
    if instring(inv[f],encode)
      sh += 1
      store[sh] = inv[f]
      if sh = 4
        n = Take32(b[1],b[2],b[3],store)
        outv[outf] = chr(b[1])
        outf += 1
        if n = 1 then break .
        outv[outf] = chr(b[2])
        outf += 1
        if n = 2 then break .
        outv[outf] = chr(b[3])
        outf += 1
        sh = 0
      end
    end
    f += 1
  end
  base64String = outv[1 : outf - 1]
  dispose(outv)
  return base64String

Take24 procedurE(byte h, byte m, byte l, *string into)
b6 byte,auto
   code
   ! First 6 bits? What does the 'high bit is counted first' expression mean?
   ! I'm assuming top 6 bits of h
   b6 = bshift(h,-2)
   into[1] = encode[B6+1]
   ! Second 6 bits become bottom 2 of h (up 4) and top 4 of m (down 4)
   b6 = bor(band(bshift(h,4),030H),bshift(m,-4))
   into[2] = encode[B6+1]
   ! Third 6 bits are bottom 4 of m (up two) and top 2 of l (down 6)
   b6 = bor(band(bshift(m,2),03CH),bshift(l,-6))
   into[3] = encode[B6+1]
   ! Last 6 come from bottom 6 of l
   into[4] = encode[band(l,03FH)+1]
 
Take32 procedure(*byte h, *byte m, *byte l, *string sfrom)
buff byte,dim(4),auto
i byte,auto
  code
  loop I = 1 to 4
    buff[I] = instring(sfrom[I],encode)
?   assert(buff[I])
    buff[I] -= 1
  end
? assert(bufF[1]<>64)
? assert(buff[2]<>64)
  ! Whole of first 6 bits up two and first two of second (down 4)
  h = bor(bshift(buff[1],2),bshift(buff[2],-4))
  if buff[3] = 64 then return 1 .
  ! Middle is bottom 4 bits of second (up 4) and top 4 bits of third (down 2)
  m = bor(bshift(buff[2],4),bshift(buff[3],-2))
  if buff[4] = 64 then return 2 .
  ! Bottom is bottom two bits of third (up 6) and whole of fourth
  l = bor(bshift(buff[3],6),buff[4])
  return 3

