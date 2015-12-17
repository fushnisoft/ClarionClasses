  MEMBER()

  MAP
    MODULE('zd_alloc.clw')
      zdMemProcessStart(),NAME('zdMemProcessStart')
      zdMemProcessStop(),NAME('zdMemProcessStop')
    END
  END

  PRAGMA('compile(zd_alloc.clw)')

  INCLUDE('zdMem.inc'),ONCE

zdMem.Construct       PROCEDURE
  CODE
  zdMemProcessStart()

zdMem.Destruct        PROCEDURE
  CODE
  zdMemProcessStop()

zdMem.Test            PROCEDURE()
myStringQueue QUEUE
theNewString &CSTRING
 END
startTime LONG
  CODE
  COMPILE('ENDCOMPILE',DEBUG=1)  !development context
    Message('Warning, you have DEBUG flags enabled for zd_alloc. This dramatically impairs performance, so should be turned off in a release build!')
  ENDCOMPILE
  IF MallocIsDIY
    Message('About to NEW a bunch of string, a bunch of times using the slab allocator defined in zd_alloc!')
  ELSE
    Message('About to NEW a bunch of string, a bunch of times using the normal Clarion RTL malloc!')
  END
  
  startTime = Clock()
  LOOP 1000 TIMES
    LOOP 10000 TIMES
      myStringQueue.theNewString &= NEW(CSTRING(10000))
      myStringQueue.theNewString = All('*', 10000-1)
      Add(myStringQueue)
    END

    Get(myStringQueue, 0)
    LOOP
      Get(myStringQueue, Pointer(myStringQueue) + 1)
      IF ErrorCode()
        BREAK
      END
      Dispose(myStringQueue.theNewString)
    END
    Free(myStringQueue)
  END

  IF MallocIsDIY
    Message('Processing time (zd_alloc): ' & Format(Clock()-startTime, @T04))
  ELSE
    Message('Processing time (RTL): ' & Format(Clock()-startTime, @T04))
  END

