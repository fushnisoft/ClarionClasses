  MEMBER()

  MAP
  END
  INCLUDE('EQUATES.CLW'),ONCE
  INCLUDE('QueueManager.inc'),ONCE

QueueManager.Construct                      PROCEDURE()
  CODE

QueueManager.Destruct                       PROCEDURE()
  CODE

QueueManager.Set                            PROCEDURE(*QUEUE pQRef)
  CODE
  SELF.QRef &= pQRef
  Get(SELF.QRef, 0)

QueueManager.Next                           PROCEDURE() !,BYTE,PROC
  CODE
  Get(SELF.QRef, Pointer(SELF.QRef)+1)
  IF ErrorCode()
    Clear(SELF.QRef)
    RETURN Level:Notify
  ELSE
    RETURN Level:Benign
  END
