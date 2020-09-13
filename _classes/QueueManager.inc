QueueManager  CLASS(),TYPE,MODULE('QueueManager.Clw'),LINK('QueueManager.Clw',1)
! Properties
Qref &QUEUE
! Methods
Construct                      PROCEDURE()
Destruct                       PROCEDURE() ,VIRTUAL
Set                            PROCEDURE(*QUEUE pQRef)
Next                           PROCEDURE() ,BYTE,PROC
  END
