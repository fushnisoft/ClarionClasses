! Thanks Mark... https://github.com/MarkGoldberg/CwUnit/blob/master/Libsrc/Time.equ
TIME:Tick   EQUATE(  1)
TIME:Second EQUATE(100 * TIME:Tick)
TIME:Minute EQUATE( 60 * TIME:Second)
TIME:Hour   EQUATE( 60 * TIME:Minute)
TIME:Day    EQUATE( 24 * TIME:Hour  )

Stopwatch  CLASS(),TYPE,MODULE('Stopwatch.Clw'),LINK('Stopwatch.Clw')
startTime           LONG
startDate           LONG
isStarted           BYTE

Start               PROCEDURE()
StartNew            PROCEDURE()
Stop                PROCEDURE()
Reset               PROCEDURE()
Restart             PROCEDURE()
Elapsed             PROCEDURE() ,REAL
ToString            PROCEDURE(<STRING pPicture>) ,STRING
  END
