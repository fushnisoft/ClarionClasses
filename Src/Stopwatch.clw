  MEMBER()

  MAP
  END

  INCLUDE('Stopwatch.inc'),ONCE

Stopwatch.Start               PROCEDURE()
  CODE

  IF SELF.isStarted = FALSE
    SELF.startDate = Today()
    SELF.startTime = Clock()
    SELF.isStarted = TRUE
  END

Stopwatch.StartNew            PROCEDURE()
  CODE

  SELF.Reset()
  SELF.Start()

Stopwatch.Stop                PROCEDURE()
  CODE

  SELF.isStarted = FALSE

Stopwatch.Reset               PROCEDURE()
  CODE

  SELF.Stop()
  Clear(SELF.startDate)
  Clear(SELF.startTime)

Stopwatch.Restart             PROCEDURE()
  CODE
  SELF.Reset()
  SELF.Start()

Stopwatch.Elapsed             PROCEDURE() !,REAL
dayTicks    REAL
timeTicks   LONG
  CODE

  dayTicks = (Today() - SELF.startDate) * TIME:Day
  timeTicks = Clock() - SELF.startTime
  RETURN (dayTicks + timeTicks)

Stopwatch.ToString            PROCEDURE(<STRING pPicture>) !,STRING
dayTicks    REAL
dayString   CSTRING(21)
  CODE

  dayTicks = (Today() - SELF.startDate) * TIME:Day
  CASE dayTicks
  OF TIME:Day
    dayString = '1 Day, '
  OF 0
    dayString = ''
  ELSE
    dayString = dayTicks/TIME:Day & ' Days, '
  END

  RETURN dayString & Format(SELF.Elapsed()-dayTicks, Choose(Omitted(pPicture)=TRUE, '@T04B', pPicture))
