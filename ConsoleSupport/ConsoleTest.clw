  PROGRAM
  Include('ConsoleSupport.inc'),ONCE
  MAP
  END

Console   ConsoleSupport
  CODE
  IF Console.Init() 
    Halt()
  END

  Console.WriteLine('*** Clarion console app! ***')
  ! =======================
  ! Do your stuff in here
  ! =======================

  ! =======================
  Console.ReadKey()