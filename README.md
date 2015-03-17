# ClarionClasses
Some handy classes that I use.

## CStringClass

Just look at the code, it should be pretty self explanitory.

## ConsoleSupport

Make sure to have CUI in your EXP
```
NAME 'CONSOLETEST' CUI
```

Then you can use it somethign like this:

```
  PROGRAM
  Include('ConsoleSupport.inc'),ONCE
  MAP
  END

Console     ConsoleSupport
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
```

## Stopwatch

Based on the .NET Stopwatch Class but only using the precision of Clarion Clock() and Today()

https://msdn.microsoft.com/en-us/library/system.diagnostics.stopwatch(v=vs.110).aspx

Implemented:

* Start
* StartNew
* Stop
* Reset
* Restart
* Elapsed
* ToString

```
  PROGRAM
  Include('Stopwatch.inc'),ONCE
  MAP
  END

SW Stopwatch
    CODE

    SW.Start()

    ! Do some things that take time...
    ! Perhaps Thread.Sleep(10000)

    SW.Stop()

    Message('Elapsed Ticks: ' & SW.Elapsed())
    Message('Time Elapsed: ' & SW.ToString())  ! e.g. 2 days, 00:12:54
```

## SqlCommand

Sort of based on the .NET version, this is a simple class to execute a command either with a single value reponse `ExecuteReader` or just simple execute `ExecuteNonQuery`.

*Example usage*:

```
Sql SqlCommand
  CODE

  Sql.Init(GLO:FILE:TurboSQLOptions, GLO:FILE:OwnerString)
  Sql.Str('CALL MyStoredProc(123,''Somestring'')')

  Message('Result=' & Sql.ExecuteReader())
```
