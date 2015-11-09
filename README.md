# How to Run the Examples

Examples are setup for Clarion10. If you use an earlier verison then you may have redirection issues but otherwise the classes themselves should work just the same.

The examples use redirection files to point to the common `_classes` directory. 
In order to compile the examples as-is you need to make sure you clone this *entire* repository. Otherwise, make sure to adjust the RED files or to place the classes in the right locations for your environment.

e.g. The TilesExample has this file:

`ClarionClasses/TilesExample/Clarion100.red`

Which is simply an include:

```
{include ..\Clarion100.red}
```

This points back to the RED file in the root which does the real redirection magic :)

# ButtonTiles
Turn buttons into "tiles". This only supports very basic "button" functionality but it is pretty neat I think :)

At *design time* you see this:

![ButtonTiles DesignTime](/_docs/ButtonTiles_DesignTime.png?raw=true)

At *run time* you see this:

![ButtonTiles RunTime](/_docs/ButtonTiles_RunTime.png?raw=true)

The code looks like this:

```
  Tiles.Init(SELF, 'Segoe UI')
  Tiles.AddButtonMimic(?ButtonUsers, 0C67200h, 0D79C4Ch)
  Tiles.AddButtonMimic(?ButtonDashboard, 0998500h, 0B7A94Ch)
  Tiles.AddButtonMimic(?ButtonDownload, 0525252h, 0858585h)
  Tiles.AddButtonMimic(?ButtonHelp, 00070CCh, 04C9ADBh)
  Tiles.AddButtonMimic(?ButtonExit, 038703Eh, 0739A77h)
```

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
