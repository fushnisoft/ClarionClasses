# How to Run the Examples

Examples are setup for Clarion10. If you use an earlier version then you may have redirection issues but otherwise the classes themselves should work just the same.

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

As mentioned on the [ClarionHub!](http://clarionhub.com/t/buttontiles-class/474)

Turn buttons into "tiles". This only supports very basic "button" functionality but it is pretty neat I think :)

At *design time* you see this:

![ButtonTiles DesignTime](/_docs/ButtonTiles_DesignTime.png?raw=true)

At *run time* you see this:

![ButtonTiles RunTime](/_docs/ButtonTiles_RunTime.png?raw=true)

The code looks like this:

```
  Tiles.Init(SELF, 'Segoe UI')
  Tiles.AddButtonMimic(?ButtonUsers, 0C67200h)
  Tiles.AddButtonMimic(?ButtonDashboard, 0998500h)
  Tiles.AddButtonMimic(?ButtonDownload, 0525252h)
  Tiles.AddButtonMimic(?ButtonHelp, 00070CCh)
  Tiles.AddButtonMimic(?ButtonExit, 038703Eh)
```
Updated to no longer require the "hot" color. This is calculated using the MixColors from CWUTIL.
The code has also been refactored to allow it to track the underling button position and respond to changes via the `RefreshTile` method.
Oh yeah and there is also a dark color on MouseDown too.

# ClarionClasses
Some handy classes that I use.

## CStringClass

Just look at the code, it should be pretty self explanatory.

## ConsoleSupport

Make sure to have CUI in your EXP
```
NAME 'CONSOLETEST' CUI
```

Then you can use it something like this:

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

Sort of based on the .NET version, this is a simple class to execute a command either with a single value response `ExecuteReader` or just simple execute `ExecuteNonQuery`.

*Example usage*:

```
Sql SqlCommand
  CODE

  Sql.Init(GLO:FILE:TurboSQLOptions, GLO:FILE:OwnerString)
  Sql.Str('CALL MyStoredProc(123,''Somestring'')')

  Message('Result=' & Sql.ExecuteReader())
```
