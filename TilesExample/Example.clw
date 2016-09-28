  PROGRAM

  INCLUDE('ABERROR.INC'),ONCE
  INCLUDE('ABUTIL.INC'),ONCE
  INCLUDE('ERRORS.CLW'),ONCE
  INCLUDE('KEYCODES.CLW'),ONCE
  INCLUDE('ButtonTiles.inc'),ONCE
  INCLUDE('ABTOOLBA.INC'),ONCE
  INCLUDE('ABWINDOW.INC'),ONCE

  MAP
Main    PROCEDURE   !
  END

GlobalErrorStatus ErrorStatusClass,THREAD
GlobalErrors  ErrorClass                            ! Global error manager
GlobalRequest BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse    BYTE(0),THREAD                        ! Set to the response from the form

  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  Main()


Main  PROCEDURE 

Window  WINDOW('ButtonTiles Example Application'),AT(,,400,56),FONT('Segoe Print',9,,,CHARSET:DEFAULT), |
        RESIZE,GRAY,IMM,SYSTEM
          BUTTON('Browse Users Details'),AT(10,10,80,34),USE(?ButtonUsers),ICON('users_tile.ico')
          BUTTON('Explore Dashboard'),AT(94,10,80,34),USE(?ButtonDashboard),ICON('gauge_tile.ico')
          BUTTON('Download Some Stuff'),AT(177,10,80,34),USE(?ButtonDownload),FONT(,,,FONT:regular+FONT:underline), |
          ICON('web_tile.ico')
          BUTTON('Help'),AT(306,10,40,34),USE(?ButtonHelp),FONT(,,,FONT:regular+FONT:underline),ICON('help_tile.ico')
          BUTTON('Exit'),AT(350,10,40,34),USE(?ButtonExit),FONT(,,,FONT:regular),ICON('exit_tile.ico')
        END

ThisWindow  CLASS(WindowManager)
Init          PROCEDURE(),BYTE,PROC,DERIVED
Kill          PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted  PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent     PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent   PROCEDURE(),BYTE,PROC,DERIVED
            END

Toolbar ToolbarClass
Tiles   ButtonTiles

  CODE
  GlobalResponse =  ThisWindow.Run()

SetButtonPositions    ROUTINE
  ?ButtonExit{PROP:Xpos} = 0{PROP:Width} - ?ButtonExit{PROP:Width} - 10
  Tiles.RefreshTile(?ButtonExit)
  ?ButtonHelp{PROP:Xpos} = ?ButtonExit{PROP:Xpos} - ?ButtonExit{PROP:Width} - 4
  Tiles.RefreshTile(?ButtonHelp)
  

ThisWindow.Init   PROCEDURE

ReturnValue         BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?ButtonUsers
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.Open(Window)                                        ! Open window
  Tiles.Init(SELF, 'Segoe Print')
  Tiles.AddButtonMimic(?ButtonUsers, 0C67200h)
  Tiles.AddButtonMimic(?ButtonDashboard, 0998500h)
  Tiles.AddButtonMimic(?ButtonDownload, 0525252h)
  Tiles.AddButtonMimic(?ButtonHelp, 00070CCh)
  Tiles.AddButtonMimic(?ButtonExit, 038703Eh)
  RETURN ReturnValue


ThisWindow.Kill   PROCEDURE

ReturnValue         BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted   PROCEDURE
ReturnValue                 BYTE,AUTO
Looped                      BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    ReturnValue = PARENT.TakeAccepted()
    IF Accepted(){PROP:Type} = CREATE:button
      Message('Button "' & Accepted(){PROP:Text} & '" Accepted.', 'ButtonTiles Class!') ! You could do this although the icons have no background so it looks odd --> '~' & Accepted(){PROP:Icon})
    END
    CASE ACCEPTED()
    OF ?ButtonExit
      ThisWindow.Update()
      Post(EVENT:CloseWindow)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent  PROCEDURE

ReturnValue             BYTE,AUTO

Looped                  BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    DO SetButtonPositions
    ReturnValue = PARENT.TakeEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent    PROCEDURE

ReturnValue                     BYTE,AUTO

Looped                          BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:DoResize
      !  DO SetButtonPositions
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

