  PROGRAM

  INCLUDE('ABWINDOW.INC'),ONCE
  INCLUDE('ButtonTile.inc'),ONCE
  INCLUDE('TileManager.inc'),ONCE

GlobalErrorStatus ErrorStatusClass,THREAD
GlobalErrors      ErrorClass
GlobalRequest     BYTE(0),THREAD
GlobalResponse    BYTE(0),THREAD

  MAP
Main              PROCEDURE
  END

  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  Main()

! --------------------------------------
Main  PROCEDURE 
Window  WINDOW('ButtonTiles Example Application'),AT(,,400,236),GRAY,IMM,SYSTEM, |
        FONT('Segoe UI',9,,,CHARSET:DEFAULT),COLOR(COLOR:White),RESIZE
          PROMPT('Tiles from buttons with only Text'),AT(10,6,380),USE(?PROMPT1:2), |
          FONT(,,COLOR:White,FONT:bold),COLOR(08C2676H),CENTER
          BUTTON('Browse Users Details'),AT(10,19,80,22),USE(?ButtonUsers_TEXT)
          BUTTON('Explore Dashboard'),AT(94,19,80,22),USE(?ButtonDashboard_TEXT)
          BUTTON('Download Some Stuff'),AT(177,19,80,22),USE(?ButtonDownload_TEXT)
          BUTTON('Help'),AT(292,19,48,22),USE(?ButtonHelp_TEXT),TIP('Press this if you' & |
          ' need help...')
          BUTTON('Exit'),AT(343,19,47,22),USE(?ButtonExit_TEXT),TIP('This is the Exit ' & |
          'button!')
          PROMPT('Tiles from buttons with Icons and optionally'),AT(10,44,380,10), |
          USE(?PROMPT1),FONT(,,COLOR:White,FONT:bold),COLOR(08C2676H),CENTER
          BUTTON('Browse Users Details'),AT(10,58,80,34),USE(?ButtonUsers),FONT('Segoe UI'), |
          ICON('users_tile.ico')
          BUTTON('Explore Dashboard'),AT(94,58,80,34),USE(?ButtonDashboard), |
          ICON('gauge_tile.ico')
          BUTTON('Download Some Stuff'),AT(177,58,80,34),USE(?ButtonDownload), |
          FONT(,,,FONT:regular),ICON('web_tile.ico')
          BUTTON,AT(292,58,48,34),USE(?ButtonHelp),ICON('help_tile.ico'),TIP('Help')
          BUTTON,AT(342,58,48,34),USE(?ButtonExit),ICON('exit_tile.ico'),TIP('Exit')
          PROMPT('Tiles acting as a toggle set'),AT(10,129,380,10),USE(?PROMPT1:3), |
          FONT(,,COLOR:White,FONT:bold),COLOR(08C2676H),CENTER
          BUTTON('STUFF'),AT(341,142,48,22),USE(?ButtonDownload_TOGGLE),FONT(,12,,FONT:bold)
          BUTTON('DASHBOARD'),AT(266,142,74,22),USE(?ButtonDashboard_TOGGLE), |
          FONT(,12,,FONT:bold)
          BUTTON('USERS'),AT(210,142,54,22),USE(?ButtonUsers_TOGGLE),FONT(,12,,FONT:bold)
          PROMPT('Navigate using the toggle tiles!'),AT(10,167,380,61),USE(?PromptSelectedToggle) |
          ,FONT(,28,0141414H,FONT:bold),COLOR(0C8C8C8H),CENTER
          CHECK(' Hide A Button'),AT(119,95),USE(?CheckHideAButton)
          CHECK(' Disable A Button'),AT(193,95,64,10),USE(?CheckDisableAButton), |
          COLOR(COLOR:White)
          PROMPT('Click on a tile!'),AT(10,109,380,17),USE(?PromptSelectedTile), |
          FONT(,16,0141414H,FONT:bold),COLOR(0C8C8C8H),CENTER
        END

ThisWindow  CLASS(WindowManager)
Init              PROCEDURE(),BYTE,PROC,DERIVED
Kill              PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted      PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent   PROCEDURE(),BYTE,PROC,DERIVED
            END

Tiles             TileManager
ToggleTiles       TileManager
Resizer           WindowResizeClass
  CODE
  GlobalResponse =  ThisWindow.Run()

ThisWindow.Init   PROCEDURE

ReturnValue       BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.

  SELF.FirstField = ?ButtonUsers
  SELF.Errors &= GlobalErrors
  SELF.Open(Window)
  Resizer.Init(AppStrategy:Resize)
  SELF.AddItem(Resizer)
  Resizer.Resize()
  0{PROP:Buffer} = 1

  Tiles.Init(SELF)

  Tiles.AddButtonMimic(?ButtonUsers_TEXT, 0C67200h)
  Tiles.AddButtonMimic(?ButtonDashboard_TEXT, 0998500h)
  Tiles.AddButtonMimic(?ButtonDownload_TEXT, 0525252h)
  Tiles.AddButtonMimic(?ButtonHelp_TEXT, 00070CCh)
  Tiles.AddButtonMimic(?ButtonExit_TEXT, 038703Eh)

  Tiles.AddButtonMimic(?ButtonUsers, 0C67200h)
  Tiles.AddButtonMimic(?ButtonDashboard, 0998500h)
  Tiles.AddButtonMimic(?ButtonDownload, 0525252h)
  Tiles.AddButtonMimic(?ButtonHelp, 00070CCh)
  Tiles.AddButtonMimic(?ButtonExit, 038703Eh)
  
  ToggleTiles.enableToggleSet = TRUE
  ToggleTiles.Init(SELF, 100)
  ToggleTiles.AddButtonMimic(?ButtonUsers_TOGGLE, 0C67200h)
  ToggleTiles.AddButtonMimic(?ButtonDashboard_TOGGLE, 0C67200h)
  ToggleTiles.AddButtonMimic(?ButtonDownload_TOGGLE, 0C67200h)
  Post(EVENT:Accepted, ?ButtonUsers_TOGGLE)
  
  RETURN ReturnValue

ThisWindow.Kill   PROCEDURE
ReturnValue       BYTE,AUTO
  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue

ThisWindow.TakeAccepted   PROCEDURE
ReturnValue               BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeAccepted()
  CASE ACCEPTED()
  OF ?ButtonExit
  OROF ?ButtonExit_TEXT
    ThisWindow.Update()
    Post(EVENT:CloseWindow)
  OF ?ButtonUsers_TOGGLE
  OROF ?ButtonDownload_TOGGLE
  OROF ?ButtonDashboard_TOGGLE
    ?PromptSelectedToggle{PROP:Text} = 'Selected Toggle:<13,10>' & Field(){PROP:Text}
  OF ?CheckHideAButton
    Tiles.HideButton(?ButtonDashboard, ?CheckHideAButton{PROP:Checked})
  OF ?CheckDisableAButton
    ?ButtonDownload{PROP:Disable} = Choose(?ButtonDownload{PROP:Disable}=FALSE)
    SELF.Reset()
  ELSE
    IF Field(){PROP:Type} = CREATE:button
      ?PromptSelectedTile{PROP:Text} = 'Tile Clicked: ' & |
       Choose(Field(){PROP:Text} = '', Field(){PROP:Tip}, Field(){PROP:Text})
    END
  END
  
  RETURN ReturnValue

ThisWindow.TakeWindowEvent    PROCEDURE()!,BYTE,PROC,DERIVED
ReturnValue                     BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeWindowEvent()
  CASE EVENT()
  OF EVENT:Sized
    Resizer.Resize()
    SELF.Reset()
  END
  RETURN ReturnValue
