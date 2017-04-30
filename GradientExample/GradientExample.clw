  PROGRAM

  INCLUDE('ABWINDOW.INC'),ONCE
  INCLUDE('GradientClass.inc'),ONCE

GlobalErrorStatus ErrorStatusClass,THREAD
GlobalErrors  ErrorClass
GlobalRequest BYTE(0),THREAD
GlobalResponse    BYTE(0),THREAD
Gradient  GradientClass

  MAP
Main    PROCEDURE
  END

  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  Main()

! --------------------------------------
Main  PROCEDURE 
AppFrame    APPLICATION('Application'),AT(,,505,318),CENTER,MASK,SYSTEM,MAX, |
            ICON('WAFRAME.ICO'),STATUS(-1,80,120,45),FONT('Microsoft Sans Serif',8,, |
            FONT:regular),RESIZE
              MENUBAR,USE(?Menubar)
                MENU('&File'),USE(?FileMenu)
                  ITEM('&Print Setup ...'),USE(?PrintSetup),MSG('Setup printer'), |
                  STD(STD:PrintSetup)
                  ITEM(''),SEPARATOR,USE(?SEPARATOR1)
                  ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                END
                MENU('&Edit'),USE(?EditMenu)
                  ITEM('Cu&t'),USE(?Cut),MSG('Remove item to Windows Clipboard'),STD(STD:Cut)
                  ITEM('&Copy'),USE(?Copy),MSG('Copy item to Windows Clipboard'),STD(STD:Copy)
                  ITEM('&Paste'),USE(?Paste),MSG('Paste contents of Windows Clipboard'), |
                  STD(STD:Paste)
                END
                MENU('&Window'),USE(?WindowMenu),STD(STD:WindowList)
                  ITEM('T&ile'),USE(?Tile),MSG('Make all open windows visible'), |
                  STD(STD:TileWindow)
                  ITEM('&Cascade'),USE(?Cascade),MSG('Stack all open windows'),STD(STD:CascadeWindow) |
            
                  ITEM('&Arrange Icons'),USE(?Arrange),MSG('Align all window icons'), |
                  STD(STD:ArrangeIcons)
                END
                MENU('&Help'),USE(?HelpMenu)
                  ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'), |
                  STD(STD:HelpIndex)
                  ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on ' & |
                  'a subject'),STD(STD:HelpSearch)
                  ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('How to use Windows Help'), |
                  STD(STD:HelpOnHelp)
                END
              END
            END

ThisWindow  CLASS(WindowManager)
Init          PROCEDURE(),BYTE,PROC,DERIVED
Kill          PROCEDURE(),BYTE,PROC,DERIVED
            END

  CODE
  GlobalResponse =  ThisWindow.Run()

ThisWindow.Init   PROCEDURE

ReturnValue         BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.

  SELF.Errors &= GlobalErrors
  SELF.Open(AppFrame)
  Gradient.Init(SELF, GRADIENT_FILL_RECT_H, 000b5f8h, 0bbeafch)

  RETURN ReturnValue

ThisWindow.Kill   PROCEDURE
ReturnValue         BYTE,AUTO
  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue

