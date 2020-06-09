
  PROGRAM

  MAP
    MODULE('C%V%RUN%X%')
      DebugerNameMessage(*CSTRING, UNSIGNED EventNum ),NAME('WslDebug$NameMessage'),RAW   !Note: use   EVENT() + EVENT:FIRST else will get WM_*
    END
    MODULE('Win32Api')
CallWindowProc    PROCEDURE(LONG wndprc, |
                  ULONG hWnd, |
                  ULONG uMsg, |
                  ULONG wParam, |
                  LONG  lParam ),LONG,PASCAL,NAME('CallWindowProcA')
    END
SubClassFunc    Procedure(UNSIGNED hWnd,   |
                LONG uMsg,        |
                LONG wParam,      |
                LONG lParam       |
                ),LONG,PASCAL
  END

VK_RIGHT  EQUATE(027H)
VK_LEFT   EQUATE(025H)
WM_HSCROLL    EQUATE (0114H)
WM_KEYDOWN    EQUATE (0100H)

HORIZ_EVENTS  ITEMIZE(Event:User + 12)  !select your starting point.
                                                           ! It sure would be nice if clarion had a way to register events so we
                                                           ! (the developers) did not trample on each others events.
EVENT:HScroll   EQUATE
EVENT:H_ScrollLeft  EQUATE
EVENT:H_ScrollRight EQUATE
EVENT:H_PageLeft    EQUATE
EVENT:H_PageRight   EQUATE
EVENT:H_Left    EQUATE
EVENT:H_Right   EQUATE
EVENT:H_ScrollDrag  EQUATE
              END

SB_LINELEFT   EQUATE(0)
SB_LINERIGHT  EQUATE(1)
SB_PAGELEFT   EQUATE(2)
SB_PAGERIGHT  EQUATE(3)
SB_THUMBPOSITION  EQUATE(4)
SB_LEFT   EQUATE(6)
SB_RIGHT  EQUATE(7)
SB_ENDSCROLL  EQUATE(8)


listFEQ   SIGNED
savedProc LONG
draggingThumb LONG
lastScrollPos LONG
listQ QUEUE()
id      LONG
eventCode   LONG
eventName   CSTRING(128)
      END
MyWindow  WINDOW('ListHScroll'),AT(,,263,242),GRAY,SYSTEM,FONT('Segoe UI',9,, |
          FONT:regular),DOUBLE
            LIST,AT(2,2,258,220),USE(?List),HVSCROLL,FROM(listQ),IMM
          END


  CODE
  OPEN(MyWindow)
  savedProc = ?List{PROP:WNDProc}
  ?List{PROP:WNDProc} = Address(SubClassFunc)
  listFEQ = ?List
  ACCEPT
    IF Event()
      listQ.id += 1
      listQ.eventCode = Event()
      CASE Event()
      OF EVENT:H_ScrollLeft
        listQ.eventName = ' ScrollLeft '
      OF EVENT:H_ScrollRight
        listQ.eventName = ' ScrollRight '
      OF EVENT:H_PageLeft
        listQ.eventName = ' PageLeft '
      OF EVENT:H_PageRight
        listQ.eventName = ' PageRight '
      OF EVENT:H_Left
        listQ.eventName = ' Left '
      OF EVENT:H_Right
        listQ.eventName = ' Right '
      OF EVENT:H_ScrollDrag
        listQ.eventName = ' ScrollDrag '
      OF EVENT:HScroll
        listQ.eventName = 'EVENT:HScroll'
      ELSE
        DebugerNameMessage(listQ.eventName, Event() + 0A000H)
      END
      
      Add(listQ)
      Sort(listQ, -listQ.id)
      Display(?List)
    END
    
    CASE FIELD()
    OF 0
      CASE EVENT()
      OF EVENT:OpenWindow
      END
    END
  END
 
SubClassFunc  Procedure(UNSIGNED hWnd,   |
              LONG uMsg,   |
              LONG wParam, |
              LONG lParam) !,LONG
msgGroup        GROUP,OVER(wParam)
lowVal            USHORT
highVal           USHORT
                END
  CODE
  CASE uMsg
  OF WM_KEYDOWN
    CASE wParam
    OF VK_RIGHT 
    OROF VK_LEFT
      Post(Event:HScroll,listFEQ)
    End
  OF WM_HSCROLL
    Post(Event:HScroll,listFEQ)
    If lParam
      Case MsgGroup.LowVal
      OF SB_LINELEFT
        draggingThumb = -1
        Post(EVENT:H_ScrollLeft ,listFEQ)
      OF SB_LINERIGHT
        draggingThumb = -1
        Post(EVENT:H_ScrollRight ,listFEQ)
      OF SB_PAGELEFT
        draggingThumb = -1
        Post(EVENT:H_PageLeft ,listFEQ)
      OF SB_PAGERIGHT
        draggingThumb = -1
        Post(EVENT:H_PageRight ,listFEQ)
      OF SB_LEFT
        draggingThumb = -1
        Post(EVENT:H_Left ,listFEQ)
      OF SB_RIGHT
        draggingThumb = -1
        Post(EVENT:H_Right ,listFEQ)
      OF SB_THUMBPOSITION
        draggingThumb = msgGroup.highVal
      OF SB_ENDSCROLL
        IF draggingThumb <> lastScrollPos And draggingThumb > 0
          lastScrollPos = draggingThumb
          draggingThumb = -1
          Post(EVENT:H_ScrollDrag ,listFEQ)
        END
      END
    END
  END
  RETURN CallWindowProc(savedProc, hWnd, uMsg, wParam, lParam)