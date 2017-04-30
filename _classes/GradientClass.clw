      Member()
MODULE:savedProc LONG
  INCLUDE('svapi.inc'),once
      Map
WindowCallBack_GradientClass PROCEDURE (UNSIGNED hWnd, UNSIGNED wMsg, UNSIGNED wParam, LONG lParam),LONG,PASCAL
    MODULE('WIN32API')
      GetDC(uLONG),uLONG,PASCAL
      ReleaseDC(ulong, ulong),ulong,PASCAL,proc
      DeleteDC(ulong),ulong,pascal,raw,name('DeleteDC'),proc
      GradientFill( ULong, long, LONG, long, LONG, LONG),long,pascal,raw,name('GradientFill'),proc
      CallWindowProc(LONG lpPrevWndProc, UNSIGNED hWnd, UNSIGNED Msg, UNSIGNED wParam, LONG lParam),LONG,RAW,PASCAL,NAME('CallWindowProcA'),DLL(dll_mode)
    END
      End 
      Include('GradientClass.inc'),ONCE
GradientClass.WindowComponent.Kill PROCEDURE           ! Declare Procedure
  CODE
GradientClass.WindowComponent.Reset PROCEDURE  (BYTE Force) ! Declare Procedure
  CODE
GradientClass.WindowComponent.ResetRequired PROCEDURE  () !,BYTE ! Declare Procedure
  CODE
  RETURN FALSE
GradientClass.WindowComponent.SetAlerts PROCEDURE      ! Declare Procedure
  CODE
GradientClass.WindowComponent.TakeEvent PROCEDURE  () !,BYTE ! Declare Procedure
  CODE
  CASE Event()
  OF EVENT:OpenWindow
    SELF.RegisterWindow()
  OF Event:WM_USER
  OROF EVENT:Sized
  OROF EVENT:Restored
  OROF EVENT:Expanded
  OROF EVENT:Contracted
  OROF EVENT:GainFocus
  OROF EVENT:Size
    SELF.Redraw()
  END

  RETURN Level:Benign

GradientClass.WindowComponent.Update PROCEDURE         ! Declare Procedure
  CODE
GradientClass.WindowComponent.UpdateWindow PROCEDURE   ! Declare Procedure
  CODE
GradientClass.Init PROCEDURE  (WindowManager WM, BYTE pFillOrder, LONG pStartColour, LONG pEndColour) ! Declare Procedure
  CODE
  SELF.StartColour_Clarion = pStartColour
  SELF.EndColour_Clarion = pEndColour
  SELF.FillOrder = pFillOrder
  WM.AddItem(SELF.WindowComponent)

GradientClass.Redraw PROCEDURE                         ! Declare Procedure
  CODE
  IF SELF.windowActuallyOpen
    SELF.DrawGradient()
  END

GradientClass.DrawGradient PROCEDURE                   ! Declare Procedure
originalPropPixels BYTE
  CODE
  originalPropPixels = 0{PROP:Pixels}
  0{PROP:Pixels} = 1

  SELF.DCHandle = GetDC(0{PROP:ClientHandle})

  SELF.RectVertex[1].RVX         = 0
  SELF.RectVertex[1].RVY         = 0
  SELF.RectVertex[1].RVRed       = SELF.StartColour.Red * 255
  SELF.RectVertex[1].RVGreen     = SELF.StartColour.Green * 255
  SELF.RectVertex[1].RVBlue      = SELF.StartColour.Blue * 255
  SELF.RectVertex[1].RVAlpha     = 0

  SELF.RectVertex[2].RVX         = 0{PROP:ClientWidth}
  SELF.RectVertex[2].RVY         = 0{PROP:ClientHeight}
  SELF.RectVertex[2].RVRed       = SELF.EndColour.Red * 255
  SELF.RectVertex[2].RVGreen     = SELF.EndColour.Green * 255
  SELF.RectVertex[2].RVBlue      = SELF.EndColour.Blue * 255
  SELF.RectVertex[2].RVAlpha     = 0

  0{PROP:Pixels} = originalPropPixels

  CASE SELF.FillOrder
  OF GRADIENT_FILL_RECT_H
    SELF.GradientRect.GRUpperLeft  = 0
    SELF.GradientRect.GRLowerRight = 1
  OF GRADIENT_FILL_RECT_V
    SELF.GradientRect.GRUpperLeft  = 1
    SELF.GradientRect.GRLowerRight = 0
  END

  GradientFill(SELF.DCHandle, Address(SELF.RectVertex), 2, Address(SELF.GradientRect), 1, SELF.FillOrder)

  ReleaseDC(0{PROP:ClientHandle}, SELF.DCHandle)
  DeleteDC(SELF.DCHandle)

GradientClass.RegisterWindow PROCEDURE                 ! Declare Procedure
  CODE
  IF SELF.windowActuallyOpen = FALSE
    MODULE:savedProc        = 0{PROP:ClientWndProc}
    0{PROP:ClientWndProc}   = Address(WindowCallBack_GradientClass)
    SELF.windowActuallyOpen = TRUE
  END

WindowCallBack_GradientClass PROCEDURE  (UNSIGNED hWnd, UNSIGNED wMsg, UNSIGNED wParam, LONG lParam) ! Declare Procedure
  CODE
  CASE wMsg
  OF WM_ERASEBKGND
    Post(Event:WM_USER)
  END

  RETURN(CallWindowProc(MODULE:savedProc, hWnd, wMsg, wParam, lParam))
