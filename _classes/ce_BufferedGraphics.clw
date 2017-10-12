

      Member()
         omit('***$***',_VER_C55)
_ABCDllMode_  EQUATE(0)
_ABCLinkMode_ EQUATE(1)
         ***$***
      Include('Equates.CLW'),ONCE
      Include('Keycodes.CLW'),ONCE
      Include('Errors.CLW'),ONCE
      Map
  Module('Win32.lib')
    GetDeviceCaps(UNSIGNED, SIGNED),SIGNED,PASCAL
    ReleaseDC(LONG, LONG),SIGNED,PASCAL
    GetDC(LONG),LONG,PASCAL
    ResetDC(LONG, LONG),LONG,PASCAL,NAME('ResetDCA')
    SetPixel(LONG, LONG, LONG, LONG),LONG,PASCAL,PROC
    CreateCompatibleDC(LONG),LONG,PASCAL
    CreateCompatibleBitmap(LONG, LONG, LONG),LONG,PASCAL
    BitBlt(long hdcDest, long nXDest, long nYDest, long nWidth, long nHeight, ulong hdcSrc, long nXSrc, long nYSrc, ulong dwRop), long, raw, pascal, name('BitBlt'),proc
    SelectObject(LONG, LONG),LONG,PASCAL,PROC
    SetStretchBltMode(long,long),long,pascal,proc
    StretchBlt(long,long,long,long,long,Long,long,long,long,long,long),long,pascal,raw,PROC
  End
      End ! map
      Include('ce_BufferedGraphics.inc'),ONCE
ce_BufferedGraphics.Allocate PROCEDURE  (SIGNED pControlFEQ) ! Declare Procedure
  CODE
  SELF.controlHWnd  = pControlFEQ{PROP:Handle}
  SELF.controlDC    = GetDC(SELF.controlHWnd)
  SELF.sourceWidth  = pControlFEQ{PROP:Width}
  SELF.sourceHeight = pControlFEQ{PROP:Height}
  SELF.memoryDC     = CreateCompatibleDC(SELF.controlDC)
  SELF.memoryBitMap = CreateCompatibleBitmap(SELF.controlDC, SELF.sourceWidth , SELF.sourceHeight)

ce_BufferedGraphics.Render PROCEDURE                       ! Declare Procedure
  CODE
  SetStretchBltMode(SELF.controlDC, 3)
  StretchBlt(SELF.controlDC, 0, 0, SELF.sourceWidth, SELF.sourceHeight, SELF.memoryDC, 0, 0, SELF.sourceWidth, SELF.sourceHeight, dwRop:SRCCOPY)

ce_BufferedGraphics.SetPixel PROCEDURE  (LONG pX, LONG pY, LONG pColour) ! Declare Procedure
  CODE
 !Stop('pX: ' & pX & ', pY: ' & pY & ', pColour: ' & pColour)
    SetPixel (SELF.memoryDC, pX, pY, pColour)
ce_BufferedGraphics.Load PROCEDURE                         ! Declare Procedure
  CODE
  SELF.memoryOriginalBitMap = SelectObject(SELF.memoryDC, SELF.memoryBitMap)
  BitBlt(SELF.memoryDC, 0, 0, SELF.sourceWidth, SELF.sourceHeight, SELF.controlDC , 0, 0, dwRop:BLACKNESS)

