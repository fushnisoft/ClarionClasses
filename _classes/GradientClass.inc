  PRAGMA('link(msimg32.lib)')
GRADIENT_FILL_RECT_H         EQUATE(0)
GRADIENT_FILL_RECT_V         EQUATE(1)
EVENT:WM_USER                EQUATE(401h)
! WM_ERASEBKGND       equate(014H)

ColourGroup_Type             GROUP,TYPE
Red                            BYTE
Green                          BYTE
Blue                           BYTE
                             END

  include('ABWINDOW.INC'),ONCE

GradientClass    Class(),Implements(WindowComponent),Type,Module('GradientClass.Clw'),LINK('GradientClass.Clw',1)
windowActuallyOpen           BYTE
DCHandle                     ULONG
FillOrder                    BYTE
StartColour_Clarion          LONG
StartColour                  GROUP(ColourGroup_Type),OVER(StartColour_Clarion)
                             END
EndColour_Clarion            LONG
EndColour                    GROUP(ColourGroup_Type),OVER(EndColour_Clarion)
                             END

RectVertex                   GROUP,PRE(),DIM(2)
RVX                            LONG
RVY                            LONG
RVRed                          USHORT
RVGreen                        USHORT
RVBlue                         USHORT
RVAlpha                        USHORT
                             END

GradientRect                 GROUP,PRE()
GRUpperLeft                    ULONG
GRLowerRight                   ULONG
                             END

Init                         PROCEDURE(WindowManager WM, BYTE pFillOrder, LONG pStartColour, LONG pEndColour)
Redraw                       PROCEDURE()
DrawGradient                 PROCEDURE()
RegisterWindow               PROCEDURE()
                     END 
