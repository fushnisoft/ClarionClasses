_ce_BufferedGraphics_ EQUATE(1)
! Generated by CapeSoft's Object Writer
dwRop:BLACKNESS              EQUATE(00000042h) ! Fills the destination rectangle using the color associated with index 0 in the physical palette. (This color is black for the default physical palette.)
dwRop:DSTINVERT              EQUATE(00550009h) ! Inverts the destination rectangle.
dwRop:MERGECOPY              EQUATE(00C000CAh) ! Merges the colors of the source rectangle with the specified pattern by using the Boolean AND operator.
dwRop:MERGEPAINT             EQUATE(00BB0226h) ! Merges the colors of the inverted source rectangle with the colors of the destination rectangle by using the Boolean OR operator.
dwRop:NOTSRCCOPY             EQUATE(00330008h) ! Copies the inverted source rectangle to the destination.
dwRop:NOTSRCERASE            EQUATE(001100A6h) ! Combines the colors of the source and destination rectangles by using the Boolean OR operator and then inverts the resultant color.
dwRop:PATCOPY                EQUATE(00F00021h) ! Copies the specified pattern into the destination bitmap.
dwRop:PATINVERT              EQUATE(005A0049h) ! Combines the colors of the specified pattern with the colors of the destination rectangle by using the Boolean XOR operator.
dwRop:PATPAINT               EQUATE(00FB0A09h) ! Combines the colors of the pattern with the colors of the inverted source rectangle by using the Boolean OR operator. The result of this operation is combined with the colors of the destination rectangle by using the Boolean OR operator.
dwRop:SRCAND                 EQUATE(008800C6h) ! Combines the colors of the source and destination rectangles by using the Boolean AND operator.
dwRop:SRCCOPY                EQUATE(00CC0020h) ! Copies the source rectangle directly to the destination rectangle.
dwRop:SRCERASE               EQUATE(00440328h) ! Combines the inverted colors of the destination rectangle with the colors of the source rectangle by using the Boolean AND operator.
dwRop:SRCINVERT              EQUATE(00660046h) ! Combines the colors of the source and destination rectangles by using the Boolean XOR operator.
dwRop:SRCPAINT               EQUATE(00EE0086h) ! Combines the colors of the source and destination rectangles by using the Boolean OR operator.
dwRop:WHITENESS              EQUATE(00FF0062h) ! Fills the destination rectangle using the color associated with index 1 in the physical palette. (This color is white for the default physical palette.)

Colour_Type                  GROUP,TYPE
red                            BYTE
green                          BYTE
blue                           BYTE
HighByte                       BYTE
                             END

!--------------------------------------------------------------------------------
!Class ce_BufferedGraphics
!
!--------------------------------------------------------------------------------
ce_BufferedGraphics  Class(),Type,Module('ce_BufferedGraphics.Clw'),LINK('ce_BufferedGraphics.Clw',1)
! Properties
memoryDC                     LONG
memoryBitMap                 LONG
memoryOriginalBitMap         LONG
controlHWnd                  LONG
controlDC                    LONG
sourceWidth                  LONG
sourceHeight                 LONG
! Methods
Allocate               PROCEDURE (SIGNED pControlFEQ) ,VIRTUAL 
Render                 PROCEDURE () ,VIRTUAL 
SetPixel               PROCEDURE (LONG pX, LONG pY, LONG pColour) ,VIRTUAL 
Load                   PROCEDURE () ,VIRTUAL 
                     END ! Class Definition
!_EndOfInclude_