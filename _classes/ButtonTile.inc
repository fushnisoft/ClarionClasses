BT_FILL_STATE ITEMIZE,PRE
NORMAL  EQUATE(0)
HOVER   EQUATE
PRESSED EQUATE
 END

ButtonTile  CLASS(),TYPE,MODULE('ButtonTile.Clw'),LINK('ButtonTile.Clw',_ABCLinkMode_),DLL(_ABCDllMode_)
buttonFEQ                 SIGNED
boxFEQ                    SIGNED
imageFEQ                  SIGNED
promptFEQ                 SIGNED
regionFEQ                 SIGNED

isHidden                  BYTE
tileColor                 LONG
lightenOnHoverPercentage  BYTE
padding                   BYTE

originalPromptHeight      SIGNED
isToggleTile              BYTE
isToggledOn               BYTE
GetFillColor              PROCEDURE(BYTE pState=BT_FILL_STATE:NORMAL) ,LONG
SetTileState              PROCEDURE(BYTE pState)
Init                      PROCEDURE(SIGNED pButtonFEQ, LONG pColor, BYTE pLightenOnHoverPercentage=33, BYTE pPadding=8)
Refresh                   PROCEDURE()
  END
