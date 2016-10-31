  INCLUDE('BaseWindowComponent.inc'),ONCE
  INCLUDE('ButtonTile.inc'),ONCE

tileManagerQ_type    QUEUE,TYPE
buttonFEQ                 SIGNED ! This is not really necessary since the buttonFEQ is also stored in the ButtonTile class but it is a convenient redundancy
Tile                      &ButtonTile
              END

TM_CURSOR:SIZEALL            EQUATE('<0FFH,01H,87H,7FH>')
TM_CURSOR:No                 EQUATE('<0FFH,01H,89H,7FH>')
TM_CURSOR:Hand              EQUATE('<0FFH,01H,8AH,7FH>') ! Label duplicates?
TM_CURSOR:APPSTARTING        EQUATE('<0FFH,01H,8BH,7FH>')
TM_CURSOR:HELP               EQUATE('<0FFH,01H,8CH,7FH>')

TileManager   CLASS(BaseWindowComponent),IMPLEMENTS(WindowComponent),TYPE,MODULE('TileManager.Clw'),LINK('TileManager.Clw',_ABCLinkMode_),DLL(_ABCDllMode_)
tileQ                     &tileManagerQ_type
padding                   BYTE
lightenOnHoverPercentage  BYTE
enableToggleSet           BYTE
currentToggleChoice       SIGNED
Construct                 PROCEDURE()
Destruct                  PROCEDURE()
Init                      PROCEDURE(WindowManager pWM, BYTE pLightenOnHoverPercentage=33, BYTE pPadding=8)
AddButtonMimic            PROCEDURE(SIGNED pButtonFEQ, LONG pColor, <BYTE pLightenOnHoverPercentage>, <BYTE pPadding>)
FindToggleRegion          PROCEDURE(SIGNED pFieldFEQ) ,BYTE
FindButton                PROCEDURE(SIGNED pFieldFEQ) ,BYTE
GetToggleChoice           PROCEDURE() ,SIGNED
HideButton                PROCEDURE(SIGNED pButtonFeq, BYTE pHide=TRUE)
              END
