  INCLUDE('BaseWindowComponent.inc'),ONCE

tileQ_Type QUEUE,TYPE
buttonFEQ           SIGNED
regionFEQ           SIGNED
boxFEQ              SIGNED
promptFEQ           SIGNED
imageFEQ            SIGNED
fillNormal          LONG
actionFactor        BYTE
  END

ButtonTiles  CLASS(BaseWindowComponent),IMPLEMENTS(WindowComponent),TYPE,MODULE('ButtonTiles.Clw'),LINK('ButtonTiles.Clw',_ABCLinkMode_),DLL(_ABCDllMode_)
tileQ               &tileQ_Type
fontName            CSTRING(255)
imageMarginLeft     BYTE
imageMarginTop      BYTE
imageWidth          BYTE
imageHeight         BYTE
promptMarginLeft    BYTE
promptMarginTop     BYTE
Construct           PROCEDURE()
Destruct            PROCEDURE()
Init                PROCEDURE(WindowManager pWM, <STRING pFont>)
AddButtonMimic      PROCEDURE(SIGNED pButtonFEQ, LONG pFillNormal, BYTE pHoverLightenFactor=66)
SetTileElements     PROCEDURE()
RefreshTile         PROCEDURE(SIGNED pButtonFEQ)
  END
