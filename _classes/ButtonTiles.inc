  INCLUDE('BaseWindowComponent.inc'),ONCE

tileQ_Type QUEUE,TYPE
buttonFEQ           SIGNED
regionFEQ           SIGNED
boxFEQ              SIGNED
promptFEQ           SIGNED
imageFEQ            SIGNED
fillNormal          LONG
fillHot             LONG
  END

ButtonTiles  CLASS(BaseWindowComponent),IMPLEMENTS(WindowComponent),TYPE,MODULE('ButtonTiles.Clw'),LINK('ButtonTiles.Clw',_ABCLinkMode_),DLL(_ABCDllMode_)
tileQ               &tileQ_Type
fontName            CSTRING(255)
Construct           PROCEDURE()
Destruct            PROCEDURE()
Init                PROCEDURE(WindowManager pWM, <STRING pFont>)
AddButtonMimic      PROCEDURE(SIGNED pButtonFEQ, LONG pFillNormal, LONG pFillHot)
  END
