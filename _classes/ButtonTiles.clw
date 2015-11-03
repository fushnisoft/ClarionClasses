  MEMBER()

  MAP
  END

  INCLUDE('ButtonTiles.inc'),ONCE

ButtonTiles.Init                PROCEDURE(WindowManager pWM, <STRING pFont>)
  CODE
  pWM.AddItem(SELF.WindowComponent)
  IF Omitted(pFont) = FALSE
    SELF.fontName = pFont
  END

ButtonTiles.AddButtonMimic      PROCEDURE(SIGNED pButtonFEQ, LONG pFillNormal, LONG pFillHot)
pos                   GROUP
XPos                    SIGNED           !Horizontal coordinate
YPos                    SIGNED           !Vertical coordinate
Width                   UNSIGNED         !Width
Height                  UNSIGNED         !Height
                      END
savePixels BYTE
  CODE
  savePixels = 0{PROP:Pixels}
  0{PROP:Pixels} = FALSE

  SELF.tileQ.buttonFEQ = pButtonFEQ
  SELF.tileQ.fillNormal = pFillNormal
  SELF.tileQ.fillHot = pFillHot

  SELF.tileQ.buttonFEQ{PROP:Hide} = TRUE
  GetPosition(SELF.tileQ.buttonFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
  
  SELF.tileQ.boxFEQ = Create(0, CREATE:Box)
  SetPosition(SELF.tileQ.boxFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
  SELF.tileQ.boxFEQ{PROP:Hide} = FALSE
  SELF.tileQ.boxFEQ{PROP:Fill} = SELF.tileQ.fillNormal

  SELF.tileQ.imageFEQ = Create(0, CREATE:Image)
  SetPosition(SELF.tileQ.imageFEQ, pos.XPos+6, pos.YPos+4, 22, 22)
  SELF.tileQ.imageFEQ{PROP:Hide} = FALSE
  SELF.tileQ.imageFEQ{PROP:Text} = SELF.tileQ.buttonFEQ{PROP:Icon}

  SELF.tileQ.promptFEQ = Create(0, CREATE:Prompt)
  SetPosition(SELF.tileQ.promptFEQ, pos.XPos+6, pos.YPos+pos.Height-11)
  SELF.tileQ.promptFEQ{PROP:Hide} = FALSE
  SELF.tileQ.promptFEQ{PROP:Text} = SELF.tileQ.buttonFEQ{PROP:Text}
  SELF.tileQ.promptFEQ{PROP:FontColor} = COLOR:White
  SELF.tileQ.promptFEQ{PROP:Trn} = TRUE
  IF SELF.fontName
    SELF.tileQ.promptFEQ{PROP:FontName} = SELF.fontName
  END

  SELF.tileQ.regionFEQ = Create(0, CREATE:Region)
  SetPosition(SELF.tileQ.regionFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
  SELF.tileQ.regionFEQ{PROP:Hide} = FALSE
  SELF.tileQ.regionFEQ{PROP:IMM} = TRUE

  0{PROP:Pixels} = savePixels

  Add(SELF.tileQ)

ButtonTiles.Construct                       PROCEDURE()
  CODE
  SELF.tileQ &= New(tileQ_Type)

ButtonTiles.Destruct                        PROCEDURE()
  CODE
  Free(SELF.tileQ)
  Dispose(SELF.tileQ)

ButtonTiles.WindowComponent.TakeEvent PROCEDURE() ! ,BYTE ! Declare Procedure
rv                           BYTE
  CODE
  rv = PARENT.WindowComponent.TakeEvent()
  IF 0{Prop:AcceptAll} = TRUE
    RETURN rv
  END

  IF FIELD()
    SELF.tileQ.regionFEQ = Field()
    Get(SELF.tileQ, SELF.tileQ.regionFEQ)
    IF ErrorCode() = FALSE
      CASE EVENT()
      OF EVENT:MouseUp
          Post(Event:Accepted, SELF.tileQ.buttonFEQ)
      OF EVENT:MouseIn
          SELF.tileQ.boxFEQ{PROP:Fill} = SELF.tileQ.fillHot
      OF EVENT:MouseOut
        SELF.tileQ.boxFEQ{PROP:Fill} = SELF.tileQ.fillNormal
      END
    END
  END

  RETURN rv