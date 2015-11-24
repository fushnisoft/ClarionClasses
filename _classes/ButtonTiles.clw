  MEMBER()

  MAP
  INCLUDE('CWUTIL.INC'),ONCE
  END

  INCLUDE('ButtonTiles.inc'),ONCE

ButtonTiles.Init                PROCEDURE(WindowManager pWM, <STRING pFont>)
  CODE
  pWM.AddItem(SELF.WindowComponent)
  IF Omitted(pFont) = FALSE
    SELF.fontName = pFont
  END

ButtonTiles.AddButtonMimic      PROCEDURE(SIGNED pButtonFEQ, LONG pFillNormal, BYTE pActionFactor=66)
parentFeq SIGNED
  CODE

  SELF.tileQ.buttonFEQ  = pButtonFEQ
  SELF.tileQ.fillNormal = pFillNormal
  SELF.tileQ.actionFactor = pActionFactor
  parentFeq = SELF.tileQ.buttonFEQ{PROP:Parent}
  ! Hide the original button but keep it around. You can adjust the original button and call SetTileElementPositions to update the tiles
  SELF.tileQ.buttonFEQ{PROP:Hide} = TRUE
  
  ! Make a box
  SELF.tileQ.boxFEQ = Create(0, CREATE:Box, parentFeq)
  SELF.tileQ.boxFEQ{PROP:Hide} = FALSE
  SELF.tileQ.boxFEQ{PROP:Fill} = SELF.tileQ.fillNormal

  ! Add an image
  SELF.tileQ.imageFEQ = Create(0, CREATE:Image, parentFeq)
  SELF.tileQ.imageFEQ{PROP:Hide} = FALSE
  SELF.tileQ.imageFEQ{PROP:Text} = SELF.tileQ.buttonFEQ{PROP:Icon}

  ! A prompt too of course
  SELF.tileQ.promptFEQ = Create(0, CREATE:Prompt, parentFeq)
  SELF.tileQ.promptFEQ{PROP:Hide} = FALSE
  SELF.tileQ.promptFEQ{PROP:Text} = SELF.tileQ.buttonFEQ{PROP:Text}
  SELF.tileQ.promptFEQ{PROP:FontColor} = COLOR:White
  SELF.tileQ.promptFEQ{PROP:Trn} = TRUE
  IF SELF.fontName
    SELF.tileQ.promptFEQ{PROP:FontName} = SELF.fontName
  END

  ! and a region with IMM for hover effects
  SELF.tileQ.regionFEQ = Create(0, CREATE:Region, parentFeq)
  SELF.tileQ.regionFEQ{PROP:Hide} = FALSE
  SELF.tileQ.regionFEQ{PROP:IMM} = TRUE

  Add(SELF.tileQ)

  SELF.SetTileElements()

ButtonTiles.SetTileElements         PROCEDURE()
pos                   GROUP
XPos                    SIGNED           !Horizontal coordinate
YPos                    SIGNED           !Vertical coordinate
Width                   UNSIGNED         !Width
Height                  UNSIGNED         !Height
                      END
savePixels            BYTE
  CODE
  savePixels = 0{PROP:Pixels}
  0{PROP:Pixels} = FALSE

  GetPosition(SELF.tileQ.buttonFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
  SetPosition(SELF.tileQ.boxFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
  SetPosition(SELF.tileQ.imageFEQ, pos.XPos+SELF.imageMarginLeft, pos.YPos+SELF.imageMarginTop, SELF.imageWidth, SELF.imageHeight)
  SetPosition(SELF.tileQ.promptFEQ, pos.XPos+SELF.promptMarginLeft, pos.YPos+pos.Height-SELF.promptMarginTop)
  SetPosition(SELF.tileQ.regionFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)

  0{PROP:Pixels} = savePixels

ButtonTiles.RefreshTile         PROCEDURE(SIGNED pButtonFEQ)
  CODE
  SELF.tileQ.buttonFEQ = pButtonFEQ
  Get(SELF.tileQ, SELF.tileQ.buttonFEQ)
  IF ErrorCode()
    RETURN
  END
  SELF.SetTileElements()

ButtonTiles.Construct                       PROCEDURE()
  CODE

  SELF.tileQ &= New(tileQ_Type)

  SELF.imageMarginLeft     = 6
  SELF.imageMarginTop      = 4
  SELF.imageWidth          = 22
  SELF.imageHeight         = 22
  SELF.promptMarginLeft    = 6
  SELF.promptMarginTop     = 11


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
      OF EVENT:MouseDown
        SELF.tileQ.boxFEQ{PROP:Fill} = MixColors(COLOR:Black, SELF.tileQ.fillNormal, SELF.tileQ.actionFactor)
      OF EVENT:MouseUp
          SELF.tileQ.boxFEQ{PROP:Fill} = SELF.tileQ.fillNormal
          Post(Event:Accepted, SELF.tileQ.buttonFEQ)
      OF EVENT:MouseIn
          SELF.tileQ.boxFEQ{PROP:Fill} = MixColors(COLOR:White, SELF.tileQ.fillNormal, SELF.tileQ.actionFactor)
      OF EVENT:MouseOut
        SELF.tileQ.boxFEQ{PROP:Fill} = SELF.tileQ.fillNormal
      END
    END
  END

  RETURN rv