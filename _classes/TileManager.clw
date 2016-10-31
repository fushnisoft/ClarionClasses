  MEMBER()

  MAP
  END

  INCLUDE('TileManager.inc'),ONCE

TileManager.Construct                       PROCEDURE()
  CODE

  SELF.tileQ &= New(tileManagerQ_type)

TileManager.Destruct                        PROCEDURE()
  CODE
  Get(SELF.tileQ, 0)
  LOOP
    Get(SELF.tileQ, Pointer(SELF.TileQ)+1)
    IF ErrorCode()
      BREAK
    END
    Dispose(SELF.tileQ.Tile)
  END
  Free(SELF.tileQ)
  Dispose(SELF.tileQ)

TileManager.Init                PROCEDURE(WindowManager pWM, BYTE pLightenOnHoverPercentage=66, BYTE pPadding=8)
  CODE
  pWM.AddItem(SELF.WindowComponent)
  SELF.lightenOnHoverPercentage = pLightenOnHoverPercentage
  SELF.padding = pPadding

TileManager.AddButtonMimic PROCEDURE(SIGNED pButtonFEQ, LONG pColor, <BYTE pLightenOnHoverPercentage>, <BYTE pPadding>)
  CODE
  SELF.tileQ.buttonFEQ = pButtonFEQ
  SELF.tileQ.Tile     &= New(ButtonTile)
  SELF.tileQ.Tile.isToggleTile = SELF.enableToggleSet
  IF SELF.enableToggleSet AND SELF.currentToggleChoice = 0
    SELF.currentToggleChoice = pButtonFEQ
    SELF.tileQ.Tile.isToggledOn = TRUE
  END

  SELF.tileQ.Tile.Init(pButtonFEQ, |
    pColor, |
    Choose(Omitted(pLightenOnHoverPercentage), SELF.lightenOnHoverPercentage, pLightenOnHoverPercentage), |
    Choose(Omitted(pPadding), SELF.padding, pPadding))

  Add(SELF.tileQ)

TileManager.WindowComponent.Reset PROCEDURE(BYTE force)
  CODE
  PARENT.WindowComponent.Reset(force)
  Get(SELF.tileQ, 0)
  LOOP
    Get(SELF.tileQ, Pointer(SELF.TileQ)+1)
    IF ErrorCode()
      BREAK
    END
    SELF.tileQ.Tile.Refresh()
  END

TileManager.WindowComponent.TakeEvent PROCEDURE() ! ,BYTE ! Declare Procedure
rv                           BYTE
  CODE
  rv = PARENT.WindowComponent.TakeEvent()
  IF 0{Prop:AcceptAll} = TRUE
    RETURN rv
  END

  ! Handle events for the underlying region control
  IF SELF.FindToggleRegion(FIELD()) = LEVEL:Benign
    CASE EVENT()
    OF EVENT:MouseDown
      SELF.tileQ.Tile.SetTileState(BT_FILL_STATE:PRESSED)
    OF EVENT:MouseUp
        SELF.tileQ.Tile.SetTileState(BT_FILL_STATE:NORMAL)
        IF SELF.tileQ.Tile.buttonFEQ{PROP:Disable} = FALSE AND |
          MouseX() > SELF.tileQ.Tile.buttonFEQ{PROP:Xpos} AND |
          MouseX() < SELF.tileQ.Tile.buttonFEQ{PROP:Xpos}+SELF.tileQ.Tile.buttonFEQ{PROP:Width} AND |
          MouseY() > SELF.tileQ.Tile.buttonFEQ{PROP:Ypos} AND |
          MouseY() < SELF.tileQ.Tile.buttonFEQ{PROP:Ypos}+SELF.tileQ.Tile.buttonFEQ{PROP:Height}
          Post(Event:Accepted, SELF.tileQ.Tile.buttonFEQ)
        END
    OF EVENT:MouseIn
        IF SELF.tileQ.Tile.buttonFEQ{PROP:Disable} = TRUE
          Setcursor(TM_CURSOR:No)
        ELSE
          Setcursor(TM_CURSOR:Hand)
        END
        SELF.tileQ.Tile.SetTileState(BT_FILL_STATE:HOVER)
    OF EVENT:MouseOut
      Setcursor()
      SELF.tileQ.Tile.SetTileState(BT_FILL_STATE:NORMAL)
    END
  END

  IF SELF.FindButton(FIELD()) = LEVEL:Benign
    CASE EVENT()
    OF EVENT:Accepted
      SELF.currentToggleChoice = Field()

      Get(SELF.tileQ, 0)
      LOOP
        Get(SELF.tileQ, Pointer(SELF.TileQ)+1)
        IF ErrorCode()
          Clear(SELF.tileQ)
          BREAK
        END
        SELF.tileQ.Tile.isToggledOn = Choose(SELF.tileQ.Tile.buttonFEQ = Field())
        SELF.tileQ.Tile.SetTileState(BT_FILL_STATE:NORMAL)
      END
    END
  END
  RETURN rv

TileManager.FindToggleRegion          PROCEDURE(SIGNED pFieldFEQ) !,BYTE
  CODE
  IF pFieldFEQ{PROP:Type} <> CREATE:region
    RETURN level:notify
  END
  Get(SELF.tileQ, 0)
  LOOP
    Get(SELF.tileQ, Pointer(SELF.TileQ)+1)
    IF ErrorCode()
      Clear(SELF.tileQ)
      BREAK
    END
    IF SELF.tileQ.Tile.regionFEQ = pFieldFEQ
      BREAK
    END
  END
  RETURN Choose(SELF.tileQ.buttonFEQ>0, LEVEL:Benign, LEVEL:Fatal)

TileManager.FindButton                PROCEDURE(SIGNED pFieldFEQ) !,BYTE
  CODE
  IF pFieldFEQ{PROP:Type} <> CREATE:Button
    RETURN level:notify
  END
  Get(SELF.tileQ, 0)
  LOOP
    Get(SELF.tileQ, Pointer(SELF.TileQ)+1)
    IF ErrorCode()
      Clear(SELF.tileQ)
      BREAK
    END
    IF SELF.tileQ.Tile.buttonFEQ = pFieldFEQ
      BREAK
    END
  END
  RETURN Choose(SELF.tileQ.buttonFEQ>0, LEVEL:Benign, LEVEL:Fatal)

TileManager.GetToggleChoice           PROCEDURE() !,SIGNED
  CODE
  RETURN SELF.currentToggleChoice

TileManager.HideButton                PROCEDURE(SIGNED pButtonFeq, BYTE pHide=TRUE)
  CODE
  IF SELF.FindButton(pButtonFeq) = LEVEL:Benign
    SELF.tileQ.Tile.isHidden = pHide
    SELF.tileQ.Tile.Refresh()
  END
