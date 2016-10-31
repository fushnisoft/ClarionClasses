  MEMBER()

  MAP
  INCLUDE('CWUTIL.INC'),ONCE
  END

  INCLUDE('ButtonTile.inc'),ONCE

ButtonTile.Init PROCEDURE(SIGNED pButtonFEQ, LONG pColor, BYTE pLightenOnHoverPercentage=66, BYTE pPadding=8)
  CODE
  SELF.buttonFEQ = pButtonFEQ
  SELF.tileColor = pColor
  SELF.lightenOnHoverPercentage = pLightenOnHoverPercentage
  SELF.padding = pPadding

  ! Store the hidden state then truely hide the original
  ! We need to keep the original button around though so we can post events to it!
  SELF.isHidden = SELF.buttonFEQ{PROP:Hide}
  SELF.buttonFEQ{PROP:Hide} = TRUE

  ! Make the controls we need
  SELF.boxFEQ = Create(0, CREATE:Box, SELF.buttonFEQ{PROP:Parent})
  SELF.imageFEQ = Create(0, CREATE:Image, SELF.buttonFEQ{PROP:Parent})
  SELF.promptFEQ = Create(0, CREATE:Prompt, SELF.buttonFEQ{PROP:Parent})
  SELF.regionFEQ = Create(0, CREATE:Region, SELF.buttonFEQ{PROP:Parent})

  SELF.Refresh()

ButtonTile.Refresh         PROCEDURE()
pos                   GROUP
XPos                    SIGNED           !Horizontal coordinate
YPos                    SIGNED           !Vertical coordinate
Width                   UNSIGNED         !Width
Height                  UNSIGNED         !Height
                      END
imageWidth            UNSIGNED
imageHeight           UNSIGNED
savePixels            BYTE
font                  GROUP
typeFace                STRING(31)
size                    BYTE
color                   LONG
style                   LONG
charSet                 LONG
                      END
  CODE

  SELF.imageFEQ{PROP:Text} = SELF.buttonFEQ{PROP:Icon}

  SELF.promptFEQ{PROP:Text} = SELF.buttonFEQ{PROP:Text}
  SELF.promptFEQ{PROP:FontColor} = COLOR:White
  SELF.promptFEQ{PROP:Trn} = TRUE
  !SELF.promptFEQ{PROP:Color} = color:red
  GetFont(SELF.buttonFEQ, font.typeFace, font.size, font.color, font.style, font.charSet)
  SetFont(SELF.promptFEQ, font.typeFace, font.size, font.color, font.style, font.charSet)

  SELF.regionFEQ{PROP:IMM} = TRUE
  SELF.regionFEQ{PROP:TIP} = SELF.buttonFEQ{PROP:TIP}

  savePixels = 0{PROP:Pixels}
  0{PROP:Pixels} = FALSE
  ! Clarion seems to like to add 2 DLUs to the bottom of a prompt.
  ! This makes it weird so we remove them from the height
  IF SELF.originalPromptHeight = 0
    SELF.originalPromptHeight = SELF.promptFEQ{PROP:Height}
  END
  SELF.promptFEQ{PROP:Height} = SELF.originalPromptHeight - 2

  ! The rest we do in pixels
  0{PROP:Pixels} = TRUE

  GetPosition(SELF.buttonFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)

  ! Configure the prompt position and width
  IF SELF.imageFEQ{PROP:Text}
    SetPosition(SELF.promptFEQ, pos.XPos+SELF.padding, pos.YPos+pos.Height-SELF.promptFEQ{PROP:Height}-SELF.padding, pos.Width-(SELF.padding*2))
    SELF.promptFEQ{PROP:CENTER} = FALSE
  ELSE
    SetPosition(SELF.promptFEQ, pos.XPos+SELF.padding, pos.YPos+((pos.Height-SELF.promptFEQ{PROP:Height})/2), pos.Width-(SELF.padding*2))
    SELF.promptFEQ{PROP:CENTER} = TRUE
  END

  ! Setup the box and region
  SetPosition(SELF.boxFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
  SELF.SetTileState(BT_FILL_STATE:NORMAL)

  SetPosition(SELF.regionFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)

  ! Calculate the image size and position
  imageWidth = SELF.buttonFEQ{PROP:Width}-(SELF.promptFEQ{PROP:XPos}-SELF.buttonFEQ{PROP:XPos})
  imageHeight = SELF.promptFEQ{PROP:YPos}-pos.YPos
  IF imageWidth < imageHeight
    imageHeight = imageWidth
  ELSIF imageHeight < imageWidth
    imageWidth = imageHeight
  END

  IF SELF.promptFEQ{PROP:Text} <> ''
    SetPosition(SELF.imageFEQ, |
      pos.XPos+SELF.padding, |
      pos.YPos+SELF.padding, |
      imageWidth, |
      imageHeight)
  ELSE
    SetPosition(SELF.imageFEQ, |
      pos.XPos+((pos.Width-SELF.imageFEQ{PROP:Width})/2), |
      pos.YPos+((pos.Height-SELF.imageFEQ{PROP:Height})/2))
  END
  0{PROP:Pixels} = savePixels

  ! Set the visability and state
  SELF.boxFEQ{PROP:Hide} = SELF.isHidden
  SELF.regionFEQ{PROP:Hide} = SELF.isHidden
  SELF.imageFEQ{PROP:Hide} = Choose(SELF.imageFEQ{PROP:Text} = '' OR SELF.isHidden)
  SELF.promptFEQ{PROP:Hide} = SELF.isHidden
  SELF.imageFEQ{PROP:Disable} = SELF.buttonFEQ{PROP:Disable} ! Doesn't seem to actually do anyting.
  SELF.promptFEQ{PROP:Disable} = SELF.buttonFEQ{PROP:Disable}

ButtonTile.GetFillColor        PROCEDURE(BYTE pState=BT_FILL_STATE:NORMAL) !,LONG
  CODE

  SELF.promptFEQ{PROP:FontColor} = COLOR:White

  IF SELF.buttonFEQ{PROP:Disable} = TRUE
    RETURN MixColors(SELF.tileColor, COLOR:White, SELF.lightenOnHoverPercentage)
  END

  IF NOT SELF.isToggleTile
    CASE pState
    OF BT_FILL_STATE:HOVER
      SELF.promptFEQ{PROP:FontColor} = Choose(SELF.lightenOnHoverPercentage > 50, MixColors(SELF.tileColor, COLOR:Black, SELF.lightenOnHoverPercentage), COLOR:White)
      RETURN MixColors(SELF.tileColor, COLOR:White, SELF.lightenOnHoverPercentage)

    OF BT_FILL_STATE:PRESSED
      RETURN MixColors(SELF.tileColor, COLOR:Black, SELF.lightenOnHoverPercentage)

    ELSE ! BT_FILL_STATE:NORMAL
      RETURN SELF.tileColor
    END
  ELSE
    ! If this button/tile is part of a toggle set then we act differently
    CASE pState
    OF BT_FILL_STATE:HOVER
      RETURN SELF.tileColor

    OF BT_FILL_STATE:PRESSED
      RETURN MixColors(SELF.tileColor, COLOR:Black, 50)

    ELSE ! BT_FILL_STATE:NORMAL
      IF SELF.isToggledOn
        SELF.promptFEQ{PROP:FontColor} = COLOR:White
        RETURN SELF.tileColor
      ELSE
        SELF.promptFEQ{PROP:FontColor} = Choose(SELF.lightenOnHoverPercentage > 50, MixColors(SELF.tileColor, COLOR:Black, SELF.lightenOnHoverPercentage), COLOR:White)
        RETURN MixColors(SELF.tileColor, COLOR:White, SELF.lightenOnHoverPercentage)
      END
    END
  END

ButtonTile.SetTileState     PROCEDURE(BYTE pState)
  CODE
  SELF.boxFEQ{PROP:Fill} = SELF.GetFillColor(pState)
  SELF.boxFEQ{PROP:Color} = SELF.boxFEQ{PROP:Fill}
