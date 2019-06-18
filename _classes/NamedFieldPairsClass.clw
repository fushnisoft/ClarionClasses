  MEMBER()

  INCLUDE('NamedFieldPairsClass.inc'),ONCE

NamedFieldPairsClass.Construct               PROCEDURE()
  CODE
  SELF.Init()

NamedFieldPairsClass.Destruct                PROCEDURE() !,VIRTUAL
  CODE
  SELF.Kill()

NamedFieldPairsClass.Init PROCEDURE()
  CODE
  SELF.Kill()
  SELF.Q &= New(NamedFieldPairsQueue)
  SELF.List &= SELF.Q
  RETURN

NamedFieldPairsClass.Kill                        PROCEDURE()
I UNSIGNED,AUTO
  CODE
  ! Note: We can't just call PARENT.Kill() here because that does a Dispose(SELF.List) which ends up in a GPF
  IF ~SELF.List &= NULL
    LOOP I = 1 TO RECORDS(SELF.List)
      GET(SELF.List,I)
      SELF.List.Left &= NULL
      SELF.List.Right &= NULL
    END
    SELF.List &= NULL
  END
  Dispose(SELF.Q)

  RETURN

NamedFieldPairsClass.AddNamedItem                PROCEDURE (*? Left, STRING pName)
  CODE
  SELF.AddItem(Left)
  SELF.Q.pairName = pName
  Put(SELF.Q)

NamedFieldPairsClass.AddNamedPair                PROCEDURE(*? Left,*? Right, STRING pName)
  CODE
  SELF.AddPair(Left, Right)
  SELF.Q.pairName = pName
  Put(SELF.Q)

NamedFieldPairsClass.GetLeft  PROCEDURE(STRING pName) !,?
  CODE
  SELF.Q.pairName = pName
  Get(SELF.Q, SELF.Q.pairName)
  IF ErrorCode() = FALSE
    RETURN SELF.Q.Left
  END
  RETURN ''

NamedFieldPairsClass.GetRight PROCEDURE(STRING pName) !,?
  CODE
  SELF.Q.pairName = pName
  Get(SELF.Q, SELF.Q.pairName)
  IF ErrorCode() = FALSE
    RETURN SELF.Q.Right
  END
  RETURN ''
