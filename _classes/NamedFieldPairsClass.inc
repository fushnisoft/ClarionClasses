  INCLUDE('ABUTIL.INC'),ONCE

NamedFieldPairsQueue QUEUE(FieldPairsQueue),TYPE
pairName CSTRING(255)
  END

NamedFieldPairsClass   CLASS(FieldPairsClass ),TYPE,MODULE('NamedFieldPairsClass.Clw'),LINK('NamedFieldPairsClass.Clw',_ABCLinkMode_),DLL(_ABCDllMode_)
Q                           &NamedFieldPairsQueue
Construct                   PROCEDURE()
Destruct                    PROCEDURE() ,VIRTUAL
Kill                        PROCEDURE()
Init                        PROCEDURE()
AddNamedItem                PROCEDURE(*? Left, STRING pName)
AddNamedPair                PROCEDURE(*? Left,*? Right, STRING pName)
GetLeft                     PROCEDURE(STRING pName) ,?
GetRight                    PROCEDURE(STRING pName) ,?
  END
