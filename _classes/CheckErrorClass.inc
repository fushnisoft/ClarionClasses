    Include('CStringClass.inc'),ONCE
    Include('Errors.CLW'),ONCE

CheckErrorClass         CLASS(),TYPE,MODULE('CheckErrorClass.Clw'),LINK('CheckErrorClass.Clw',1)
CheckError                  PROCEDURE  (<STRING pMessage>) ,BYTE,PROC
                        END
