    Include('CStringClass.inc'),ONCE
    INCLUDE('BaseWindowComponent.inc'),ONCE
    INCLUDE('SimpleCache.inc'),ONCE

PhoneMaskHandler         CLASS(BaseWindowComponent),IMPLEMENTS(WindowComponent),TYPE,MODULE('PhoneMaskHandler.Clw'),LINK('PhoneMaskHandler.Clw',1)
WM                             &WindowManager
FieldCache                     &SimpleCache
Construct                      PROCEDURE()
Destruct                       PROCEDURE() ,VIRTUAL
Init                           PROCEDURE(WindowManager pWM, BYTE pEnabled)
TakeFieldEvent                 PROCEDURE(SIGNED pField)
                        END

