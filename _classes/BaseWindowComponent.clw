      MEMBER()
      MAP
      END
      INCLUDE('BaseWindowComponent.inc'),ONCE
BaseWindowComponent.WindowComponent.Kill PROCEDURE
  CODE
BaseWindowComponent.WindowComponent.Reset PROCEDURE(BYTE force)
  CODE
BaseWindowComponent.WindowComponent.ResetRequired PROCEDURE()!,BYTE
  CODE
  RETURN FALSE
BaseWindowComponent.WindowComponent.TakeEvent PROCEDURE()!,BYTE
  CODE
  RETURN Level:Benign
BaseWindowComponent.WindowComponent.SetAlerts PROCEDURE
  CODE
BaseWindowComponent.WindowComponent.Update PROCEDURE
  CODE
BaseWindowComponent.WindowComponent.UpdateWindow PROCEDURE
  CODE
