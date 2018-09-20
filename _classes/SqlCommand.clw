  MEMBER()

  INCLUDE('EQUATES.CLW'),ONCE

  MAP
  END

  INCLUDE('SqlCommand.inc'),ONCE

SqlCommand.Init            PROCEDURE(STRING pOptions, STRING pOwner)
fieldGroup                   GROUP(TFieldGrp)
                             END
  CODE
  SELF.DynFile.UnfixFormat()
  SELF.DynFile.ResetAll()
  SELF.DynFile.SetDriver('MSSQL')
  SELF.DynFile.SetName('dbo.SqlCommandTempTable' & Random(1,1000) & Thread())
  SELF.DynFile.SetCreate(TRUE)
  SELF.DynFile.SetOwner(pOwner)

  fieldGroup.FieldNbr  = 1
  fieldGroup.Label     = 'Result'
  fieldGroup.Type      = 'CSTRING'
  fieldGroup.Size      = 1024
  SELF.DynFile.AddField(fieldGroup)

  SELF.DynFile.FixFormat()
  SELF.TheFile &= SELF.DynFile.GetFileRef()
  SELF.TheFile{PROP:DriverString} = pOptions


SqlCommand.ExecuteReader     PROCEDURE() !,STRING,PROC
RV        CStringClass
  CODE

  SELF.SetLastError()
  IF SELF.isManualConnection = FALSE
    SELF.Open(SELF.isManualConnection)
  END

  SELF.TheFile{PROP:SQL} = SELF.Str()
  SELF.SetLastError(Choose(ErrorCode()=90, FileError(), Error()),|
                    Choose(ErrorCode()=90, FileErrorCode(), ErrorCode()))
  SELF.Errors.CheckError('SELF.TheFile{{PROP:SQL}||SQL: ' & SELF.Str())

  IF SELF.isManualConnection = FALSE
    RV.Str(SELF.Read())
    SELF.Close()
  END

  RETURN RV.Str()

SqlCommand.ExecuteNonQuery   PROCEDURE(BYTE pSilent=TRUE) !,BYTE,PROC
  CODE

  SELF.SetLastError()
  Open(SELF.TheFile)
  IF pSilent=FALSE
    SELF.Errors.CheckError('Open(SELF.TheFile)')
  END

  SELF.TheFile{PROP:SQL} = SELF.Str()
  SELF.SetLastError(Choose(ErrorCode()=90, FileError(), Error()),|
                    Choose(ErrorCode()=90, FileErrorCode(), ErrorCode()))

  IF pSilent=FALSE
    SELF.Errors.CheckError('SELF.TheFile{{PROP:SQL}||SQL: ' & SELF.Str())
  END

  Close(SELF.TheFile)
  IF pSilent=FALSE
    SELF.Errors.CheckError('Close(SELF.TheFile)')
  END

  RETURN Choose(SELF.lastFileErrorCode>0, level:notify, level:benign)

SqlCommand.SetLastError        PROCEDURE(<STRING pFileError>, <STRING pFileErrorCode>)
  CODE
  SELF.lastFileError       = Choose(Omitted(pFileError),'',pFileError)
  SELF.lastFileErrorCode   = Choose(Omitted(pFileErrorCode),'',pFileErrorCode)

SqlCommand.Open                        PROCEDURE(BYTE pIsManualConnect=TRUE)
  CODE
  SELF.isManualConnection = pIsManualConnect
  Open(SELF.TheFile)
  SELF.Errors.CheckError('Open(SELF.TheFile)')

SqlCommand.Close                       PROCEDURE()
  CODE
  Close(SELF.TheFile)
  SELF.isManualConnection = FALSE ! Reset this after close!

SqlCommand.Read                        PROCEDURE() !,STRING
  CODE
  Next(SELF.TheFile)
  IF ErrorCode()
    RETURN ''
  END
  RETURN SELF.DynFile.GetField('Result')

SqlCommand.Construct                   PROCEDURE()
  CODE
  SELF.Errors &= New(CheckErrorClass)
  SELF.DynFile &= New(DynFile)

SqlCommand.Destruct                    PROCEDURE()
  CODE
  Dispose(SELF.Errors)
  SELF.DynFile.UnfixFormat()
  Dispose(SELF.DynFile)