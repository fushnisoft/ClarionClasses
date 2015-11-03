      Member()
      Include('ConsoleSupport.inc'),ONCE

fpAttachConsole LONG,NAME('AttachConsole')

	  Map
        MODULE('API')
            ! General functions
            GetLastError(),DWORD,PASCAL

            ! Console functions
            GetStdHandle(DWORD),HANDLE,PASCAL,PROC,RAW
            WriteConsole(Handle,Long,Dword,long,long),bool,Raw,Pascal,name('WriteConsoleA')
            WriteFile(Handle,Long,Dword,long,long),bool,Raw,Pascal,name('WriteFile')

            ReadConsole(Handle,Long,Dword,long,long),bool,Raw,Pascal,name('ReadConsoleA')
            SetConsoleTitle(Long),Bool,Raw,Pascal,name('SetConsoleTitleA')
            GetConsoleTitle(Long,dword),Bool,Raw,Pascal,name('GetConsoleTitleA')
            SetConsoleMode(Handle,dWord),BOOL,RAW,PASCAL
            GetConsoleMode(Handle,Long),BOOL,RAW,PASCAL
            FormatMessage(long,long,long,long,*cstring,long,long),long,Pascal,raw,name('FormatMessageA'),Dll(dll_mode)
        END
      END

ConsoleSupport.Construct PROCEDURE

  CODE

ConsoleSupport.Destruct PROCEDURE

  CODE

ConsoleSupport.Init				   PROCEDURE () !,BYTE,VIRTUAL
  CODE

    SELF.OutputHandle = GetStdHandle(STD_OUTPUT_HANDLE)
    IF SELF.OutputHandle = INVALID_HANDLE_VALUE
        Halt(1,'Unable to get output handle (' & SELF.GetLastSystemError() & ')')
        RETURN INVALID_HANDLE_VALUE
    END

    SELF.InputHandle = GetStdHandle(STD_INPUT_HANDLE)
    IF SELF.InputHandle = INVALID_HANDLE_VALUE
        Halt(2,'Unable to get console input handle (' & SELF.GetLastSystemError() & ')')
        RETURN INVALID_HANDLE_VALUE
    END

    IF SELF.InputHandle <> 0
      IF ~SetConsoleMode(SELF.InputHandle,ENABLE_PROCESSED_INPUT )
         Halt(3,'Unable to set console mode (' & SELF.GetLastSystemError() & ') SELF.InputHandle = ' & SELF.InputHandle)
         RETURN INVALID_OTHER
      END
    END

    RETURN FALSE

ConsoleSupport.WriteLine			   PROCEDURE (STRING pText) !,BYTE,PROC,VIRTUAL
  CODE
    SELF.TextBuffer = SELF.Prefix & pText & '<13,10>'
    IF WriteConsole(SELF.OutputHandle, ADDRESS(SELF.TextBuffer), LEN(SELF.TextBuffer),ADDRESS(SELF.BytesWritten), NULL) = 0
      ! IF WriteConsole fails then maybe we are supposed to use a "file" stream...
      IF WriteFile(SELF.OutputHandle, ADDRESS(SELF.TextBuffer), LEN(SELF.TextBuffer),ADDRESS(SELF.BytesWritten), NULL) = 0
        Halt(4,'WriteConsoleError (' & SELF.GetLastSystemError() & ')')
        RETURN -1
      END
    END
    RETURN FALSE

Consolesupport.ReadKey  			   PROCEDURE () !,STRING,PROC,VIRTUAL
  CODE
  SELF.WriteLine('Press any key to continue...')
  Clear(SELF.InBuffer)
  Loop
    IF ReadConsole(SELF.InputHandle,Address(SELF.InBuffer),100,Address(SELF.BytesRead),NULL) = 0 THEN
      Halt(5,'Error on read console (' & SELF.GetLastSystemError() & ')')
      Break
    END
  Until SELF.BytesRead > 0
  RETURN SELF.InBuffer


Consolesupport.GetLastSystemError                  PROCEDURE ( LONG pLastErr=0 ) !,STRING
err       long
mess      cstring(256)
result    long
  CODE
  IF pLastErr = 0
    err = GetLastError() ! instead get the error from LastError()
  ELSE
    err = pLastErr
  END
  result = FormatMessage(1000h, 0, err, 0, mess, size(mess)-1, 0) ! result = number of TCHAR characters
  IF result >= 2 and result <= size (mess)
    ! Strip off last CRLF IF there is one.
    IF mess [result-1 : result] = '<13,10>'
      mess [result-1] = '<0>'
    END
  END
  RETURN(clip(mess) & ' (' & Err & ')')
