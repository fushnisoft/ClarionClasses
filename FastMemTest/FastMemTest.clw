  PROGRAM

  PRAGMA('project(#pragma define(MallocIsRTL=>0))')
  PRAGMA('project(#pragma define(MallocIsDIY=>1))')

  Include('FastMem.inc'),ONCE
EnableFastMem FastMem

  MAP
  END

  CODE
  EnableFastMem.Test()