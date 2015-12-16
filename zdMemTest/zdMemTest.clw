  PROGRAM

  PRAGMA('project(#pragma define(MallocIsRTL=>0))')
  PRAGMA('project(#pragma define(MallocIsDIY=>1))')

  Include('zdMem.inc'),ONCE
EnableZdMem zdMem

  MAP
  END

  CODE
  EnableZdMem.Test()