  PROGRAM

  PRAGMA('project(#pragma define(MallocIsDIY=>1))') ! use the slab allocator defined in this file
  !PRAGMA('project(#pragma define(MallocIsDIY=>0))') ! use the standard Clarion RTL allocator
  PRAGMA('project(#pragma define(MallocIsDLL=>1))') ! the project is using the DLL link model
  !PRAGMA('project(#pragma define(MallocIsDLL=>0))') ! the project is *not* using the DLL link model

  Include('zdMem.inc'),ONCE
EnableZdMem zdMem

  MAP
  END

  CODE
  EnableZdMem.Test()