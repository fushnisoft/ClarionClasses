!{{{}}}

  MEMBER()

!{{{  history

! 24/04/15 DCN Created by extraction from match_do.inc
! 03/12/15 DCN Remove nedMalloc option
!{{{  04/12/15 DCN

!This memory allocator was written by Dave Nichols as part of the Match-IT
!production control system to dramatically improve its memory allocation
!performance. The improvement is in speed at the cost of some space wastage.

!Its is donated to the Clarion community in the hope that it proves useful
!to others. It is in use in our production control software which is a mission
!critical application for manufacturers - http://www.match-it.com

!The author can be contacted at dave.nichols@match-it.com

!}}}
! 12/12/15 DCN Add clear stats command
!              Simplify pragmas, only need MallocIsDIY set TRUE
! 16/12/15 DCN zdMemAligned, zdMemProcessStart

!}}}
!{{{  license.txt (MIT)
OMIT('ENDOMIT')

Copyright (c) 2015 Dave Nichols

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

!ENDOMIT
!}}}
!{{{  description

!In the context of Match-IT, this memory allocator is up to 100 times faster
!than the native Clarion one. Yes ONE HUNDRED!!

!This file is completely stand-alone, it has no dependencies,
!not even on the Clarion RTL (other than finding where _malloc
!et al are located).

!This file controls the memory allocation system to use. There are two
!options controlled by a pragma in the build file:
!  MallocIsDIY=>1  use the slab allocator defined in this file
!  MallocIsDIY=>0  use the standard Clarion RTL allocator
!  MallocIsDLL=>1  the project is using the DLL link model
!  MallocIsDLL=>0  the project is *not* using the DLL link model

!Uncomment these lines if you do not want to add the pragmas to your project:
!MallocIsDIY EQUATE(1)
!MallocIsDLL EQUATE(1)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

!The DIY scheme implements a very simple but very fast 'slab' memory allocator.
!The slab allocator is lock-less, its organised in such a way that for the
!vast majority of allocations there is no locking required, the allocation
!will complete in finite time even if multiple threads attempt the same
!allocation at the same time (with multi-core processors this is common).
!It replaces the Clarion RTL malloc by run-time patching the code to jump
!to this logic instead of Clarion.

!The logic here is completely standalone with just three functions visible
!to the application for the purposes of starting and stopping the allocator.

!{{{  concepts

!Over 90% of typical allocations are in the 2..32 byte range, so the allocator
!is biased to make such allocations (and de-allocations) very fast and space
!efficient (i.e. low management overhead).

!This 'slab' allocator breaks up memory allocation lengths into
!fixed size 'bins'. These bin sizes start at a minimum of 4 and
!increase approximately exponentially up to about 512K, thereafter
!the bin size is whatever the caller asks for rounded up by the allocation
!granularity. The maximum allocation possible is 2G (i.e. a positive int32).

!{{{  locks

!The algorithm does not use locks for normal allocations. The only time a
!lock is used is for garbage collection (which is very rare). The essence
!of the algorithm is singly-linked lists of free memory chunks. Each 'bin'
!has its own list with a known beginning. The beginning of the free list is
!modified when a free chunk is removed or added to the list. The adding/removing
!operation involves a single assign and the algorithm *gaurantees* this change
!is atomic and the free list pointer is *always* valid. It relies on the
!InterlockedCompareExchange64 operation - see 
!https://msdn.microsoft.com/en-us/library/windows/desktop/ms683562%28v=vs.85%29.aspx
!This provides for an atomic change to memory provided the existing value is
!that expected. This is all that is needed to implement an atomic change to
!a single-linked list.

!The logic reads the current value of the list base, calculates what it needs
!to be changed to, then attempts the change. If it succeeds then there was no
!contention and the logic can continue. If it fails it means some other thread
!snuck in, so the logic just starts again until it does succeed. It will within
!N tries, where N is the number of logical cores running the process. So no context
!switching is required, ever!

!}}}
!{{{  address space

!This allocator only works for a 32 bit address space. If your app
!ever goes 64 bit this allocator will need updating. Will Clarion
!ever do that?

!The whole of the 4G address space is considered to consist of
!64K 'pages', so there are 64K pages to span the whole address
!space. These pages are a mixture of those controlled by this
!allocator and those controlled by some other (e.g. the Clarion
!RTL during start-up). Pages controlled by this allocator have
!a 'bin' number associated with them. A bin number is in the range
!1..255 (see zdMemSize2BinNum), so a 'foriegn' page address can be
!easily detected by a single look-up table with 64K entries. Foreign
!addresses will have bin number 0.

!The allocator allocates bin size chunks from 'slabs'. A slab is a
!set of contiguous pages that is at least 64K and is page aligned.
!A slab has a bin number associated with it and the slab is divided
!into a set of fixed size chunks. Unused chunks are singly linked
!into a free list. There may be many slabs with the same bin number
!and the free list may span slab boundaries. Associated with each
!bin number is the head of the free list for that bin size.

!In Windows 7, the virtual address space allocation page size is
!4K, so our 64K page is an exact multiple of that which means we
!waste nothing. Also, the allocation granularity in Windows 7 is
!64K and our minimum slab size is also 64K which is an exact multiple,
!so we won't waste any space due to granularity either.

!With this arrangement the absolute worst case memory wastage is
!64K * 256 (i.e. 16M) which is considered tolerable.

!}}}
!{{{  commands

!Because allocation sizes must be positive, negative sizes are used
!for 'commands' directed at the allocator. Such commands can get information
!out of the allocator or set parameters. This mechanism is useful 'cos
!the allocator functions are not visible to the main application.
!The 'address' returned by such commands is either the direct result
!or the address of some static structure with the result in it.

!}}}

!}}}

!}}}
!{{{  how to use it

!At the very beginning of your main app source file add this:
!PatchProcess CLASS
!               Construct PROCEDURE
!               Destruct  PROCEDURE
!             END

!and in your MAP section add this:
!  MODULE('zd_alloc.clw')
!    zdMemProcessStart(),NAME('zdMemProcessStart')
!    zdMemProcessStop(),NAME('zdMemProcessStop')
!  END

!and in your code section add this:
!PatchProcess.Construct PROCEDURE
!  CODE
!  zdMemProcessStart()
!PatchProcess.Destruct PROCEDURE
!  CODE
!  zdMemProcessStop()

!Then include this source file in your build.

!That's it!

!NB: Only do this once in your main .EXE app, do *NOT* do it in any DLLs you may also have in the project.

!The PatchProcess CLASS must be as earlier as possible as its constructor is
!called by the Clarion RTL as your program starts and we want to setup the memory
!allocator before the RTL has done too much work and gobbled up a load of memory.

!NB: InterlockedCompareExchange64 is only available from Windoze Vista onwards.
!    So if you're using XP, this allocator falls back to a ticket-lock, this
!    means under XP this is *NOT* a lock-less allocator and its performance
!    falls off a cliff (but still better than Native Clarion)!

!{{{  main source file skeleton
OMIT('ENDOMIT')

!This is a skeleton of how your main source file should look after you've done the above:

  PROGRAM

  MAP
    !... your stuff
    MODULE('zd_alloc.clw')
      zdMemProcessStart(),NAME('zdMemProcessStart')
      zdMemProcessStop(),NAME('zdMemProcessStop')
    END
  END

PatchProcess CLASS
Construct      PROCEDURE
Destruct       PROCEDURE
             END

!...  your stuff

  CODE
  !... your stuff

PatchProcess.Construct PROCEDURE
  CODE
  zdMemProcessStart()


PatchProcess.Destruct PROCEDURE
  CODE
  zdMemProcessStop()

!ENDOMIT
!}}}

!}}}
!{{{  debug aids

!In the authors system DEBUG is a project pragma that is =1 in a development
!context and =0 in a release context, change it to suit your system.

!Uncomment this line if you do not want to put a DEBUG pragma in your project
!DEBUG    EQUATE(0)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

!These flags turn on/off various logging/checking facilities to aid in diagnosing
!memory allocation issues. They dramatically impair performance, so should be
!turned off in a release build.

COMPILE('ENDCOMPILE',DEBUG=1)  !development context
LOGGING  EQUATE(FALSE)         !iff TRUE log activity for diagnostics (debug aid)
CHECKING EQUATE(FALSE)         !iff TRUE check address validity etc   (debug aid)  ***WARNING*** slows things down dramatically
HOOKING  EQUATE(FALSE)         !iff TRUE provide debug break hooks
XP_MODE  EQUATE(FALSE)         !iff TRUE run as if under XP
ENDCOMPILE
OMIT('ENDOMIT',DEBUG=1)        !release context
LOGGING  EQUATE(FALSE)         !don't want these in the wild
CHECKING EQUATE(FALSE)         !..
HOOKING  EQUATE(FALSE)         !....
XP_MODE  EQUATE(FALSE)         !......
ENDOMIT

!}}}
!{{{  MAP

  MAP

    zdMemProcessStart(),NAME('zdMemProcessStart')  !hook our malloc into the RTL etc
    zdMemProcessStop(),NAME('zdMemProcessStop')    !un-hook it
    zdMemSetPanic(LONG),NAME('zdMemSetPanic')      !set the out of memory panic function

    !DIY version when running under XP
    zdMemLockedCompareExchange64(LONG Destination,LONG ExchangeLS,LONG ExchangeMS,LONG ComparandLS,LONG ComparandMS),LONG,PASCAL
    zdMemLock64()                                  !required to emulate 64 atomic access under win XP
    zdMemUnLock64()                                !..

    zdMemDivide(LONG A,LONG B),LONG                !integer floor division
    zdMemAbove(LONG,LONG),LONG                     !unsigned comparison
    zdMemMakeBinEdges()
    zdMemMakeSmallSizeBins()
    zdMemSize2BinNum(LONG),LONG
    zdMemSize2SlabSize(LONG),LONG
    zdMemAddress2SlabTop(LONG,LONG),LONG
    zdMemUsedAddressRange(LONG,LONG,LONG)
    zdMemUnusedAddressRange(LONG,LONG),LONG,PROC
    zdMemSlab2Size(LONG),LONG
    zdMemBin2Residue(LONG),LONG
    zdMemBin2Chunks(LONG),LONG
    zdMemCommand(LONG),LONG

    zdMemAlloc(LONG,LONG=256),LONG
    zdMemFree(LONG)
    zdMemAllocSlab(LONG,LONG),LONG
    zdMemFreeSlab(LONG bin,LONG addr),LONG,PROC
    zdMemTrimSlabs(LONG bin=0),LONG,PROC
    zdMemMalloc(LONG),LONG
    zdMemAligned(LONG,LONG),LONG
    zdMemRealloc(LONG,LONG),LONG
    zdMemCalloc(LONG,LONG),LONG
    zdMemPanic(LONG,LONG)                            !called when run out of space
    zdMemAbort(LONG PanicCode)

    !Debug helpers
    zdMemValidateAddress(LONG,LONG,LONG=0,LONG=0)
    zdMemBreak(LONG len,BYTE condition)

    !Diagnostics
    zdMemLogSlabAlloc (LONG Bin)                     !log a slab allocation in the approp bin size
    zdMemLogSlabFree  (LONG Bin)                     !log a slab de-allocation in the approp bin size
    zdMemLogChunkAlloc(LONG ptr,LONG len)            !log a chunk allocation in the approp bin size
    zdMemLogChunkFree (LONG ptr)                     !log a chunk de-allocation in the approp bin size
    zdMemLogAligned   (LONG Alignment,LONG len)      !log an aligned allocation in the approp bin size
    zdMemLogClash     (LONG bin)                     !log a thread clash in the approp bin
    zdMemLogTrim      (LONG bin)                     !log a trim operation in the approp bin
    zdMemLogBinUp     (LONG len)                     !log a realloc bin size increase
    zdMemLogBinDown   (LONG len)                     !log a realloc bin size decrease
    zdMemLogBinSame   (LONG len)                     !log a realloc bin size no-change
    zdMemLogPanic     (LONG len)                     !log a bin panic (run out of memory)
    zdMemClearStats   (LONG=0)                       !clear usage statistics

    MODULE('application')
      SysPanic(),LONG,PROC,NAME('SysPanic'),DLL(1)   !Manually 'linked' to SysPanicP via having the same name
    END

    !{{{  MODULE('Clarion')
    MODULE('Clarion')
      oldMalloc(LONG),LONG,NAME('_malloc')                     !ptr = (size)
      oldRealloc(LONG,LONG),LONG,NAME('_realloc')              !ptr = (ptr,size)
      oldCalloc(LONG,LONG),LONG,NAME('_calloc')                !ptr = (num,size)
      oldFree(LONG),NAME('_free')                              !(ptr)
      oldAligned(LONG,LONG),LONG,NAME('__aligned_malloc')      !ptr = (size,alignment)
      memSet(LONG,LONG,LONG),NAME('_memset')                   !(ptr,val,len)
      memCpy(LONG,LONG,LONG),NAME('_memcpy')                   !(dst,src,len)
    END
    !}}}
    !{{{  MODULE('Windows')
    MODULE('Windows')
      !if returns NULL it failed, use GetLastError() to find out why
      !LPVOID WINAPI VirtualAlloc(
      !  _In_opt_  LPVOID lpAddress,       //0=let the OS find space
      !  _In_      SIZE_T dwSize,          //auto rounded up to next page size
      !  _In_      DWORD flAllocationType, //use MEM_RESERVE+MEM_COMMIT
      !  _In_      DWORD flProtect         //use PAGE_READWRITE
      !);
      VirtualAlloc(LONG,LONG,LONG,LONG),LONG,RAW,PASCAL,NAME('VirtualAlloc')
      !iff returns FALSE it failed, use GetLastError() to find out why
      !BOOL WINAPI VirtualFree(
      !  _In_  LPVOID lpAddress,           //use address from VirtualAlloc
      !  _In_  SIZE_T dwSize,              //0=release the lot
      !  _In_  DWORD dwFreeType            //use MEM_RELEASE to uncommit and release
      !);
      VirtualFree(LONG,LONG,LONG),LONG,RAW,PASCAL,NAME('VirtualFree')

      VirtualProtect(LONG,LONG,LONG,*LONG),LONG,PROC,RAW,PASCAL,NAME('VirtualProtect')
      MessageBeep(LONG BeepType=010h),RAW,PASCAL,NAME('MessageBeep')   !type 010h is the standard critical error noise
      LockedIncrement(*LONG),LONG,PROC,PASCAL,RAW,NAME('InterlockedIncrement')
      LockedDecrement(*LONG),LONG,PROC,PASCAL,RAW,NAME('InterlockedDecrement')
      LockedAdd(*LONG,LONG),LONG,PROC,PASCAL,RAW,NAME('InterlockedAdd')
      !{{{  LockedCompareExchange64
      !This function in win32 requires a pointer to a 64 bit value and 2 64 bit value params
      !and returns a 64 bit value. Clarion can't do that, so we have to play tricks. The address
      !is no problem, the 2 64 bit operands are treated as 4 32 bit ones, the nett effect in the
      !stack is the same, the 64 bit result is in EDX and EAX, Clarion just grabs EAX, so we
      !arrange things such that the EAX part of the result is the critical bit (the aba counter).
      !This function only exists from Windoze Vista onwards, its part of kernel32.dll and is not
      !included in the Clarion win32.lib, these 2 factors mean we dynamically link to this function
      !at run-time if it exists, or use a (slow) DIY equivalent if it doesn't.
      LockedCompareExchange64(LONG Destination,LONG ExchangeLS,LONG ExchangeMS,LONG ComparandLS,LONG ComparandMS),LONG,PASCAL,RAW,DLL(1),NAME('InterlockedCompareExchange64') !returns initial value of destination
      !}}}
      Snooze(UNSIGNED),PASCAL,NAME('Sleep')
      StartDebugger(),PASCAL,NAME('DebugBreak')
      AbortApp(LONG=0,LONG),PASCAL,RAW,NAME('FatalAppExitA')
      ThreadId(),LONG,PASCAL,RAW,NAME('GetCurrentThreadId')
      LoadLibrary(*CSTRING),LONG,RAW,PASCAL,NAME('LoadLibraryA')
      GetProcAddress(LONG,*CSTRING),LONG,RAW,PASCAL,NAME('GetProcAddress')
    END
    !}}}
    
  END

!}}}

!{{{  constants

SECTION('MEMCONSTANTS')
!{{{  memory allocator commands

!The memory allocator recognises these special lengths as commands rather
!than requests for memory. Such commands can be used to interrogate various
!parameters/statistics of the allocator. They are all -ve so can be easily
!detected by "IF len < 0 THEN ...got command...".

zdMem:Command        EQUATE(080000000h)  !MSB set means its a commnand (makes length -ve)
zdMem:OpCodeMask     EQUATE(0FF000000h)  !BAND mask for the op-code               (op-code is 7 bits)
zdMem:ParamMask      EQUATE(00000FFFFh)  !BAND mask for the op parameter (if any) (param is 16 bits)

zdMem:SetHoldLimit   EQUATE(081000000h)  !set the max RAM hold limit, param is limit in 64K pages, 0=no limit
zdMem:GetHoldLimit   EQUATE(082000000h)  !returns the max RAM hold limit in 64K pages
zdMem:GetHeldNow     EQUATE(083000000h)  !returns the currently held number of 64K pages
zdMem:GetStatsTable  EQUATE(084000000h)  !returns the address of a zdMemStatsType structure
zdMem:GetMaxBin      EQUATE(085000000h)  !returns the highest used fixed size bin number (1..255)
zdMem:GetForeign     EQUATE(086000000h)  !returns the count of 'foriegn' frees seen
zdMem:TrimSlabs      EQUATE(087000000h)  !return unused slabs to the OS, param is bin to trim, 0=all, return #bytes released
zdMem:GetChunkSize   EQUATE(088000000h)  !returns bin chunk size in bytes, param is bin number
zdMem:GetSlabSize    EQUATE(089000000h)  !returns bin slab size in bytes, param is bin number
zdMem:GetSlabResidue EQUATE(08A000000h)  !returns bin slab unusable size in bytes, param is bin number
zdMem:GetOptions     EQUATE(08B000000h)  !returns LOGGING(1),CHECKING(2),HOOKING(4),XP_MODE(8) as OR'd bits
zdMem:ClearStats     EQUATE(08C000000h)  !clear count accumulator statistics, returns the address of a zdMemStatsType structure

!}}}
!{{{  zdMemStatsType

zdMemMaxBin    EQUATE(256)               !this is the absolute max possible, actual usage is lower

!These are the memory allocation statistics gathered by our memory allocator.

zdMemStatsType GROUP,TYPE
SlabCount        LONG,DIM(zdMemMaxBin)   !total number of slabs allocated
Slabs            LONG,DIM(zdMemMaxBin)   !number of slabs in use
MaxSlabs         LONG,DIM(zdMemMaxBin)   !max in use slabs
ChunkCount       LONG,DIM(zdMemMaxBin)   !total chunks allocation
Chunks           LONG,DIM(zdMemMaxBin)   !chunks currently in use
MaxChunks        LONG,DIM(zdMemMaxBin)   !max in use chunks
Align2Count      LONG,DIM(zdMemMaxBin)   !total allocation alignments for this alignment
Align4Count      LONG,DIM(zdMemMaxBin)   !total allocation alignments for this length
ClashCount       LONG,DIM(zdMemMaxBin)   !number of thread clashes accessing this bin
BinUpCount       LONG,DIM(zdMemMaxBin)   !number of re-allocs requiring a bin size increase
BinDownCount     LONG,DIM(zdMemMaxBin)   !number of re-allocs requiring a bin size decrease
BinSameCount     LONG,DIM(zdMemMaxBin)   !number of re-allocs requiring no bin size change
PanicCount       LONG,DIM(zdMemMaxBin)   !number of panics on this bin size
TrimCount        LONG,DIM(zdMemMaxBin)   !number of trims on this bin size
               END

!}}}
SECTION('ENDMEMCONSTANTS')

!{{{  validate address options

!These are bit masks that can be passed to zdMemValidateAddress to tell
!it what to check.

Validate:Bin       EQUATE(1)             !check the bin number is consistent with the address
Validate:Size      EQUATE(2)             !when CHECKING - check the tail contains the bin number
Validate:Residue   EQUATE(4)             !check the residue is consistent with the bin number

!}}}
!{{{  'jump' jamming stuff

JumpType               GROUP,TYPE              !What we jam into the function
Op                       BYTE
Offset                   LONG
                       END

ASM_JMP32REL           EQUATE(0E9h)            !JUMP instruction with a 32-bit offset

ASM_JMPLEN             EQUATE(SIZE(JumpType))  !how big our 'jump' instruction is

PAGE_EXECUTE_READWRITE EQUATE(040h)            !allow us to mess with the EXE code

!}}}

PAGE_READWRITE     EQUATE(04h)           !virtual address protection we use

MEM_COMMIT         EQUATE(1000h)         !virtual allocation type
MEM_RESERVE        EQUATE(2000h)         !..

MEM_RELEASE        EQUATE(8000h)         !virtual free type

!This is the minimum virtual page size we allocate.
!All pages we allocate via VirtualAlloc are multiples of this.
!We use 64K as the quanta as that is the allocation granularity of
!VirtualAlloc when reserving address space (in Win7/32 at least).

zdMemQuanta        EQUATE(64*1024)
zdMemAddrShift     EQUATE(16)            !divide by 64K (must match quanta above)
zdMemMaxQuanta     EQUATE(64*1024)       !maximum number of quanta in 4G

!When CHECKING buffers are over-allocated by this and the extra filled
!with the bin number. This is checked on a 'free' to detect buffer over
!runs. Also, an extra 4 bytes is allocated on the front of the chunk to
!hold the requested size. This is used for checking the actual usage
!rather than the (larger) chunk size.

zdMemCheckPrefix   EQUATE(4)                   !extra space on the front (to hold actual length)
zdMemCheckSuffix   EQUATE(zdMemCheckPrefix+8)  !extra space on the back (to hold check pattern)

!Aligned allocation max alignment

zdMemMaxAlignment  EQUATE(zdMemQuanta)

!}}}
!{{{  'panic' stuff

!This points to the application function to call when we run out of room.
SysPanicP        LONG,NAME('SysPanic')         !address of the SysPanic() function

!Elbow room for the Panic function
PanicSpaceSize   EQUATE(010000h)               !64K

!Panic function reason codes (called when we run out of space or detect a problem)

  ITEMIZE
!Recoverable
Panic:Malloc         EQUATE
Panic:Realloc        EQUATE
Panic:Calloc         EQUATE
Panic:Aligned        EQUATE
!Put new recoverable codes before this line

!Fatal
Panic:FirstFatal     EQUATE
Panic:Free           EQUATE
Panic:Alignment      EQUATE
Panic:Foreign        EQUATE
Panic:BadBin         EQUATE
Panic:BadAddress     EQUATE
Panic:BadChunk       EQUATE
Panic:BadBinSizes    EQUATE
Panic:BadBinSizesEnd EQUATE
Panic:Overflow       EQUATE
Panic:FailLoadKernel EQUATE
Panic:CyclicFreeList EQUATE
Panic:AddressUsed    EQUATE
Panic:AddressNotUsed EQUATE
Panic:FreeWhenFree   EQUATE
Panic:FreeListBroken EQUATE
!Put new fatal codes before this line

Panic:LastPanic      EQUATE
  END

!Panic messages - these must all be the same size and the active part must end
!in a <0> to make it look like a CSTRING when its address is passed to AppAbort.
!Also, keep them a multiple of 4 bytes in length to maintain LONG alignment.

!The order in this table must match the panic codes above.
PanicMsgs:String  STRING( |
  |         1         2         3         4         5         6         7         8
  |12345678901234567890123456789012345678901234567890123456789012345678901234567890
  'Run out of memory - Malloc<0>                                     ' & |Panic:Malloc
  'Run out of memory - Realloc<0>                                    ' & |Panic:Realloc
  'Run out of memory - Calloc<0>                                     ' & |Panic:Calloc
  'Run out of memory - Aligned<0>                                    ' & |Panic:Aligned
  '<0>                                                               ' & |Panic:FirstFatal (dummy - not used)
  'Given a corrupted memory block - Free<0>                          ' & |Panic:Free
  'Given a bad alignment - Alignment<0>                              ' & |Panic:Alignment
  'Given a foreign memory block - Realloc<0>                         ' & |Panic:Foreign
  'Given a bin to unuse that was not that used - UnusedAddress<0>    ' & |Panic:BadBin
  'Given a bad address - AddressValidate<0>                          ' & |Panic:BadAddress
  'Given a bad chunk - LogChunk<0>                                   ' & |Panic:BadChunk
  'Too many bin edges (tune the parameters in zdMemBinSizes)<0>      ' & |Panic:BadBinSizes
  'zdMemBinSizes table not properly terminated<0>                    ' & |Panic:BadBinSizesEnd
  'Detected a buffer overrun - AddressValidate<0>                    ' & |Panic:Overflow
  'Cannot load kernel32.dll - ProcessStart<0>                        ' & |Panic:FailLoadKernel
  'Detected a cyclic free list - TrimSlabs<0>                        ' & |Panic:CyclicFreeList
  'Given a used address - UsedAddress<0>                             ' & |Panic:AddressUsed
  'Given an un-used address - UnusedAddress<0>                       ' & |Panic:AddressNotUsed
  'Given an already free address - Free<0>                           ' & |Panic:FreeWhenFree
  'Broken free list - TrimSlabs<0>                                   ' & |Panic:FreeListBroken
  '<0>                                                               ' & |Panic:LastPanic  (dummy - not used)
  |12345678901234567890123456789012345678901234567890123456789012345678901234567890
  |         1         2         3         4         5         6         7         8
  '')
PanicMsgs STRING(64),DIM(Panic:LastPanic),OVER(PanicMsgs:String)                  !string size must match above (NB: <0>=1)

!}}}
!{{{  data
      
!{{{  LockedCompareExchange64

!This is a manual link to get round the fact that this function does not
!exist in Windoze XP.

LockedCompareExchange64Ptr   LONG,NAME('InterlockedCompareExchange64')
LockedCompareExchange64Name  CSTRING('InterlockedCompareExchange64')
kernel32DllName              CSTRING('kernel32.dll')

!This is the ticket-lock used when running under Win XP when the above
!function is not available.

zdMemCompareExchange64Lock   GROUP
NextTicket                     LONG
NowServing                     LONG
                             END

zdMemXPmode                  LONG        !TRUE iff we're running in XP mode

!}}}
!{{{  zdMemTrimLock

!This is a locking mechanism to serialise 'trim' requests and to
!lock out alloc/free requests while a trim is in progress for a bin.

zdMemTrimLock:Readers  LONG,DIM(zdMemMaxBin)   !incremented/decremented around alloc/free requests
                                               !thus its zero when no alloc/free requests are active

zdMemTrimLock:Writers  LONG,DIM(zdMemMaxBin)   !incremented/decremented around trim requests
                                               !thus its zero when no trim requests are active

!These two counters are used to implement a single-writer/multi-reader lock like this:

!Reader:                                    Writer:
!  LOOP                                       LOOP
!    readers += 1                               writers += 1
!    IF writers                                 IF writers > 1
!      readers -= 1  !a writer is active          writers -= 1
!      sleep(0)                                   sleep(0)     !another write active
!    ELSE                                       ELSE
!      break         !we're in                    break        !we're in
!    END                                        END
!  END                                        END
!  !readers > 0, writers=0                    LOOP WHILE readers
!  ...                                          sleep(0)       !a reader is still active
!  readers -= 1                               END
!                                             !writers=1, readers=0...
!                                             ...
!                                             writers -= 1     !unlock

!The increment/decrement of readers/writers must be atomic.

!}}}

!{{{  zdMemBinSizes - tune these to your APP

!This table specifies where the bin range boundaries are.
!This table is used at run-time to construct length-->bin and bin-->length look-up tables.
!These edges are tuned to minimise waste on sizes typically requested by applications.
!So bin ranges are small where there is a lot of variation and big elsewhere.
!The numbers here require less than 255 bins. The size distribution is approx exponential.
!They have been chosen such that the worst case wastage is 1/16th of a chunk and 1/8th of
!a slab. The slab size is the *maximum* number of zdMemQuanta to allocate for new slabs.
!A zero slab size means use the minimum that encompasses the length.

zdMemBinSizeTable  GROUP
!                    Min Chunk size | Chunk increment | Slab size | Size number
                     LONG(0)        ; LONG(4)         ; LONG(0)   ! 1
                     LONG(64)       ; LONG(8)         ; LONG(0)   ! 2
                     LONG(256)      ; LONG(16)        ; LONG(0)   ! 3
                     LONG(1024)     ; LONG(64)        ; LONG(0)   ! 4
                     LONG(4*1024)   ; LONG(256)       ; LONG(0)   ! 5
                     LONG(8*1024)   ; LONG(512)       ; LONG(3)   ! 6
                     LONG(16*1024)  ; LONG(1024)      ; LONG(4)   ! 7
                     LONG(32*1024)  ; LONG(2*1024)    ; LONG(4)   ! 8
                     LONG(64*1024)  ; LONG(4*1024)    ; LONG(4)   ! 9
                     LONG(128*1024) ; LONG(8*1024)    ; LONG(0)   ! 10
                     LONG(256*1024) ; LONG(32*1024)   ; LONG(0)   ! 11
                     LONG(512*1024) ; LONG(64*1024)   ; LONG(0)   ! 12
                     LONG(1024*1024); LONG(0)         ; LONG(0)   ! 13 - terminator
                   END
zdMemBinSizes      GROUP,DIM(13),OVER(zdMemBinSizeTable) !compiler too stupid to do DIM(SIZE(zdMemBinSizeTable)/12)
ChunkSize            LONG
ChunkIncrement       LONG
SlabSize             LONG
                   END

!}}}
!{{{  zdMemBinFreeList

!Each bin has a free list anchored here.
!The free list header consists of a pointer to the first free chunk
!and an 'aba' counter (see http://en.wikipedia.org/wiki/ABA_problem).
!These are packaged into an 8-byte (64 bits) unit that is 8-byte
!aligned so that both parts can be changed atomically using the
!InterlockedCompareExchange64 function provided by Windoze.

zdMemLong            LONG      !here purely so we can say SIZE(zdMemLong)

zdMemListHeadType    GROUP,TYPE
aba                    LONG    !the 'aba' part - must be first element
head                   LONG    !the list head part
                     END
zdMemListHeadSize    EQUATE(SIZE(zdMemListHeadType))
zdMemListHeadAlign   EQUATE(zdMemListHeadSize)         !alignment required for the free list headers
zdMemListHeadOffset  EQUATE(SIZE(zdMemLong))           !offset within zdMemListHeadType of the .head member

!We can't gaurantee 8-byte alignment here and we don't want to
!dynamically allocate the structure either (catch-22), so we
!just allocate a static array that we know is too big and then
!adjust our access to it at run-time with a bit of address arithmetic.

zdMemFreeListSpace   LONG,DIM((zdMemMaxBin+1)*zdMemListHeadSize) !1 entry too big
zdMemBinFreeList     LONG                              !address of the zdMemMaxBin array of zdMemListHeadType
                                                       !within the space above (at most we'll waste 1 LONG)

!The 'aba' part is incremented for each change to the free list.
!Its used to guard againts this happenning:

!  1. Thread 1 and 2 both access the free list top, so both now have the same 'head'
!  2. Thread 1 completes so now the original 'head' link is invalid
!  3. Thread 2 reads the next link from its copy of 'head', so now its 'next' address is invalid
!  4. Thread 1 frees 'head', so now its the free list top again, making 'head' valid to Thread 2
!  5. Thread 2 sets the new free list to the invalid 'next' - bang!

!By incremenenting the 'aba' count and treating the count and the head as a
!single atomic unit, this scenario can be prevented because the atomic compare-and-swap
!action will fail.

!}}}
!{{{  zdMemBinEdges

!This table is constructed at start-up.
!An 'edge' is the size at which the next bin range must be used.
!It provides for looking up the chunk size from a bin number.

zdMemBinEdges      LONG,DIM(zdMemMaxBin)
zdMemBinEdgeFinal  LONG        !last bin edge actually used (must be < zdMemMaxBin)

!This bin number is used to indicate the bin is allocated and released
!on demand. Its used for very big allocations and is rare, so we don't
!care about performance for these.

zdMemVarBinNum     EQUATE(zdMemBinEdgeFinal)

!}}}
!{{{  zdMemBinNumChunks

!This table is constructed at start-up.
!It provides for looking up the number of chunks a bin slab can hold.

zdMemBinNumChunks  LONG,DIM(zdMemMaxBin)

!}}}
!{{{  zdMemSmallSizeBins

!This table is constructed at start-up.
!It allows a very fast bin look-up for the vast majority of allocation lengths.
!The length is just used as an index into this table.

zdMemSmallSizeBins BYTE,DIM(64*1024)     !arbitrary size, make as big as you/debugger can tolerate

!}}}
!{{{  zdMemBinSlabs

!This table is constructed at start-up.
!It provides for looking up the allocation slab size from a bin number.

zdMemBinSlabs      LONG,DIM(zdMemMaxBin)

!}}}
!{{{  (M) state

!These cannot be in M 'cos we need to guarantee they are 0 at start-up
M_Patched    LONG(0)                     !TRUE if we've hooked our malloc into the RTL
M_Stopped    LONG(0)                     !TRUE if we're stopping (outer destruct called)

!This *MUST* be AUTO else __attach_process procedure does too much
M            GROUP,AUTO
Malloc         LONG                      !the address of the orig
Realloc        LONG                      !..
Calloc         LONG                      !....
Free           LONG                      !......
Aligned        LONG                      !........(__aligned_malloc)
MallocWas      STRING(ASM_JMPLEN)        !the instructions we clobbered in the orig
ReallocWas     STRING(ASM_JMPLEN)        !..
CallocWas      STRING(ASM_JMPLEN)        !....
FreeWas        STRING(ASM_JMPLEN)        !......
AlignedWas     STRING(ASM_JMPLEN)        !........
Pad1           STRING(ASM_JMPLEN),DIM(3) !ensure multiple of 4 to maintain LONG alignment

!Panic status
PanicSpace     LONG                      !Panic function emergency space (address of a buffer we keep aside)
Panicing       LONG                      !set to the thread that first panic'd - recursion interlock
PanicUsed      LONG                      !bytes allocated since panic started
PanicFree      LONG                      !bytes released since panic started

!Statistics
S              LIKE(zdMemStatsType)      !see constants above
LeakedEarly    LONG                      !count of 'foreign' free's we've seen while starting
LeakedLate     LONG                      !count of 'foreign' free's we've seen while stopping
LeakEarlyTop   LONG                      !head of linked list of early 'foriegn' frees
LeakLateTop    LONG                      !head of linked list of late 'foriegn' frees
ListsLockClash LONG                      !thread collisions accessing the preferred lists (should be rare)
PagesHoldLimit LONG                      !max 64K pages we should hold, 0=no limit
PagesHeldNow   LONG                      !total 64K pages we are currently holding
PageReducing   LONG                      !lock to prevent more than one thread reducing pages over limit
             END

!}}}
!{{{  zdMemAddressMap

!This allocator is relying on being able to map an address to the associated
!bin that it was allocated from. All bins have a number in the range 1..255.
!Each bin consists of a linked list of chunks that are all the same size. Once
!we know the bin, allocating is just a matter of taking the front of the free
!list and free'ing just puts it back. We allocate virtual address space in a
!minimum size of 64K, so there are only 64K such spaces in the entire 4G address
!space the application can use. This table has an entry for each one - 64K bytes
!is considered small-beer in the context of the memory a big application uses.

zdMemAddressMap  BYTE,DIM(zdMemMaxQuanta)

!}}}
!{{{  zdMemSlabMap

!This table is used to map an address to the start of its slab. This is the
!biggy 'cos we need a pointer (4 bytes) for every entry. See discussion in
!zdMemAddressMap about our position on using large lookup tables. The size
!of this table could be halved if we commit to 64K slabs, 'cos then the LS
!16 bits are always zero. With 64K slabs and a LONG we're using 256K here.

zdMemSlabMap   LONG,DIM(zdMemMaxQuanta)

!}}}
!{{{  zdMemSlabFreeList

!This is another biggy. Its the root of a free list of chunks within the slab.
!Its used by zdMemTrimSlabs to break-up the free list into a separate list for
!each slab.

zdMemSlabFreeList  LONG,DIM(zdMemMaxQuanta)

!}}}
!{{{  zdMemSlabSize

!Slabs that are allocated in the zdMemVarBinNum bin are of varying sizes.
!This table maps all possible slab addresses to their allocation chunk
!size. When a slab is allocated, its size is lodged in here. When it is
!released the entry in here is cleared. The size is only required by realloc
!(so data can be copied) and for diagnostics. The size of this table could
!be halved if we commit to 64K slabs, 'cos then the LS 16 bits are always
!zero. With 64K slabs and a LONG we're using 256K here.

zdMemSlabSize  LONG,DIM(zdMemMaxQuanta)

!}}}
!{{{  zdMemBinBreak

!This is a debug helper to allow a DebugBreak when a specific bin is accessed.
!Set a code in the bin to indicate which condition should DebugBreak.
!The conditions are bit masks as follows:

zdMemBreak:Malloc      EQUATE(01h)
zdMemBreak:Free        EQUATE(02h)
zdMemBreak:Realloc     EQUATE(04h)
zdMemBreak:Aligned     EQUATE(08h)
zdMemBreak:Calloc      EQUATE(10h)
zdMemBreak:NewPage     EQUATE(20h)
zdMemBreak:FreePage    EQUATE(40h)
zdMemBreak:spare80     EQUATE(80h)

zdMemBinBreak          BYTE,DIM(zdMemMaxBin)
zdMemBreakLimit        LONG                    !set this to the number of times to see the condition
                                               !before the limit counter is incremented
zdMemBreakConditionMet LONG                    !count of number break conditions seen
zdMemBreakLimitMet     LONG                    !count of number break conditions seen over the limit

!}}}
!{{{  zdMemSlabChunks

!This is a diagnostic aid used to check for cyclic free lists when
!trimming slabs. Its incremented for each free chunk discovered in
!a slab. If that exceeds the max chunks that can fit in the slab
!then we've got a cycle someplace.

COMPILE('ENDCOMPILE',CHECKING=1)

zdMemSlabChunks LONG,DIM(zdMemMaxQuanta)

ENDCOMPILE

!}}}

!}}}

!{{{  the 'slab' allocator

!{{{  zdMemMakeBinEdges

!{{{  history

!24/04/15 DCN Created

!}}}
!{{{  description

!This function is run once at start-up to construct the zdMemBinEdges, zdMemBinSlabs
!and zdMemBinNumChunks tables. Its constructed at run-time rather than being statically
!defined 'cos its regular and thus easier to contruct algorithmically.

!zdMemMakeBinEdges()

!}}}

zdMemMakeBinEdges PROCEDURE
edge              LONG,AUTO
ptr               LONG,AUTO
bestSize          LONG,AUTO
bestFit           LONG,AUTO
  CODE
  edge = 0
  ptr  = 1
  LOOP zdMemBinEdgeFinal = 1 TO MAXIMUM(zdMemBinEdges,1)
    IF zdMemBinEdgeFinal >= zdMemMaxBin
      zdMemAbort(Panic:BadBinSizes)
      LOOP.
    END
    edge += zdMemBinSizes[ptr].ChunkIncrement
    zdMemBinEdges[zdMemBinEdgeFinal] = edge
    zdMemBinSlabs[zdMemBinEdgeFinal] = zdMemBinSizes[ptr].SlabSize * zdMemQuanta
    IF zdMemBinSlabs[zdMemBinEdgeFinal] < zdMemSize2SlabSize(edge)
      zdMemBinSlabs[zdMemBinEdgeFinal] = zdMemSize2SlabSize(edge)
    END
    !{{{  reduce slab size to the best fit
    !What we want to do here is reduce the slab size looking for the best fit.
    !The best fit is the smallest residue/slab size ratio.
    !Note best so far
    bestSize = zdMemBinSlabs[zdMemBinEdgeFinal]
    bestFit  = zdMemDivide(zdMemBinSlabs[zdMemBinEdgeFinal]*16,zdMemBin2Residue(zdMemBinEdgeFinal))
    !Look for better
    LOOP WHILE zdMemBinSlabs[zdMemBinEdgeFinal] > zdMemQuanta
      !Tentatively reduce slab size
      zdMemBinSlabs[zdMemBinEdgeFinal] -= zdMemQuanta
      !Test if this is better - better == bigger fit
      IF zdMemDivide(zdMemBinSlabs[zdMemBinEdgeFinal]*16,zdMemBin2Residue(zdMemBinEdgeFinal)) < bestFit
        !Its gone worse, keep looking
      ELSE
        !This is as good or better, so keep it
        bestSize = zdMemBinSlabs[zdMemBinEdgeFinal]
        bestFit  = zdMemDivide(zdMemBinSlabs[zdMemBinEdgeFinal]*16,zdMemBin2Residue(zdMemBinEdgeFinal))
      END
    END
    !Set the best slab size we found
    zdMemBinSlabs[zdMemBinEdgeFinal] = bestSize  
    !}}}
    zdMemBinNumChunks[zdMemBinEdgeFinal] = zdMemBin2Chunks(zdMemBinEdgeFinal)
    IF edge >= zdMemBinSizes[ptr+1].ChunkSize
      IF ~zdMemBinSizes[ptr+1].ChunkIncrement
        !That's it
        RETURN
      END
      ptr += 1
    END
  END
  !We shouldn't get here
  zdMemAbort(Panic:BadBinSizesEnd)
  LOOP.

!}}}
!{{{  zdMemMakeSmallSizeBins

!{{{  history

!24/04/15 DCN Created

!}}}
!{{{  description

!This function is run once at start-up to construct the zdMemSmallSizeBins
!table. Its constructed at run-time rather than statically 'cos its big
!and would be very tedious to define statically (unless Clarion has macros
!but it doesn't).

!zdMemMakeSmallSizeBins()

!}}}

zdMemMakeSmallSizeBins PROCEDURE
len                    LONG,AUTO
bin                    LONG,AUTO
  CODE
  !This is an O[n2] algorithm but is simple and only run once, so OK.
  LOOP len = 1 TO MAXIMUM(zdMemSmallSizeBins,1)
    LOOP bin = 1 TO zdMemBinEdgeFinal
      IF zdMemBinEdges[bin] >= len
        zdMemSmallSizeBins[len] = bin
        BREAK
      END
    END
  END
  
!}}}
!{{{  zdMemDivide

!{{{  history

!02/06/15 DCN Created

!}}}
!{{{  description

!This is an integer divide that does not use the Clarion primitives.
!It returns the floor of A/B. Its only intended for divisions that
!yield a small result.

!zdMemDivide(LONG A,LONG B),LONG
!            !      !       !floor of A/B
!            !      !divisor, must be +ve
!            !dividend, must be +ve

!}}}

zdMemDivide FUNCTION(A,B)
Result      LONG,AUTO
  CODE
  !NB: We must achieve integer division here and truncate, but we want to
  !    do it without invoking any Clarion RTL functions, hence this odd
  !    looking code using a loop instead of a divide.
  IF A < B THEN RETURN 0.
  Result = 1
  IF B > 0
    LOOP WHILE (Result * B) <= A
      Result += 1
    END
    Result -= 1
  ELSE
    !Consider divide by zero as infinite - max +ve is as close as we can get
    Result = 07FFFFFFFh
  END
  RETURN Result

!}}}
!{{{  zdMemAbove

!{{{  history

!02/06/15 DCN Created

!}}}
!{{{  description 

!This function does a ULONG > ULONG comparision, but does it without
!using the Clarion ULONG data type, 'cos to do so invokes a wodge of
!Clarion RTL functions.

!zdMemAbove(LONG,LONG),LONG
!           !    !     !TRUE iff this > that when treated as unsigned
!           !    !that (may be 0)
!           !this (cannot be 0)

!NB: this and that are interpreted as memory addresses.

!}}}

zdMemAbove FUNCTION(this,that)
  CODE
  IF ~that
    !Anything is bigger than 0
    RETURN TRUE
  ELSIF this < 0
    !Got an address in upper 2G address space
    IF that < 0
      !both are, the less -ve is the bigger  (-2=FE, -1=FF, FF>FE)
      IF this > that THEN RETURN TRUE.
      RETURN FALSE
    ELSE
      !anything +ve is smaller than anything -ve
      RETURN TRUE
    END
  ELSE
    !Got an address in the lower 2G address space
    IF that > 0
      !both are, the most +ve is the bigger
      IF this > that THEN RETURN TRUE.
      RETURN FALSE
    ELSE
      !anything +ve is smaller than anything -ve
      RETURN FALSE
    END
  END

!}}}

!{{{  zdMemBin2Chunks

!{{{  history

!28/04/15 DCN Created

!}}}
!{{{  description

!Translate the given bin number into the number of chunks that its slab
!can accomodate.

!zdMemBin2Chunks(LONG),LONG
!                !     !number of chunks its slab can hold
!                !the bin number of interest, it *MUST* be valid

!NB: This function must *NOT* use the zdMemBinNumChunks array, 'cos
!    we're called before that array is valid.

!}}}

zdMemBin2Chunks FUNCTION(bin)
  CODE
  RETURN zdMemDivide(zdMemBinSlabs[bin],zdMemBinEdges[bin])

!}}}
!{{{  zdMemSize2SlabSize

!{{{  history

!29/04/15 DCN Created

!}}}
!{{{  description

!Given a size, determine the minimum slab size required to accomodate it.
!Slabs are a multiple of the page allocation granularity (zdMemQuanta).
!This is the base function for making this judgement, everything else is
!dependant on this. It must be self standing.

!zdMemSize2SlabSize(LONG),LONG
!                   !     !corresponding slab size
!                   !length to consider

!Note: This must round UP in determining quanta numbers.

!}}}

zdMemSize2SlabSize FUNCTION(len)
  CODE
  RETURN BSHIFT(BSHIFT(len+zdMemQuanta-1,-zdMemAddrShift),zdMemAddrShift)

!}}}
!{{{  zdMemSize2BinNum
!{{{  history

!24/04/15 DCN Created

!}}}
!{{{  description

!Given a required memory allocation length, return the bin number to use for it.
!There are 2 special bins: 0 and 255. 0==illegal length, 255==variable length.

!zdMemSize2BinNum(LONG),LONG
!                 !     !bin number to use or 0 or 255
!                 !memory allocation length (in bytes)

!}}}

zdMemSize2BinNum FUNCTION(len)
bin              LONG,AUTO
  CODE
  !NB: This logic is arranged such that it is equivalent to "bin = function(len)".
  !    This means this section of code can be INCLUDEd wherever we want to do that.
  !    This is a crude in-lining mechanism.
SECTION('__bin=function(len)')
  IF len < 1
    bin = 0
  ELSIF len <= MAXIMUM(zdMemSmallSizeBins,1)
    bin = zdMemSmallSizeBins[len]
  ELSIF len >= zdMemBinEdges[zdMemBinEdgeFinal]
    bin = zdMemVarBinNum
  ELSE
    !Sizes this big are rare, so we find the bin by searching edges from the end
    !NB: It is *guaranteed* that we will find one.
    LOOP bin = zdMemBinEdgeFinal-1 TO 1 BY -1
      IF len > zdMemBinEdges[bin]
        bin += 1
        BREAK
      END
    END
  END
SECTION('__bin=function(len):END')
  RETURN bin

!}}}
!{{{  zdMemAddress2SlabTop

!{{{  history

!29/04/15 DCN Created

!}}}
!{{{  description

!Given a bin number and an address, return the address of the
!top of the slab that address is within if possible, else
!return 0.

!zdMemAddress2SlabTop(LONG,LONG),LONG
!                     !    !     !slab address or 0 if not known
!                     !    !the address we want it for
!                     !the bin its in

!Note: This must round DOWN in determining quanta numbers.

!}}}

zdMemAddress2SlabTop FUNCTION(bin,addr)
  CODE
  RETURN zdMemSlabMap[BSHIFT(addr,-zdMemAddrShift)]

!}}}
!{{{  zdMemUsedAddressRange

!{{{  history

!27/04/15 DCN Created

!}}}
!{{{  description

!Given a bin, an address and a length, note that address range
!has been allocated to that bin in all the pages it spans. The
!slab base is also noted for each page. The given address must
!be a slab base.

!zdMemUsedAddressRange(LONG,LONG,LONG)
!                      !    !    !the length allocated
!                      !    !address allocated
!                      !bin number it was allocated in

!Note: This must round DOWN in determining quanta numbers.

!}}}

zdMemUsedAddressRange PROCEDURE(bin,addr,len)
slab                  LONG,AUTO
limit                 LONG,AUTO
  CODE
  len   = zdMemSize2SlabSize(len)
  slab  = addr
  limit = addr + len
  LOOP
    IF CHECKING
      IF zdMemAddressMap[BSHIFT(addr,-zdMemAddrShift)] |
      OR zdMemSlabMap   [BSHIFT(addr,-zdMemAddrShift)] |
      OR zdMemSlabSize  [BSHIFT(addr,-zdMemAddrShift)] THEN
        !Using a slab that is already in use!
        zdMemPanic(bin,Panic:AddressUsed)
      END
    END
    zdMemAddressMap[BSHIFT(addr,-zdMemAddrShift)] = bin
    zdMemSlabMap   [BSHIFT(addr,-zdMemAddrShift)] = slab
    zdMemSlabSize  [BSHIFT(addr,-zdMemAddrShift)] = len
    LockedIncrement(M.PagesHeldNow)
    addr += zdMemQuanta
    IF addr = limit THEN RETURN.         !NB: Do NOT be tempted to use >= here, we're dealing with addresses and they are not signed
  END

!}}}
!{{{  zdMemUnusedAddressRange

!{{{  history

!28/04/15 DCN Created

!}}}
!{{{  description

!Given a bin number, an address and a length, note that address range
!has NOT been allocated from that bin. The given address must be a
!slab base.

!zdMemUnusedAddressRange(LONG,LONG),LONG,PROC
!                        !    !     !bytes released
!                        !    !address allocated
!                        !bin number it was allocated in

!Note: This must round DOWN in determining quanta numbers.

!}}}

zdMemUnusedAddressRange FUNCTION(bin,addr)
slabSize                LONG,AUTO
limit                   LONG,AUTO
  CODE
  slabSize = zdMemSlab2Size(addr)
  limit    = addr + slabSize
  LOOP
    IF CHECKING
      zdMemValidateAddress(bin,addr,Validate:Bin)
      IF ~zdMemAddressMap[BSHIFT(addr,-zdMemAddrShift)] |
      OR ~zdMemSlabMap   [BSHIFT(addr,-zdMemAddrShift)] |
      OR ~zdMemSlabSize  [BSHIFT(addr,-zdMemAddrShift)] THEN
        !Unusing a slab that is not in use!
        zdMemPanic(bin,Panic:AddressNotUsed)
      END
    END
    zdMemAddressMap[BSHIFT(addr,-zdMemAddrShift)] = 0
    zdMemSlabMap   [BSHIFT(addr,-zdMemAddrShift)] = 0
    zdMemSlabSize  [BSHIFT(addr,-zdMemAddrShift)] = 0
    LockedDecrement(M.PagesHeldNow)
    addr += zdMemQuanta
    IF addr = limit THEN RETURN slabSize.      !NB: Do NOT be tempted to use >= here, we're dealing with addresses and they are not signed
  END

!}}}
!{{{  zdMemSlab2Size

!{{{  history

!08/05/15 DCN Created

!}}}
!{{{  description

!Given a slab address, return its allocation size.

!zdMemSlab2Size(LONG),LONG
!               !     !the length allocated or 0 if not allocated
!               !address allocated

!Note: This must round DOWN in determining quanta numbers.

!}}}

zdMemSlab2Size FUNCTION(addr)
  CODE
  RETURN zdMemSlabSize[BSHIFT(addr,-zdMemAddrShift)]

!}}}
!{{{  zdMemBin2Residue

!{{{  history

!01/06/15 DCN Created

!}}}
!{{{  description

!Given a bin number, return the residue in the slab size it uses.

!zdMemBin2Residue(LONG),LONG
!                 !     !residue in bytes
!                 !bin number we want it for, it *MUST* be valid

!NB: This function must *NOT* use the zdMemBinNumChunks array, 'cos
!    we're called before that array is valid.

!}}}

zdMemBin2Residue FUNCTION(bin)
  CODE
  RETURN zdMemBinSlabs[bin] - (zdMemBinEdges[bin] * zdMemBin2Chunks(bin))

!}}}

!{{{  zdMemLock64

!{{{  history

!26/05/15 DCN Created

!}}}
!{{{  description

!This is the lock function used when in XP mode that is required to
!emulate InterlockedCompareExchange64 on an XP machine. It completely
!nullifies the performance benefit of this allocator - but tough!
    
!zdMemLock64()

!}}}

zdMemLock64 PROCEDURE
MyTicket    LONG,AUTO
  CODE
  MyTicket = LockedIncrement(zdMemCompareExchange64Lock.NextTicket) - 1
  LOOP WHILE MyTicket <> zdMemCompareExchange64Lock.NowServing
    Snooze(0)
  END

!}}}
!{{{  zdMemUnLock64

!{{{  history

!26/05/15 DCN Created

!}}}
!{{{  description

!This is the un-lock function used when in XP mode that is required to
!emulate InterlockedCompareExchange64 on an XP machine. It completely
!nullifies the performance benefit of this allocator - but tough!
    
!zdMemUnLock64()

!}}}

zdMemUnLock64 PROCEDURE
  CODE
  zdMemCompareExchange64Lock.NowServing += 1

!}}}
!{{{  zdMemLockedCompareExchange64

!{{{  history

!26/05/15 DCN Created

!}}}
!{{{  description

!This is a DIY replacement for InterlockedCompareExchange64 when running
!under windoze XP. Its far slower as it uses a global ticket-lock for all
!bins. So under XP, this allocator is not lock-less. Tough.
    
!zdMemLockedCompareExchange64(LONG Destination,LONG ExchangeMS,LONG ExchangeLS,LONG ComparandLS,LONG ComparandMS),LONG,PASCAL

!}}}

zdMemLockedCompareExchange64 FUNCTION(LONG Destination,LONG ExchangeLS,LONG ExchangeMS,LONG ComparandLS,LONG ComparandMS)
OldLS                        LONG,AUTO
OldMS                        LONG,AUTO
  CODE
  zdMemLock64()
  PEEK(Destination,OldLS)
  PEEK(Destination+SIZE(OldLS),OldMS)
  IF OldLS = ComparandLS AND OldMS = ComparandMS
    POKE(Destination,ExchangeLS)
    POKE(Destination+SIZE(ExchangeLS),ExchangeMS)
  END
  zdMemUnLock64()
  RETURN OldLS

!}}}

!{{{  zdMemCommand

!{{{  history

!27/04/15 DCN Created
!12/12/15 DCN Add clear stats command

!}}}
!{{{  description

!Decode the given length as a command and action it.

!zdMemCommand(LONG),LONG
!             !     !command response
!             !command code

!}}}
    
zdMemCommand FUNCTION(len)
param        LONG,AUTO
bin          EQUATE(param)
pages        EQUATE(param)
result       EQUATE(param)
  CODE

  param = BAND(len,zdMem:ParamMask)

  CASE BAND(len,zdMem:OpCodeMask)
  OF zdMem:SetHoldLimit
    M.PagesHoldLimit = pages
  OF zdMem:GetHoldLimit
    RETURN M.PagesHoldLimit
  OF zdMem:GetHeldNow
    RETURN M.PagesHeldNow
  OF zdMem:GetStatsTable
    RETURN ADDRESS(M.S)
  OF zdMem:GetMaxBin
    RETURN zdMemBinEdgeFinal
  OF zdMem:GetForeign
    RETURN M.LeakedEarly + M.LeakedLate
  OF zdMem:TrimSlabs
    IF bin >= 0 AND bin <= zdMemBinEdgeFinal   !NB: bin=0 is valid
      RETURN zdMemTrimSlabs(bin)
    END
  OF zdMem:GetChunkSize
    IF bin > 0 AND bin <= zdMemBinEdgeFinal
      RETURN zdMemBinEdges[bin]
    END
  OF zdMem:GetSlabSize
    IF bin > 0 AND bin <= zdMemBinEdgeFinal
      RETURN zdMemBinSlabs[bin]
    END
  OF zdMem:GetSlabResidue
    IF bin > 0 AND bin <= zdMemBinEdgeFinal
      RETURN zdMemBin2Residue(bin)
    END
  OF zdMem:GetOptions
    result = 0
    IF LOGGING     THEN result += 1.
    IF CHECKING    THEN result += 2.
    IF HOOKING     THEN result += 4.
    IF zdMemXPmode THEN result += 8.
    RETURN result
  OF zdMem:ClearStats
    zdMemClearStats()
    RETURN ADDRESS(M.S)
  END

  RETURN 0

!}}}
!{{{  zdMemValidateAddress

!{{{  history

!29/04/15 DCN Created

!}}}
!{{{  description

!Given a bin number and an address, check the address is valid for that bin.
!To be valid it must be within a page allocated to that bin and it must be a
!multiple of the chunk size for that bin. If the address is not valid we call
!zdMemPanic and the application is shut-down. So if the function returns the
!address is valid.

!This is a debug/diagnostic aid.

!zdMemValidateAddress(LONG,LONG,LONG check=0,LONG checksize=0)
!                     !    !    !            !iff given the size to check when doing Validate:Size
!                     !    !    !iff given perform the requested check, see Validate:... constants
!                     !    !    !0=do all checks
!                     !    !address to check (must in the given bin)
!                     !the bin it should be associated with

!}}}

zdMemValidateAddress PROCEDURE(bin,addr,check,checksize)
offset               LONG,AUTO
chunkSize            LONG,AUTO
chunkResidue         LONG,AUTO
COMPILE('ENDCOMPILE',CHECKING=1)
trueSize             LONG,AUTO
binNum               BYTE,AUTO
ENDCOMPILE
  CODE
  IF ~check THEN check = -1.   !turn on all options
  IF BAND(check,Validate:Bin)
    IF zdMemAddressMap[BSHIFT(addr,-zdMemAddrShift)] <> bin
      zdMemPanic(bin,Panic:BadAddress)
      LOOP.
    END
  END
  COMPILE('ENDCOMPILE',CHECKING=1)
  IF BAND(check,Validate:Size) AND checksize
    !Check the tail of the allocation contains the bin number.
    !This is a buffer overflow detector. CheckSize is the actual allocation.
    !When CHECKING is asserted all buffers are allocated N bytes too big and
    !those N bytes are filled with the bin number. If they are not intact
    !when the buffer is free'd then someone has been a very naughty boy!
    PEEK(addr,trueSize)
    LOOP offset = addr + zdMemCheckPrefix + trueSize TO addr + checksize - 1
      PEEK(offset,binNum)
      IF binNum <> bin
        zdMemPanic(bin,Panic:Overflow)
        LOOP.
      END
    END
  END
  ENDCOMPILE
  IF BAND(check,Validate:Residue)
    IF bin = zdMemVarBinNum THEN RETURN.
    offset      = addr - zdMemAddress2SlabTop(bin,addr)
    chunkSize   = zdMemBinEdges[bin]
    !NB: Do not do this with a divide - the Clarion compiler calls the RTL to do that and that does mallocs!!
    chunkResidue = offset % chunkSize
    IF chunkResidue
      zdMemPanic(bin,Panic:BadAddress)
      LOOP.
    END
  END

!}}}

!{{{  zdMemAllocSlab

!{{{  history

!12/05/15 DCN Created

!}}}
!{{{  description

!Allocate a new slab to accomodate the given length on behalf of the given bin.

!zdMemAllocSlab(LONG,LONG),LONG
!               !    !     !address allocated
!               !    !length required
!               !the bin its being allocated against (it must *NOT* be locked)

!This function only returns if it succeeds.
!Failure will abort the application.
!However, it attempts to garbage collect before it gives up.

!}}}

zdMemAllocSlab FUNCTION(bin,len)
addr           LONG,AUTO
  CODE
  LOOP
    addr = VirtualAlloc(0,zdMemSize2SlabSize(len),MEM_RESERVE+MEM_COMMIT,PAGE_READWRITE)
    IF ~addr
      !Oops - we're outa space
      zdMemPanic(bin,Panic:Malloc)
      CYCLE
    END
    IF HOOKING THEN zdMemBreak(len,zdMemBreak:NewPage).
    BREAK
  END
  IF CHECKING
    memSet(addr,bin,zdMemSize2SlabSize(len))   !to check nothing relying on contents, fill with bin#
  END
  zdMemUsedAddressRange(bin,addr,len)          !note we allocated this
  IF LOGGING
    zdMemLogSlabAlloc(bin)
  END
  RETURN addr

!}}}
!{{{  zdMemFreeSlab

!{{{  history

!12/05/15 DCN Created

!}}}
!{{{  description

!Release the given slab from the given bin

!zdMemFreeSlab(LONG bin,LONG addr),LONG,PROC
!              !        !          !nunmber of bytes released
!              !        !slab address to release
!              !bin number its being released from

!}}}

zdMemFreeSlab FUNCTION(bin,addr)
slabSize      LONG,AUTO
  CODE
  IF CHECKING
    zdMemValidateAddress(bin,addr)
  END
  IF HOOKING
    zdMemBreak(0-bin,zdMemBreak:FreePage)
  END
  !NB: Must do this before do the VirtualFree 'cos once we've done that
  !    the free'd address space becomes available to other threads.
  slabSize = zdMemUnusedAddressRange(bin,addr)   !note we have no longer allocated this
  !Now give it back
  IF ~VirtualFree(addr,0,MEM_RELEASE)
    !failed, what does that mean?
    zdMemPanic(bin,Panic:Free)
    !we never get here
    LOOP.
  END
  IF LOGGING
    zdMemLogSlabFree(bin)
    zdMemLogChunkFree(addr)
  END
  RETURN slabSize

!}}}
!{{{  zdMemTrimSlabs

!{{{  history

!11/05/15 DCN Created

!}}}
!{{{  description

!Find any totally unused slabs and release them back to the OS.
!This is an extremely slow process and should only be done under duress!

!zdMemTrimSlabs(LONG bin=0),LONG,PROC
!               !           !how many bytes were released to the OS
!               !which bin to do it for, 0=all

!NB: No reader locks must be in force when this is called.

!The algorithm is to remove every chunk from the free list and add it to
!an internal list for the slab its within. This has the effect of localising
!the free list within its slab. For every slab that is completely free
!it is returned to the OS. The rest are just put back on the free list.
!Its easy to detect if a slab is completely free by counting the number
!of free chunks it contains. If its less than the max chunks then at least
!one is still in use.

!This function is not expected to be called very often, so we don't
!concern ourselves with performance, we just do what's simple. Also,
!we're inside a CS, so we don't need to worry about other threads
!messing with our lists.

!}}}

zdMemTrimSlabs FUNCTION(bin)
freed          LONG,AUTO
  CODE
  freed = 0
  IF bin > 0
    DO TrimBin
  ELSE
    LOOP bin = 1 TO zdMemBinEdgeFinal
      DO TrimBin
    END
  END
  RETURN freed

!{{{  TrimBin ROUTINE

TrimBin ROUTINE
  DATA
freeList_head &LONG,AUTO
slabList      &LONG,AUTO
slab           LONG,AUTO
chunk          LONG,AUTO
chunks         LONG,AUTO
freeChunks     LONG,AUTO
pred           LONG,AUTO
succ           LONG,AUTO
slabTop        LONG,AUTO
  CODE

  !{{{  obtain the writer lock
  LOOP WHILE LockedIncrement(zdMemTrimLock:Writers[bin]) > 1
    !Another thread is doing a trim
    LockedDecrement(zdMemTrimLock:Writers[bin])
    Snooze(0)
  END
  !Writers=1, readers=?
  LOOP WHILE zdMemTrimLock:Readers[bin]
    !Another thread is still doing an allocate/free
    Snooze(0)
  END
  !Writers=1, readers=0
  !}}}

  freeList_head &= (zdMemBinFreeList + ((bin-1)*zdMemListHeadSize) + zdMemListHeadOffset)

  !{{{  empty the slab free lists
  LOOP slab = 1 TO MAXIMUM(zdMemSlabFreeList,1)
    zdMemSlabFreeList[slab] = 0
  END
  !}}}
  !{{{  prepare the free list cycle detector
  COMPILE('ENDCOMPILE',CHECKING=1)
    LOOP slab = 1 TO MAXIMUM(zdMemSlabChunks,1)
      zdMemSlabChunks[slab] = 0
    END
  ENDCOMPILE
  !}}}
  !{{{  empty the free list into its separate slab lists
  freeChunks = 0                         !not seen any free chunks yet
  LOOP WHILE freeList_head
    !There is another entry on the free list, remove it
    freeChunks += 1                      !diagnostic aid
    COMPILE('ENDCOMPILE',CHECKING=1)
      !Attempt to detect a cyclic free list
      slabTop = zdMemAddress2SlabTop(bin,freeList_head)
      zdMemSlabChunks[BSHIFT(slabTop,-zdMemAddrShift)] += 1
      IF zdMemSlabChunks[BSHIFT(slabTop,-zdMemAddrShift)] > zdMemBinNumChunks[bin]
        !Too many chunks, there must be a cycle someplace
        zdMemPanic(bin,Panic:CyclicFreeList)
        LOOP.
      END
    ENDCOMPILE
    chunk = freeList_head                !grab the chunk addr
    PEEK(freeList_head,freeList_head)    !remove it from the free list
    !Put this chunk on its slab list
    slabList &= ADDRESS(zdMemSlabFreeList[BSHIFT(zdMemAddress2SlabTop(bin,chunk),-zdMemAddrShift)])
    POKE(chunk,slabList)                 !point us at the slab list top
    slabList = chunk                     !point slab list at us
  END
  !freeChunks is now the total free chunks
  !}}}
  !{{{  walk each slab list
  LOOP slab = 1 TO MAXIMUM(zdMemSlabFreeList,1)
    slabTop = zdMemSlabFreeList[slab]
    chunks  = 0
    !Count free chunks in this slab
    LOOP WHILE slabTop
      chunks += 1
      PEEK(slabTop,slabTop)
    END
    IF chunks AND chunks = zdMemBinNumChunks[bin]
      !the entire slab is free, give it back to the OS
      slabTop = zdMemAddress2SlabTop(bin,zdMemSlabFreeList[slab])
      freed  += zdMemSlab2Size(slabTop)  !do this before free it
      zdMemFreeSlab(bin,slabTop)
      freeChunks -= chunks               !reduce size of free list
      IF LOGGING
        zdMemLogTrim(bin)
      END
    ELSIF chunks
      !{{{  not all free, put chunks back onto free list
      !we just append the slab list to the free list, but first we have to find the end of the free list
      pred = ADDRESS(freeList_head)
      LOOP
        PEEK(pred,succ)
        IF succ
          !not the end yet
          pred = succ
          PEEK(pred,succ)
        ELSE
          !found the end, pred is now the last in the free list
          BREAK
        END
      END
      POKE(pred,zdMemSlabFreeList[slab]) !connect end of free list to our slab list
      !}}}
    END
    zdMemSlabFreeList[slab] = 0          !empty the slab list
  END
  !}}}
  !{{{  check free list is still the correct size
  !the free list should now have freeChunks on it
  COMPILE('ENDCOMPILE',CHECKING=1)
    !count chunks on the free list
    chunks = 0
    pred   = freeList_head
    LOOP WHILE pred
      chunks += 1
      IF chunks > freeChunks
        !this implies a loop in the free list
        zdMemPanic(bin,Panic:FreeListBroken)
        LOOP.
      END
      PEEK(pred,pred)
    END
    !are there what there should be?
    IF freeChunks <> chunks
      zdMemPanic(bin,Panic:FreeListBroken)
      LOOP.
    END
  ENDCOMPILE
  !}}}
  
  !{{{  release the lock
  LockedDecrement(zdMemTrimLock:Writers[bin])
  Snooze(0)                    !JIC we're holding up other threads
  !}}}

!}}}

!}}}
!{{{  zdMemClearStats

!{{{  history

! 12/12/15 DCN Created

!}}}
!{{{  description

!Clear the usage statistics.
!This is useful from a UI that is showing the stats after performing some
!action to get a 'feel' for the effect of that action on memory.

!zdMemClearStats(LONG=0)
!                !iff TRUE do a full initialisation,
!                !else just the count accumulators are cleared

!}}}

zdMemClearStats PROCEDURE(FullInit)
Bin             LONG,AUTO
  CODE

  !NB: DO NOT USE Clear() here - it uses malloc functions!!
  LOOP Bin = 1 TO zdMemMaxBin
    M.S.SlabCount   [Bin] = 0
    M.S.ChunkCount  [Bin] = 0
    M.S.Align2Count [Bin] = 0
    M.S.Align4Count [Bin] = 0
    M.S.ClashCount  [Bin] = 0
    M.S.BinUpCount  [Bin] = 0
    M.S.BinDownCount[Bin] = 0
    M.S.BinSameCount[Bin] = 0
    M.S.PanicCount  [Bin] = 0
    IF FullInit
      M.S.Slabs     [Bin] = 0
      M.S.Chunks    [Bin] = 0
      M.S.MaxSlabs  [Bin] = 0
      M.S.MaxChunks [Bin] = 0
    END
  END

  IF FullInit
    M.LeakedEarly    = 0
    M.LeakEarlyTop   = 0
    M.LeakedLate     = 0
    M.LeakLateTop    = 0
    M.ListsLockClash = 0
  END

!}}}

!{{{  zdMemAlloc

!{{{  history

!27/04/15 DCN Created
!06/05/15 DCN Renamed from zdMemMalloc to zdMemAlloc and added the clear parameter

!}}}
!{{{  description

!Allocate a memory block of at least the size requested and fill as directed.

!zdMemAlloc(LONG,LONG=256),LONG
!           !    !         !address of allocation
!           !    !iff <256 fill buffer with this
!           !size required, -ve sizes have a special meaning

!Size requests below zero are a command code, not a true allocation.

!}}}

zdMemAlloc FUNCTION(len,fill)
bin        LONG,AUTO
chunk      LONG,AUTO
chunks     LONG,AUTO
fillLen    LONG,AUTO
freeList   LONG,AUTO
oldList    LIKE(zdMemListHeadType),AUTO
newList    LIKE(zdMemListHeadType),AUTO
addr       EQUATE(oldList.head)
ptr        EQUATE(newList.head)
  CODE
  INCLUDE('zd_alloc.clw','__bin=function(len)') !bin = zdMemSize2BinNum(len)
  IF ~bin THEN RETURN zdMemCommand(len). !assume its a command, NB: this catches len=0 too
  fillLen = len
  IF CHECKING
    !Over-allocate so can check for buffer overruns
    len += zdMemCheckPrefix + zdMemCheckSuffix !add extras
    bin  = zdMemSize2BinNum(len)               !get the new bin
  END
  IF bin = zdMemVarBinNum
    !{{{  this is a big allocation - do that conventionally with VirtualAlloc
    addr  = zdMemAllocSlab(bin,len)
    chunk = zdMemSize2SlabSize(len)
    !}}}
  ELSE
    !{{{  try to allocate the request
    LOOP
      !{{{  acquire the reader lock
      LOOP
        LockedIncrement(zdMemTrimLock:Readers[bin])
        IF zdMemTrimLock:Writers[bin]
          !A write-lock is active, back out and try again
          LockedDecrement(zdMemTrimLock:Readers[bin])
          Snooze(0)
        ELSE
          BREAK
        END
      END
      !Readers>0, writers=0
      !}}}
      freeList = zdMemBinFreeList + ((bin-1)*zdMemListHeadSize)
      !When in XP mode the whole 64 bits must be atomically loaded
      !'cos under XP its possible for aba to be updated, then we get here
      !then head gets updated, then bang! So in XP mode there must be a
      !lock around this too.
      IF zdMemXPmode THEN zdMemLock64().
      !NB: Must access aba first
      PEEK(freeList,oldList.aba)                     !this is atomic
      PEEK(freeList+SIZE(oldList.aba),oldList.head)
      IF zdMemXPmode THEN zdMemUnLock64().
      IF ~addr
        !Free list is empty, get another slab
        LockedDecrement(zdMemTrimLock:Readers[bin])  !release the reader lock
        addr = zdMemAllocSlab(bin,zdMemBinSlabs[bin])
        !{{{  add each chunk of this slab to the free list
        ptr   = addr
        chunk = zdMemBinEdges[bin]
        IF CHECKING
          ptr += zdMemCheckPrefix              !offset all our chunks by the prefix
        END
        LOOP chunks = 1 TO zdMemBinNumChunks[bin]
          !{{{  make it look as if we allocated this
          IF CHECKING
            POKE(ptr-zdMemCheckPrefix,fillLen)
            IF LOGGING
              zdMemLogChunkAlloc(ptr-zdMemCheckPrefix,len)
            END
          ELSIF LOGGING
            zdMemLogChunkAlloc(ptr,len)
          END
          !}}}
          !We've just played God and invented a new chunk,
          !so just free it to get it on the free list
          zdMemFree(ptr)
          ptr += chunk                         !move to next (contiguous) chunk
        END
        !}}}
        CYCLE                                        !now go get top of new free list
      END
      IF CHECKING
        zdMemValidateAddress(bin,addr)
      END
      !Unlink first chunk from free list
      newList.aba = oldList.aba + 1                  !set next counter
      PEEK(oldList.head,newList.head)                !NB: What we're accessing *MUST* be in our address space
      IF LockedCompareExchange64(freeList,newList.aba,newList.head,oldList.aba,oldList.head) <> oldList.aba
        !Another thread has snuck in - start again
        IF LOGGING
          zdMemLogClash(bin)
        END
        LockedDecrement(zdMemTrimLock:Readers[bin])  !release the reader lock
        CYCLE
      END
      !We did it
      LockedDecrement(zdMemTrimLock:Readers[bin])    !release the reader lock
      BREAK
    END
    chunk = zdMemBinEdges[bin]
    !}}}
    IF M.Panicing THEN M.PanicUsed += zdMemBinEdges[bin].
  END

  IF LOGGING
    zdMemLogChunkAlloc(addr,len)
  END
  IF CHECKING
    POKE(addr,fillLen)
    addr += zdMemCheckPrefix
  END
  
  IF fill < 256
    memSet(addr,fill,fillLen)
  END

  RETURN addr

!}}}
!{{{  zdMemFree

!{{{  history

!28/04/15 DCN Created

!}}}
!{{{  description

!Free a chunk of memory previously allocated by zdMemAlloc.
!If the given address was not allocated by us its a 'foreign'
!allocation, we just log and ignore those. They are typically
!things the Clarion RTL allocated during start-up and is now
!ditching. There won't be much of that and we just waste it.

!zdMemFree(LONG)
!          !address to free

!}}}

zdMemFree PROCEDURE(addr)
bin       LONG,AUTO
freeList  LONG,AUTO
oldList   LIKE(zdMemListHeadType),AUTO
newList   LIKE(zdMemListHeadType),AUTO
  CODE
  IF ~addr THEN RETURN.
  bin = zdMemAddressMap[BSHIFT(addr,-zdMemAddrShift)]        !NB: Still valid even with check prefix
  IF HOOKING AND bin THEN zdMemBreak(0-bin,zdMemBreak:Free).
  IF ~bin
    !{{{  foreign free - ignore it (assume memory leak is not significant)
    !Add them to a list JIC (NB: Assuming at least 4 bytes available)
    IF M_Stopped
      M.LeakedLate += 1
      POKE(addr,M.LeakLateTop)
      M.LeakLateTop = addr
    ELSE
      M.LeakedEarly += 1
      POKE(addr,M.LeakEarlyTop)
      M.LeakEarlyTop = addr
    END
    !}}}
  ELSIF bin = zdMemVarBinNum
    !{{{  its a big allocation - give these back each time
    IF CHECKING
      addr -= zdMemCheckPrefix
      zdMemValidateAddress(bin,addr,,zdMemSlab2Size(addr))
    END
    zdMemFreeSlab(bin,addr)
    !}}}
  ELSE
    !{{{  add this chunk to the front of the free list for this bin
    IF M.Panicing THEN M.PanicFree += zdMemBinEdges[bin].
    IF CHECKING
      addr -= zdMemCheckPrefix
      zdMemValidateAddress(bin,addr,,zdMemBinEdges[bin])
      !{{{  check not already on the free list
      !Freeing something that is already free will create loops in the free list.
      !Must do this before do the memSet
      !03/06/15 DCN Can't do this 'cos the list is changing under us by
      !             other threads and to cater for that is too expensive.
      !             It also has a dramatic (bad) effect on performance.
      !freeList = zdMemBinFreeList + ((bin-1)*zdMemListHeadSize)
      !PEEK(freeList+SIZE(oldList.aba),oldList.head)
      !LOOP WHILE oldList.head
      !  IF addr = oldList.head
      !    !Eek! Already free
      !    zdMemPanic(bin,Panic:FreeWhenFree)
      !    LOOP.
      !  END
      !  PEEK(oldList.head,oldList.head)  !03/06/15 DCN causes access violation if list changes
      !END
      !}}}
      memSet(addr,bin,zdMemBinEdges[bin])  !to check nothing relying on contents, fill with bin#
    END
    IF LOGGING
      zdMemLogChunkFree(addr)
    END
    LOOP
      !{{{  acquire the reader lock
      LOOP
        LockedIncrement(zdMemTrimLock:Readers[bin])
        IF zdMemTrimLock:Writers[bin]
          !A write-lock is active, back out and try again
          LockedDecrement(zdMemTrimLock:Readers[bin])
          Snooze(0)
        ELSE
          BREAK
        END
      END
      !Readers>0, writers=0
      !}}}
      !{{{  link this chunk to current free list
      freeList = zdMemBinFreeList + ((bin-1)*zdMemListHeadSize)
      !When in XP mode the whole 64 bits must be atomically loaded
      !'cos under XP its possible for aba to be updated, then we get here
      !then head gets updated, then bang! So in XP mode there must be a
      !lock around this too.
      IF zdMemXPmode THEN zdMemLock64().
      !NB: Must access aba first
      PEEK(freeList,oldList.aba)                     !this is atomic
      PEEK(freeList+SIZE(oldList.aba),oldList.head)
      IF zdMemXPmode THEN zdMemUnLock64().
      POKE(addr,oldList.head)                        !link self to front of existing free list
      newList.aba  = oldList.aba + 1                 !set next counter
      newList.head = addr                            !set new front
      IF LockedCompareExchange64(freeList,newList.aba,newList.head,oldList.aba,oldList.head) <> oldList.aba
        !Some other thread snuck in and changed the free list, start again
        IF LOGGING
          zdMemLogClash(bin)
        END
        LockedDecrement(zdMemTrimLock:Readers[bin])  !release the reader lock
        CYCLE
      END
      !}}}
      !We did it
      LockedDecrement(zdMemTrimLock:Readers[bin])    !release the reader lock
      BREAK
    END
    !}}}
  END
  IF M.PagesHoldLimit AND M.PagesHeldNow > M.PagesHoldLimit
    !Holding too much, ditch something
    IF LockedIncrement(M.PageReducing) > 1
      !Another thread is already doing it
      LockedDecrement(M.PageReducing)
    ELSE
      !Start from biggest bin and work down until get under the limit
      LOOP bin = zdMemBinEdgeFinal TO 1 BY -1
        zdMemTrimSlabs(bin)
        IF M.PagesHeldNow <= M.PagesHoldLimit THEN BREAK.  !made it
      END
      LockedDecrement(M.PageReducing)
    END
  END

!}}}

!{{{  zdMemMalloc

!{{{  history

!27/04/15 DCN Created
!06/05/15 DCN Reduced to a shell to call zdMemAlloc

!}}}
!{{{  description

!Allocate a memory block of at least the size requested.

!zdMemMalloc(LONG),LONG
!            !     !address of allocation
!            !size required, -ve sizes have a special meaning

!Size requests below zero are a command code, not a true allocation.

!}}}

zdMemMalloc FUNCTION(len)
  CODE
  IF HOOKING AND len > 0 THEN zdMemBreak(len,zdMemBreak:Malloc).
  RETURN zdMemAlloc(len)

!}}}
!{{{  zdMemAligned

!{{{  history

!28/04/15 DCN Created
!16/12/15 DCN Allow for a length that is not a multiple of the alignment size.

!}}}
!{{{  description

!Allocate an aligned memory block.
!In this allocator all memory allocations are aligned on their bin chunk
!size up to the allocation granularity (64K), so as long as the chunk size
!is a multiple of the alignment this is a no-op except for very large
!allocations. Those are merely dis-allowed (by aborting the app).
!We also chuck out any alignment that is not a multiple of 4 as our min chunk
!size is 4 and our chunk size increments are all multiples of 4.

!zdMemAligned(LONG,LONG),LONG
!             !    !     !address allocated
!             !    !alignment required
!             !size wanted

!}}}

zdMemAligned FUNCTION(len,alignment)
rem          LONG,AUTO
  CODE

  IF len <= 0 THEN RETURN 0.
  IF HOOKING THEN zdMemBreak(len,zdMemBreak:Aligned).
 
  IF BAND(alignment,3) OR alignment > zdMemMaxAlignment
    zdMemPanic(zdMemSize2BinNum(alignment),Panic:Alignment)
    LOOP.
  END

  IF LOGGING
    zdMemLogAligned(alignment,len)
  END

  rem = (len % alignment)
  IF rem
    !Jack the length up to be a multiple of the alignment
    len = (len - rem) + alignment
  END

  RETURN zdMemAlloc(len)
  
!}}}
!{{{  zdMemCalloc

!{{{  history

!28/04/15 DCN Created

!}}}
!{{{  description

!Allocate a number of objects of the given size and return their address.
!This differs from malloc in that the space allocated is zero filled.

!zdMemCalloc(LONG,LONG),LONG
!            !    !     !address allocated
!            !    !length of each object
!            !number of objects required

!}}}

zdMemCalloc FUNCTION(num,len)
  CODE
  IF num <= 0 OR len <= 0 THEN RETURN 0.
  IF HOOKING THEN zdMemBreak(num*len,zdMemBreak:Calloc).
  RETURN zdMemAlloc(num*len,0)           !calloc must clear to 0

!}}}
!{{{  zdMemRealloc

!{{{  history

!28/04/15 DCN Created

!}}}
!{{{  description

!Change an allocation from its current length to the given new length.
!If its staying in the same bin, its a no-op, otherwise we have to
!allocate from another bin and copy the data over.

!zdMemRealloc(LONG,LONG),LONG
!             !    !     !address allocated
!             !    !new length required
!             !current allocation

!}}}

zdMemRealloc FUNCTION(addr,len)
oldBin       LONG,AUTO
newBin       LONG,AUTO
newAddr      LONG,AUTO
oldSize      LONG,AUTO
newSize      LONG,AUTO
bin          EQUATE(newBin)
  CODE

  IF HOOKING THEN zdMemBreak(len,zdMemBreak:Realloc).
  IF ~addr   THEN RETURN zdMemAlloc(len).
  IF ~len    THEN zdMemFree(addr); RETURN 0.

  oldBin = zdMemAddressMap[BSHIFT(addr,-zdMemAddrShift)]     !NB: still valid even with the check prefix
  IF HOOKING THEN zdMemBreak(0-oldBin,zdMemBreak:Realloc).
  IF ~oldBin
    !{{{  its 'foriegn'
    !Can't deal with this without more info - like the orig len
    !So just free and alloc afresh - not propagating the orig contents
    !Will we get away with it?
    zdMemPanic(0,Panic:Foreign)          !08/07/11 DCN Hack to see if it happens
                                         !22/04/15 DCN It doesn't
    LOOP.
    !}}}
  ELSIF oldBin = zdMemVarBinNum
    !{{{  its a big slab
    oldSize = zdMemSlab2Size(addr)       !NB: still valid even with the check prefix
    IF ~oldSize
      !This shouldn't be possible
      zdMemPanic(0,Panic:Foreign)
      LOOP.
    END
    !}}}
  ELSE
    oldSize = zdMemBinEdges[oldBin]
  END
  IF CHECKING
    zdMemValidateAddress(oldBin,addr-zdMemCheckPrefix,,oldSize)
    !{{{  adjust old size for the 'true' bin
    oldSize -= zdMemCheckPrefix + zdMemCheckSuffix
    IF oldSize < 4
      !This shouldn't be possible
      zdMemPanic(oldBin,Panic:Overflow)
      LOOP.
    END
    oldBin = zdMemSize2BinNum(oldSize)
    IF oldBin = zdMemVarBinNum
      oldSize = zdMemSize2SlabSize(oldSize)
    ELSE
      oldSize = zdMemBinEdges[oldBin]
    END
    !}}}
  END

  INCLUDE('zd_alloc.clw','__bin=function(len)')  !newBin = zdMemSize2BinNum(len)
  IF newBin = zdMemVarBinNum
    newSize = zdMemSize2SlabSize(len)
  ELSE
    newSize = zdMemBinEdges[newBin]
  END

  IF LOGGING
    IF oldSize < newSize
      zdMemLogBinUp(oldSize)
    ELSIF oldSize > newSize
      zdMemLogBinDown(oldSize)
    ELSE
      zdMemLogBinSame(oldSize)
    END
  END

  IF HOOKING THEN zdMemBreak(newSize,zdMemBreak:Realloc).

  IF CHECKING
    !Do it even if same size bin, so suffix gets reset
  ELSE
    IF newSize = oldSize THEN RETURN addr.     !no-op if staying in the same bin
  END

  !{{{  get a new allocation and copy contents

  !Get a new allocation
  newAddr = zdMemAlloc(newSize)
  IF ~newAddr THEN RETURN 0.             !malloc would've tried to recover, so no point trying it again now

  IF CHECKING
    PEEK(addr-zdMemCheckPrefix,oldSize)
    newSize = len                        !set the true size
  END

  !Copy old contents
  IF oldSize < newSize
    !Its growing - so use old length
    memCpy(newAddr,addr,oldSize)         !NB: This is over copying, so caller better not
                                         !    be relying on all the new bit being zeroed!
  ELSE
    !Its shrinking - so use new length
    memCpy(newAddr,addr,newSize)
  END

  IF CHECKING
    POKE(newAddr-zdMemCheckPrefix,len)   !set actual size
  END

  !Free the orig
  zdMemFree(addr)

  !}}}

  RETURN newAddr

!}}}

!{{{  zdMemPanic

!{{{  history

! 08/07/11 DCN Created
! 25/11/11 DCN Call SysPanic if we've got one
! 08/03/13 DCN Add alignment stuff
!              Use FatalAppExit not HALT
! 12/07/13 DCN Do fatal exit if recurse into here (not try to wait for space)

!}}}
!{{{  description

!This is called if the memory allocator runs out of memory.
!We must stop dead in this context 'cos the Clarion RTL behaves
!very badly when a memory allocation fails (usually just not
!allowing for it, so creating memory access violations).

!zdMemPanic(LONG,LONG)
!           !    !a reason code
!           !the bin access causing the panic

!The idiom for calling this function is this:
! ...
! LOOP
!   ptr = malloc(...)
!   IF ~ptr THEN Panic(bin,Panic:Malloc); CYCLE.
!   BREAK
! END
! ...

!The logic in here must not do anything that might cause a malloc
!to be attempted, either explicilty or implicitly, so its very
!limited on what it can do!

!First off we try to release any slabs that are not being used, i.e.
!all their chunks are on the free list. If this is achieved, we return
!to the caller so they can have another go. If that is not adequate
!they'll be back in here again and this time there will be no un-used
!slabs so it'll drop through to the next phase.

!In the next phase, we release a chunk of memory we allocated on start-up
!to give us enough elbow room.

!The next phase will call SysPanic if there is one to ask it to release
!memory. If that comes back TRUE we re-grab our elbow room and return to
!the caller.

!If it fails, that's it, we're stuffed, so we abort the application.

!Thus if the function returns at all it means "have another go".

!}}}

zdMemPanic PROCEDURE(bin,Reason)
  CODE

  IF M.Panicing = ThreadId()
    !This is self recursing - means our elbow room has run out

  ELSIF M.Panicing
    !already in a panic!
    IF Reason >= Panic:FirstFatal
      !Can't re-try these
    ELSE
      Snooze(0)                !assume the other thread will sort it
      RETURN                   !so keep re-trying here
    END

  ELSE
    M.Panicing = ThreadId()    !only do this for the first thread that gets into a panic

    IF Reason >= Panic:FirstFatal
      !Do nothing
    ELSE
      IF LOGGING
        zdMemLogPanic(bin)
      END
      !{{{  phase 1 - try to find and release unused slabs
      
      IF zdMemTrimSlabs()
        !We released at least one unused slab, have another go
        !This will only help if what we released is a different bin size
        !to that required. If it doesn't help, we'll be back because the
        !OS won't be able to allocate a slab big enough. If we've fragemented
        !the process address space too much - tough! We'll abort.
        M.Panicing = 0         !not panicing anymore
        RETURN                 !tell caller to have anothet go
      END
        
      !}}}
      !{{{  phase 2 - ask the application to make some space
      
      IF M.PanicSpace
        zdMemFree(M.PanicSpace)          !make some elbow room
        M.PanicSpace = 0
      END

      IF SysPanicP AND SysPanic()
        !We released some space
        !If what is required is now on one of our free lists, the job's a good 'un,
        !if not we'll be back and more free slabs will be released in phase 1. If
        !that's not good enough - tough! We'll abort.
        !Get our elbow room back
        IF ~M.PanicSpace
          M.PanicSpace = zdMemAlloc(PanicSpaceSize)
          !If we get here it succeeded
        END
        M.Panicing = 0                   !not panicing anymore
        RETURN                           !tell caller to have anothet go
      END
      
      !}}}
      !phase 3 - give up
    END

  END

  zdMemAbort(Reason)

!}}}
!{{{  zdMemAbort

!{{{  history

!11/05/15 DCN Created

!}}}
!{{{  description

!Abort the appliaction for the given reason.

!zdMemAbort(LONG PanicCode)
!           !reason, must be one of the Panic:... codes

!}}}

zdMemAbort PROCEDURE(Reason)
  CODE
  IF CHECKING
    StartDebugger()
  END
  AbortApp(0,ADDRESS(PanicMsgs[Reason]))
  LOOP.

!}}}
!{{{  zdMemBreak

!{{{  history

!06/05/15 DCN Created

!}}}
!{{{  description

!Provide a place to put a DebugBreak if the required condition is
!met for the given length.

!zdMemBreak(LONG len,BYTE condition)
!           !        !condition now
!           !length involved, -ve is a bin number, +ve is a length

!}}}

zdMemBreak PROCEDURE(len,condition)
bin        LONG,AUTO
  CODE
  IF len > 0
    bin = zdMemSize2BinNum(len)
  ELSIF len < 0
    bin = 0-len
  ELSE
    bin = 0
  END
  IF ~bin THEN RETURN.
  IF BAND(zdMemBinBreak[bin],condition)
    zdMemBreakConditionMet += 1          !break here for all occurances
    IF zdMemBreakConditionMet > zdMemBreakLimit
      zdMemBreakLimitMet += 1            !break here for the N'th occurance
    END
  END

!}}}

!}}}
!{{{  usage statistics

!06/03/13 DCN In C8, allocation sizes range from 2^5 to 2^16, with
!             the vast majority in 2^5 and 2^6.

!These stats are used to find what the typical memory usage pattern is
!so the allocator can be tuned to that pattern. These stats are not
!gathered in a release build.

!The objective is to discover if a 'Simple Segregated Storage' system
!is feasible.

!See 'http://www.boost.org/doc/libs/1_35_0/libs/pool/doc/concepts.html'
!for a description of this idea. In the context here it can be *very*
!simple 'cos we don't allocate large objects and we don't have large
!numbers of threads. Also typical apps tend to keep using the same things
!over and over (e.g. adding and deleting Q entries).

!{{{  zdMemLogSlabAlloc

!Given a bin number, log a slab allocation in that bin

zdMemLogSlabAlloc PROCEDURE(LONG Bin)
  CODE
  IF ~Bin THEN RETURN.

  c# = LockedIncrement(M.S.SlabCount[Bin])
  s# = LockedIncrement(M.S.Slabs    [Bin])

  IF s# > M.S.MaxSlabs[Bin] THEN M.S.MaxSlabs[Bin] = s#.

!}}}
!{{{  zdMemLogSlabFree

!Given a bin number, log a slab de-allocation in the associated bin size

zdMemLogSlabFree PROCEDURE(LONG Bin)
  CODE

  IF ~Bin THEN RETURN.

  LockedDecrement(M.S.Slabs[Bin])

!}}}
!{{{  zdMemLogChunkAlloc

!Given an allocation length, log an allocation in the associated bin size

zdMemLogChunkAlloc PROCEDURE(LONG ptr,LONG len)

Bin                LONG,AUTO

  CODE

  Bin = zdMemAddressMap[BSHIFT(ptr,-zdMemAddrShift)]
  IF ~Bin THEN RETURN.

  IF ~M.S.Slabs[Bin]
    zdMemPanic(Bin,Panic:BadChunk)
    LOOP.
  END

  cc# = LockedIncrement(M.S.ChunkCount[Bin])
  c#  = LockedIncrement(M.S.Chunks    [Bin])

  IF c# > M.S.MaxChunks[Bin] THEN M.S.MaxChunks[Bin] = c#.

!}}}
!{{{  zdMemLogChunkFree

!Given an allocation pointer, log a de-allocation in the associated bin size

zdMemLogChunkFree PROCEDURE(LONG ptr)

Bin               LONG,AUTO

  CODE

  Bin = zdMemAddressMap[BSHIFT(ptr,-zdMemAddrShift)]
  IF ~Bin THEN RETURN.

  IF ~M.S.Slabs[Bin]
    zdMemPanic(Bin,Panic:BadChunk)
    LOOP.
  END

  LockedDecrement(M.S.Chunks[Bin])

!}}}
!{{{  zdMemLogAligned

!Given an alignment, log an allocation in the associated bin size

zdMemLogAligned PROCEDURE(LONG Alignment,LONG len)

Bin            LONG,AUTO

  CODE

  Bin = zdMemSize2BinNum(Alignment)

  LockedIncrement(M.S.Align2Count[Bin])

  Bin = zdMemSize2BinNum(len)

  LockedIncrement(M.S.Align4Count[Bin])

!}}}
!{{{  zdMemLogClash

!Given a bin, log an thread clash accessing it.
!A thread clash is when more than one thread is doing an allocate/free at
!the same time and had to do a re-try.

zdMemLogClash PROCEDURE(LONG bin)
  CODE

  LockedIncrement(M.S.ClashCount[Bin])

!}}}
!{{{  zdMemLogTrim

!Given a bin, log a trim operation for it.

zdMemLogTrim PROCEDURE(LONG bin)
  CODE

  LockedIncrement(M.S.TrimCount[Bin])

!}}}
!{{{  zdMemLogPanic

!Given a bin, log a recoverable 'panic' associated with it.
!A panic happens if a new slab is required and we've run out of address space.

zdMemLogPanic PROCEDURE(LONG bin)
  CODE

  LockedIncrement(M.S.PanicCount[Bin])

!}}}
!{{{  zdMemLogBinUp

!Given a realloc size log a bin size increase in the associated bin size

zdMemLogBinUp PROCEDURE(LONG len)

Bin           LONG,AUTO

  CODE

  Bin = zdMemSize2BinNum(len)

  LockedIncrement(M.S.BinUpCount[Bin])

!}}}
!{{{  zdMemLogBinDown

!Given a realloc size log a bin size decrease in the associated bin size

zdMemLogBinDown PROCEDURE(LONG len)

Bin             LONG,AUTO

  CODE

  Bin = zdMemSize2BinNum(len)

  LockedIncrement(M.S.BinDownCount[Bin])

!}}}
!{{{  zdMemLogBinSame

!Given a realloc size log a bin size no-change in the associated bin size

zdMemLogBinSame PROCEDURE(LONG len)

Bin             LONG,AUTO

  CODE

  Bin = zdMemSize2BinNum(len)

  LockedIncrement(M.S.BinSameCount[Bin])

!}}}

!}}}

!==============================================================================
!Called from your application
!==============================================================================

!{{{  zdMemProcessStart

!{{{  history

!27/04/15 DCN Extracted from match_do.inc
!12/12/15 DCN Use zdMemClearStats not DIY
!16/12/15 DCN Explicitly de-reference _malloc et al when using DLL link mode

!}}}
!{{{  description

!This must be called once during system start-up and as early as possible.
!The earliest (that I have discovered) is to create a static CLASS with a
!constructor and place that class as early as possible in the main source
!file of the system.

!The function prepares the memory allocator and replaces the Clarion one with this one.
!We do that by jamming a 'jump' to our code in the front of theirs.

!zdMemProcessStart()

!}}}

zdMemProcessStart PROCEDURE

ptr               LONG,AUTO
OldProtect        LONG,AUTO
Bin               LONG,AUTO
DllHandle         LONG,AUTO

Jump              GROUP(JumpType),AUTO.

  CODE

  IF ~MallocIsDIY
    !Do nothing

  ELSIF ~M_Patched
    M_Patched = TRUE
    !{{{  setup our free list headers

    !The free list headers must be 8-byte algined.
    !This rig-ma-role ensures that.

    zdMemBinFreeList = ADDRESS(zdMemFreeListSpace) + (ADDRESS(zdMemFreeListSpace) % zdMemListHeadAlign)
    !                                                 !<--------------------0..7-------------------->!

    !Just to be sure, clear it
    memSet(zdMemBinFreeList,0,zdMemListHeadSize*zdMemMaxBin)

    !}}}
    !{{{  dynamically link to InterlockedCompareExchange64

    IF XP_MODE
      LockedCompareExchange64Ptr = 0
    ELSE
      DLLhandle = LoadLibrary(Kernel32DllName)
      IF ~DllHandle
        zdMemPanic(0,Panic:FailLoadKernel)
        LOOP.
      END
      LockedCompareExchange64Ptr = GetProcAddress(DLLHandle,LockedCompareExchange64Name)
    END
    IF ~LockedCompareExchange64Ptr
      !Function not defined, we must be Win XP, use our DIY replacement
      LockedCompareExchange64Ptr = ADDRESS(zdMemLockedCompareExchange64)
      zdMemXPmode = TRUE
    ELSE
      zdMemXPmode = FALSE
    END

    !}}}
    !{{{  prepare our look-up tables

    zdMemMakeBinEdges()
    zdMemMakeSmallSizeBins()

    !}}}
    !{{{  init the statitsics

    zdMemClearStats(TRUE)

    !}}}
    !{{{  prepare page hold limiting system

    M.PagesHoldLimit = 0
    M.PagesHeldNow   = 0
    M.PageReducing   = 0

    !}}}
    !{{{  setup the malloc pointers

    ptr = ADDRESS(oldMalloc ); IF MallocIsDLL THEN PEEK(ptr,ptr).; M.Malloc  = ptr; memCpy(ADDRESS(M.MallocWas ),M.Malloc ,SIZE(M.MallocWas ))
    ptr = ADDRESS(oldRealloc); IF MallocIsDLL THEN PEEK(ptr,ptr).; M.Realloc = ptr; memCpy(ADDRESS(M.ReallocWas),M.Realloc,SIZE(M.ReallocWas))
    ptr = ADDRESS(oldCalloc ); IF MallocIsDLL THEN PEEK(ptr,ptr).; M.Calloc  = ptr; memCpy(ADDRESS(M.CallocWas ),M.Calloc ,SIZE(M.CallocWas ))
    ptr = ADDRESS(oldFree   ); IF MallocIsDLL THEN PEEK(ptr,ptr).; M.Free    = ptr; memCpy(ADDRESS(M.FreeWas   ),M.Free   ,SIZE(M.FreeWas   ))
    ptr = ADDRESS(oldAligned); IF MallocIsDLL THEN PEEK(ptr,ptr).; M.Aligned = ptr; memCpy(ADDRESS(M.AlignedWas),M.Aligned,SIZE(M.AlignedWas))

    !}}}

    !{{{  patch RTL to use out Malloc

    !NB: Do free first so RTL can't possibly see any allocs we do
    
    VirtualProtect(M.Free,SIZE(M.FreeWas),PAGE_EXECUTE_READWRITE,OldProtect)
      Jump.Op     = ASM_JMP32REL
      Jump.Offset = ADDRESS(zdMemFree) - M.Free - ASM_JMPLEN
      memCpy(M.Free,ADDRESS(Jump),SIZE(Jump))
    VirtualProtect(M.Free,SIZE(M.FreeWas),OldProtect,OldProtect)

    VirtualProtect(M.Realloc,SIZE(M.ReallocWas),PAGE_EXECUTE_READWRITE,OldProtect)
      Jump.Op     = ASM_JMP32REL
      Jump.Offset = ADDRESS(zdMemRealloc) - M.Realloc - ASM_JMPLEN
      memCpy(M.Realloc,ADDRESS(Jump),SIZE(Jump))
    VirtualProtect(M.Realloc,SIZE(M.ReallocWas),OldProtect,OldProtect)
    
    VirtualProtect(M.Malloc,SIZE(M.MallocWas),PAGE_EXECUTE_READWRITE,OldProtect)
      Jump.Op     = ASM_JMP32REL
      Jump.Offset = ADDRESS(zdMemMalloc) - M.Malloc - ASM_JMPLEN
      memCpy(M.Malloc,ADDRESS(Jump),SIZE(Jump))
    VirtualProtect(M.Malloc,SIZE(M.MallocWas),OldProtect,OldProtect)
    
    VirtualProtect(M.Calloc,SIZE(M.CallocWas),PAGE_EXECUTE_READWRITE,OldProtect)
      Jump.Op     = ASM_JMP32REL
      Jump.Offset = ADDRESS(zdMemCalloc) - M.Calloc - ASM_JMPLEN
      memCpy(M.Calloc,ADDRESS(Jump),SIZE(Jump))
    VirtualProtect(M.Calloc,SIZE(M.CallocWas),OldProtect,OldProtect)
    
    VirtualProtect(M.Aligned,SIZE(M.AlignedWas),PAGE_EXECUTE_READWRITE,OldProtect)
      Jump.Op     = ASM_JMP32REL
      Jump.Offset = ADDRESS(zdMemAligned) - M.Aligned - ASM_JMPLEN
      memCpy(M.Aligned,ADDRESS(Jump),SIZE(Jump))
    VirtualProtect(M.Aligned,SIZE(M.AlignedWas),OldProtect,OldProtect)
    
    !}}}

    M.PanicSpace = zdMemAlloc(PanicSpaceSize)  !elbow room for the panic function

  END

!}}}
!{{{  zdMemProcessStop

!{{{  history

!27/04/15 DCN Clear SysPanicP

!}}}
!{{{  description

!This must be called as late as possible when the system is shutting down.

!zdMemProcessStop()

!}}}

zdMemProcessStop PROCEDURE

OldProtect       LONG,AUTO

  CODE

  IF ~MallocIsDIY
    !Do nothing

  ELSE

    M_Stopped = TRUE
    SysPanicP = 0

    RETURN !04/06/11 DCN HACK - leave it else Clarion gets given blocks to free
           !                    that were allocated by our malloc and it aborts,
           !                    the process is about to exit anyway so who cares about the leak

    IF M_Patched
      M_Patched = FALSE

      VirtualProtect(M.Malloc,SIZE(M.MallocWas),PAGE_EXECUTE_READWRITE,OldProtect)
        memCpy(M.Malloc,ADDRESS(M.MallocWas),SIZE(M.MallocWas))
      VirtualProtect(M.Malloc,SIZE(M.MallocWas),OldProtect,OldProtect)

      VirtualProtect(M.Realloc,SIZE(M.ReallocWas),PAGE_EXECUTE_READWRITE,OldProtect)
        memCpy(M.Realloc,ADDRESS(M.ReallocWas),SIZE(M.ReallocWas))
      VirtualProtect(M.Realloc,SIZE(M.ReallocWas),OldProtect,OldProtect)

      VirtualProtect(M.Calloc,SIZE(M.CallocWas),PAGE_EXECUTE_READWRITE,OldProtect)
        memCpy(M.Calloc,ADDRESS(M.CallocWas),SIZE(M.CallocWas))
      VirtualProtect(M.Calloc,SIZE(M.CallocWas),OldProtect,OldProtect)

      VirtualProtect(M.Free,SIZE(M.FreeWas),PAGE_EXECUTE_READWRITE,OldProtect)
        memCpy(M.Free,ADDRESS(M.FreeWas),SIZE(M.FreeWas))
      VirtualProtect(M.Free,SIZE(M.FreeWas),OldProtect,OldProtect)

      VirtualProtect(M.Aligned,SIZE(M.AlignedWas),PAGE_EXECUTE_READWRITE,OldProtect)
        memCpy(M.Aligned,ADDRESS(M.AlignedWas),SIZE(M.AlignedWas))
      VirtualProtect(M.Aligned,SIZE(M.AlignedWas),OldProtect,OldProtect)

    END

  END

!}}}

!{{{  zdMemSetPanic

!{{{  history

!27/04/15 DCN Created

!}}}
!{{{  description

!Set the SysPanic function that is called when we run out of memory.
!That function must have a prototype like this:
!  SysPanic(),LONG
!It should try and take steps to release memory (without calling any functions
!that try to allocate more, so calling the Clarion RTL, UI, etc is a no-no!)
!and return TRUE iff it did or FALSE if it failed. If it returns TRUE the allocator
!attempts its failed allocation again. If it returns FALSE a fatal exit is done.

!zdMemSetPanic(LONG)
!              !address of a SysPanic function, or 0 to turn it off

!}}}

zdMemSetPanic PROCEDURE(SysPanicPtr)
  CODE

  IF ~MallocIsDIY
    !Do nothing

  ELSE
    SysPanicP = SysPanicPtr

  END

!}}}
