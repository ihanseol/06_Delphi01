{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{*******************************************************}
{             System Initialization Unit                }
{*******************************************************}

unit SysInit;


interface

{$H+,I-,R-,S-,O+,W-}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNSAFE_TYPE OFF}

// X86ASM
// X64ASM
{$IF defined(CPUX86) and defined(ASSEMBLER)}
  {$DEFINE X86ASM}
{$ELSEIF defined(CPUX64) and defined(ASSEMBLER)}
  {$DEFINE X64ASM}
{$ENDIF}

{$IFDEF CPUX86}
  {$IFNDEF PC_MAPPED_EXCEPTIONS}
    {$DEFINE STACK_BASED_EXCEPTIONS}
  {$ENDIF !PC_MAPPED_EXCEPTIONS}
{$ELSE !CPUX86}
  {$IFDEF WIN64}
    {$DEFINE TABLE_BASED_EXCEPTIONS}
  {$ENDIF WIN64}
{$ENDIF !CPUX86}

{$IFDEF LINUX32}
const
  ExeBaseAddress = Pointer($8048000) platform;
{$ENDIF LINUX32}
{$IFDEF LINUX64}
const
  ExeBaseAddress = Pointer($400000) platform;
{$ENDIF LINUX64}
{$IFDEF MACOS32}
const
  ExeBaseAddress = Pointer($10000) platform;
{$ENDIF MACOS32}
{$IFDEF ANDROID}
const
  ExeBaseAddress = Pointer($0) platform;
{$ENDIF}

{$IFDEF MACOS}
{
  We save off the old exception ports for the task.  We expect a maximum
  of 16 of these for any task.
}
const
  MAX_EXCEPTION_PORTS = 16;
{$ENDIF MACOS}

{$IFDEF MACOS}
type
  // Mach exception handler data
  TMachEHData = record
    eh_port_masks: array [0..MAX_EXCEPTION_PORTS-1] of uint32;
    eh_ports: array [0..MAX_EXCEPTION_PORTS-1] of uint32;
    eh_behaviors: array [0..MAX_EXCEPTION_PORTS-1] of Integer;
    eh_flavors: array [0..MAX_EXCEPTION_PORTS-1] of Integer;
    eh_portcnt: uint32;

    function Get(task: uint32; mask: uint32) : Integer;
    procedure Apply(task: uint32);
  end;
{$ENDIF MACOS}


{$IF defined(EXTERNALLINKER)}
var
  TlsStart: NativeInt;          { Specially handled with TlsLast by the code generator }
{$ENDIF}

var
  ModuleIsLib: Boolean;         { True if this module is a dll (a library or a package) }
  ModuleIsPackage: Boolean;     { True if this module is a package }
  ModuleIsCpp: Boolean;         { True if this module is compiled using C++ Builder }
  {$IFDEF LINUX}                { Thread local storage index.  Should match pthread_key_t for POSIX and DWORD for Windows }
  TlsIndex: Cardinal = Cardinal(-1);
  {$ELSEIF defined(ANDROID)}
  TlsIndex: Integer  = -1;
  {$ELSE}
  TlsIndex: LongWord = LongWord(-1);
  {$ENDIF}

  TlsLast: Byte;                { Set by linker so its offset is last in TLS segment }
  HInstance: HINST;             { Handle of this instance }
  ImageBase: Pointer;           { Special symbol that is actually located at the module load address. @ImageBase. }
  {$EXTERNALSYM ImageBase}
  __ImageBase: Pointer;         { Same as ImageBase, only with a different name }
  {$EXTERNALSYM __ImageBase}
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM HInstance}
  (*$HPPEMIT 'namespace Sysinit' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT 'extern PACKAGE HINSTANCE HInstance;' *)
  (*$HPPEMIT '} /* namespace Sysinit */' *)
{$ENDIF}
  DllProc: TDLLProc;            { Called whenever DLL entry point is called }
  { DllProcEx passes the Reserved param provided by WinNT on DLL load & exit }
  DllProcEx: TDLLProcEx absolute DllProc;
  DataMark: Integer = 0;        { Used to find the virtual base of DATA seg }
  CoverageLibraryName: array [0..128] of _AnsiChr = '*'; { initialized by the linker! }
{$IFDEF ELF}
  TypeImportsTable: array [0..0] of Pointer platform;  { VMT and RTTI imports table for exes }
{$ENDIF ELF}
  dbkFCallWrapperAddr: Pointer;
{$IF defined(EXTERNALLINKER)}
  dbk_RTL_initialized: Integer;
{$ENDIF}
{$IFDEF MACOS}
  __lldbFCallExceptionType: PWideChar;
  ExcThreadPort: UInt64;
  RTLExceptionPort: uint32;
  OrigEHData: TMachEHData;
{$ENDIF MACOS}
{$IF defined(ELF) or defined(MACOS) or defined(ANDROID)}
  _GLOBAL_OFFSET_TABLE_: ARRAY [0..2] OF Cardinal platform;
  (* _DYNAMIC: ARRAY [0..0] OF Elf32_Dyn; *)
{$ENDIF ELF or MACOS or ANDROID}
{$IFDEF PC_MAPPED_EXCEPTIONS}
  TextStartAdj: Byte platform;            { See GetTextStart }
  CodeSegSize: Byte platform;             { See GetTextStart }

function GetTextStart: NativeInt; platform;
{$ENDIF PC_MAPPED_EXCEPTIONS}

const
  PtrToNil: Pointer = nil;     // provides pointer to nil for compiler codegen

{$IF defined(CPUX64)}
                                                                          
// Be careful with using FSSegBase and GSSegBase.
// FS:[x] or GS:[x] memory addresses may be generated but not in all cases.
// If backend decided to use LEA instruction, FS: or GS: prefix will not
// be generated in later code.
const
  FSSegBase: Pointer = nil; // provides FS segment access for compiler codegen
  GSSegBase: Pointer = nil; // provides GS segment access for compiler codegen
{$ENDIF}

{$IFDEF POSIX}
var
  ThreadInitProc: Pointer = nil;
{$ENDIF POSIX}

function _GetTls: Pointer;

{$IFDEF POSIX}
procedure _GetCallerEIP;
{$ENDIF POSIX}

{$IFDEF MACOS}
procedure lldb_invoke_except_handler(ExcType: String);
{$ENDIF MACOS}
{
  Startup/shutdown routines, called by the code generator.  These are
  platform specific.
}
{$IF defined(LINUX) or defined(MACOS) or defined(ANDROID)}
{$IF defined(EXTERNALLINKER)}
procedure _InitLib(Context: PInitContext; InitTable: Pointer);
procedure _ExitLib; cdecl;
{$ELSE}
procedure _InitLib(Context: PInitContext);
function _ExitLib: Integer; cdecl;
{$ENDIF}
function _InitPkg: LongBool;
{$ENDIF LINUX or MACOS or ANDROID}
{$IF defined(MACOS) and defined(EXTERNALLINKER)}
procedure _InitExe(InitTable: Pointer; Argc: Integer; Argp, Envp, Prog: Pointer);
{$ENDIF MACOS and EXTERNALLINKER}
{$IF defined(MACOS) and (not defined(EXTERNALLINKER))}
procedure _InitExe(InitTable: Pointer; ProgramParams: Pointer);
{$ENDIF MACOS and !EXTERNALLINKER}
{$IF defined(ANDROID)}
procedure _InitExe(InitTable: Pointer);
procedure InitExeCPP;
{$ENDIF ANDROID}
{$IFDEF LINUX}
procedure _InitExe(InitTable: Pointer; Argc: Integer; Argp: Pointer);
procedure InitExeCPP;
{$ENDIF LINUX}
{$IF Defined(LINUX32) and Defined(CPUX86)}
procedure _start; cdecl;
{$ENDIF CPUX86}
{$IFDEF LINUX64}
                     
// procedure _start; cdecl;
{$ENDIF LINUX64}
{$IFDEF MSWINDOWS}
{$IF defined(CPU386)}
procedure _InitLib;
{$ELSE !CPU386}
procedure _InitLib(Context: PInitContext; InitTable: PackageInfo; AHInst: HINST; Reason: LongWord; Reserved: Pointer);
{$ENDIF !CPU386}
procedure _InitExe(InitTable: Pointer);
function _InitPkg(AHInst: HINST; Reason: Integer; Resvd: Pointer): LongBool; stdcall;
{$ENDIF MSWINDOWS}

procedure _PackageLoad(const Table: PackageInfo);
procedure _PackageUnload(const Table: PackageInfo);

{ Invoked by C++ startup code to allow initialization of VCL global vars }
{$IF defined(CPU386)}
procedure VclInit(isDLL, isPkg: Boolean; hInst: LongInt; isGui: Boolean); cdecl;
{$ELSE}
procedure VclInit(isDLL, isPkg: Boolean; hInst: HINST; isGui: Boolean); cdecl;
{$ENDIF}
procedure VclExit; cdecl;

{$IFDEF POSIX}
function GetThisModuleHandle: THandle;
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  PImageThunkData = ^TImageThunkData;
  TImageThunkData = record
    case Byte of
      0: (ForwarderString: NativeUInt); // PBYTE
      1: (_Function: NativeUInt);       // PLongWord Function -> _Function
      2: (Ordinal: NativeUInt);
      3: (AddressOfData: NativeUInt);   // PIMAGE_IMPORT_BY_NAME
  end;
  {$EXTERNALSYM TImageThunkData}
  {$EXTERNALSYM PImageThunkData}

  ImgDelayDescr = record
    grAttrs:     LongWord;          { attributes                        }
    szName:      _PAnsiChr;         { pointer to dll name               }
    hmod:        HMODULE;           { address of module handle          }
    pIAT:        PImageThunkData;   { address of the IAT                }
    pINT:        PImageThunkData;   { address of the INT                }
    pBoundIAT:   PImageThunkData;   { address of the optional bound IAT }
    pUnloadIAT:  PImageThunkData;   { address of optional copy of
                                       original IAT                     }
    dwTimeStamp: LongWord;          { 0 if not bound,                   }
                                    { O.W. date/time stamp of DLL bound
                                       to (Old BIND)                    }
  end;
  {$EXTERNALSYM ImgDelayDescr}
  TImgDelayDescr = ImgDelayDescr;
  {$EXTERNALSYM TImgDelayDescr}
  PImgDelayDescr = ^TImgDelayDescr;
  {$EXTERNALSYM PImgDelayDescr}

{ Delay load import hook notifications }

  dliNotification = (
    dliStartProcessing,            { used to bypass or note helper only     }
    dliNoteStartProcessing = dliStartProcessing,
    dliNotePreLoadLibrary,         { called just before LoadLibrary, can    }
                                   {  override w/ new HMODULE return val    }
    dliNotePreGetProcAddress,      { called just before GetProcAddress, can }
                                   {  override w/ new Proc address return   }
                                   {  value                                 }
    dliFailLoadLibrary,            { failed to load library, fix it by      }
                                   {  returning a valid HMODULE             }
    dliFailGetProcAddress,         { failed to get proc address, fix it by  }
                                   {  returning a valid Proc address        }
    dliNoteEndProcessing           { called after all processing is done,   }
                                   {  no bypass possible at this point      }
                                   {  except by raise, or
                                       RaiseException.                       }
  );
  {$EXTERNALSYM dliNotification}

  DelayLoadProc = record
    fImportByName:      LongBool;
    case Byte of
      0: (szProcName:   _PAnsiChr);
      1: (dwOrdinal:    LongWord);
  end;
  {$EXTERNALSYM DelayLoadProc}
  TDelayLoadProc = DelayLoadProc;
  {$EXTERNALSYM TDelayLoadProc}
  PDelayLoadProc = ^TDelayLoadProc;
  {$EXTERNALSYM PDelayLoadProc}

  DelayLoadInfo = record
    cb:          LongWord;       { size of structure                 }
    pidd:        PImgDelayDescr; { raw form of data (everything is
                                   there)                            }
    ppfn:        Pointer;        { points to address of function to
                                   load                              }
    szDll:       _PAnsiChr;      { name of dll                       }
    dlp:         TDelayLoadProc; { name or ordinal of procedure      }
    hmodCur:     HMODULE;        { the hInstance of the library we
                                   have loaded                       }
    pfnCur:      Pointer;        { the actual function that will be
                                   called                            }
    dwLastError: LongWord;       { error received (if an error
                                   notification)                     }
  end;
  {$EXTERNALSYM DelayLoadInfo}
  TDelayLoadInfo = DelayLoadInfo;
  {$EXTERNALSYM TDelayLoadInfo}
  PDelayLoadInfo = ^TDelayLoadInfo;
  {$EXTERNALSYM PDelayLoadInfo}

  PfnDliHook = function (dliNotify: dliNotification; pdli: PDelayLoadInfo): Pointer; stdcall;
  {$EXTERNALSYM PfnDliHook}
  TDelayedLoadHook = PfnDliHook;
  {$EXTERNALSYM TDelayedLoadHook}

var
  __pfnDliNotifyHook2: Pointer;
  __pfnDliFailureHook2: Pointer;

procedure __delayLoadHelper2;
procedure __FUnloadDelayLoadedDLL2;
procedure __HrLoadAllImportsForDll;

{ Unload support }

var
  UnloadDelayLoadedDLLPtr: Pointer = @__FUnloadDelayLoadedDLL2;
  DelayLoadHelper: Pointer = @__delayLoadHelper2;
  pfnDliNotifyHook: Pointer = @__pfnDliNotifyHook2;
  pfnDliFailureHook: Pointer = @__pfnDliFailureHook2;
  HrLoadAllImportsForDll: Pointer = @__HrLoadAllImportsForDll;

{ Hook pointers }

{ The "notify hook" gets called for every call to the
   delay load helper.  This allows a user to hook every call and
   skip the delay load helper entirely.

   dliNotify =
   (
       dliNoteStartProcessing   or
       dliNotePreLoadLibrary    or
       dliNotePreGetProcAddress or
       dliNoteEndProcessing
   )

   on this call.
}

function SetDliNotifyHook2(HookProc: TDelayedLoadHook): TDelayedLoadHook;
function DliNotifyHook2: TDelayedLoadHook;
{$EXTERNALSYM SetDliNotifyHook2}
{$EXTERNALSYM DliNotifyHook2}

{ This is the failure hook,

   dliNotify =
   (
       dliFailLoadLibrary       or
       dliFailGetProcAddress
   )
}
function SetDliFailureHook2(HookProc: TDelayedLoadHook): TDelayedLoadHook;
function DliFailureHook2: TDelayedLoadHook;
{$EXTERNALSYM SetDliFailureHook2}
{$EXTERNALSYM DliFailureHook2}

{ takes a pointer to a name to unload, or NULL to unload all the delay load dlls in the list. }

procedure UnloadDelayLoadedDLL2(szDll: _PAnsiChr); stdcall;


{ Snap load support - Load and resolve all delay-loaded entrypoints for the given dll }
procedure LoadAllImportsForDll(szDll: _PAnsiChr); stdcall;

{$IF defined(X86ASM)}
procedure _DelayLoadHelper2;
{$ELSE !X86ASM}
function _DelayLoadHelper2(var Address: Pointer; DelayImportDescriptor: Pointer): Pointer;
{$ENDIF !X86ASM}

{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}
const
  kernel = 'kernel32.dll';

function InterlockedExchange(var Target: Integer; Value: Integer): Integer; stdcall;
begin
  Result := AtomicExchange(Target, Value);
end;

function FreeLibrary(ModuleHandle: HINST): LongBool; stdcall;
  external kernel name 'FreeLibrary';

function GetModuleFileNameA(Module: Integer; Filename: _PAnsiChr; Size: Integer): Integer; stdcall;
  external kernel name 'GetModuleFileNameA';
function GetModuleFileNameW(Module: Integer; Filename: PWideChar; Size: Integer): Integer; stdcall;
  external kernel name 'GetModuleFileNameW';
{$IFNDEF UNICODE}
function GetModuleFileName(Module: Integer; Filename: _PAnsiChr; Size: Integer): Integer; stdcall;
  external kernel name 'GetModuleFileNameA';
{$ELSE}
function GetModuleFileName(Module: Integer; Filename: PWideChar; Size: Integer): Integer; stdcall;
  external kernel name 'GetModuleFileNameW';
{$ENDIF}

function GetModuleHandleA(ModuleName: _PAnsiChr): HINST; stdcall;
  external kernel name 'GetModuleHandleA';
function GetModuleHandleW(ModuleName: PWideChar): HINST; stdcall;
  external kernel name 'GetModuleHandleW';
{$IFNDEF UNICODE}
function GetModuleHandle(ModuleName: _PAnsiChr): HINST; stdcall;
  external kernel name 'GetModuleHandleA';
{$ELSE}
function GetModuleHandle(ModuleName: PWideChar): HINST; stdcall;
  external kernel name 'GetModuleHandleW';
{$ENDIF}

function LocalAlloc(flags, size: Integer): Pointer; stdcall;
  external kernel name 'LocalAlloc';

function LocalFree(addr: Pointer): Pointer; stdcall;
  external kernel name 'LocalFree';

function TlsAlloc: Integer; stdcall;
  external kernel name 'TlsAlloc';

function TlsFree(TlsIndex: Integer): Boolean; stdcall;
  external kernel name 'TlsFree';

function TlsGetValue(TlsIndex: Integer): Pointer; stdcall;
  external kernel name 'TlsGetValue';

function TlsSetValue(TlsIndex: Integer; TlsValue: Pointer): Boolean; stdcall;
  external kernel name 'TlsSetValue';

function GetCommandLineA: _PAnsiChr; stdcall;
  external kernel name 'GetCommandLineA';
function GetCommandLineW: PWideChar; stdcall;
  external kernel name 'GetCommandLineW';
{$IFNDEF UNICODE}
function GetCommandLine: PAnsiChar; stdcall;
  external kernel name 'GetCommandLineA';
{$ELSE}
function GetCommandLine: PWideChar; stdcall;
  external kernel name 'GetCommandLineW';
{$ENDIF}

function GetLastError: Integer; stdcall;
  external kernel name 'GetLastError';

const
{$IF defined(CPU386)}
  tlsArray      = $2C;    { offset of tls array from FS: }
{$ENDIF}
{$IF defined(CPUX64)}
  tlsArray      = $58;    { offset of tls array from FS: }
{$ENDIF}
  LMEM_ZEROINIT = $40;

{$IFDEF MSWINDOWS}

{$IFDEF WIN32}
{$L delayhlp.obj}
{$ENDIF WIN32}
{$IFDEF WIN64}
{$L delayhlp.o}
{$ENDIF WIN64}

type
  DWORD = LongWord;
  PDWORD = ^DWORD;

function LoadLibraryA(lpLibFileName: PAnsiChar): HMODULE; stdcall;
  external kernel name 'LoadLibraryA';
procedure lstrcmpiA; external kernel name 'lstrcmpiA';
procedure RaiseException(dwExceptionCode, dwExceptionFlags, nNumberOfArguments: DWORD;
  lpArguments: PDWORD); stdcall;
  external kernel name 'RaiseException';
function  GetProcAddress(hModule: HINST; lpProcName: PAnsiChar): Pointer; stdcall; external kernel name 'GetProcAddress';
procedure __delayLoadHelper2; external;
procedure __FUnloadDelayLoadedDLL2; external;
procedure __HrLoadAllImportsForDll; external;
procedure ShutdownDelayHelp2; external;

{$IFDEF WIN64}
function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
begin
  Result := AtomicExchange(Target, Value);
end;
{$ENDIF WIN64}

function SetDliNotifyHook2(HookProc: TDelayedLoadHook): TDelayedLoadHook;
begin
  Result := AtomicExchange(PPointer(pfnDliNotifyHook)^, @HookProc);
end;

function DliNotifyHook2: TDelayedLoadHook;
begin
  Result := PPointer(pfnDliNotifyHook)^;
end;

function SetDliFailureHook2(HookProc: TDelayedLoadHook): TDelayedLoadHook;
begin
  Result := AtomicExchange(PPointer(pfnDliFailureHook)^, @HookProc);
end;

function DliFailureHook2: TDelayedLoadHook;
begin
  Result := PPointer(pfnDliFailureHook)^;
end;

                                                                    
{$IF defined(X86ASM)}
procedure _DelayLoadHelper2;
asm
     JMP [delayLoadHelper]
end;
{$ELSE !X86ASM}
function _DelayLoadHelper2(var Address: Pointer; DelayImportDescriptor: Pointer): Pointer;
type
  TDelayLoadHelperProc = function(var Address: Pointer; DelayImportDescriptor: Pointer): Pointer;
begin
  Result := TDelayLoadHelperProc(DelayLoadHelper)(Address, DelayImportDescriptor);
end;
{$ENDIF !X86ASM}

                                                                                          
procedure UnloadDelayLoadedDLL2(szDll: _PAnsiChr);
{$IF defined(X86ASM) or defined(X64ASM)}
asm
{$IF defined(X86ASM)}
     POP EBP
{$ENDIF}
     JMP [UnloadDelayLoadedDLLPtr]
end;
{$ELSE !(X86ASM or X64ASM)}
type
  TUnloadDelayLoadedDLLProc = procedure(szDll: PAnsiChar);
begin
  TUnloadDelayLoadedDLLProc(UnloadDelayLoadedDLLPtr)(szDll);
end;
{$ENDIF !(X86ASM or X64ASM)}

procedure LoadAllImportsForDll(szDll: _PAnsiChr); stdcall;
{$IF defined(X86ASM) or defined(X64ASM)}
asm
{$IF defined(X86ASM)}
     POP EBP
{$ENDIF}
     JMP [HrLoadAllImportsForDll]
end;
{$ELSE !(X86ASM or X64ASM)}
type
  TUnloadDelayLoadedDLLProc = procedure(szDll: _PAnsiChr);
begin
  TUnloadDelayLoadedDLLProc(HrLoadAllImportsForDll)(szDll);
end;
{$ENDIF !(X86ASM or X64ASM)}

{$ENDIF MSWINDOWS}

function AllocTlsBuffer(Size: Integer): Pointer;
begin
  Result := LocalAlloc(LMEM_ZEROINIT, Size);
end;

var
  tlsBuffer: Pointer;    // RTM32 DOS support
{$ENDIF}

{$IFNDEF POSIX} // Used for unsigned operations with -1.  POSIX version in block below.
const
  NEG_ONE = LongWord(-1);
{$ENDIF}

{$IFDEF POSIX}
{$I PosixAPIs.inc}
const
  NEG_ONE = pthread_key_t(-1);

function TlsGetValue(Key: pthread_key_t): Pointer; cdecl;
  external libpthread name _PU + 'pthread_getspecific';

function TlsSetValue(Key: pthread_key_t; Ptr: Pointer): Integer; cdecl;
  external libpthread name _PU + 'pthread_setspecific';

function AllocTlsBuffer(Size: Cardinal): Pointer;
begin
  // The C++ rtl handles all tls in a C++ module
  if ModuleIsCpp then
    RunError(226);

  Result := __malloc(Size);
  if Result <> nil then
    FillChar(Result^, Size, 0);
end;

procedure FreeTLSBuffer(ValueInKey: Pointer); export cdecl;
begin
  // The C++ rtl handles all tls in a C++ module
  if ModuleIsCpp then
    RunError(226);
  __free(ValueInKey);
end;

procedure AllocTlsIndex; cdecl export;
begin
  // guaranteed to reach here only once per process
  // The C++ rtl handles all tls in a C++ module
  if ModuleIsCpp then
    RunError(226);
  if pthread_key_create(TlsIndex, FreeTLSBuffer) <> 0 then
  begin
    TlsIndex := NEG_ONE;
    RunError(226);
  end;
end;

{$IFDEF PIC}
{$IF defined(X86ASM)}
function GetGOT: Pointer; export;
begin
  asm
  MOV Result,EBX
  end;
end;
{$ENDIF X86ASM}
{$ENDIF PIC}

function GetThisModuleHandle: THandle;
var
  Info: dl_info;
begin
{$IFDEF MACOS64}                                     
 if (dladdr(UIntPtr(@GetThisModuleHandle), Info) = 0) then
    Info.dli_fname := nil; // if we're not in a library, we must be main exe
{$ELSE !MACOS64}
 if (dladdr(UIntPtr(@GetThisModuleHandle), Info) = 0) or (Info.dli_fbase = ExeBaseAddress) then
    Info.dli_fname := nil; // if we're not in a library, we must be main exe
{$ENDIF MACOS64}

  Result := dlopen(Info.dli_fname, RTLD_LAZY);
  if Result <> 0 then
    dlclose(Result);
end;

{$IFDEF MACOS64}                                    
Var
  SysinitResSym: NativeUInt;

exports
  SysinitResSym;
{$ENDIF MACOS64}

{$IFDEF LINUX}
var
                                                                                    
//  InitOnceSemaphore: pthread_once_t = ONCE_INIT;
  InitOnceSemaphore: pthread_once_t = 0;
{$ENDIF LINUX}
{$IFDEF MACOS}
var
  InitOnceSemaphore: pthread_once_t = (__sig: $30B1BCBA);
{$ENDIF MACOS}
{$IFDEF ANDROID}
var
//  From pthread.h
  InitOnceSemaphore: pthread_once_t = 0;
{$ENDIF ANDROID}
{$ENDIF POSIX}

var
  Module: TLibModule = (
    Next: nil;
    Instance: 0;
    CodeInstance: 0;
    DataInstance: 0;
    ResInstance: 0;
    TypeInfo: nil;
    Reserved: 0
{$IF defined(LINUX) or defined(MACOS) or defined(ANDROID)}
    ; InstanceVar: nil;
    GOT: 0
{$ENDIF LINUX or MACOS or ANDROID}
{$IFDEF PC_MAPPED_EXCEPTIONS}
    ; CodeSegStart: 0;
    CodeSegEnd: 0
{$ENDIF PC_MAPPED_EXCEPTIONS}
    );

function GetTlsSize: Integer;
{$IF defined(POSIX) and defined(CPUX86) and (not defined(EXTERNALLINKER))}
asm
        // Use assembler code not to include PIC base gain
        MOV  EAX, offset TlsLast
end;
{$ELSE}
begin
  Result := NativeInt(@TlsLast);
  {$IF DECLARED(TlsStart)}
  Result := Result - NativeInt(@TlsStart);
  {$ENDIF}
end;
{$ENDIF}

procedure       InitThreadTLS;
var
  p: Pointer;
  tlsSize: Integer;
begin
  tlsSize := GetTlsSize;
  if tlsSize = 0 then  Exit;
{$IFDEF POSIX}
  pthread_once(InitOnceSemaphore, AllocTlsIndex);
{$ENDIF}
  if TlsIndex = NEG_ONE then
    RunError(226);
  p := AllocTlsBuffer(tlsSize);
  if p = nil then
    RunError(226)
  else
    TlsSetValue(TlsIndex, p);
end;

{$IFDEF MSWINDOWS}
procedure       InitProcessTLS;
begin
  if @TlsLast = nil then
    Exit;
  TlsIndex := TlsAlloc;
  InitThreadTLS;
  tlsBuffer := TlsGetValue(TlsIndex);  // RTM32 DOS support
end;

procedure       ExitThreadTLS;
var
  p: Pointer;
begin
  if @TlsLast = nil then
    Exit;
  if TlsIndex <> NEG_ONE then
  begin
    p := TlsGetValue(TlsIndex);
    if p <> nil then
    begin
      LocalFree(p);
      TlsSetValue(TlsIndex, nil);
    end;
  end;
end;

procedure       ExitProcessTLS;
begin
  if @TlsLast = nil then
    Exit;
  ExitThreadTLS;
  if TlsIndex <> NEG_ONE then
    TlsFree(TlsIndex);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  UseTlsAPI: Boolean; { True if _GetTls should use API mode }

function isWine: Boolean;
begin
//https://www.winehq.org/pipermail/wine-devel/2008-September/069387.html
  Result := GetProcAddress(GetModuleHandle('ntdll.dll'), 'wine_get_version') <> nil
end;
{$ENDIF}

const
  DLL_PROCESS_DETACH = 0;
  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH  = 2;
  DLL_THREAD_DETACH  = 3;

function _GetTls: Pointer;
{$IFDEF POSIX}
type
  ThreadInitProcType = procedure;
begin
  Result := TlsGetValue(TlsIndex);
  if Result = nil then
  begin
    InitThreadTLS;
    Result := TlsGetValue(TlsIndex);
    if ThreadInitProc <> nil then
      ThreadInitProcType(ThreadInitProc);
  end;
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IF defined(CPUX64)}
type
  PPPointerArray = ^PPointerArray;
var
  P: PPointerArray;
begin
  if UseTlsAPI then
  begin
    Result := TlsGetValue(TlsIndex);
    if Result = nil then
    begin
      InitThreadTLS;
      Result := TlsGetValue(TlsIndex);
      if Result = nil then
        Result := tlsBuffer;
    end;
  end
  else
  begin
    //P := PPPointerArray(ReadGSQWord(tlsArray));
    P := PPPointerArray(PByte(@GSSegBase) + tlsArray)^;
    Result := P^[TlsIndex];
  end;
end;
{$ELSE !CPUX64}
asm
        MOV     CL,UseTlsAPI
        MOV     EAX,TlsIndex
        TEST    CL,CL
        JNE     @@isDll
        MOV     EDX,FS:tlsArray
        MOV     EAX,[EDX+EAX*4]
        RET

@@initTls:
        CALL    InitThreadTLS
        MOV     EAX,TlsIndex
        PUSH    EAX
        CALL    TlsGetValue
        TEST    EAX,EAX
        JE      @@RTM32
        RET

@@RTM32:
        MOV     EAX, tlsBuffer
        RET

@@isDll:
        PUSH    EAX
        CALL    TlsGetValue
        TEST    EAX,EAX
        JE      @@initTls
end;
{$ENDIF !CPUX64}

const
  TlsProc: array [DLL_PROCESS_DETACH..DLL_THREAD_DETACH] of procedure =
    (ExitProcessTLS,InitProcessTLS,InitThreadTLS,ExitThreadTLS);
{$ENDIF MSWINDOWS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  UNWINDFI_TOPOFSTACK =   $BE00EF00;

{
  The linker sets the value of TextStartAdj to be the delta between GetTextStart
  and the start of the text segment.  This allows us to get the pointer to the
  start of the text segment in a position independent fashion.
}
function GetTextStart : NativeInt;
asm
        CALL  @@label1
@@label1:
        POP   EAX
        SUB   EAX, 5 + offset TextStartAdj
end;

{
  The linker sets the value of CodeSegSize to the length of the text segment,
  excluding the PC map.  This allows us to get the pointer to the exception
  information that we need at runtime, also in a position independent fashion.
}
function GetTextEnd : NativeInt;
asm
        CALL  GetTextStart
        ADD   EAX, offset CodeSegSize
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

procedure InitializeModule;
begin
  RegisterModule(@Module);
end;

{$IFDEF POSIX}
procedure FreeLibrary(Handle: LongWord);
begin
   dlclose(Handle);
end;
{$ENDIF POSIX}

procedure UninitializeModule;
begin
  UnregisterModule(@Module);
  if (Module.ResInstance <> Module.Instance) and (Module.ResInstance <> 0) then
    FreeLibrary(Module.ResInstance);
end;

{$IF defined(CPU386)}
procedure VclInit(isDLL, isPkg: Boolean; hInst: LongInt; isGui: Boolean); cdecl;
{$ELSE}
procedure VclInit(isDLL, isPkg: Boolean; hInst: HINST; isGui: Boolean); cdecl;
{$ENDIF}
begin
  ModuleIsLib := isDLL;
{$IFDEF MSWINDOWS}
  UseTlsAPI := isDLL or isWine;
{$ENDIF}
  ModuleIsPackage := isPkg;
  IsLibrary := isDLL and not isPkg;
  HInstance := hInst;
  Module.Instance := hInst;
  Module.CodeInstance := 0;
  Module.DataInstance := 0;
  ModuleIsCpp := True;
{$IF defined(LINUX) or defined(MACOS)}
  if ModuleIsLib then
    Module.InstanceVar := @HInstance;
{$IFDEF PIC}
{$IF defined(X86ASM)}
  Module.GOT := UIntPtr(GetGot);
{$ELSE}
                                                                 
  Module.GOT := UIntPtr(@_Global_Offset_Table_);
{$ENDIF}
{$ENDIF PIC}
  { Module.CodeSegStart, Module.CodeSegEnd not used:  the C++
    rtl will feed the unwinder. }
{$ENDIF LINUX or MACOS}
  InitializeModule;
  if not ModuleIsLib then
  begin
    Module.CodeInstance := FindHInstance(@VclInit);
    Module.DataInstance := FindHInstance(@DataMark);
{$IFDEF MSWINDOWS}
    CmdLine := GetCommandLine;
    IsConsole := not isGui;
{$ENDIF MSWINDOWS}
  end;
end;

procedure VclExit; cdecl;
var
  P: procedure;
begin
  if not ModuleIsLib then
    while ExitProc <> nil do
    begin
      @P := ExitProc;
      ExitProc := nil;
      P;
    end;
  UnInitializeModule;
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure RegisterPCMap;
begin
{$IF defined(ELF) or defined(MACOS)}
  SysRegisterIPLookup(GetTextStart,
                      GetTextEnd,
                      Pointer(GetTextEnd),
                      UIntPtr(@_Global_Offset_Table_));
{$ELSE !(ELF or MACOS)}
  SysRegisterIPLookup(GetTextStart,
                      GetTextEnd,
                      Pointer(GetTextEnd),
                      0);
{$ENDIF !(ELF or MACOS)}
end;

procedure UnregisterPCMap;
begin
  SysUnregisterIPLookup(GetTextStart);
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

// __dbkDummyProc is only used to generate padding in __dbkFCallArgs for the debugger
// to use as a place to generate code.
procedure __dbkDummyProc;
begin
end;

procedure __dbkFCallArgs(
  a01: Pointer; a02: Pointer; a03: Pointer; a04:Pointer; a05: Pointer;
  a06: Pointer; a07: Pointer; a08: Pointer; a09:Pointer; a10: Pointer;
  a11: Pointer; a12: Pointer; a13: Pointer; a14:Pointer; a15: Pointer;
  a16: Pointer; a17: Pointer; a18: Pointer; a19:Pointer; a20: Pointer;
  a21: Pointer; a22: Pointer; a23: Pointer; a24:Pointer; a25: Pointer;
  a26: Pointer; a27: Pointer; a28: Pointer; a29:Pointer; a30: Pointer);
begin
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;
  __dbkDummyProc; __dbkDummyProc; __dbkDummyProc; __dbkDummyProc;

end;

{$IF defined(MACOS)}
const
{$IF     defined(CPUARM32) or defined(CPUARM64)}
  MACHINE_THREAD_STATE = 1; // ARM_THREAD_STATE;
{$ELSEIF defined(CPUX86) or defined(CPUX64)}
  MACHINE_THREAD_STATE = 7; //x86_THREAD_STATE;
{$ENDIF }

var
  SavedPortMasks: array [0..MAX_EXCEPTION_PORTS-1] of uint32;
  SavedPorts: array [0..MAX_EXCEPTION_PORTS-1] of uint32;
  SavedBehaviors: array [0..MAX_EXCEPTION_PORTS-1] of Integer;
  SavedFlavors: array [0..MAX_EXCEPTION_PORTS-1] of Integer;
  SavedPortCount: uint32;

function mach_task_self: uint32; cdecl external libc name _PU + 'mach_task_self';

function task_get_exception_ports(task: uint32; exception_types: uint32; masks: Pointer;
    var masksCnt: uint32; old_handlers: Pointer; old_behaviors: Pointer; old_flavors: Pointer
  ): Integer; cdecl external libc name _PU + 'task_get_exception_ports';

function task_set_exception_ports(task: uint32; exception_types: uint32; new_port: uint32;
    behavior: Integer; new_flavor: Integer
  ): Integer; cdecl
  external libc name _PU + 'task_set_exception_ports';

function TMachEHData.Get(task: uint32; mask: uint32) : Integer;
begin
  FillChar(Self, SizeOf(TMachEHData), 0);
  Self.eh_portcnt := MAX_EXCEPTION_PORTS - 1;
  Result := task_get_exception_ports(task, mask, @Self.eh_port_masks, Self.eh_portcnt,
                                     @Self.eh_ports, @Self.eh_behaviors, @Self.eh_flavors);
end;

procedure TMachEHData.Apply(task: uint32);
var
  Ind: uint32;
begin
  for Ind := 0 to Self.eh_portcnt - 1 do
    task_set_exception_ports(task, Self.eh_port_masks[Ind], Self.eh_ports[Ind],
                             Self.eh_behaviors[Ind], Self.eh_flavors[Ind]);

end;

procedure MachExceptionsSuspend; export;
var
  Task: uint32;
  Mask: uint32;
begin
  Task := mach_task_self;
  Mask := $E; //EXC_MASK_BAD_ACCESS or EXC_MASK_ARITHMETIC or EXC_MASK_BAD_INSTRUCTION;
  SavedPortCount := MAX_EXCEPTION_PORTS-1;
  task_get_exception_ports(Task, Mask, @SavedPortMasks, SavedPortCount, @SavedPorts, @SavedBehaviors, @SavedFlavors);
  task_set_exception_ports(Task, Mask, 0, 3, MACHINE_THREAD_STATE); // EXCEPTION_STATE_IDENTITY = 3;
end;

procedure MachExceptionsResume; export;
var
  Task: uint32;
  Ind: uint32;
begin
  Task := mach_task_self;

  for Ind := 0 to SavedPortCount - 1 do
    task_set_exception_ports(Task, SavedPortMasks[Ind], SavedPorts[Ind], SavedBehaviors[Ind], SavedFlavors[Ind]);
end;

function thread_resume(target_act: UInt32): Integer; cdecl
  external libc name _PU + 'thread_resume';

function thread_abort(target_act: UInt32): Integer; cdecl
  external libc name _PU + 'thread_abort';

// lldb sets breakpoint here to catch unhandled exceptions
procedure __lldb_fcall_except_handler; export;
begin
end;

// Helper function which invokes lldb exception handler
// setting exception type description in global variable.
procedure lldb_invoke_except_handler(ExcType : String);
begin
    __lldbFCallExceptionType := PWideChar(ExcType);
    __lldb_fcall_except_handler;
end;


// Substitute RTL exception port with debugserver's one.
procedure __lldb_subst_eh_port;
var
  eh_data: TMachEHData;
begin
  if ExcThreadPort <> 0 then
  begin
    eh_data.Get(mach_task_self(), $E);
    if (eh_data.eh_portcnt = 1) and (eh_data.eh_ports[0] = RTLExceptionPort) then
    begin
      OrigEHData.Apply(mach_task_self());
      // Send abort notification to exception handler thread,
      // which will restore RTL exception port after debugger
      // continues execution
      thread_abort(ExcThreadPort);
    end;

  end;
end;

{
  Function call wrapper for lldb.
  arg2 always points to __lldb_arg structure
  arg1 is reserved and always equal to arg2
  arg0 depends on context: it may point to Self in case execution
    stops in class method, otherwise it is equal to arg2

  Function passes arguments to __dbkFCallArgs, which contains JIT compiled
  expression code (see comments in BorlandLanguageRuntime.cpp in lldb
  source tree).
}
procedure __lldb_fcall_wrapper(arg0, arg1, arg2 : Pointer); export;
type TLLDBExprProc = procedure(arg0, arg1, arg2 : Pointer);
begin
  try
    {
      Before evaluating expression, debugserver suspends all application threads,
      except one which executes the expression code. As exception handling is running
      in separate thread, user will exprerience debugger hang if mach exception is raised
      somewhere in expression code. To avoid this we resume the exception handler thread here.
    }
    __lldb_subst_eh_port;
    TLLDBExprProc(@__dbkFCallArgs)(arg0, arg1, arg2);
  except
    on E:TObject do lldb_invoke_except_handler(E.ClassName);
  end;
end;
{$ENDIF MACOS}

procedure __dbk_fcall_wrapper; export;
var p: Pointer;
begin
  p := nil;
  try
    __dbkFCallArgs( // reserved stack arg space
      p, p, p, p, p , p, p, p, p, p,
      p, p, p, p, p , p, p, p, p, p,
      p, p, p, p, p , p, p, p, p, p);
  except
    __dbkFCallArgs(
      p, p, p, p, p , p, p, p, p, p,
      p, p, p, p, p , p, p, p, p, p,
      p, p, p, p, p , p, p, p, p, p);
  end;
end;

exports
  dbkFCallWrapperAddr,
{$IF defined(EXTERNALLINKER)}
  dbk_RTL_initialized,
{$ENDIF}
{$IF defined(MACOS)}
  MachExceptionsSuspend,
  MachExceptionsResume,
  __lldbFCallExceptionType,
  __lldb_fcall_except_handler,
  __lldb_fcall_wrapper,
{$ENDIF MACOS}
  __dbk_fcall_wrapper;

{$IFDEF MSWINDOWS}
function _InitPkg(AHInst: HINST; Reason: Integer; Resvd: Pointer): Longbool; stdcall;
begin
  ModuleIsLib := True;
  UseTlsAPI := True;
  ModuleIsPackage := True;
  Module.Instance := AHInst;
  Module.CodeInstance := 0;
  Module.DataInstance := 0;
  HInstance := AHInst;
  if @TlsLast <> nil then
    TlsProc[Reason];
  if Reason = DLL_PROCESS_ATTACH then
    InitializeModule
  else if Reason = DLL_PROCESS_DETACH then
    UninitializeModule;
  _InitPkg := True;
end;
{$ENDIF}
{$IF defined(LINUX) or defined(MACOS) or defined(ANDROID)}

{$IFDEF LINUX64}
const
  librtlhelpername = {$IFDEF PIC} 'librtlhelper_PIC.a' {$ELSE} 'librtlhelper.a' {$ENDIF};
{$ENDIF LINUX64}
{$IFDEF OSX64}
const
  librtlhelpername = 'librtlhelper.a';
{$ENDIF OSX64}
{$IFDEF CPUARM32}
const
  librtlhelpername = 'librtlhelper.a';
{$ENDIF CPUARM32}

{$IF defined(LINUX64) or defined(OSX64)}
// Add a reference to _init_mod in librtlhelper.a
procedure _init_mod; cdecl;
  external librtlhelpername name '_Z9_init_modv';
{$ENDIF LINUX64 or OSX64}

function _InitPkg: LongBool;
begin
{$IFDEF DEBUG_STARTUP}
asm
INT 3
end;
{$ENDIF DEBUG_STARTUP}
{$IF defined(OSX64)}
  if TlsIndex <> LongWord(-1) then Exit(False);
{$ENDIF OSX64}
{$IFDEF PC_MAPPED_EXCEPTIONS}
  RegisterPCMap;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  TlsIndex := NEG_ONE;
  ModuleIsLib := True;
{$IFDEF MSWINDOWS}
  UseTlsAPI := True;
{$ENDIF}
  ModuleIsPackage := True;
  Module.Instance := GetThisModuleHandle;
  Module.InstanceVar := @HInstance;
  Module.CodeInstance := 0;
  Module.DataInstance := 0;
  Module.GOT := UIntPtr(@_Global_Offset_Table_);
  dbkFCallWrapperAddr := @__dbk_fcall_wrapper;
{$IFDEF PC_MAPPED_EXCEPTIONS}
  Module.CodeSegStart := GetTextStart;
  Module.CodeSegEnd := GetTextEnd;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  HInstance := Module.Instance;
  InitThreadTLS;
  InitializeModule;
  _InitPkg := True;
end;

{$ENDIF LINUX or MACOS or ANDROID}

{$IF defined(LINUX64) or defined(OSX64)}
procedure _fini_mod; cdecl;
  external librtlhelpername name '_Z9_fini_modv';

function _FiniPkg: LongBool;
begin
  UnInitializeModule;
  Result := True;
end;
{$ENDIF LINUX64 or OSX64}

procedure _PackageLoad(const Table: PackageInfo);
begin
  System._PackageLoad(Table, @Module);
end;

procedure _PackageUnload(const Table: PackageInfo);
begin
  System._PackageUnload(Table, @Module);
end;

{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
                                            
procedure _InitLib(Context: PInitContext; InitTable: PackageInfo; AHInst: HINST; Reason: LongWord; Reserved: Pointer);
begin
  if Reason = DLL_PROCESS_ATTACH then
  begin
    ModuleIsLib := True;
    UseTlsAPI := True;
    HInstance := AHInst;
    Module.Instance := AHInst;
    Module.CodeInstance := 0;
    Module.DataInstance := 0;
    Module.TypeInfo := @InitTable.TypeInfo;
    InitializeModule;
  end;
  _StartLib(Context, InitTable, @Module, @TlsProc, DllProcEx, AHInst, Reason, Reserved);
end;
{$ELSE CPU386}
procedure _InitLib;
asm
        { ->    EAX Inittable   }
        {       [EBP+8] Hinst   }
        {       [EBP+12] Reason }
        {       [EBP+16] Resvd  }

        MOV     EDX,offset Module
        CMP     dword ptr [EBP+12],DLL_PROCESS_ATTACH
        JNE     @@notInit

        PUSH    EAX
        PUSH    EDX
        MOV     ModuleIsLib,1
        MOV     UseTlsAPI,1
        MOV     ECX,[EBP+8]
        MOV     HInstance,ECX
        MOV     [EDX].TLibModule.Instance,ECX
        MOV     [EDX].TLibModule.CodeInstance,0
        MOV     [EDX].TLibModule.DataInstance,0
        LEA     EAX,[EAX].PackageInfoTable.TypeInfo
        MOV     [EDX].TLibModule.TypeInfo,EAX
        CALL    InitializeModule
        POP     EDX
        POP     EAX

@@notInit:
        PUSH    DllProc
        MOV     ECX,offset TlsProc
        CALL    _StartLib
end;
{$ENDIF CPU386}

// ExitLib is the same as InitLib in Windows.

{$ENDIF MSWINDOWS}

{$IF defined(LINUX64) or defined(OSX64)}
function __atexit(ptr: Pointer): Integer; cdecl;
  external libc name _PU + 'atexit';
//{$EXTERNALSYM __atexit}
var
  __InitContext: TInitContext;
{$ENDIF LINUX64 or OSX64}

                                                 
{$IF defined(LINUX) or defined(MACOS) or defined(ANDROID)}
{$IF defined(EXTERNALLINKER)}
procedure _InitLib(Context: PInitContext; InitTable: Pointer);
{$ELSE !EXTERNALLINKER}
procedure _InitLib(Context: PInitContext);
{$ENDIF !EXTERNALLINKER}
begin
{$IFDEF DEBUG_STARTUP}
asm
        INT 3
end;
{$ENDIF DEBUG_STARTUP}
{$IF Defined(CPUX86) and (not defined(EXTERNALLINKER))}
  asm
        PUSH    UNWINDFI_TOPOFSTACK
{$IFDEF ALIGN_STACK}
        SUB   ESP, 12
{$ENDIF ALIGN_STACK}
  end;
{$ENDIF CPUX86 and (not EXTERNALLINKER)}
{$IF defined(OSX64) and defined(CPUX64)}
  if TlsIndex <> LongWord(-1) then Exit;
{$ENDIF OSX64}
{$IF defined(OSX64) and defined(CPUARM64)}
  if ModuleIsPackage then
    ModuleIsPackage := False
  else if TlsIndex <> LongWord(-1) then Exit;
{$ENDIF OSX64}
{$IF defined(EXTERNALLINKER)}
  dbk_RTL_initialized := 0;
{$ENDIF}
{$IF defined(LINUX64) or defined(OSX64)}
  __InitContext := PInitContext(Context)^;
  Context := @__InitContext;
{$ENDIF LINUX64 or OSX64}
{$IF defined(EXTERNALLINKER)}
  Context.InitTable := InitTable;
{$ENDIF}
  Context.DLLInitState := DLL_PROCESS_ATTACH;
{$IF defined(LINUX64) or defined(OSX64)}
                              
// TlsIndex was initialized to -1 already.
  ModuleIsLib := True;
  if not ModuleIsPackage then
    LibModuleList := nil;
  ModuleIsPackage := False;
{$ELSE !(LINUX64 or OSX64)}
  TlsIndex := NEG_ONE;
  ModuleIsLib := True;
{$ENDIF LINUX64 or OSX64}
{$IFDEF MSWINDOWS}
  UseTlsAPI := True;
{$ENDIF}
  HInstance := GetThisModuleHandle;
  Module.Instance := HInstance;
  Module.InstanceVar := @HInstance;
  Module.CodeInstance := 0;
  Module.DataInstance := 0;
{$IF defined(EXTERNALLINKER) and (defined(LINUX64) or defined(OSX64))}
  Module.TypeInfo := @PackageInfo(InitTable).TypeInfo;
{$ENDIF EXTERNALLINKER and (defined(LINUX64) or defined(OSX64))}
  Module.GOT := UIntPtr(@_Global_Offset_Table_);
{$IF defined(WIN64) or defined(MACOS) or defined(ANDROID) or defined(LINUX)}
  dbkFCallWrapperAddr := @__dbk_fcall_wrapper;
{$ENDIF WIN64 or MACOS or ANDROID or LINUX}
{$IFDEF PC_MAPPED_EXCEPTIONS}
  Module.CodeSegStart := GetTextStart;
  Module.CodeSegEnd := GetTextEnd;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  InitializeModule;
  InitThreadTLS;
{$IFDEF PC_MAPPED_EXCEPTIONS}
  RegisterPCMap;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  _StartLib(Context, @Module, DLLProcEx);
{$IF Defined(CPUX86) and (not defined(EXTERNALLINKER))}
  asm
{$IFDEF ALIGN_STACK}
        ADD   ESP, 16
{$ELSE !ALIGN_STACK}
        ADD   ESP, 4
{$ENDIF !ALIGN_STACK}
  end;
{$ENDIF CPUX86 and (not EXTERNALLINKER)}
end;

// InnerExitLib provides GOT fixup and global var addressing
function InnerExitLib(Context: PInitContext): Integer;
begin
  Result := 0;
  if ModuleIsPackage then
  begin
    UninitializeModule;
{$IFDEF PC_MAPPED_EXCEPTIONS}
    UnregisterPCMap;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  end
  else
    _StartLib(Context, @Module, DLLProcEx);
end;

                                                
                                                 
{$IF defined(X86ASM)}
function _ExitLib: Integer; cdecl;
asm
{$IFDEF DEBUG_STARTUP}
        INT 3
{$ENDIF DEBUG_STARTUP}
        PUSH    EBP
        MOV     EBP,ESP
        PUSH    UNWINDFI_TOPOFSTACK
        XOR     EAX,EAX
        PUSH    DLL_PROCESS_DETACH    // InitContext.DLLInitState
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        PUSH    EBP
        PUSH    EAX                   // InitContext.Module
        PUSH    EAX                   // InitContext.InitCount
        PUSH    EAX                   // InitContext.InitTable (filled in later)
        PUSH    EAX                   // InitContext.OuterContext
        MOV     EAX,ESP
        CALL    InnerExitLib;
        ADD     ESP, 16
        POP     EBP
        POP     EBX
        POP     ESI
        POP     EDI
        MOV     ESP,EBP
        POP     EBP
end;
{$ELSE !X86ASM}
{$IF defined(EXTERNALLINKER)}
procedure _ExitLib; cdecl;
{$IF defined(LINUX64) or defined(OSX64)}
var
  Context: TInitContext;
{$ENDIF LINUX64 or OSX64}
begin
{$IF defined(LINUX64) or defined(OSX64)}
  FillChar(Context, SizeOf(Context), 0);
  Context.DLLInitState := DLL_PROCESS_DETACH;
  InnerExitLib(@Context);
{$ENDIF LINUX64 or OSX64}
end;
{$ELSE}
function _ExitLib: Integer; cdecl;
begin
  Result := 0;
end;
{$ENDIF}
{$ENDIF !X86ASM}

{$ENDIF LINUX or MACOS or ANDROID}

                                                      
{$IFDEF POSIX}
procedure _GetCallerEIP;
{$IF Defined(CPUX86) and (not defined(EXTERNALLINKER))}
asm
        MOV     EBX, [ESP]
end;
{$ELSE}
begin
end;
{$ENDIF}
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
procedure _InitExe(InitTable: Pointer);
begin
  TlsIndex := 0;
  HInstance := GetModuleHandle(nil);
  UseTlsAPI := IsWine;
  Module.Instance := HInstance;
  Module.CodeInstance := 0;
  Module.DataInstance := 0;
  Module.TypeInfo := @PackageInfo(InitTable)^.TypeInfo;
{$IFDEF WIN64}
  dbkFCallWrapperAddr := @__dbk_fcall_wrapper;
{$ENDIF WIN64}
  InitializeModule;
  _StartExe(InitTable, @Module);
end;
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
procedure InitVmtImports;
var
  P: PNativeInt;
begin
  P := @TypeImportsTable;
  if P = nil then Exit;
  while P^ <> 0 do
  begin
    P^ := NativeInt(dlsym(0, _PAnsiChr(P^)));
    Inc(P);
  end;
end;
{$ENDIF LINUX}

//{$DEFINE DEBUG_STARTUP}
{$IF defined(LINUX) or defined(MACOS) or defined(ANDROID)}
{$IFDEF LINUX}
procedure _InitExe(InitTable: Pointer; Argc: Integer; Argp: Pointer); export;
{$ENDIF LINUX}
{$IF defined(MACOS) and defined(EXTERNALLINKER)}
procedure _InitExe(InitTable: Pointer; Argc: Integer; Argp, Envp, Prog: Pointer);
{$ENDIF MACOS and EXTERNALLINKER}
{$IF defined(MACOS) and (not defined(EXTERNALLINKER))}
procedure _InitExe(InitTable: Pointer; ProgramParams: Pointer); export;
var
  Argc: Integer;
  Argp: Pointer;
{$ENDIF MACOS and !EXTERNALLINKER}
{$IF defined(ANDROID)}
procedure _InitExe(InitTable: Pointer);
var
  Argc: Integer;
  Argp: Pointer;
{$ENDIF ANDROID}
begin
{$IFDEF DEBUG_STARTUP}
  asm
    INT 3
  end;
{$ENDIF DEBUG_STARTUP}
{$IF defined(EXTERNALLINKER)}
  dbk_RTL_initialized := 0;
{$ENDIF}
  HInstance := GetThisModuleHandle;
  Module.Instance := HInstance;
  Module.InstanceVar := @HInstance;
  Module.CodeInstance := 0;
  Module.TypeInfo := @PackageInfo(InitTable)^.TypeInfo;
  Module.DataInstance := 0;
{$IF defined(MACOS) or defined(ANDROID) or defined(LINUX)}
  dbkFCallWrapperAddr := @__dbk_fcall_wrapper;
{$ENDIF MACOS or ANDROID or LINUX}
  InitializeModule;
  InitThreadTLS;
{$IFDEF PC_MAPPED_EXCEPTIONS}
  RegisterPCMap();
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF LINUX}
  InitVmtImports;
{$ENDIF LINUX}
{$IF defined(MACOS) and (not defined(EXTERNALLINKER))}
  {
    The Mac loader sets up argc/argv, et al a little differently.  The stack
    looks like this when the loader hands control over to the image:

    NULL
    ptr -> fully qualified program name
    NULL
    [0..n] pointers -> environment
    NULL
    [0..n] pointers -> arguments
    argc

    The prologue code generated by the compiler for the main program unit
    is special.  It loads EDX with a pointer to the argc location above,
    then aligns the stack to meet the MACH ABI.  We have to parse that pointer
    a little to get the parameters for _StartExe.
  }
  Argc := PInteger(ProgramParams)^;
  Argp := Pointer(PByte(ProgramParams) + Sizeof(Pointer));
{$ENDIF MACOS and !EXTERNALLINKER}
{$IFDEF ANDROID}
  Argc := 0;
  Argp := nil;
{$ENDIF ANDROID}
  _StartExe(InitTable, @Module, Argc, Argp);
end;

{$ENDIF LINUX || MACOS || ANDROID}

{$IF defined(ANDROID) or defined(LINUX)}
procedure InitExeCPP;
begin
{$IF defined(EXTERNALLINKER)}
  dbk_RTL_initialized := 0;
{$ENDIF}
  HInstance := GetThisModuleHandle;
  Module.Instance := HInstance;
  Module.InstanceVar := @HInstance;
  Module.CodeInstance := 0;
  Module.DataInstance := 0;
  dbkFCallWrapperAddr := @__dbk_fcall_wrapper;
  InitializeModule;
{$IFDEF LINUX}
  InitVmtImports;
{$ENDIF LINUX}
  MainInstance := Module.Instance;
  IsLibrary := False;
end;
{$ENDIF defined(ANDROID) or defined(LINUX)}

{$IF Defined(LINUX32) and Defined(CPUX86)}
var
  InitAddr: Pointer;

function _main(argc: Integer; argv: Pointer; envp: Pointer): Integer; export cdecl;
type
  TInitFunction = function (argc: Integer; argv, envp: Pointer): Integer; cdecl;
  TExternalInit = function (argc: Integer; argv, envp: Pointer; InitExe: TInitFunction): Integer; cdecl;
var
  ExternalInit: TExternalInit;
  InitFunc: TInitFunction;
begin
  @ExternalInit := dlsym(GetThisModuleHandle, 'ExternalInit');
  @InitFunc := InitAddr;
  System.envp := envp;
  if @ExternalInit <> nil then
    ExternalInit(argc, argv, envp, InitFunc);
  Result := InitFunc(argc, argv, envp);
end;

function __libc_start_main (Main: Pointer; Argc: Integer; Argv: Pointer;
          Init, Fini, rtld_Fini: Pointer; StackEnd: Pointer)
        : Integer;
        cdecl;
        external libc name '__libc_start_main';

{ Program entry point }
procedure _start;
asm
{$IFDEF DEBUG_STARTUP}
        INT 3
{$ENDIF}
        { Mark outermost frame, suggested by ELF i386 ABI.  }
        xor ebp,ebp

        { Get data passed on stack }
        pop eax   { argc }
        mov ecx,esp   { argv }

        { Align stack }
        and esp,0fffffff8h
{$IFDEF PC_MAPPED_EXCEPTIONS}
        { Mark the top of the stack with a signature }
        push  UNWINDFI_TOPOFSTACK
{$ENDIF PC_MAPPED_EXCEPTIONS}
        push  ebp   { padding }
        push  esp   { crt1.o does this, don't know why }
        push  edx   { function to be registered with
                      atexit(), passed by loader }
        push  offset @@ret  { _fini dummy }
        push  offset @@ret  { _init dummy }
        push  ecx   { argv }
        push  eax   { argc }
  { We need a symbol for the Pascal entry point (main unit's
    body).  An external symbol `main' fixed up by the linker
    would be fine.  Alas, external declarations can't do that;
    they must be resolved either in the same file with a $L
    directive, or in a shared object.  Hack: use a bogus,
    distinctive symbol to mark the fixup, find and patch it
    in the linker.  }
{$IFDEF PIC}
        call    GetGOT
        mov     ebx, eax
        add     [esp+12],ebx
        add     [esp+8],ebx
        // Linker will replace _GLOBAL_OFFSET_TABLE_ address with main program block
        mov     eax, offset _GLOBAL_OFFSET_TABLE_
        add     eax, ebx
        mov     [ebx].InitAddr, eax
        mov     eax, offset _main
        add     eax, ebx
        push    eax
{$ELSE !PIC}
        // Linker will replace _GLOBAL_OFFSET_TABLE_ address with main program block
        push  offset _GLOBAL_OFFSET_TABLE_
        pop InitAddr
        push  offset _main
{$ENDIF !PIC}
        call  __libc_start_main
        hlt     { they never come back }

@@ret:
end;
{$ENDIF LINUX32}

{$IFDEF IOS}
{$IFDEF CPUARM32}
// Add a reference to librtlhelper.a library to override libSystem's _Unwind_SjLj_Register
procedure ___Unwind_SjLj_Register_(context: PUnwind_Context); cdecl;
  external librtlhelpername name '__Unwind_SjLj_Register';
{$ENDIF CPUARM32}
{$ENDIF IOS}

{$IFDEF LINUX}
// Add a reference to libgcc_s
const
  libcppabi = 'libgcc_s.so.1';
procedure _Unwind_Resume(exception_object: Pointer); cdecl;
  external libcppabi name _PU + '_Unwind_Resume';
{$ENDIF LINUX}

{$IFDEF MSWINDOWS}
initialization
finalization
  ShutdownDelayHelp2;
{$ENDIF}
end.
