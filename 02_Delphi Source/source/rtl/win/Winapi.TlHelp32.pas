{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{          File: tlhelp32.h                             }
{          Copyright (c) Microsoft Corporation          }
{          All Rights Reserved.                         }
{                                                       }
{       Translator: Embarcadero Technologies, Inc.      }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{*******************************************************}
{       Tool Help Functions, Types, and Definitions     }
{*******************************************************}

unit Winapi.TlHelp32;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses Winapi.Windows;

{$HPPEMIT '#include <tlhelp32.h>'}

const
{$EXTERNALSYM MAX_MODULE_NAME32}
  MAX_MODULE_NAME32 = 255;

(****** Shapshot function **********************************************)

{$EXTERNALSYM CreateToolhelp32Snapshot}
function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): THandle;

type
  TCreateToolhelp32Snapshot = function (dwFlags, th32ProcessID: DWORD): THandle stdcall;
//
// The th32ProcessID argument is only used if TH32CS_SNAPHEAPLIST or
// TH32CS_SNAPMODULE is specified. th32ProcessID == 0 means the current
// process.
//
// NOTE that all of the snapshots are global except for the heap and module
//  lists which are process specific. To enumerate the heap or module
//  state for all WIN32 processes call with TH32CS_SNAPALL and the
//  current process. Then for each process in the TH32CS_SNAPPROCESS
//  list that isn't the current process, do a call with just
//  TH32CS_SNAPHEAPLIST and/or TH32CS_SNAPMODULE.
//
// dwFlags
//
const
{$EXTERNALSYM TH32CS_SNAPHEAPLIST}
  TH32CS_SNAPHEAPLIST = $00000001;
{$EXTERNALSYM TH32CS_SNAPPROCESS}
  TH32CS_SNAPPROCESS  = $00000002;
{$EXTERNALSYM TH32CS_SNAPTHREAD}
  TH32CS_SNAPTHREAD   = $00000004;
{$EXTERNALSYM TH32CS_SNAPMODULE}
  TH32CS_SNAPMODULE   = $00000008;
{$EXTERNALSYM TH32CS_SNAPALL}
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or
    TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;
{$EXTERNALSYM TH32CS_INHERIT}
  TH32CS_INHERIT      = $80000000;
//
// Use CloseHandle to destroy the snapshot
//

(****** heap walking ***************************************************)

type
{$EXTERNALSYM tagHEAPLIST32}
  tagHEAPLIST32 = record
    dwSize: SIZE_T;
    th32ProcessID: DWORD;  // owning process
    th32HeapID: ULONG_PTR;   // heap (in owning process's context!)
    dwFlags: DWORD;
  end;
{$EXTERNALSYM HEAPLIST32}
  HEAPLIST32 = tagHEAPLIST32;
{$EXTERNALSYM PHEAPLIST32}
  PHEAPLIST32 = ^tagHEAPLIST32;
{$EXTERNALSYM LPHEAPLIST32}
  LPHEAPLIST32 = ^tagHEAPLIST32;
  THeapList32 = tagHEAPLIST32;
//
// dwFlags
//
const
{$EXTERNALSYM HF32_DEFAULT}
  HF32_DEFAULT = 1;  // process's default heap
{$EXTERNALSYM HF32_SHARED}
  HF32_SHARED  = 2;  // is shared heap

{$EXTERNALSYM Heap32ListFirst}
function Heap32ListFirst(hSnapshot: THandle; var lphl: THeapList32): BOOL;
{$EXTERNALSYM Heap32ListNext}
function Heap32ListNext(hSnapshot: THandle; var lphl: THeapList32): BOOL;

type
  THeap32ListFirst = function (hSnapshot: THandle; var lphl: THeapList32): BOOL stdcall;
  THeap32ListNext = function (hSnapshot: THandle; var lphl: THeapList32): BOOL stdcall;

type
{$EXTERNALSYM tagHEAPENTRY32}
  tagHEAPENTRY32 = record
    dwSize: SIZE_T;
    hHandle: THandle;     // Handle of this heap block
    dwAddress: ULONG_PTR; // Linear address of start of block
    dwBlockSize: SIZE_T;  // Size of block in bytes
    dwFlags: DWORD;
    dwLockCount: DWORD;
    dwResvd: DWORD;
    th32ProcessID: DWORD; // owning process
    th32HeapID: ULONG_PTR;// heap block is in
  end;
{$EXTERNALSYM HEAPENTRY32}
  HEAPENTRY32 = tagHEAPENTRY32;
{$EXTERNALSYM PHEAPENTRY32}
  PHEAPENTRY32 = ^tagHEAPENTRY32;
{$EXTERNALSYM LPHEAPENTRY32}
  LPHEAPENTRY32 = ^tagHEAPENTRY32;
  THeapEntry32 = tagHEAPENTRY32;
//
// dwFlags
//
const
{$EXTERNALSYM LF32_FIXED}
  LF32_FIXED    = $00000001;
{$EXTERNALSYM LF32_FREE}
  LF32_FREE     = $00000002;
{$EXTERNALSYM LF32_MOVEABLE}
  LF32_MOVEABLE = $00000004;

{$EXTERNALSYM Heap32First}
function Heap32First(var lphe: THeapEntry32; th32ProcessID: Cardinal; th32HeapID: ULONG_PTR): BOOL;
{$EXTERNALSYM Heap32Next}
function Heap32Next(var lphe: THeapEntry32): BOOL;
{$EXTERNALSYM Toolhelp32ReadProcessMemory}
function Toolhelp32ReadProcessMemory(th32ProcessID: DWORD; lpBaseAddress: Pointer;
  var lpBuffer; cbRead: SIZE_T; var lpNumberOfBytesRead: SIZE_T): BOOL;

type
  THeap32First = function (var lphe: THeapEntry32; th32ProcessID: Cardinal;
    th32HeapID: ULONG_PTR): BOOL stdcall;
  THeap32Next = function (var lphe: THeapEntry32): BOOL stdcall;
  TToolhelp32ReadProcessMemory = function (th32ProcessID: DWORD;
    lpBaseAddress: Pointer; var lpBuffer; cbRead: SIZE_T;
    var lpNumberOfBytesRead: SIZE_T): BOOL stdcall;

(***** Process walking *************************************************)

type
  tagPROCESSENTRY32A = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ProcessID: DWORD;       // this process
    th32DefaultHeapID: ULONG_PTR;
    th32ModuleID: DWORD;        // associated exe
    cntThreads: DWORD;
    th32ParentProcessID: DWORD; // this process's parent process
    pcPriClassBase: Longint;    // Base priority of process's threads
    dwFlags: DWORD;
    szExeFile: array[0..MAX_PATH - 1] of AnsiChar;// Path
  end;
  PROCESSENTRY32A = tagPROCESSENTRY32A;
  PPROCESSENTRY32A = ^tagPROCESSENTRY32A;
  LPPROCESSENTRY32A = ^tagPROCESSENTRY32A;
  TProcessEntry32A = tagPROCESSENTRY32A;

function Process32FirstA(hSnapshot: THandle; var lppe: TProcessEntry32A): BOOL;
function Process32NextA(hSnapshot: THandle; var lppe: TProcessEntry32A): BOOL;

type
  TProcess32FirstA = function (hSnapshot: THandle; var lppe: TProcessEntry32A): BOOL stdcall;
  TProcess32NextA = function (hSnapshot: THandle; var lppe: TProcessEntry32A): BOOL stdcall;

{$EXTERNALSYM tagPROCESSENTRY32W}
  tagPROCESSENTRY32W = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ProcessID: DWORD;       // this process
    th32DefaultHeapID: ULONG_PTR;
    th32ModuleID: DWORD;        // associated exe
    cntThreads: DWORD;
    th32ParentProcessID: DWORD; // this process's parent process
    pcPriClassBase: Longint;    // Base priority of process's threads
    dwFlags: DWORD;
    szExeFile: array[0..MAX_PATH - 1] of WChar;// Path
  end;
{$EXTERNALSYM PROCESSENTRY32W}
  PROCESSENTRY32W = tagPROCESSENTRY32W;
{$EXTERNALSYM PPROCESSENTRY32W}
  PPROCESSENTRY32W = ^tagPROCESSENTRY32W;
{$EXTERNALSYM LPPROCESSENTRY32W}
  LPPROCESSENTRY32W = ^tagPROCESSENTRY32W;
  TProcessEntry32W = tagPROCESSENTRY32W;

{$EXTERNALSYM Process32FirstW}
function Process32FirstW(hSnapshot: THandle; var lppe: TProcessEntry32W): BOOL;
{$EXTERNALSYM Process32NextW}
function Process32NextW(hSnapshot: THandle; var lppe: TProcessEntry32W): BOOL;

type
  TProcess32FirstW = function (hSnapshot: THandle; var lppe: TProcessEntry32W): BOOL stdcall;
  TProcess32NextW = function (hSnapshot: THandle; var lppe: TProcessEntry32W): BOOL stdcall;

{$IFDEF UNICODE}
  tagPROCESSENTRY32 = tagPROCESSENTRY32W;
  PROCESSENTRY32 = tagPROCESSENTRY32W;
  PPROCESSENTRY32 = ^tagPROCESSENTRY32W;
  LPPROCESSENTRY32 = ^tagPROCESSENTRY32W;
  TProcessEntry32 = tagPROCESSENTRY32W;
  TProcess32First = TProcess32FirstW;
  TProcess32Next = TProcess32NextW;
{$ELSE}
  tagPROCESSENTRY32 = tagPROCESSENTRY32A;
  PROCESSENTRY32 = tagPROCESSENTRY32A;
  PPROCESSENTRY32 = ^tagPROCESSENTRY32A;
  LPPROCESSENTRY32 = ^tagPROCESSENTRY32A;
  TProcessEntry32 = tagPROCESSENTRY32A;
  TProcess32First = TProcess32FirstA;
  TProcess32Next = TProcess32NextA;
{$ENDIF}

{$EXTERNALSYM tagPROCESSENTRY32}
{$EXTERNALSYM PPROCESSENTRY32}
{$EXTERNALSYM LPPROCESSENTRY32}

function Process32First(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL;
{$EXTERNALSYM Process32First}
function Process32Next(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL;
{$EXTERNALSYM Process32Next}

(***** Thread walking **************************************************)

type
{$EXTERNALSYM tagTHREADENTRY32}
  tagTHREADENTRY32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ThreadID: DWORD;       // this thread
    th32OwnerProcessID: DWORD; // Process this thread is associated with
    tpBasePri: Longint;
    tpDeltaPri: Longint;
    dwFlags: DWORD;
  end;
{$EXTERNALSYM THREADENTRY32}
  THREADENTRY32 = tagTHREADENTRY32;
{$EXTERNALSYM PTHREADENTRY32}
  PTHREADENTRY32 = ^tagTHREADENTRY32;
{$EXTERNALSYM LPTHREADENTRY32}
  LPTHREADENTRY32 = ^tagTHREADENTRY32;
  TThreadEntry32 = tagTHREADENTRY32;

{$EXTERNALSYM Thread32First}
function Thread32First(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL; stdcall;
{$EXTERNALSYM Thread32Next}
function Thread32Next(hSnapshot: THandle; var lpte: TThreadENtry32): BOOL; stdcall;

type
  TThread32First = function (hSnapshot: THandle; var lpte: TThreadEntry32): BOOL stdcall;
  TThread32Next = function (hSnapshot: THandle; var lpte: TThreadENtry32): BOOL stdcall;

(***** Module walking *************************************************)

type
  tagMODULEENTRY32A = record
    dwSize: DWORD;
    th32ModuleID: DWORD;  // This module
    th32ProcessID: DWORD; // owning process
    GlblcntUsage: DWORD;  // Global usage count on the module
    ProccntUsage: DWORD;  // Module usage count in th32ProcessID's context
    modBaseAddr: PBYTE;   // Base address of module in th32ProcessID's context
    modBaseSize: DWORD;   // Size in bytes of module starting at modBaseAddr
    hModule: HMODULE;     // The hModule of this module in th32ProcessID's context
    szModule: array[0..MAX_MODULE_NAME32] of AnsiChar;
    szExePath: array[0..MAX_PATH - 1] of AnsiChar;
  end;
  MODULEENTRY32A = tagMODULEENTRY32A;
  PMODULEENTRY32A = ^tagMODULEENTRY32A;
  LPMODULEENTRY32A = ^tagMODULEENTRY32A;
  TModuleEntry32A = tagMODULEENTRY32A;

//
// NOTE CAREFULLY that the modBaseAddr and hModule fields are valid ONLY
// in th32ProcessID's process context.
//

function Module32FirstA(hSnapshot: THandle; var lpme: TModuleEntry32A): BOOL;
function Module32NextA(hSnapshot: THandle; var lpme: TModuleEntry32A): BOOL;

type
  TModule32FirstA = function (hSnapshot: THandle; var lpme: TModuleEntry32A): BOOL stdcall;
  TModule32NextA = function (hSnapshot: THandle; var lpme: TModuleEntry32A): BOOL stdcall;

{$EXTERNALSYM tagMODULEENTRY32W}
  tagMODULEENTRY32W = record
    dwSize: DWORD;
    th32ModuleID: DWORD;  // This module
    th32ProcessID: DWORD; // owning process
    GlblcntUsage: DWORD;  // Global usage count on the module
    ProccntUsage: DWORD;  // Module usage count in th32ProcessID's context
    modBaseAddr: PBYTE;   // Base address of module in th32ProcessID's context
    modBaseSize: DWORD;   // Size in bytes of module starting at modBaseAddr
    hModule: HMODULE;     // The hModule of this module in th32ProcessID's context
    szModule: array[0..MAX_MODULE_NAME32] of WChar;
    szExePath: array[0..MAX_PATH - 1] of WChar;
  end;
{$EXTERNALSYM MODULEENTRY32}
  MODULEENTRY32W = tagMODULEENTRY32W;
{$EXTERNALSYM PMODULEENTRY32}
  PMODULEENTRY32W = ^tagMODULEENTRY32W;
{$EXTERNALSYM LPMODULEENTRY32}
  LPMODULEENTRY32W = ^tagMODULEENTRY32W;
  TModuleEntry32W = tagMODULEENTRY32W;

//
// NOTE CAREFULLY that the modBaseAddr and hModule fields are valid ONLY
// in th32ProcessID's process context.
//

{$EXTERNALSYM Module32FirstW}
function Module32FirstW(hSnapshot: THandle; var lpme: TModuleEntry32W): BOOL;
{$EXTERNALSYM Module32NextW}
function Module32NextW(hSnapshot: THandle; var lpme: TModuleEntry32W): BOOL;

type
  TModule32FirstW = function (hSnapshot: THandle; var lpme: TModuleEntry32W): BOOL stdcall;
  TModule32NextW = function (hSnapshot: THandle; var lpme: TModuleEntry32W): BOOL stdcall;

{$IFDEF UNICODE}
  tagMODULEENTRY32 = tagMODULEENTRY32W;
  MODULEENTRY32 = tagMODULEENTRY32W;
  PMODULEENTRY32 = ^tagMODULEENTRY32W;
  LPMODULEENTRY32 = ^tagMODULEENTRY32W;
  TModuleEntry32 = tagMODULEENTRY32W;
  TModule32First = TModule32FirstW;
  TModule32Next = TModule32NextW;
{$ELSE}
  tagMODULEENTRY32 = tagMODULEENTRY32A;
  MODULEENTRY32 = tagMODULEENTRY32A;
  PMODULEENTRY32 = ^tagMODULEENTRY32A;
  LPMODULEENTRY32 = ^tagMODULEENTRY32A;
  TModuleEntry32 = tagMODULEENTRY32A;
  TModule32First = TModule32FirstA;
  TModule32Next = TModule32NextA;
{$ENDIF}

{$EXTERNALSYM tagMODULEENTRY32}
{$EXTERNALSYM MODULEENTRY32}
{$EXTERNALSYM PMODULEENTRY32}
{$EXTERNALSYM LPMODULEENTRY32}

function Module32First(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;
{$EXTERNALSYM Module32First}
function Module32Next(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;
{$EXTERNALSYM Module32Next}

implementation

const
  kernel32 = 'kernel32.dll';

var
  KernelHandle: THandle;
  _CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
  _Heap32ListFirst: THeap32ListFirst;
  _Heap32ListNext: THeap32ListNext;
  _Heap32First: THeap32First;
  _Heap32Next: THeap32Next;
  _Toolhelp32ReadProcessMemory: TToolhelp32ReadProcessMemory;
  _Process32First: TProcess32First;
  _Process32Next: TProcess32Next;
  _Process32FirstA: TProcess32FirstA;
  _Process32NextA: TProcess32NextA;
  _Process32FirstW: TProcess32FirstW;
  _Process32NextW: TProcess32NextW;
  _Thread32First: TThread32First;
  _Thread32Next: TThread32Next;
  _Module32First: TModule32First;
  _Module32Next: TModule32Next;
  _Module32FirstA: TModule32FirstA;
  _Module32NextA: TModule32NextA;
  _Module32FirstW: TModule32FirstW;
  _Module32NextW: TModule32NextW;

function InitToolHelp: Boolean;
begin
  if KernelHandle = 0 then
  begin
    KernelHandle := GetModuleHandle(kernel32);
    if KernelHandle <> 0 then
    begin
      @_CreateToolhelp32Snapshot := GetProcAddress(KernelHandle, 'CreateToolhelp32Snapshot');
      @_Heap32ListFirst := GetProcAddress(KernelHandle, 'Heap32ListFirst');
      @_Heap32ListNext := GetProcAddress(KernelHandle, 'Heap32ListNext');
      @_Heap32First := GetProcAddress(KernelHandle, 'Heap32First');
      @_Heap32Next := GetProcAddress(KernelHandle, 'Heap32Next');
      @_Toolhelp32ReadProcessMemory := GetProcAddress(KernelHandle, 'Toolhelp32ReadProcessMemory');
      @_Process32FirstA := GetProcAddress(KernelHandle, 'Process32First');
      @_Process32NextA := GetProcAddress(KernelHandle, 'Process32Next');
      @_Process32FirstW := GetProcAddress(KernelHandle, 'Process32FirstW');
      @_Process32NextW := GetProcAddress(KernelHandle, 'Process32NextW');
{$IFDEF UNICODE}
      @_Process32First := GetProcAddress(KernelHandle, 'Process32FirstW');
      @_Process32Next := GetProcAddress(KernelHandle, 'Process32NextW');
{$ELSE}
      @_Process32First := GetProcAddress(KernelHandle, 'Process32First');
      @_Process32Next := GetProcAddress(KernelHandle, 'Process32Next');
{$ENDIF}
      @_Thread32First := GetProcAddress(KernelHandle, 'Thread32First');
      @_Thread32Next := GetProcAddress(KernelHandle, 'Thread32Next');
      @_Module32FirstA := GetProcAddress(KernelHandle, 'Module32First');
      @_Module32NextA := GetProcAddress(KernelHandle, 'Module32Next');
      @_Module32FirstW := GetProcAddress(KernelHandle, 'Module32FirstW');
      @_Module32NextW := GetProcAddress(KernelHandle, 'Module32NextW');
{$IFDEF UNICODE}
      @_Module32First := GetProcAddress(KernelHandle, 'Module32FirstW');
      @_Module32Next := GetProcAddress(KernelHandle, 'Module32NextW');
{$ELSE}
      @_Module32First := GetProcAddress(KernelHandle, 'Module32First');
      @_Module32Next := GetProcAddress(KernelHandle, 'Module32Next');
{$ENDIF}
    end;
  end;
  Result := (KernelHandle <> 0) and Assigned(_CreateToolhelp32Snapshot);
end;

function CreateToolhelp32Snapshot;
begin
  if InitToolHelp then
    Result := _CreateToolhelp32Snapshot(dwFlags, th32ProcessID)
  else Result := INVALID_HANDLE_VALUE;
end;

function Heap32ListFirst;
begin
  if InitToolHelp then
    Result := _Heap32ListFirst(hSnapshot, lphl)
  else Result := False;
end;

function Heap32ListNext;
begin
  if InitToolHelp then
    Result := _Heap32ListNext(hSnapshot, lphl)
  else Result := False;
end;

function Heap32First;
begin
  if InitToolHelp then
    Result := _Heap32First(lphe, th32ProcessID, th32HeapID)
  else Result := False;
end;

function Heap32Next;
begin
  if InitToolHelp then
    Result := _Heap32Next(lphe)
  else Result := False;
end;

function Toolhelp32ReadProcessMemory;
begin
  if InitToolHelp then
    Result := _Toolhelp32ReadProcessMemory(th32ProcessID, lpBaseAddress,
      lpBuffer, cbRead, lpNumberOfBytesRead)
  else Result := False;
end;

function Process32First;
begin
  if InitToolHelp then
    Result := _Process32First(hSnapshot, lppe)
  else Result := False;
end;

function Process32Next;
begin
  if InitToolHelp then
    Result := _Process32Next(hSnapshot, lppe)
  else Result := False;
end;

function Process32FirstA;
begin
  if InitToolHelp then
    Result := _Process32FirstA(hSnapshot, lppe)
  else Result := False;
end;

function Process32NextA;
begin
  if InitToolHelp then
    Result := _Process32NextA(hSnapshot, lppe)
  else Result := False;
end;

function Process32FirstW;
begin
  if InitToolHelp then
    Result := _Process32FirstW(hSnapshot, lppe)
  else Result := False;
end;

function Process32NextW;
begin
  if InitToolHelp then
    Result := _Process32NextW(hSnapshot, lppe)
  else Result := False;
end;

function Thread32First;
begin
  if InitToolHelp then
    Result := _Thread32First(hSnapshot, lpte)
  else Result := False;
end;

function Thread32Next;
begin
  if InitToolHelp then
    Result := _Thread32Next(hSnapshot, lpte)
  else Result := False;
end;

function Module32First;
begin
  if InitToolHelp then
    Result := _Module32First(hSnapshot, lpme)
  else Result := False;
end;

function Module32Next;
begin
  if InitToolHelp then
    Result := _Module32Next(hSnapshot, lpme)
  else Result := False;
end;

function Module32FirstA;
begin
  if InitToolHelp then
    Result := _Module32FirstA(hSnapshot, lpme)
  else Result := False;
end;

function Module32NextA;
begin
  if InitToolHelp then
    Result := _Module32NextA(hSnapshot, lpme)
  else Result := False;
end;

function Module32FirstW;
begin
  if InitToolHelp then
    Result := _Module32FirstW(hSnapshot, lpme)
  else Result := False;
end;

function Module32NextW;
begin
  if InitToolHelp then
    Result := _Module32NextW(hSnapshot, lpme)
  else Result := False;
end;

end.


