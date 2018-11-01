{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Dcc32FileAgePatch.pas, released on 2008-02-10.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2008 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit Dcc32FileAgePatch;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, SysUtils, DelphiData, CapExec, ShlObj;

procedure Dcc32SpeedInjection(const ProcessInfo: TProcessInformation);
function GetCompilerSpeedPackInjection(Target: TCompileTarget): TInjectionProc;

implementation

uses
  PackageInformation;

type
  PCFileAgeRec = ^TCFileAgeRec;
  TCFileAgeRec = packed record
    PushEbx: Byte; // $53
    AddEsp: record
      Add: Word; // $81 $C4
      Offset: LongWord; // $fffffeb4
    end;
    MovEbxEax: Word; // $8B $D8
    LeaEaxEsp0C: LongWord; // $8D $44 $24  $0C
    PushEax: Byte; // $50
    PushEbx_2: Byte; // $53
    CallFindFirstFile: record
      Call: Byte; // $E8
      Offset: LongWord;
    end;
    CmpEaxFF: record
      CmpEax: Word; // $83 $F8
      Offset: Byte; // $FF
    end;
    Jz: record
      Jz: Byte; // $74
      Offset: Byte; // $4D
    end;
    PushEax_2: Byte; // $50
    CallFindClose: record
      Call: Byte; // $E8
      Offset: LongWord;
    end;
    Buf: array[0..30] of Byte;
    Nops: array[0..22] of Byte;
  end;


{dcc32speed.dpr.51: if GetFileAttributesEx(Pointer(Filename), GetFileExInfoStandard, @FindData) then
004043E5 8D45D0           lea eax,[ebp-$30]
004043E8 50               push eax
004043E9 6A00             push $00
004043EB 53               push ebx
004043EC E867FFFFFF       call GetFileAttributesEx
004043F1 85C0             test eax,eax
004043F3 7428             jz $0040441d
}
  PCFileAgeNewRec = ^TCFileAgeNewRec;
  TCFileAgeNewRec = packed record
    PushEbx: Byte; // $53
    AddEsp: record
      Add: Word; // $81 $C4
      Offset: LongWord; // $fffffeb4
    end;
    MovEbxEax: Word; // $8B $D8
    LeaEaxEsp0C: LongWord; // $8D $44 $24  $0C
    PushEax: Byte; // $50
    {*}Push00: Word; // $6A 00
    PushEbx_2: Byte; // $53
    CallGetFileAttributesEx: record
      Call: Byte; // $E8
      Offset: LongWord;
    end;
    {*}TestEaxEax: Word; // $85 $C0
    Jz: record
      Jz: Byte; // $74
      Offset: Byte; // $4D-1
    end;
    {*}Nop: Byte;
    {*}Nop_2: LongWord;
    Buf: array[0..30] of Byte;
    Nops: array[0..22] of Byte;
  end;

  LongRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;

procedure PatchCFileAgeMem(CFileAge: PCFileAgeRec; Base: Cardinal);
var
  P: PCFileAgeNewRec;
  ProcAddr: Cardinal;
  KernelModule: HMODULE;
  I: Integer;
begin
  KernelModule := GetModuleHandle(kernel32);
  ProcAddr := Cardinal(GetProcAddress(KernelModule, 'GetFileAttributesExA'));

  P := PCFileAgeNewRec(CFileAge);
  P.Push00 := $006A;  // inserted
  P.PushEbx_2 := $53; // moved
  P.CallGetFileAttributesEx.Call := $E8; // moved
  P.CallGetFileAttributesEx.Offset := ProcAddr - (Base + Cardinal(@P.CallGetFileAttributesEx) + 5 - Cardinal(P));
  P.TestEaxEax := $C085;
  P.Jz.Jz := $74;
  P.Jz.Offset := $4D - 1;
  P.Nop := $90;
  P.Nop_2 := $90909090;
  for I := Low(P.Nops) to High(P.Nops) do
    P.Nops[I] := $90;
end;

function PatchDcc32FileAge(hProcess: THandle): Boolean;
var
  MemInfo: TMemoryBasicInformation;
  Start, Stop: Pointer;
  CFileAge: PCFileAgeRec;
  Count: Integer;
  P, Buffer, EndAddr: PByte;
  ReadBytes: {$IFDEF RTL230_UP}NativeUInt{$ELSE}Cardinal{$ENDIF};
  LastBufferSize: Cardinal;
  Base: Cardinal;
begin
  LastBufferSize := 0;
  Buffer := nil;
  try
    Count := 0;
    Start := Pointer(HInstance);
    Stop := Start;
    while (VirtualQueryEx(hProcess, Stop, MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo))
          and (MemInfo.AllocationBase = Start) do
    begin
      Start := Stop;
      Stop := Pointer(PAnsiChar(Stop) + MemInfo.RegionSize);

      if (MemInfo.RegionSize > 0) and
         ((MemInfo.AllocationProtect = PAGE_EXECUTE_READ) or
          (MemInfo.AllocationProtect = PAGE_EXECUTE_READWRITE) or
          (MemInfo.AllocationProtect = PAGE_EXECUTE_WRITECOPY)) then
      begin
        if MemInfo.RegionSize > LastBufferSize then
        begin
          if Buffer <> nil then
            FreeMem(Buffer);
          LastBufferSize := MemInfo.RegionSize;
          GetMem(Buffer, LastBufferSize);
        end;
        if ReadProcessMemory(hProcess, MemInfo.BaseAddress, Buffer, MemInfo.RegionSize, ReadBytes) then
        begin
          P := Buffer;
          EndAddr := PByte(Cardinal(Buffer) + ReadBytes);
          while P <> EndAddr do
          begin
            CFileAge := PCFileAgeRec(P);
            if (CFileAge.PushEbx = $53) and
               (CFileAge.AddEsp.Add = $C481) and
               (CFileAge.AddEsp.Offset = $FFFFFEB4) and
               (CFileAge.MovEbxEax = $D88B) and
               (CFileAge.LeaEaxEsp0C = $0C24448D) and
               (CFileAge.PushEax = $50) and
               (CFileAge.PushEbx_2 = $53) and
               (CFileAge.CallFindFirstFile.Call = $E8) and

               (CFileAge.CmpEaxFF.CmpEax = $F883) and
               (CFileAge.CmpEaxFF.Offset = $FF) and
               (CFileAge.Jz.Jz = $74) and
               (CFileAge.Jz.Offset = $4D) and
               (CFileAge.PushEax_2 = $50) and
               (CFileAge.CallFindClose.Call = $E8) then
            begin
              Base := Cardinal(MemInfo.BaseAddress) + (Cardinal(CFileAge) - Cardinal(Buffer));
              PatchCFileAgeMem(CFileAge, Base);
              if WriteProcessMemory(hProcess,
                   Pointer(Base),
                   CFileAge, SizeOf(TCFileAgeRec), ReadBytes) then
              begin
                Inc(Count);
              end;
            end;
            Inc(P);
          end;
        end;
      end;
    end;
  finally
    if Buffer <> nil then
      FreeMem(Buffer);
  end;
  Result := Count <= 1;

  {if Count <> 1 then
    WriteLn(ErrOutput, 'DCC32 FileAge patching failed.');}
end;

procedure Dcc32SpeedInjection(const ProcessInfo: TProcessInformation);
begin
  if GetProcAddress(GetModuleHandle(kernel32), 'GetFileAttributesExA') <> nil then
    PatchDcc32FileAge(ProcessInfo.hProcess);
end;


var
  _CreateRemoteThread: function(hProcess: THandle; lpThreadAttributes: Pointer;
                                dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine;
                                lpParameter: Pointer; dwCreationFlags: DWORD;
                                var lpThreadId: DWORD): THandle; stdcall;

{ <inject code> }

type
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF not  declared(SIZE_T)}
  SIZE_T = DWORD;
    {$IFEND}
  {$ELSE}
  SIZE_T = DWORD;
  {$ENDIF CONDITIONALEXPRESSIONS}

  PMem = ^TMem;
  TMem = packed record
    jmp: Word;
    LoadLibaryAddr: Pointer;
    GetModuleHandleAddr: Pointer;
    DllName: PChar;
  end;

function LoadHookInjDll{(lParam: Pointer)}: Integer; stdcall;
asm
  jmp @@Start // 2 Bytes

@@LoadLibraryAddr:
  dd 0
@@GetModuleHandleAddr:
  dd 0
@@DllName:
  dd 0 // pointer to dll filename
  db 0
  nop
  nop
  nop
  nop
  nop

@@Start: // must be a near jump from the function start
  mov edx, [esp + 4] // lParam = @LoadHookInjDll

  mov eax, OFFSET @@DllName
  sub eax, OFFSET LoadHookInjDll
  add eax, edx
  push [eax]

  mov eax, OFFSET @@LoadLibraryAddr
  sub eax, OFFSET LoadHookInjDll
  add eax, edx
  call [eax]

  // return LoadLibrary((char*)@@LoadLibraryAddr)
  ret 4
end;

procedure InjectCodeEnd;
begin
end;

{ </inject code> }

function InjectHookDllProcess(hProcess: THandle; const DllName: string; Wait: Boolean): Boolean;
var
  hLoadThread: THandle;
  Mem: Pointer;
  DllNameMem: PChar;
  Size, SizeP: Cardinal;
  n: SIZE_T;
  Buf: Pointer;
  Id: Cardinal;
  ThreadExitCode: Cardinal;
  hKernel: THandle;
begin
  hKernel := GetModuleHandle(kernel32);
  if not Assigned(_CreateRemoteThread) then
    _CreateRemoteThread := GetProcAddress(hKernel, 'CreateRemoteThread');

  Size := Cardinal(@InjectCodeEnd) - Cardinal(@LoadHookInjDll);
  SizeP := Size + (4096 - (Size mod 4096));

  ExitCode := 0;
  Result := False;
  if hProcess = 0 then
    Exit;
  DllNameMem := VirtualAllocEx(hProcess, nil, (Length(DllName) + 1) * SizeOf(Char), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if DllNameMem = nil then
    Exit;
  // write string with #0
  WriteProcessMemory(hProcess, DllNameMem, PChar(DllName), (Length(DllName) + 1) * SizeOf(Char), n);

  Mem := VirtualAllocEx(hProcess, nil, SizeP, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if Mem = nil then
  begin
    VirtualFreeEx(hProcess, DllNameMem, 0, MEM_RELEASE);
    Exit;
  end;
  GetMem(Buf, Size);
  try
    if not ReadProcessMemory(GetCurrentProcess, @LoadHookInjDll, Buf, Size, n) then
      Exit;
    {$IFDEF UNICODE}
    PMem(Buf)^.LoadLibaryAddr := GetProcAddress(hKernel, 'LoadLibraryW');
    {$ELSE}
    PMem(Buf)^.LoadLibaryAddr := GetProcAddress(hKernel, 'LoadLibraryA');
    {$ENDIF UNICODE}
    PMem(Buf)^.GetModuleHandleAddr := nil;
    PMem(Buf)^.DllName := DllNameMem;

    if not WriteProcessMemory(hProcess, Mem, Buf, Size, n) then
      Exit;
  finally
    FreeMem(Buf);
  end;

  { Start remote thread and wait until the thread has terminated what is the
    case when the DLL is fully initialized or failed to load. }
  hLoadThread := _CreateRemoteThread(hProcess, nil, 0, Mem, Mem, 0, Id);
  Result := hLoadThread <> 0;
  if Result then
  begin
    if Wait then
    begin
      WaitForSingleObject(hLoadThread, INFINITE);
      GetExitCodeThread(hLoadThread, ThreadExitCode);
      CloseHandle(hLoadThread);
      Result := ThreadExitCode <> 0;
    end
    else
    begin
      CloseHandle(hLoadThread);
      Result := True;
      Exit; // we can't release the memory
    end;
  end;

  VirtualFreeEx(hProcess, Mem, 0, MEM_RELEASE);
end;


var
  FastDCCHookDll: string; // We start DCCxx.EXE sequencially so it is save to use the global variable

procedure FastDCCHookInjection(const ProcessInfo: TProcessInformation);
begin
  if (FastDCCHookDll <> '') and FileExists(FastDCCHookDll) then
    InjectHookDllProcess(ProcessInfo.hProcess, FastDCCHookDll, True);
end;

function GetCompilerSpeedPackInjection(Target: TCompileTarget): TInjectionProc;
const
  {$IFNDEF DELPHI2007_UP}
  CSIDL_COMMON_DOCUMENTS = $002e;
  {$ENDIF ~DELPHI2007_UP}
  ExpertsDirStudio = 'Embarcadero\Studio\%s\Experts'; // XE6+
  ExpertsDirRADStudio = 'RAD Studio\%s\Experts';
var
  Buffer: array[0..MAX_PATH * 2] of Char;
  ExpertsDir, DccHook, DccCompiler: string;
begin
  Result := nil;
  if Target.Version >= 20 then // Delphi 2009+
  begin
    if not SHGetSpecialFolderPath(0, Buffer, CSIDL_COMMON_DOCUMENTS, False) then
      Exit;

    if Target.Version >= 20 then // XE6+
      ExpertsDir := ExpertsDirStudio
    else
      ExpertsDir := ExpertsDirRADStudio;
    ExpertsDir := Format(ExpertsDir, [Target.IDEVersionStr]);

    FastDCCHookDll := ExcludeTrailingPathDelimiter(Buffer) + '\' + ExpertsDir;

    if Target.Platform = ctpWin64 then
    begin
      FastDCCHookDll := FastDCCHookDll + '\fastdcc64Hook.dllx';
      DccHook := ExtractFilePath(Target.Dcc32) + 'dcc64Hook.dllx';
      DccCompiler := ExtractFilePath(Target.Dcc32) + 'dcc64compiler.exe';
    end
    else
    begin
      FastDCCHookDll := FastDCCHookDll + '\fastdcc32Hook.dllx';
      DccHook := ExtractFilePath(Target.Dcc32) + 'dcc32Hook.dllx';
      DccCompiler := ExtractFilePath(Target.Dcc32) + 'dcc32compiler.exe';
    end;

    if FileExists(FastDCCHookDll) then
    begin
      // If DCCxx.EXE was replaced by the CompilerSpeedPack's binary then we don't want to
      // install the FastDCC Hook a second time.
      if FileExists(DccHook) and FileExists(DccCompiler) then
        Result := nil
      else
        Result := FastDCCHookInjection;
    end;
  end;
end;

end.