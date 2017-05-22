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
  Windows;

procedure Dcc32SpeedInjection(const ProcessInfo: TProcessInformation);

implementation

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
      Stop := Pointer(Cardinal(Stop) + MemInfo.RegionSize);

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

end.