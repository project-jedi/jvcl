{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}


unit JvInterpreter_SysUtils;

interface

uses SysUtils, JvInterpreter;

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

  function SearchRec2Var(const SearchRec: TSearchRec): Variant;
  function Var2SearchRec(const SearchRec: Variant): TSearchRec;

implementation

{$IFDEF LINUX}
uses Variants;
{$ENDIF}

{ TSearchRec }

function SearchRec2Var(const SearchRec: TSearchRec): Variant;
var
  Rec: ^TSearchRec;
begin
  New(Rec);
  Rec^ := SearchRec;
  Result := R2V('TSearchRec', Rec);
end;

function Var2SearchRec(const SearchRec: Variant): TSearchRec;
begin
  Result := TSearchRec(V2R(SearchRec)^);
end;

  { Exception }

{ constructor Create(Msg: string) }
procedure Exception_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(Exception.Create(Args.Values[0]));
end;

{ constructor CreateFmt(Msg: string; Args: array) }
procedure Exception_CreateFmt(var Value: Variant; Args: TArgs);
begin
//  Value := O2V(Exception.CreateFmt(Args.Values[0], Args.Values[1]));
  NotImplemented('Exception.CreateFmt');
end;

{ constructor CreateRes(Ident: Integer) }
procedure Exception_CreateRes(var Value: Variant; Args: TArgs);
begin
  Value := O2V(Exception.CreateRes(Args.Values[0]));
end;

{ constructor CreateResFmt(Ident: Integer; Args: array) }
procedure Exception_CreateResFmt(var Value: Variant; Args: TArgs);
begin
//  Value := O2V(Exception.CreateResFmt(Args.Values[0], Args.Values[1]));
  NotImplemented('Exception.CreateResFmt');
end;

{ constructor CreateHelp(Msg: string; AHelpContext: Integer) }
procedure Exception_CreateHelp(var Value: Variant; Args: TArgs);
begin
  Value := O2V(Exception.CreateHelp(Args.Values[0], Args.Values[1]));
end;

{ constructor CreateFmtHelp(Msg: string; Args: array; AHelpContext: Integer) }
procedure Exception_CreateFmtHelp(var Value: Variant; Args: TArgs);
begin
//  Value := O2V(Exception.CreateFmtHelp(Args.Values[0], Args.Values[1], Args.Values[2]));
  NotImplemented('Exception.CreateFmtHelp');
end;

{ constructor CreateResHelp(Ident: Integer; AHelpContext: Integer) }
procedure Exception_CreateResHelp(var Value: Variant; Args: TArgs);
begin
  Value := O2V(Exception.CreateResHelp(Args.Values[0], Args.Values[1]));
end;

{ constructor CreateResFmtHelp(Ident: Integer; Args: array; AHelpContext: Integer) }
procedure Exception_CreateResFmtHelp(var Value: Variant; Args: TArgs);
begin
//  Value := O2V(Exception.CreateResFmtHelp(Args.Values[0], Args.Values[1], Args.Values[2]));
  NotImplemented('Exception.CreateResFmtHelp');
end;

{ property Read HelpContext: Integer }
procedure Exception_Read_HelpContext(var Value: Variant; Args: TArgs);
begin
  Value := Exception(Args.Obj).HelpContext;
end;

{ property Write HelpContext(Value: Integer) }
procedure Exception_Write_HelpContext(const Value: Variant; Args: TArgs);
begin
  Exception(Args.Obj).HelpContext := Value;
end;

{ property Read Message: string }
procedure Exception_Read_Message(var Value: Variant; Args: TArgs);
begin
  Value := Exception(Args.Obj).Message;
end;

{ property Write Message(Value: string) }
procedure Exception_Write_Message(const Value: Variant; Args: TArgs);
begin
  Exception(Args.Obj).Message := Value;
end;

  { EAbort }

  { EOutOfMemory }

  { EInOutError }

  { EIntError }

  { EDivByZero }

  { ERangeError }

  { EIntOverflow }

  { EMathError }

  { EInvalidOp }

  { EZeroDivide }

  { EOverflow }

  { EUnderflow }

  { EInvalidPointer }

  { EInvalidCast }

  { EConvertError }

  { EAccessViolation }

  { EPrivilege }

  { EStackOverflow }

  { EControlC }

  { EVariantError }

  { EPropReadOnly }

  { EPropWriteOnly }

  { EExternalException }

  { EAssertionFailed }

  { EAbstractError }

  { EIntfCastError }

  { EInvalidContainer }

  { EInvalidInsert }

  { EPackageError }

  { EWin32Error }


{ function AllocMem(Size: Cardinal): Pointer; }
procedure JvInterpreter_AllocMem(var Value: Variant; Args: TArgs);
begin
  Value := P2V(AllocMem(Args.Values[0]));
end;

{$IFNDEF COMPILER6_UP}
{ function NewStr(const S: string): PString; }
procedure JvInterpreter_NewStr(var Value: Variant; Args: TArgs);
begin
  Value := P2V(NewStr(Args.Values[0]));
end;

{ procedure DisposeStr(P: PString); }
procedure JvInterpreter_DisposeStr(var Value: Variant; Args: TArgs);
begin
  DisposeStr(V2P(Args.Values[0]));
end;

{ procedure AssignStr(var P: PString; const S: string); }
procedure JvInterpreter_AssignStr(var Value: Variant; Args: TArgs);
begin
  AssignStr(PString(TVarData(Args.Values[0]).vPointer), Args.Values[1]);
end;

{ procedure AppendStr(var Dest: string; const S: string); }
procedure JvInterpreter_AppendStr(var Value: Variant; Args: TArgs);
begin
  AppendStr(string(TVarData(Args.Values[0]).vString), Args.Values[1]);
end;
{$ENDIF COMPILER6_UP} // {$IFNDEF COMPILER6_UP}

{ function UpperCase(const S: string): string; }
procedure JvInterpreter_UpperCase(var Value: Variant; Args: TArgs);
begin
  Value := UpperCase(Args.Values[0]);
end;

{ function LowerCase(const S: string): string; }
procedure JvInterpreter_LowerCase(var Value: Variant; Args: TArgs);
begin
  Value := LowerCase(Args.Values[0]);
end;

{ function CompareStr(const S1, S2: string): Integer; }
procedure JvInterpreter_CompareStr(var Value: Variant; Args: TArgs);
begin
  Value := CompareStr(Args.Values[0], Args.Values[1]);
end;

{$IFDEF COMPILER3_UP}
{ function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; }
procedure JvInterpreter_CompareMem(var Value: Variant; Args: TArgs);
begin
  Value := CompareMem(V2P(Args.Values[0]), V2P(Args.Values[1]), Args.Values[2]);
end;
{$ENDIF COMPILER3_UP}

{ function CompareText(const S1, S2: string): Integer; }
procedure JvInterpreter_CompareText(var Value: Variant; Args: TArgs);
begin
  Value := CompareText(Args.Values[0], Args.Values[1]);
end;

{ function AnsiUpperCase(const S: string): string; }
procedure JvInterpreter_AnsiUpperCase(var Value: Variant; Args: TArgs);
begin
  Value := AnsiUpperCase(Args.Values[0]);
end;

{ function AnsiLowerCase(const S: string): string; }
procedure JvInterpreter_AnsiLowerCase(var Value: Variant; Args: TArgs);
begin
  Value := AnsiLowerCase(Args.Values[0]);
end;

{ function AnsiCompareStr(const S1, S2: string): Integer; }
procedure JvInterpreter_AnsiCompareStr(var Value: Variant; Args: TArgs);
begin
  Value := AnsiCompareStr(Args.Values[0], Args.Values[1]);
end;

{ function AnsiCompareText(const S1, S2: string): Integer; }
procedure JvInterpreter_AnsiCompareText(var Value: Variant; Args: TArgs);
begin
  Value := AnsiCompareText(Args.Values[0], Args.Values[1]);
end;

{$IFDEF COMPILER3_UP}
{ function AnsiStrComp(S1, S2: PChar): Integer; }
procedure JvInterpreter_AnsiStrComp(var Value: Variant; Args: TArgs);
begin
  Value := AnsiStrComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])));
end;

{ function AnsiStrIComp(S1, S2: PChar): Integer; }
procedure JvInterpreter_AnsiStrIComp(var Value: Variant; Args: TArgs);
begin
  Value := AnsiStrIComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])));
end;

{ function AnsiStrLComp(S1, S2: PChar; MaxLen: Cardinal): Integer; }
procedure JvInterpreter_AnsiStrLComp(var Value: Variant; Args: TArgs);
begin
  Value := AnsiStrLComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer; }
procedure JvInterpreter_AnsiStrLIComp(var Value: Variant; Args: TArgs);
begin
  Value := AnsiStrLIComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ function AnsiStrLower(Str: PChar): PChar; }
procedure JvInterpreter_AnsiStrLower(var Value: Variant; Args: TArgs);
begin
  Value := string(AnsiStrLower(PChar(string(Args.Values[0]))));
end;

{ function AnsiStrUpper(Str: PChar): PChar; }
procedure JvInterpreter_AnsiStrUpper(var Value: Variant; Args: TArgs);
begin
  Value := string(AnsiStrUpper(PChar(string(Args.Values[0]))));
end;

{ function AnsiLastChar(const S: string): PChar; }
procedure JvInterpreter_AnsiLastChar(var Value: Variant; Args: TArgs);
begin
  Value := string(AnsiLastChar(Args.Values[0]));
end;

{ function AnsiStrLastChar(P: PChar): PChar; }
procedure JvInterpreter_AnsiStrLastChar(var Value: Variant; Args: TArgs);
begin
  Value := string(AnsiStrLastChar(PChar(string(Args.Values[0]))));
end;
{$ENDIF COMPILER3_UP}

{ function Trim(const S: string): string; }
procedure JvInterpreter_Trim(var Value: Variant; Args: TArgs);
begin
  Value := Trim(Args.Values[0]);
end;

{ function TrimLeft(const S: string): string; }
procedure JvInterpreter_TrimLeft(var Value: Variant; Args: TArgs);
begin
  Value := TrimLeft(Args.Values[0]);
end;

{ function TrimRight(const S: string): string; }
procedure JvInterpreter_TrimRight(var Value: Variant; Args: TArgs);
begin
  Value := TrimRight(Args.Values[0]);
end;

{ function QuotedStr(const S: string): string; }
procedure JvInterpreter_QuotedStr(var Value: Variant; Args: TArgs);
begin
  Value := QuotedStr(Args.Values[0]);
end;

{$IFDEF COMPILER3_UP}
{ function AnsiQuotedStr(const S: string; Quote: Char): string; }
procedure JvInterpreter_AnsiQuotedStr(var Value: Variant; Args: TArgs);
begin
  Value := AnsiQuotedStr(Args.Values[0], string(Args.Values[1])[1]);
end;

{ function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string; }
procedure JvInterpreter_AnsiExtractQuotedStr(var Value: Variant; Args: TArgs);
begin
  Value := AnsiExtractQuotedStr(PChar(TVarData(Args.Values[0]).vPointer), string(Args.Values[1])[1]);
end;
{$ENDIF COMPILER3_UP}

{ function AdjustLineBreaks(const S: string): string; }
procedure JvInterpreter_AdjustLineBreaks(var Value: Variant; Args: TArgs);
begin
  Value := AdjustLineBreaks(Args.Values[0]);
end;

{ function IsValidIdent(const Ident: string): Boolean; }
procedure JvInterpreter_IsValidIdent(var Value: Variant; Args: TArgs);
begin
  Value := IsValidIdent(Args.Values[0]);
end;

{ function IntToStr(Value: Integer): string; }
procedure JvInterpreter_IntToStr(var Value: Variant; Args: TArgs);
begin
  Value := IntToStr(Args.Values[0]);
end;

{ function IntToHex(Value: Integer; Digits: Integer): string; }
procedure JvInterpreter_IntToHex(var Value: Variant; Args: TArgs);
begin
  Value := IntToHex(Args.Values[0], Args.Values[1]);
end;

{ function StrToInt(const S: string): Integer; }
procedure JvInterpreter_StrToInt(var Value: Variant; Args: TArgs);
begin
  Value := StrToInt(Args.Values[0]);
end;

{ function StrToIntDef(const S: string; Default: Integer): Integer; }
procedure JvInterpreter_StrToIntDef(var Value: Variant; Args: TArgs);
begin
  Value := StrToIntDef(Args.Values[0], Args.Values[1]);
end;

{ function LoadStr(Ident: Integer): string; }
procedure JvInterpreter_LoadStr(var Value: Variant; Args: TArgs);
begin
  Value := LoadStr(Args.Values[0]);
end;

(*
{ function FmtLoadStr(Ident: Integer; const Args: array of const): string; }
procedure JvInterpreter_FmtLoadStr(var Value: Variant; Args: TArgs);
begin
  Value := FmtLoadStr(Args.Values[0], Args.Values[1]);
end;
*)

{ function FileOpen(const FileName: string; Mode: Integer): Integer; }
procedure JvInterpreter_FileOpen(var Value: Variant; Args: TArgs);
begin
  Value := FileOpen(Args.Values[0], Args.Values[1]);
end;

{ function FileCreate(const FileName: string): Integer; }
procedure JvInterpreter_FileCreate(var Value: Variant; Args: TArgs);
begin
{$IFDEF MSWINDOWS}
  Value := FileCreate(Args.Values[0]);
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  Value := FileCreate(VarToStr(Args.Values[0]));
{$ENDIF LINUX}
end;

{ function FileRead(Handle: Integer; var Buffer; Count: Integer): Integer; }
procedure JvInterpreter_FileRead(var Value: Variant; Args: TArgs);
begin
  Value := FileRead(Args.Values[0], TVarData(Args.Values[1]).vInteger, Args.Values[2]);
end;

{ function FileWrite(Handle: Integer; const Buffer; Count: Integer): Integer; }
procedure JvInterpreter_FileWrite(var Value: Variant; Args: TArgs);
begin
  Value := FileWrite(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function FileSeek(Handle, Offset, Origin: Integer): Integer; }
procedure JvInterpreter_FileSeek(var Value: Variant; Args: TArgs);
begin
  Value := FileSeek(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure FileClose(Handle: Integer); }
procedure JvInterpreter_FileClose(var Value: Variant; Args: TArgs);
begin
  FileClose(Args.Values[0]);
end;

{ function FileAge(const FileName: string): Integer; }
procedure JvInterpreter_FileAge(var Value: Variant; Args: TArgs);
begin
  Value := FileAge(Args.Values[0]);
end;

{ function FileExists(const FileName: string): Boolean; }
procedure JvInterpreter_FileExists(var Value: Variant; Args: TArgs);
begin
  Value := FileExists(Args.Values[0]);
end;


{ function FindFirst(const Path: string; Attr: Integer; var F: TSearchRec): Integer; }
procedure JvInterpreter_FindFirst(var Value: Variant; Args: TArgs);
begin
  Value := FindFirst(Args.Values[0], Args.Values[1], TSearchRec(V2R(Args.Values[2])^));
end;

{ function FindNext(var F: TSearchRec): Integer; }
procedure JvInterpreter_FindNext(var Value: Variant; Args: TArgs);
begin
  Value := FindNext(TSearchRec(V2R(Args.Values[0])^));
end;

{ procedure FindClose(var F: TSearchRec); }
procedure JvInterpreter_FindClose(var Value: Variant; Args: TArgs);
begin
  FindClose(TSearchRec(V2R(Args.Values[0])^));
end;


{ function FileGetDate(Handle: Integer): Integer; }
procedure JvInterpreter_FileGetDate(var Value: Variant; Args: TArgs);
begin
  Value := FileGetDate(Args.Values[0]);
end;

{ function FileSetDate(Handle: Integer; Age: Integer): Integer; }
procedure JvInterpreter_FileSetDate(var Value: Variant; Args: TArgs);
begin
{$IFDEF MSWINDOWS}
  Value := FileSetDate(Args.Values[0], Args.Values[1]);
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  Value := FileSetDate(VarToStr(Args.Values[0]), Args.Values[1]);
{$ENDIF LINUX}
end;

{$IFDEF MSWINDOWS}
{ function FileGetAttr(const FileName: string): Integer; }
procedure JvInterpreter_FileGetAttr(var Value: Variant; Args: TArgs);
begin
  Value := FileGetAttr(Args.Values[0]);
end;

{ function FileSetAttr(const FileName: string; Attr: Integer): Integer; }
procedure JvInterpreter_FileSetAttr(var Value: Variant; Args: TArgs);
begin
  Value := FileSetAttr(Args.Values[0], Args.Values[1]);
end;
{$ENDIF MSWINDOWS}

{ function DeleteFile(const FileName: string): Boolean; }
procedure JvInterpreter_DeleteFile(var Value: Variant; Args: TArgs);
begin
  Value := DeleteFile(Args.Values[0]);
end;

{ function RenameFile(const OldName, NewName: string): Boolean; }
procedure JvInterpreter_RenameFile(var Value: Variant; Args: TArgs);
begin
  Value := RenameFile(Args.Values[0], Args.Values[1]);
end;

{ function ChangeFileExt(const FileName, Extension: string): string; }
procedure JvInterpreter_ChangeFileExt(var Value: Variant; Args: TArgs);
begin
  Value := ChangeFileExt(Args.Values[0], Args.Values[1]);
end;

{ function ExtractFilePath(const FileName: string): string; }
procedure JvInterpreter_ExtractFilePath(var Value: Variant; Args: TArgs);
begin
  Value := ExtractFilePath(Args.Values[0]);
end;

{ function ExtractFileDir(const FileName: string): string; }
procedure JvInterpreter_ExtractFileDir(var Value: Variant; Args: TArgs);
begin
  Value := ExtractFileDir(Args.Values[0]);
end;

{ function ExtractFileDrive(const FileName: string): string; }
procedure JvInterpreter_ExtractFileDrive(var Value: Variant; Args: TArgs);
begin
  Value := ExtractFileDrive(Args.Values[0]);
end;

{ function ExtractFileName(const FileName: string): string; }
procedure JvInterpreter_ExtractFileName(var Value: Variant; Args: TArgs);
begin
  Value := ExtractFileName(Args.Values[0]);
end;

{ function ExtractFileExt(const FileName: string): string; }
procedure JvInterpreter_ExtractFileExt(var Value: Variant; Args: TArgs);
begin
  Value := ExtractFileExt(Args.Values[0]);
end;

{ function ExpandFileName(const FileName: string): string; }
procedure JvInterpreter_ExpandFileName(var Value: Variant; Args: TArgs);
begin
  Value := ExpandFileName(Args.Values[0]);
end;

{ function ExpandUNCFileName(const FileName: string): string; }
procedure JvInterpreter_ExpandUNCFileName(var Value: Variant; Args: TArgs);
begin
  Value := ExpandUNCFileName(Args.Values[0]);
end;

{$IFDEF COMPILER3_UP}
{ function ExtractRelativePath(const BaseName, DestName: string): string; }
procedure JvInterpreter_ExtractRelativePath(var Value: Variant; Args: TArgs);
begin
  Value := ExtractRelativePath(Args.Values[0], Args.Values[1]);
end;
{$ENDIF COMPILER3_UP}

{ function FileSearch(const Name, DirList: string): string; }
procedure JvInterpreter_FileSearch(var Value: Variant; Args: TArgs);
begin
  Value := FileSearch(Args.Values[0], Args.Values[1]);
end;

{$IFDEF MSWINDOWS}
{ function DiskFree(Drive: Byte): Integer; }
procedure JvInterpreter_DiskFree(var Value: Variant; Args: TArgs);
begin
  Value := Integer(DiskFree(Args.Values[0]));
end;

{ function DiskSize(Drive: Byte): Integer; }
procedure JvInterpreter_DiskSize(var Value: Variant; Args: TArgs);
begin
  Value := Integer(DiskSize(Args.Values[0]));
end;
{$ENDIF MSWINDOWS}

{ function FileDateToDateTime(FileDate: Integer): TDateTime; }
procedure JvInterpreter_FileDateToDateTime(var Value: Variant; Args: TArgs);
begin
  Value := FileDateToDateTime(Args.Values[0]);
end;

{ function DateTimeToFileDate(DateTime: TDateTime): Integer; }
procedure JvInterpreter_DateTimeToFileDate(var Value: Variant; Args: TArgs);
begin
  Value := DateTimeToFileDate(Args.Values[0]);
end;

{ function GetCurrentDir: string; }
procedure JvInterpreter_GetCurrentDir(var Value: Variant; Args: TArgs);
begin
  Value := GetCurrentDir;
end;

{ function SetCurrentDir(const Dir: string): Boolean; }
procedure JvInterpreter_SetCurrentDir(var Value: Variant; Args: TArgs);
begin
  Value := SetCurrentDir(Args.Values[0]);
end;

{ function CreateDir(const Dir: string): Boolean; }
procedure JvInterpreter_CreateDir(var Value: Variant; Args: TArgs);
begin
  Value := CreateDir(Args.Values[0]);
end;

{ function RemoveDir(const Dir: string): Boolean; }
procedure JvInterpreter_RemoveDir(var Value: Variant; Args: TArgs);
begin
  Value := RemoveDir(Args.Values[0]);
end;

{ function StrLen(Str: PChar): Cardinal; }
procedure JvInterpreter_StrLen(var Value: Variant; Args: TArgs);
begin
  Value := Integer(StrLen(PChar(string(Args.Values[0]))));
end;

{ function StrEnd(Str: PChar): PChar; }
procedure JvInterpreter_StrEnd(var Value: Variant; Args: TArgs);
begin
  Value := string(StrEnd(PChar(string(Args.Values[0]))));
end;

{ function StrMove(Dest, Source: PChar; Count: Cardinal): PChar; }
procedure JvInterpreter_StrMove(var Value: Variant; Args: TArgs);
begin
  Value := string(StrMove(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]));
end;

{ function StrCopy(Dest, Source: PChar): PChar; }
procedure JvInterpreter_StrCopy(var Value: Variant; Args: TArgs);
begin
  Value := string(StrCopy(PChar(string(Args.Values[0])), PChar(string(Args.Values[1]))));
end;

{ function StrECopy(Dest, Source: PChar): PChar; }
procedure JvInterpreter_StrECopy(var Value: Variant; Args: TArgs);
begin
  Value := string(StrECopy(PChar(string(Args.Values[0])), PChar(string(Args.Values[1]))));
end;

{ function StrLCopy(Dest, Source: PChar; MaxLen: Cardinal): PChar; }
procedure JvInterpreter_StrLCopy(var Value: Variant; Args: TArgs);
begin
  Value := string(StrLCopy(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]));
end;

{ function StrPCopy(Dest: PChar; const Source: string): PChar; }
procedure JvInterpreter_StrPCopy(var Value: Variant; Args: TArgs);
begin
  Value := string(StrPCopy(PChar(string(Args.Values[0])), Args.Values[1]));
end;

{ function StrPLCopy(Dest: PChar; const Source: string; MaxLen: Cardinal): PChar; }
procedure JvInterpreter_StrPLCopy(var Value: Variant; Args: TArgs);
begin
  Value := string(StrPLCopy(PChar(string(Args.Values[0])), Args.Values[1], Args.Values[2]));
end;

{ function StrCat(Dest, Source: PChar): PChar; }
procedure JvInterpreter_StrCat(var Value: Variant; Args: TArgs);
begin
  Value := string(StrCat(PChar(string(Args.Values[0])), PChar(string(Args.Values[1]))));
end;

{ function StrLCat(Dest, Source: PChar; MaxLen: Cardinal): PChar; }
procedure JvInterpreter_StrLCat(var Value: Variant; Args: TArgs);
begin
  Value := string(StrLCat(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]));
end;

{ function StrComp(Str1, Str2: PChar): Integer; }
procedure JvInterpreter_StrComp(var Value: Variant; Args: TArgs);
begin
  Value := StrComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])));
end;

{ function StrIComp(Str1, Str2: PChar): Integer; }
procedure JvInterpreter_StrIComp(var Value: Variant; Args: TArgs);
begin
  Value := StrIComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])));
end;

{ function StrLComp(Str1, Str2: PChar; MaxLen: Cardinal): Integer; }
procedure JvInterpreter_StrLComp(var Value: Variant; Args: TArgs);
begin
  Value := StrLComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ function StrLIComp(Str1, Str2: PChar; MaxLen: Cardinal): Integer; }
procedure JvInterpreter_StrLIComp(var Value: Variant; Args: TArgs);
begin
  Value := StrLIComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ function StrScan(Str: PChar; Chr: Char): PChar; }
procedure JvInterpreter_StrScan(var Value: Variant; Args: TArgs);
begin
  Value := string(StrScan(PChar(string(Args.Values[0])), string(Args.Values[1])[1]));
end;

{ function StrRScan(Str: PChar; Chr: Char): PChar; }
procedure JvInterpreter_StrRScan(var Value: Variant; Args: TArgs);
begin
  Value := string(StrRScan(PChar(string(Args.Values[0])), string(Args.Values[1])[1]));
end;

{ function StrPos(Str1, Str2: PChar): PChar; }
procedure JvInterpreter_StrPos(var Value: Variant; Args: TArgs);
begin
  Value := string(StrPos(PChar(string(Args.Values[0])), PChar(string(Args.Values[1]))));
end;

{ function StrUpper(Str: PChar): PChar; }
procedure JvInterpreter_StrUpper(var Value: Variant; Args: TArgs);
begin
  Value := string(StrUpper(PChar(string(Args.Values[0]))));
end;

{ function StrLower(Str: PChar): PChar; }
procedure JvInterpreter_StrLower(var Value: Variant; Args: TArgs);
begin
  Value := string(StrLower(PChar(string(Args.Values[0]))));
end;

{ function StrPas(Str: PChar): string; }
procedure JvInterpreter_StrPas(var Value: Variant; Args: TArgs);
begin
  Value := StrPas(PChar(string(Args.Values[0])));
end;

{ function StrAlloc(Size: Cardinal): PChar; }
procedure JvInterpreter_StrAlloc(var Value: Variant; Args: TArgs);
begin
  Value := string(StrAlloc(Args.Values[0]));
end;

{ function StrBufSize(Str: PChar): Cardinal; }
procedure JvInterpreter_StrBufSize(var Value: Variant; Args: TArgs);
begin
  Value := Integer(StrBufSize(PChar(string(Args.Values[0]))));
end;

{ function StrNew(Str: PChar): PChar; }
procedure JvInterpreter_StrNew(var Value: Variant; Args: TArgs);
begin
  Value := string(StrNew(PChar(string(Args.Values[0]))));
end;

{ procedure StrDispose(Str: PChar); }
procedure JvInterpreter_StrDispose(var Value: Variant; Args: TArgs);
begin
  StrDispose(PChar(string(Args.Values[0])));
end;

{ function Format(const Format: string; const Args: array of const): string; }
procedure JvInterpreter_Format(var Value: Variant; Args: TArgs);
 function FormatWorkaround(const MyFormat: string; const Args: array of const):string;
  begin
     result := Format(MyFormat, Args);
  end;
begin
  Args.OpenArray(1);
  Value := FormatWorkaround(Args.Values[0], Slice(Args.OA^, Args.OAS));
end;

{ procedure FmtStr(var Result: string; const Format: string; const Args: array of const); }
procedure JvInterpreter_FmtStr(var Value: Variant; Args: TArgs);
 procedure FmtStrWorkaround(var Result: string; const Format: string; const Args: array of const);
  begin
     FmtStr(Result, Format, Args);
  end;
begin
  Args.OpenArray(2);
  FmtStrWorkaround(string(TVarData(Args.Values[0]).vString), Args.Values[1], Slice(Args.OA^, Args.OAS));
end;

{ function StrFmt(Buffer, Format: PChar; const Args: array of const): PChar; }
procedure JvInterpreter_StrFmt(var Value: Variant; Args: TArgs);
 function StrFmtWorkaround(Buffer, Format: PChar; const Args: array of const): PChar;
  begin
     result := StrFmt(Buffer,Format, Args);
  end;
begin
  Args.OpenArray(2);
  Value := string(StrFmtWorkaround(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Slice(Args.OA^, Args.OAS)));
end;

{ function StrLFmt(Buffer: PChar; MaxLen: Cardinal; Format: PChar; const Args: array of const): PChar; }
procedure JvInterpreter_StrLFmt(var Value: Variant; Args: TArgs);
 function StrLFmtWorkaround(Buffer: PChar; MaxLen: Cardinal; Format: PChar; const Args: array of const): PChar;
  begin
     result :=  StrLFmt(Buffer,MaxLen, Format, Args);
  end;

begin
  Args.OpenArray(3);
  Value := string(StrLFmtWorkaround(PChar(string(Args.Values[0])), Args.Values[1], PChar(string(Args.Values[2])), Slice(Args.OA^, Args.OAS)));
end;

{ function FormatBuf(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal; const Args: array of const): Cardinal; }
procedure JvInterpreter_FormatBuf(var Value: Variant; Args: TArgs);
 function FormatBufWorkaround(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal; const Args: array of const): Cardinal;
  begin
     result :=  FormatBuf(Buffer,BufLen, Format, FmtLen, Args);
  end;
begin
  Args.OpenArray(4);
  Value := Integer(FormatBufWorkaround(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], Slice(Args.OA^, Args.OAS)));
end;

{ function FloatToStr(Value: Extended): string; }
procedure JvInterpreter_FloatToStr(var Value: Variant; Args: TArgs);
begin
  Value := FloatToStr(Args.Values[0]);
end;

{ function CurrToStr(Value: Currency): string; }
procedure JvInterpreter_CurrToStr(var Value: Variant; Args: TArgs);
begin
  Value := CurrToStr(Args.Values[0]);
end;

{ function FloatToStrF(Value: Extended; Format: TFloatFormat; Precision, Digits: Integer): string; }
procedure JvInterpreter_FloatToStrF(var Value: Variant; Args: TArgs);
begin
  Value := FloatToStrF(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ function CurrToStrF(Value: Currency; Format: TFloatFormat; Digits: Integer): string; }
procedure JvInterpreter_CurrToStrF(var Value: Variant; Args: TArgs);
begin
  Value := CurrToStrF(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

(*
{ function FloatToText(Buffer: PChar; const Value; ValueType: TFloatValue; Format: TFloatFormat; Precision, Digits: Integer): Integer; }
procedure JvInterpreter_FloatToText(var Value: Variant; Args: TArgs);
begin
  Value := FloatToText(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2], Args.Values[3], Args.Values[4], Args.Values[5]);
end;
*)

{ function FormatFloat(const Format: string; Value: Extended): string; }
procedure JvInterpreter_FormatFloat(var Value: Variant; Args: TArgs);
begin
  Value := FormatFloat(Args.Values[0], Args.Values[1]);
end;

{ function FormatCurr(const Format: string; Value: Currency): string; }
procedure JvInterpreter_FormatCurr(var Value: Variant; Args: TArgs);
begin
  Value := FormatCurr(Args.Values[0], Args.Values[1]);
end;

(*
{ function FloatToTextFmt(Buffer: PChar; const Value; ValueType: TFloatValue; Format: PChar): Integer; }
procedure JvInterpreter_FloatToTextFmt(var Value: Variant; Args: TArgs);
begin
  Value := FloatToTextFmt(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2], PChar(string(Args.Values[3])));
end;
*)

{ function StrToFloat(const S: string): Extended; }
procedure JvInterpreter_StrToFloat(var Value: Variant; Args: TArgs);
begin
  Value := StrToFloat(Args.Values[0]);
end;

{ function StrToCurr(const S: string): Currency; }
procedure JvInterpreter_StrToCurr(var Value: Variant; Args: TArgs);
begin
  Value := StrToCurr(Args.Values[0]);
end;

(*
{ function TextToFloat(Buffer: PChar; var Value; ValueType: TFloatValue): Boolean; }
procedure JvInterpreter_TextToFloat(var Value: Variant; Args: TArgs);
begin
  Value := TextToFloat(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;
*)
(* need record
{ procedure FloatToDecimal(var Result: TFloatRec; const Value; ValueType: TFloatValue; Precision, Decimals: Integer); }
procedure JvInterpreter_FloatToDecimal(var Value: Variant; Args: TArgs);
begin
  FloatToDecimal(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], Args.Values[4]);
end;
*)

(* need record
{ function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp; }
procedure JvInterpreter_DateTimeToTimeStamp(var Value: Variant; Args: TArgs);
begin
  Value := DateTimeToTimeStamp(Args.Values[0]);
end;

{ function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime; }
procedure JvInterpreter_TimeStampToDateTime(var Value: Variant; Args: TArgs);
begin
  Value := TimeStampToDateTime(Args.Values[0]);
end;

{ function MSecsToTimeStamp(MSecs: Comp): TTimeStamp; }
procedure JvInterpreter_MSecsToTimeStamp(var Value: Variant; Args: TArgs);
begin
  Value := MSecsToTimeStamp(Args.Values[0]);
end;

{ function TimeStampToMSecs(const TimeStamp: TTimeStamp): Comp; }
procedure JvInterpreter_TimeStampToMSecs(var Value: Variant; Args: TArgs);
begin
  Value := TimeStampToMSecs(Args.Values[0]);
end;
*)

{ function EncodeDate(Year, Month, Day: Word): TDateTime; }
procedure JvInterpreter_EncodeDate(var Value: Variant; Args: TArgs);
begin
  Value := EncodeDate(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime; }
procedure JvInterpreter_EncodeTime(var Value: Variant; Args: TArgs);
begin
  Value := EncodeTime(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word); }
procedure JvInterpreter_DecodeDate(var Value: Variant; Args: TArgs);
begin
  DecodeDate(Args.Values[0], Word(TVarData(Args.Values[1]).vSmallint), Word(TVarData(Args.Values[2]).vSmallint), Word(TVarData(Args.Values[3]).vSmallint));
end;

{ procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word); }
procedure JvInterpreter_DecodeTime(var Value: Variant; Args: TArgs);
begin
  DecodeTime(Args.Values[0], Word(TVarData(Args.Values[1]).vSmallint), Word(TVarData(Args.Values[2]).vSmallint), Word(TVarData(Args.Values[3]).vSmallint), Word(TVarData(Args.Values[4]).vSmallint));
end;

(* need record
{ procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime); }
procedure JvInterpreter_DateTimeToSystemTime(var Value: Variant; Args: TArgs);
begin
  DateTimeToSystemTime(Args.Values[0], Args.Values[1]);
end;

{ function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime; }
procedure JvInterpreter_SystemTimeToDateTime(var Value: Variant; Args: TArgs);
begin
  Value := SystemTimeToDateTime(Args.Values[0]);
end;
*)

{ function DayOfWeek(Date: TDateTime): Integer; }
procedure JvInterpreter_DayOfWeek(var Value: Variant; Args: TArgs);
begin
  Value := DayOfWeek(Args.Values[0]);
end;

{ function Date: TDateTime; }
procedure JvInterpreter_Date(var Value: Variant; Args: TArgs);
begin
  Value := Date;
end;

{ function Time: TDateTime; }
procedure JvInterpreter_Time(var Value: Variant; Args: TArgs);
begin
  Value := Time;
end;

{ function Now: TDateTime; }
procedure JvInterpreter_Now(var Value: Variant; Args: TArgs);
begin
  Value := Now;
end;

{$IFDEF COMPILER3_UP}
{ function IncMonth(const Date: TDateTime; NumberOfMonths: Integer): TDateTime; }
procedure JvInterpreter_IncMonth(var Value: Variant; Args: TArgs);
begin
  Value := IncMonth(Args.Values[0], Args.Values[1]);
end;

{ function IsLeapYear(Year: Word): Boolean; }
procedure JvInterpreter_IsLeapYear(var Value: Variant; Args: TArgs);
begin
  Value := IsLeapYear(Args.Values[0]);
end;
{$ENDIF COMPILER3_UP}

{ function DateToStr(Date: TDateTime): string; }
procedure JvInterpreter_DateToStr(var Value: Variant; Args: TArgs);
begin
  Value := DateToStr(Args.Values[0]);
end;

{ function TimeToStr(Time: TDateTime): string; }
procedure JvInterpreter_TimeToStr(var Value: Variant; Args: TArgs);
begin
  Value := TimeToStr(Args.Values[0]);
end;

{ function DateTimeToStr(DateTime: TDateTime): string; }
procedure JvInterpreter_DateTimeToStr(var Value: Variant; Args: TArgs);
begin
  Value := DateTimeToStr(Args.Values[0]);
end;

{ function StrToDate(const S: string): TDateTime; }
procedure JvInterpreter_StrToDate(var Value: Variant; Args: TArgs);
begin
  Value := StrToDate(Args.Values[0]);
end;

{ function StrToTime(const S: string): TDateTime; }
procedure JvInterpreter_StrToTime(var Value: Variant; Args: TArgs);
begin
  Value := StrToTime(Args.Values[0]);
end;

{ function StrToDateTime(const S: string): TDateTime; }
procedure JvInterpreter_StrToDateTime(var Value: Variant; Args: TArgs);
begin
  Value := StrToDateTime(Args.Values[0]);
end;

{ function FormatDateTime(const Format: string; DateTime: TDateTime): string; }
procedure JvInterpreter_FormatDateTime(var Value: Variant; Args: TArgs);
begin
  Value := FormatDateTime(Args.Values[0], Args.Values[1]);
end;

{ procedure DateTimeToString(var Result: string; const Format: string; DateTime: TDateTime); }
procedure JvInterpreter_DateTimeToString(var Value: Variant; Args: TArgs);
begin
  DateTimeToString(string(TVarData(Args.Values[0]).vString), Args.Values[1], Args.Values[2]);
end;

{ function SysErrorMessage(ErrorCode: Integer): string; }
procedure JvInterpreter_SysErrorMessage(var Value: Variant; Args: TArgs);
begin
  Value := SysErrorMessage(Args.Values[0]);
end;

{ function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string; }
procedure JvInterpreter_GetLocaleStr(var Value: Variant; Args: TArgs);
begin
  Value := GetLocaleStr(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char; }
procedure JvInterpreter_GetLocaleChar(var Value: Variant; Args: TArgs);
begin
  Value := GetLocaleChar(Args.Values[0], Args.Values[1], string(Args.Values[2])[1]);
end;

{ procedure GetFormatSettings; }
procedure JvInterpreter_GetFormatSettings(var Value: Variant; Args: TArgs);
begin
  GetFormatSettings;
end;

{ function ExceptObject: TObject; }
procedure JvInterpreter_ExceptObject(var Value: Variant; Args: TArgs);
begin
  Value := O2V(ExceptObject);
end;

{ function ExceptAddr: Pointer; }
procedure JvInterpreter_ExceptAddr(var Value: Variant; Args: TArgs);
begin
  Value := P2V(ExceptAddr);
end;

{$IFDEF COMPILER3_UP}
{ function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer; Buffer: PChar; Size: Integer): Integer; }
procedure JvInterpreter_ExceptionErrorMessage(var Value: Variant; Args: TArgs);
begin
  Value := ExceptionErrorMessage(V2O(Args.Values[0]), V2P(Args.Values[1]), PChar(string(Args.Values[2])), Args.Values[3]);
end;
{$ENDIF COMPILER3_UP}

{ procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer); }
procedure JvInterpreter_ShowException(var Value: Variant; Args: TArgs);
begin
  ShowException(V2O(Args.Values[0]), V2P(Args.Values[1]));
end;

{ procedure Abort; }
procedure JvInterpreter_Abort(var Value: Variant; Args: TArgs);
begin
  Abort;
end;

{ procedure OutOfMemoryError; }
procedure JvInterpreter_OutOfMemoryError(var Value: Variant; Args: TArgs);
begin
  OutOfMemoryError;
end;

{ procedure Beep; }
procedure JvInterpreter_Beep(var Value: Variant; Args: TArgs);
begin
  Beep;
end;

{$IFDEF COMPILER3_UP}
{ function ByteType(const S: string; Index: Integer): TMbcsByteType; }
procedure JvInterpreter_ByteType(var Value: Variant; Args: TArgs);
begin
  Value := ByteType(Args.Values[0], Args.Values[1]);
end;

{ function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType; }
procedure JvInterpreter_StrByteType(var Value: Variant; Args: TArgs);
begin
  Value := StrByteType(PChar(string(Args.Values[0])), Args.Values[1]);
end;

{ function ByteToCharLen(const S: string; MaxLen: Integer): Integer; }
procedure JvInterpreter_ByteToCharLen(var Value: Variant; Args: TArgs);
begin
  Value := ByteToCharLen(Args.Values[0], Args.Values[1]);
end;

{ function CharToByteLen(const S: string; MaxLen: Integer): Integer; }
procedure JvInterpreter_CharToByteLen(var Value: Variant; Args: TArgs);
begin
  Value := CharToByteLen(Args.Values[0], Args.Values[1]);
end;

{ function ByteToCharIndex(const S: string; Index: Integer): Integer; }
procedure JvInterpreter_ByteToCharIndex(var Value: Variant; Args: TArgs);
begin
  Value := ByteToCharIndex(Args.Values[0], Args.Values[1]);
end;

{ function CharToByteIndex(const S: string; Index: Integer): Integer; }
procedure JvInterpreter_CharToByteIndex(var Value: Variant; Args: TArgs);
begin
  Value := CharToByteIndex(Args.Values[0], Args.Values[1]);
end;

{ function IsPathDelimiter(const S: string; Index: Integer): Boolean; }
procedure JvInterpreter_IsPathDelimiter(var Value: Variant; Args: TArgs);
begin
  Value := IsPathDelimiter(Args.Values[0], Args.Values[1]);
end;

{ function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean; }
procedure JvInterpreter_IsDelimiter(var Value: Variant; Args: TArgs);
begin
  Value := IsDelimiter(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function LastDelimiter(const Delimiters, S: string): Integer; }
procedure JvInterpreter_LastDelimiter(var Value: Variant; Args: TArgs);
begin
  Value := LastDelimiter(Args.Values[0], Args.Values[1]);
end;

{ function AnsiCompareFileName(const S1, S2: string): Integer; }
procedure JvInterpreter_AnsiCompareFileName(var Value: Variant; Args: TArgs);
begin
  Value := AnsiCompareFileName(Args.Values[0], Args.Values[1]);
end;

{ function AnsiLowerCaseFileName(const S: string): string; }
procedure JvInterpreter_AnsiLowerCaseFileName(var Value: Variant; Args: TArgs);
begin
  Value := AnsiLowerCaseFileName(Args.Values[0]);
end;

{ function AnsiUpperCaseFileName(const S: string): string; }
procedure JvInterpreter_AnsiUpperCaseFileName(var Value: Variant; Args: TArgs);
begin
  Value := AnsiUpperCaseFileName(Args.Values[0]);
end;

{ function AnsiPos(const Substr, S: string): Integer; }
procedure JvInterpreter_AnsiPos(var Value: Variant; Args: TArgs);
begin
  Value := AnsiPos(Args.Values[0], Args.Values[1]);
end;

{ function AnsiStrPos(Str, SubStr: PChar): PChar; }
procedure JvInterpreter_AnsiStrPos(var Value: Variant; Args: TArgs);
begin
  Value := string(AnsiStrPos(PChar(string(Args.Values[0])), PChar(string(Args.Values[1]))));
end;

{ function AnsiStrRScan(Str: PChar; Chr: Char): PChar; }
procedure JvInterpreter_AnsiStrRScan(var Value: Variant; Args: TArgs);
begin
  Value := string(AnsiStrRScan(PChar(string(Args.Values[0])), string(Args.Values[1])[1]));
end;

{ function AnsiStrScan(Str: PChar; Chr: Char): PChar; }
procedure JvInterpreter_AnsiStrScan(var Value: Variant; Args: TArgs);
begin
  Value := string(AnsiStrScan(PChar(string(Args.Values[0])), string(Args.Values[1])[1]));
end;

{ function LoadPackage(const Name: string): HMODULE; }
procedure JvInterpreter_LoadPackage(var Value: Variant; Args: TArgs);
begin
  Value := Integer(LoadPackage(Args.Values[0]));
end;

{ procedure UnloadPackage(Module: HMODULE); }
procedure JvInterpreter_UnloadPackage(var Value: Variant; Args: TArgs);
begin
  UnloadPackage(Args.Values[0]);
end;

{$IFDEF MSWINDOWS}
{ procedure RaiseLastWin32Error; }
procedure JvInterpreter_RaiseLastWin32Error(var Value: Variant; Args: TArgs);
begin
  RaiseLastWin32Error;
end;

{ function Win32Check(RetVal: BOOL): BOOL; }
procedure JvInterpreter_Win32Check(var Value: Variant; Args: TArgs);
begin
  Value := Win32Check(Args.Values[0]);
end;
{$ENDIF MSWINDOWS}
{$ENDIF COMPILER3_UP}


{ regional options }

(*
{ read CurrencyString: string }
procedure JvInterpreter_Read_CurrencyString(var Value: Variant; Args: TArgs);
begin
  Value := CurrencyString;
end;

{ write CurrencyString: string }
procedure JvInterpreter_Write_CurrencyString(var Value: Variant; Args: TArgs);
begin
  CurrencyString := Value;
end;

{ read CurrencyFormat: Byte }
procedure JvInterpreter_Read_CurrencyFormat(var Value: Variant; Args: TArgs);
begin
  Value := CurrencyFormat;
end;

{ write CurrencyFormat: Byte }
procedure JvInterpreter_Write_CurrencyFormat(var Value: Variant; Args: TArgs);
begin
  CurrencyFormat := Value;
end;

{ read NegCurrFormat: Byte }
procedure JvInterpreter_Read_NegCurrFormat(var Value: Variant; Args: TArgs);
begin
  Value := NegCurrFormat;
end;

{ write NegCurrFormat: Byte }
procedure JvInterpreter_Write_NegCurrFormat(var Value: Variant; Args: TArgs);
begin
  NegCurrFormat := Value;
end;

{ read ThousandSeparator }
procedure JvInterpreter_Read_ThousandSeparator(var Value: Variant; Args: TArgs);
begin
  Value := ThousandSeparator;
end;

{ write ThousandSeparator }
procedure JvInterpreter_Write_ThousandSeparator(var Value: Variant; Args: TArgs);
begin
  ThousandSeparator := string(Value)[1];
end;

{ read DecimalSeparator }
procedure JvInterpreter_Read_DecimalSeparator(var Value: Variant; Args: TArgs);
begin
  Value := DecimalSeparator;
end;

{ write DecimalSeparator }
procedure JvInterpreter_Write_DecimalSeparator(var Value: Variant; Args: TArgs);
begin
  DecimalSeparator := string(Value)[1];
end;

{ read CurrencyDecimals }
procedure JvInterpreter_Read_CurrencyDecimals(var Value: Variant; Args: TArgs);
begin
  Value := CurrencyDecimals;
end;

{ write CurrencyDecimals }
procedure JvInterpreter_Write_CurrencyDecimals(var Value: Variant; Args: TArgs);
begin
  CurrencyDecimals := Value;
end;

{ read DateSeparator }
procedure JvInterpreter_Read_DateSeparator(var Value: Variant; Args: TArgs);
begin
  Value := DateSeparator;
end;

{ write DateSeparator }
procedure JvInterpreter_Write_DateSeparator(var Value: Variant; Args: TArgs);
begin
  DateSeparator := string(Value)[1];
end;

{ read ShortDateFormat }
procedure JvInterpreter_Read_ShortDateFormat(var Value: Variant; Args: TArgs);
begin
  Value := ShortDateFormat;
end;

{ write ShortDateFormat }
procedure JvInterpreter_Write_ShortDateFormat(var Value: Variant; Args: TArgs);
begin
  ShortDateFormat := Value;
end;

{ read LongDateFormat }
procedure JvInterpreter_Read_LongDateFormat(var Value: Variant; Args: TArgs);
begin
  Value := LongDateFormat;
end;

{ write LongDateFormat }
procedure JvInterpreter_Write_LongDateFormat(var Value: Variant; Args: TArgs);
begin
  LongDateFormat := Value;
end;

{ read TimeSeparator }
procedure JvInterpreter_Read_TimeSeparator(var Value: Variant; Args: TArgs);
begin
  Value := TimeSeparator;
end;

{ write TimeSeparator }
procedure JvInterpreter_Write_TimeSeparator(var Value: Variant; Args: TArgs);
begin
  TimeSeparator := string(Value)[1];
end;

{ read TimeAMString }
procedure JvInterpreter_Read_TimeAMString(var Value: Variant; Args: TArgs);
begin
  Value := TimeAMString;
end;

{ write TimeAMString }
procedure JvInterpreter_Write_TimeAMString(var Value: Variant; Args: TArgs);
begin
  TimeAMString := Value;
end;

{ read TimePMString }
procedure JvInterpreter_Read_TimePMString(var Value: Variant; Args: TArgs);
begin
  Value := TimePMString;
end;

{ write TimePMString }
procedure JvInterpreter_Write_TimePMString(var Value: Variant; Args: TArgs);
begin
  TimePMString := Value;
end;

{ read ShortTimeFormat }
procedure JvInterpreter_Read_ShortTimeFormat(var Value: Variant; Args: TArgs);
begin
  Value := ShortTimeFormat;
end;

{ write ShortTimeFormat }
procedure JvInterpreter_Write_ShortTimeFormat(var Value: Variant; Args: TArgs);
begin
  ShortTimeFormat := Value;
end;

{ read LongTimeFormat }
procedure JvInterpreter_Read_LongTimeFormat(var Value: Variant; Args: TArgs);
begin
  Value := LongTimeFormat;
end;

{ write LongTimeFormat }
procedure JvInterpreter_Write_LongTimeFormat(var Value: Variant; Args: TArgs);
begin
  LongTimeFormat := Value;
end;

{$IFDEF COMPILER4_UP}
{ read TwoDigitYearCenturyWindow }
procedure JvInterpreter_Read_TwoDigitYearCenturyWindow(var Value: Variant; Args: TArgs);
begin
  Value := TwoDigitYearCenturyWindow;
end;

{ write TwoDigitYearCenturyWindow }
procedure JvInterpreter_Write_TwoDigitYearCenturyWindow(var Value: Variant; Args: TArgs);
begin
  TwoDigitYearCenturyWindow := Value;
end;

{ read ListSeparator }
procedure JvInterpreter_Read_ListSeparator(var Value: Variant; Args: TArgs);
begin
  Value := ListSeparator;
end;

{ write ListSeparator }
procedure JvInterpreter_Write_ListSeparator(var Value: Variant; Args: TArgs);
begin
  ListSeparator := string(Args.Values[0])[1];
end;
{$ENDIF COMPILER4_UP}

{ read ShortMonthNames }
procedure JvInterpreter_Read_ShortMonthNames(var Value: Variant; Args: TArgs);
begin
  Value := ShortMonthNames[Integer(Args.Values[0])];
end;

{ write ShortMonthNames }
procedure JvInterpreter_Write_ShortMonthNames(var Value: Variant; Args: TArgs);
begin
  ShortMonthNames[Integer(Args.Values[0])] := Value;
end;

{ read LongMonthNames }
procedure JvInterpreter_Read_LongMonthNames(var Value: Variant; Args: TArgs);
begin
  Value := LongMonthNames[Integer(Args.Values[0])];
end;

{ write LongMonthNames }
procedure JvInterpreter_Write_LongMonthNames(var Value: Variant; Args: TArgs);
begin
  LongMonthNames[Integer(Args.Values[0])] := Value;
end;

{ read ShortDayNames }
procedure JvInterpreter_Read_ShortDayNames(var Value: Variant; Args: TArgs);
begin
  Value := ShortDayNames[Integer(Args.Values[0])];
end;

{ write ShortDayNames }
procedure JvInterpreter_Write_ShortDayNames(var Value: Variant; Args: TArgs);
begin
  ShortDayNames[Integer(Args.Values[0])] := Value;
end;

{ read LongDayNames }
procedure JvInterpreter_Read_LongDayNames(var Value: Variant; Args: TArgs);
begin
  Value := LongDayNames[Integer(Args.Values[0])];
end;

{ write LongDayNames }
procedure JvInterpreter_Write_LongDayNames(var Value: Variant; Args: TArgs);
begin
  LongDayNames[Integer(Args.Values[0])] := Value;
end;

{$IFDEF COMPILER4_UP}
{ read EraNames }
procedure JvInterpreter_Read_EraNames(var Value: Variant; Args: TArgs);
begin
  Value := EraNames[Integer(Args.Values[0])];
end;

{ write EraNames }
procedure JvInterpreter_Write_EraNames(var Value: Variant; Args: TArgs);
begin
  EraNames[Integer(Args.Values[0])] := Value;
end;

{ read EraYearOffsets }
procedure JvInterpreter_Read_EraYearOffsets(var Value: Variant; Args: TArgs);
begin
  Value := EraYearOffsets[Integer(Args.Values[0])];
end;

{ write EraYearOffsets }
procedure JvInterpreter_Write_EraYearOffsets(var Value: Variant; Args: TArgs);
begin
  EraYearOffsets[Integer(Args.Values[0])] := Value;
end;
{$ENDIF COMPILER4_UP}
*)

type
  PSearchRec = ^TSearchRec;

procedure JvInterpreter_NewTSearchRec(var Value: Pointer);
begin
  New(PSearchRec(Value));
end;

procedure JvInterpreter_DisposeTSearchRec(const Value: Pointer);
begin
  Dispose(PSearchRec(Value));
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  with JvInterpreterAdapter do
  begin
   { Exception }
    AddClass('SysUtils', Exception, 'Exception');
    AddGet(Exception, 'Create', Exception_Create, 1, [varEmpty], varEmpty);
    AddGet(Exception, 'CreateFmt', Exception_CreateFmt, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(Exception, 'CreateRes', Exception_CreateRes, 1, [varEmpty], varEmpty);
    AddGet(Exception, 'CreateResFmt', Exception_CreateResFmt, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(Exception, 'CreateHelp', Exception_CreateHelp, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(Exception, 'CreateFmtHelp', Exception_CreateFmtHelp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(Exception, 'CreateResHelp', Exception_CreateResHelp, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(Exception, 'CreateResFmtHelp', Exception_CreateResFmtHelp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(Exception, 'HelpContext', Exception_Read_HelpContext, 0, [0], varEmpty);
    AddSet(Exception, 'HelpContext', Exception_Write_HelpContext, 0, [0]);
    AddGet(Exception, 'Message', Exception_Read_Message, 0, [0], varEmpty);
    AddSet(Exception, 'Message', Exception_Write_Message, 0, [0]);
   { EAbort }
    AddClass('SysUtils', EAbort, 'EAbort');
   { EOutOfMemory }
    AddClass('SysUtils', EOutOfMemory, 'EOutOfMemory');
   { EInOutError }
    AddClass('SysUtils', EInOutError, 'EInOutError');
   { EIntError }
    AddClass('SysUtils', EIntError, 'EIntError');
   { EDivByZero }
    AddClass('SysUtils', EDivByZero, 'EDivByZero');
   { ERangeError }
    AddClass('SysUtils', ERangeError, 'ERangeError');
   { EIntOverflow }
    AddClass('SysUtils', EIntOverflow, 'EIntOverflow');
   { EMathError }
    AddClass('SysUtils', EMathError, 'EMathError');
   { EInvalidOp }
    AddClass('SysUtils', EInvalidOp, 'EInvalidOp');
   { EZeroDivide }
    AddClass('SysUtils', EZeroDivide, 'EZeroDivide');
   { EOverflow }
    AddClass('SysUtils', EOverflow, 'EOverflow');
   { EUnderflow }
    AddClass('SysUtils', EUnderflow, 'EUnderflow');
   { EInvalidPointer }
    AddClass('SysUtils', EInvalidPointer, 'EInvalidPointer');
   { EInvalidCast }
    AddClass('SysUtils', EInvalidCast, 'EInvalidCast');
   { EConvertError }
    AddClass('SysUtils', EConvertError, 'EConvertError');
   { EAccessViolation }
    AddClass('SysUtils', EAccessViolation, 'EAccessViolation');
   { EPrivilege }
    AddClass('SysUtils', EPrivilege, 'EPrivilege');
   {$IFNDEF COMPILER6_UP}
   { EStackOverflow }
    AddClass('SysUtils', EStackOverflow, 'EStackOverflow');
   {$ENDIF COMPILER6_UP}
   { EControlC }
    AddClass('SysUtils', EControlC, 'EControlC');
   { EVariantError }
    AddClass('SysUtils', EVariantError, 'EVariantError');
   { EPropReadOnly }
    AddClass('SysUtils', EPropReadOnly, 'EPropReadOnly');
   { EPropWriteOnly }
    AddClass('SysUtils', EPropWriteOnly, 'EPropWriteOnly');
   { EExternalException }
    AddClass('SysUtils', EExternalException, 'EExternalException');
   {$IFDEF COMPILER3_UP}
   { EAssertionFailed }
    AddClass('SysUtils', EAssertionFailed, 'EAssertionFailed');
   {$IFNDEF PC_MAPPED_EXCEPTIONS} // Linux define symbol
   { EAbstractError }
    AddClass('SysUtils', EAbstractError, 'EAbstractError');
   {$ENDIF}
   { EIntfCastError }
    AddClass('SysUtils', EIntfCastError, 'EIntfCastError');
   { EInvalidContainer }
    AddClass('SysUtils', EInvalidContainer, 'EInvalidContainer');
   { EInvalidInsert }
    AddClass('SysUtils', EInvalidInsert, 'EInvalidInsert');
   { EPackageError }
    AddClass('SysUtils', EPackageError, 'EPackageError');
   {$IFDEF MSWINDOWS}
   { EWin32Error }
    AddClass('SysUtils', EWin32Error, 'EWin32Error');
   {$ENDIF MSWINDOWS}
   {$ENDIF COMPILER3_UP}

    AddFun('SysUtils', 'AllocMem', JvInterpreter_AllocMem, 1, [varEmpty], varEmpty);
   {$IFNDEF COMPILER6_UP}
    AddFun('SysUtils', 'NewStr', JvInterpreter_NewStr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'DisposeStr', JvInterpreter_DisposeStr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'AssignStr', JvInterpreter_AssignStr, 2, [varByRef, varEmpty], varEmpty);
    AddFun('SysUtils', 'AppendStr', JvInterpreter_AppendStr, 2, [varByRef, varEmpty], varEmpty);
   {$ENDIF COMPILER6_UP}
    AddFun('SysUtils', 'UpperCase', JvInterpreter_UpperCase, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'LowerCase', JvInterpreter_LowerCase, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'CompareStr', JvInterpreter_CompareStr, 2, [varEmpty, varEmpty], varEmpty);
   {$IFDEF COMPILER3_UP}
    AddFun('SysUtils', 'CompareMem', JvInterpreter_CompareMem, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
   {$ENDIF COMPILER3_UP}
    AddFun('SysUtils', 'CompareText', JvInterpreter_CompareText, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiUpperCase', JvInterpreter_AnsiUpperCase, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiLowerCase', JvInterpreter_AnsiLowerCase, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiCompareStr', JvInterpreter_AnsiCompareStr, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiCompareText', JvInterpreter_AnsiCompareText, 2, [varEmpty, varEmpty], varEmpty);
   {$IFDEF COMPILER3_UP}
    AddFun('SysUtils', 'AnsiStrComp', JvInterpreter_AnsiStrComp, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrIComp', JvInterpreter_AnsiStrIComp, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrLComp', JvInterpreter_AnsiStrLComp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrLIComp', JvInterpreter_AnsiStrLIComp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrLower', JvInterpreter_AnsiStrLower, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrUpper', JvInterpreter_AnsiStrUpper, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiLastChar', JvInterpreter_AnsiLastChar, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrLastChar', JvInterpreter_AnsiStrLastChar, 1, [varEmpty], varEmpty);
   {$ENDIF COMPILER3_UP}
    AddFun('SysUtils', 'Trim', JvInterpreter_Trim, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'TrimLeft', JvInterpreter_TrimLeft, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'TrimRight', JvInterpreter_TrimRight, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'QuotedStr', JvInterpreter_QuotedStr, 1, [varEmpty], varEmpty);
   {$IFDEF COMPILER3_UP}
    AddFun('SysUtils', 'AnsiQuotedStr', JvInterpreter_AnsiQuotedStr, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiExtractQuotedStr', JvInterpreter_AnsiExtractQuotedStr, 2, [varByRef, varEmpty], varEmpty);
   {$ENDIF COMPILER3_UP}
    AddFun('SysUtils', 'AdjustLineBreaks', JvInterpreter_AdjustLineBreaks, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'IsValidIdent', JvInterpreter_IsValidIdent, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'IntToStr', JvInterpreter_IntToStr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'IntToHex', JvInterpreter_IntToHex, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrToInt', JvInterpreter_StrToInt, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrToIntDef', JvInterpreter_StrToIntDef, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'LoadStr', JvInterpreter_LoadStr, 1, [varEmpty], varEmpty);
   // AddFun('SysUtils', 'FmtLoadStr', JvInterpreter_FmtLoadStr, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'FileOpen', JvInterpreter_FileOpen, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'FileCreate', JvInterpreter_FileCreate, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'FileRead', JvInterpreter_FileRead, 3, [varEmpty, varByRef, varEmpty], varEmpty);
    AddFun('SysUtils', 'FileWrite', JvInterpreter_FileWrite, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'FileSeek', JvInterpreter_FileSeek, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'FileClose', JvInterpreter_FileClose, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'FileAge', JvInterpreter_FileAge, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'FileExists', JvInterpreter_FileExists, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'FindFirst', JvInterpreter_FindFirst, 3, [varEmpty, varEmpty, varByRef], varEmpty);
    AddFun('SysUtils', 'FindNext', JvInterpreter_FindNext, 1, [varByRef], varEmpty);
    AddFun('SysUtils', 'FindClose', JvInterpreter_FindClose, 1, [varByRef], varEmpty);
    AddFun('SysUtils', 'FileGetDate', JvInterpreter_FileGetDate, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'FileSetDate', JvInterpreter_FileSetDate, 2, [varEmpty, varEmpty], varEmpty);
   {$IFDEF MSWINDOWS}
    AddFun('SysUtils', 'FileGetAttr', JvInterpreter_FileGetAttr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'FileSetAttr', JvInterpreter_FileSetAttr, 2, [varEmpty, varEmpty], varEmpty);
   {$ENDIF MSWINDOWS}
    AddFun('SysUtils', 'DeleteFile', JvInterpreter_DeleteFile, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'RenameFile', JvInterpreter_RenameFile, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'ChangeFileExt', JvInterpreter_ChangeFileExt, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'ExtractFilePath', JvInterpreter_ExtractFilePath, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'ExtractFileDir', JvInterpreter_ExtractFileDir, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'ExtractFileDrive', JvInterpreter_ExtractFileDrive, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'ExtractFileName', JvInterpreter_ExtractFileName, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'ExtractFileExt', JvInterpreter_ExtractFileExt, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'ExpandFileName', JvInterpreter_ExpandFileName, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'ExpandUNCFileName', JvInterpreter_ExpandUNCFileName, 1, [varEmpty], varEmpty);
   {$IFDEF COMPILER3_UP}
    AddFun('SysUtils', 'ExtractRelativePath', JvInterpreter_ExtractRelativePath, 2, [varEmpty, varEmpty], varEmpty);
   {$ENDIF COMPILER3_UP}
    AddFun('SysUtils', 'FileSearch', JvInterpreter_FileSearch, 2, [varEmpty, varEmpty], varEmpty);
   {$IFDEF MSWINDOWS}
    AddFun('SysUtils', 'DiskFree', JvInterpreter_DiskFree, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'DiskSize', JvInterpreter_DiskSize, 1, [varEmpty], varEmpty);
   {$ENDIF MSWINDOWS}
    AddFun('SysUtils', 'FileDateToDateTime', JvInterpreter_FileDateToDateTime, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'DateTimeToFileDate', JvInterpreter_DateTimeToFileDate, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'GetCurrentDir', JvInterpreter_GetCurrentDir, 0, [0], varEmpty);
    AddFun('SysUtils', 'SetCurrentDir', JvInterpreter_SetCurrentDir, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'CreateDir', JvInterpreter_CreateDir, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'RemoveDir', JvInterpreter_RemoveDir, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrLen', JvInterpreter_StrLen, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrEnd', JvInterpreter_StrEnd, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrMove', JvInterpreter_StrMove, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrCopy', JvInterpreter_StrCopy, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrECopy', JvInterpreter_StrECopy, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrLCopy', JvInterpreter_StrLCopy, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrPCopy', JvInterpreter_StrPCopy, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrPLCopy', JvInterpreter_StrPLCopy, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrCat', JvInterpreter_StrCat, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrLCat', JvInterpreter_StrLCat, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrComp', JvInterpreter_StrComp, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrIComp', JvInterpreter_StrIComp, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrLComp', JvInterpreter_StrLComp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrLIComp', JvInterpreter_StrLIComp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrScan', JvInterpreter_StrScan, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrRScan', JvInterpreter_StrRScan, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrPos', JvInterpreter_StrPos, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrUpper', JvInterpreter_StrUpper, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrLower', JvInterpreter_StrLower, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrPas', JvInterpreter_StrPas, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrAlloc', JvInterpreter_StrAlloc, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrBufSize', JvInterpreter_StrBufSize, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrNew', JvInterpreter_StrNew, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrDispose', JvInterpreter_StrDispose, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'Format', JvInterpreter_Format, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'FmtStr', JvInterpreter_FmtStr, 3, [varByRef, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrFmt', JvInterpreter_StrFmt, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrLFmt', JvInterpreter_StrLFmt, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'FormatBuf', JvInterpreter_FormatBuf, 5, [varByRef, varEmpty, varEmpty, varEmpty, varEmpty], varEmpty); 
    AddFun('SysUtils', 'FloatToStr', JvInterpreter_FloatToStr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'CurrToStr', JvInterpreter_CurrToStr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'FloatToStrF', JvInterpreter_FloatToStrF, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'CurrToStrF', JvInterpreter_CurrToStrF, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
   // AddFun('SysUtils', 'FloatToText', JvInterpreter_FloatToText, 6, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'FormatFloat', JvInterpreter_FormatFloat, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'FormatCurr', JvInterpreter_FormatCurr, 2, [varEmpty, varEmpty], varEmpty);
   // AddFun('SysUtils', 'FloatToTextFmt', JvInterpreter_FloatToTextFmt, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrToFloat', JvInterpreter_StrToFloat, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrToCurr', JvInterpreter_StrToCurr, 1, [varEmpty], varEmpty);
   // AddFun('SysUtils', 'TextToFloat', JvInterpreter_TextToFloat, 3, [varEmpty, varByRef, varEmpty], varEmpty);
   // AddFun('SysUtils', 'FloatToDecimal', JvInterpreter_FloatToDecimal, 5, [varByRef, varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
   { AddFun('SysUtils', 'DateTimeToTimeStamp', JvInterpreter_DateTimeToTimeStamp, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'TimeStampToDateTime', JvInterpreter_TimeStampToDateTime, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'MSecsToTimeStamp', JvInterpreter_MSecsToTimeStamp, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'TimeStampToMSecs', JvInterpreter_TimeStampToMSecs, 1, [varEmpty], varEmpty); }
    AddFun('SysUtils', 'EncodeDate', JvInterpreter_EncodeDate, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'EncodeTime', JvInterpreter_EncodeTime, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'DecodeDate', JvInterpreter_DecodeDate, 4, [varEmpty, varByRef, varByRef, varByRef], varEmpty);
    AddFun('SysUtils', 'DecodeTime', JvInterpreter_DecodeTime, 5, [varEmpty, varByRef, varByRef, varByRef, varByRef], varEmpty);
   { AddFun('SysUtils', 'DateTimeToSystemTime', JvInterpreter_DateTimeToSystemTime, 2, [varEmpty, varByRef], varEmpty);
    AddFun('SysUtils', 'SystemTimeToDateTime', JvInterpreter_SystemTimeToDateTime, 1, [varEmpty], varEmpty); }
    AddFun('SysUtils', 'DayOfWeek', JvInterpreter_DayOfWeek, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'Date', JvInterpreter_Date, 0, [0], varEmpty);
    AddFun('SysUtils', 'Time', JvInterpreter_Time, 0, [0], varEmpty);
    AddFun('SysUtils', 'Now', JvInterpreter_Now, 0, [0], varEmpty);
   {$IFDEF COMPILER3_UP}
    AddFun('SysUtils', 'IncMonth', JvInterpreter_IncMonth, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'IsLeapYear', JvInterpreter_IsLeapYear, 1, [varEmpty], varEmpty);
   {$ENDIF COMPILER3_UP}
    AddFun('SysUtils', 'DateToStr', JvInterpreter_DateToStr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'TimeToStr', JvInterpreter_TimeToStr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'DateTimeToStr', JvInterpreter_DateTimeToStr, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrToDate', JvInterpreter_StrToDate, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrToTime', JvInterpreter_StrToTime, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'StrToDateTime', JvInterpreter_StrToDateTime, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'FormatDateTime', JvInterpreter_FormatDateTime, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'DateTimeToString', JvInterpreter_DateTimeToString, 3, [varByRef, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'SysErrorMessage', JvInterpreter_SysErrorMessage, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'GetLocaleStr', JvInterpreter_GetLocaleStr, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'GetLocaleChar', JvInterpreter_GetLocaleChar, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'GetFormatSettings', JvInterpreter_GetFormatSettings, 0, [0], varEmpty);
    AddFun('SysUtils', 'ExceptObject', JvInterpreter_ExceptObject, 0, [0], varEmpty);
    AddFun('SysUtils', 'ExceptAddr', JvInterpreter_ExceptAddr, 0, [0], varEmpty);
   {$IFDEF COMPILER3_UP}
    AddFun('SysUtils', 'ExceptionErrorMessage', JvInterpreter_ExceptionErrorMessage, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
   {$ENDIF COMPILER3_UP}
    AddFun('SysUtils', 'ShowException', JvInterpreter_ShowException, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'Abort', JvInterpreter_Abort, 0, [0], varEmpty);
    AddFun('SysUtils', 'OutOfMemoryError', JvInterpreter_OutOfMemoryError, 0, [0], varEmpty);
    AddFun('SysUtils', 'Beep', JvInterpreter_Beep, 0, [0], varEmpty);
   {$IFDEF COMPILER3_UP}
    AddFun('SysUtils', 'ByteType', JvInterpreter_ByteType, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'StrByteType', JvInterpreter_StrByteType, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'ByteToCharLen', JvInterpreter_ByteToCharLen, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'CharToByteLen', JvInterpreter_CharToByteLen, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'ByteToCharIndex', JvInterpreter_ByteToCharIndex, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'CharToByteIndex', JvInterpreter_CharToByteIndex, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'IsPathDelimiter', JvInterpreter_IsPathDelimiter, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'IsDelimiter', JvInterpreter_IsDelimiter, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'LastDelimiter', JvInterpreter_LastDelimiter, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiCompareFileName', JvInterpreter_AnsiCompareFileName, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiLowerCaseFileName', JvInterpreter_AnsiLowerCaseFileName, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiUpperCaseFileName', JvInterpreter_AnsiUpperCaseFileName, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiPos', JvInterpreter_AnsiPos, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrPos', JvInterpreter_AnsiStrPos, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrRScan', JvInterpreter_AnsiStrRScan, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'AnsiStrScan', JvInterpreter_AnsiStrScan, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('SysUtils', 'LoadPackage', JvInterpreter_LoadPackage, 1, [varEmpty], varEmpty);
    AddFun('SysUtils', 'UnloadPackage', JvInterpreter_UnloadPackage, 1, [varEmpty], varEmpty);
   {$IFDEF MSWINDOWS}
    AddFun('SysUtils', 'RaiseLastWin32Error', JvInterpreter_RaiseLastWin32Error, 0, [0], varEmpty);
    AddFun('SysUtils', 'Win32Check', JvInterpreter_Win32Check, 1, [varEmpty], varEmpty);
   {$ENDIF MSWINDOWS}
   {$ENDIF COMPILER3_UP}
   { File open modes }
    AddConst('SysUtils', 'fmOpenRead', Integer(fmOpenRead));
    AddConst('SysUtils', 'fmOpenWrite', Integer(fmOpenWrite));
    AddConst('SysUtils', 'fmOpenReadWrite', Integer(fmOpenReadWrite));
   {$IFDEF MSWINDOWS}
    AddConst('SysUtils', 'fmShareCompat', Integer(fmShareCompat));
   {$ENDIF MSWINDOWS}
    AddConst('SysUtils', 'fmShareExclusive', Integer(fmShareExclusive));
    AddConst('SysUtils', 'fmShareDenyWrite', Integer(fmShareDenyWrite));
   {$IFDEF MSWINDOWS}
    AddConst('SysUtils', 'fmShareDenyRead', Integer(fmShareDenyRead));
   {$ENDIF MSWINDOWS}
    AddConst('SysUtils', 'fmShareDenyNone', Integer(fmShareDenyNone));
   { File attribute constants }
    AddConst('SysUtils', 'faReadOnly', Integer(faReadOnly));
    AddConst('SysUtils', 'faHidden', Integer(faHidden));
    AddConst('SysUtils', 'faSysFile', Integer(faSysFile));
    AddConst('SysUtils', 'faVolumeID', Integer(faVolumeID));
    AddConst('SysUtils', 'faDirectory', Integer(faDirectory));
    AddConst('SysUtils', 'faArchive', Integer(faArchive));
    AddConst('SysUtils', 'faAnyFile', Integer(faAnyFile));

    AddRec('SysUtils', 'TSearchRec', sizeof(TSearchRec), [
       RFD('Time', 0, varInteger),
       RFD('Size', 4, varInteger),
       RFD('Attr', 8, varInteger),
       RFD('Name', 12, varString),
       RFD('ExcludeAttr', 16, varInteger),
       RFD('FindHandle', 20, varInteger)
      ], JvInterpreter_NewTSearchRec, JvInterpreter_DisposeTSearchRec, nil
    );


    { regional options }
     { global variables are not supported by JvInterpreter :( }

  end;    { with }
end;    { RegisterJvInterpreterAdapter }

end.
