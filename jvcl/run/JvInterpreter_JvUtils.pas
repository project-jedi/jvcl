{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_JvUtils.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2003-03-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvInterpreter_JvUtils;

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  Dialogs, Menus, Math,
  {$IFNDEF COMPILER6_UP}
  FileCtrl,
  {$ENDIF}
  JvUtils, JvStrUtil, JvInterpreter_Windows;

{ function ReplaceAllStrings(S: string; Words, Frases: TStrings): string; }

procedure JvInterpreter_ReplaceAllStrings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReplaceAllStrings(Args.Values[0], V2O(Args.Values[1]) as TStrings, V2O(Args.Values[2]) as TStrings);
end;

{ function ReplaceStrings(S: string; PosBeg, Len: integer; Words, Frases: TStrings; var NewSelStart: integer): string; }

procedure JvInterpreter_ReplaceStrings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReplaceStrings(Args.Values[0], Args.Values[1], Args.Values[2], V2O(Args.Values[3]) as TStrings,
    V2O(Args.Values[4]) as TStrings, TVarData(Args.Values[5]).vInteger);
end;

{ function CountOfLines(const S: string): integer; }

procedure JvInterpreter_CountOfLines(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CountOfLines(Args.Values[0]);
end;

{ procedure DeleteEmptyLines(Ss: TStrings); }

procedure JvInterpreter_DeleteEmptyLines(var Value: Variant; Args: TJvInterpreterArgs);
begin
  DeleteEmptyLines(V2O(Args.Values[0]) as TStrings);
end;

{ procedure SQLAddWhere(SQL: TStrings; const where: string); }

procedure JvInterpreter_SQLAddWhere(var Value: Variant; Args: TJvInterpreterArgs);
begin
  SQLAddWhere(V2O(Args.Values[0]) as TStrings, Args.Values[1]);
end;

{ function ResSaveToFile(const Typ, Name: string; const Compressed: boolean; const FileName: string): boolean; }

procedure JvInterpreter_ResSaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ResSaveToFile(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ function ResSaveToFileEx(Instance: HINST; Typ, Name: PChar; const Compressed: boolean; const FileName: string): boolean; }

procedure JvInterpreter_ResSaveToFileEx(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ResSaveToFileEx(Args.Values[0], PChar(string(Args.Values[1])), PChar(string(Args.Values[2])),
    Args.Values[3], Args.Values[4]);
end;

{ function ResSaveToString(Instance: HINST; const Typ, Name: string; var S: string): boolean; }

procedure JvInterpreter_ResSaveToString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ResSaveToString(Args.Values[0], Args.Values[1], Args.Values[2], string(TVarData(Args.Values[3]).vString));
end;

{ function Execute(const CommandLine, WorkingDirectory: string): integer; }

procedure JvInterpreter_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Execute(Args.Values[0], Args.Values[1]);
end;

{ function IniReadSection(const IniFileName: TFileName; const Section: string; Ss: TStrings): boolean; }

procedure JvInterpreter_IniReadSection(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := IniReadSection(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

{ function LoadTextFile(const FileName: TFileName): string; }

procedure JvInterpreter_LoadTextFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := LoadTextFile(Args.Values[0]);
end;

{ procedure SaveTextFile(const FileName: TFileName; const Source: string); }

procedure JvInterpreter_SaveTextFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  SaveTextFile(Args.Values[0], Args.Values[1]);
end;

{ function ReadFolder(const Folder, Mask: TFileName; FileList: TStrings): integer; }

procedure JvInterpreter_ReadFolder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReadFolder(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

procedure JvInterpreter_ReadFolders(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReadFolders(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{$IFDEF COMPILER3_UP}

{ function TargetFileName(const FileName: TFileName): TFileName; }

procedure JvInterpreter_TargetFileName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TargetFileName(Args.Values[0]);
end;

{ function ResolveLink(const hwnd: HWND; const LinkFile: TFileName; var FileName: TFileName): HRESULT; }

procedure JvInterpreter_ResolveLink(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ResolveLink(Args.Values[0], Args.Values[1], TFileName(TVarData(Args.Values[2]).vString));
end;

{$ENDIF COMPILER3_UP}

{ procedure LoadIcoToImage(ALarge, ASmall: TImageList; const NameRes: string); }

procedure JvInterpreter_LoadIcoToImage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  LoadIcoToImage(V2O(Args.Values[0]) as TImageList, V2O(Args.Values[1]) as TImageList, Args.Values[2]);
end;

{ procedure RATextOut(Canvas: TCanvas; const R, RClip: TRect; const S: string); }

procedure JvInterpreter_RATextOut(var Value: Variant; Args: TJvInterpreterArgs);
begin
  RATextOut(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Var2Rect(Args.Values[2]), Args.Values[3]);
end;

{ function RATextOutEx(Canvas: TCanvas; const R, RClip: TRect; const S: string; const CalcHeight: boolean): integer; }

procedure JvInterpreter_RATextOutEx(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := RATextOutEx(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Var2Rect(Args.Values[2]),
    Args.Values[3], Args.Values[4]);
end;

{ function RATextCalcHeight(Canvas: TCanvas; const R: TRect; const S: string): integer; }

procedure JvInterpreter_RATextCalcHeight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := RATextCalcHeight(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Args.Values[2]);
end;

{ procedure Roughed(ACanvas: TCanvas; const ARect: TRect; const AVert: boolean); }

procedure JvInterpreter_Roughed(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Roughed(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Args.Values[2]);
end;

{ function BitmapFromBitmap(SrcBitmap: TBitmap; const AWidth, AHeight, index: integer): TBitmap; }

procedure JvInterpreter_BitmapFromBitmap(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(BitmapFromBitmap(V2O(Args.Values[0]) as TBitmap, Args.Values[1], Args.Values[2], Args.Values[3]));
end;

{ function TextWidth(AStr: string): integer; }

procedure JvInterpreter_TextWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TextWidth(Args.Values[0]);
end;

{ function DefineCursor(Identifier: PChar): TCursor; }

procedure JvInterpreter_DefineCursor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := DefineCursor(PChar(string(Args.Values[0])));
end;

{ function FindFormByClassName(FormClassName: string): TForm; }

procedure JvInterpreter_FindFormByClassName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(FindFormByClassName(Args.Values[0]));
end;

{ function FindByTag(WinControl: TWinControl; ComponentClass: TComponentClass; const Tag: integer): TComponent; }

procedure JvInterpreter_FindByTag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(FindByTag(V2O(Args.Values[0]) as TWinControl, TComponentClass(V2C(Args.Values[1])), Args.Values[2]));
end;

{ function ControlAtPos2(Parent: TWinControl; X, Y: integer): TControl; }

procedure JvInterpreter_ControlAtPos2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(ControlAtPos2(V2O(Args.Values[0]) as TWinControl, Args.Values[1], Args.Values[2]));
end;

{ function RBTag(Parent: TWinControl): integer; }

procedure JvInterpreter_RBTag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := RBTag(V2O(Args.Values[0]) as TWinControl);
end;

{ function AppMinimized: boolean; }

procedure JvInterpreter_AppMinimized(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := AppMinimized;
end;

{ function MsgDlg2(const Msg, ACaption: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpContext: integer; Control: TWinControl): Integer; }

procedure JvInterpreter_MsgDlg2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := MsgDlg2(Args.Values[0], Args.Values[1], Args.Values[2], TMsgDlgButtons(Word(V2S(Args.Values[3]))),
    Args.Values[4], V2O(Args.Values[5]) as TWinControl);
end;

{ function MsgDlgDef(const Msg, ACaption: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; HelpContext: integer; Control: TWinControl): Integer; }

procedure JvInterpreter_MsgDlgDef(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := MsgDlgDef(Args.Values[0], Args.Values[1], Args.Values[2], TMsgDlgButtons(Word(V2S(Args.Values[3]))),
    Args.Values[4], Args.Values[5], V2O(Args.Values[6]) as TWinControl);
end;

{ procedure Delay(MSec: longword); }

procedure JvInterpreter_Delay(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Delay(Args.Values[0]);
end;

(*
{ procedure CenterHor(Parent: TControl; MinLeft: integer; Controls: array of TControl); }
procedure JvInterpreter_CenterHor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  CenterHor(V2O(Args.Values[0]) as TControl, Args.Values[1], Args.Values[2]);
end;
*)

{ procedure EnableControls(Control: TWinControl; const Enable: boolean); }

procedure JvInterpreter_EnableControls(var Value: Variant; Args: TJvInterpreterArgs);
begin
  EnableControls(V2O(Args.Values[0]) as TWinControl, Args.Values[1]);
end;

{ procedure EnableMenuItems(MenuItem: TMenuItem; const Tag: integer; const Enable: boolean); }

procedure JvInterpreter_EnableMenuItems(var Value: Variant; Args: TJvInterpreterArgs);
begin
  EnableMenuItems(V2O(Args.Values[0]) as TMenuItem, Args.Values[1], Args.Values[2]);
end;

(*
{ procedure ExpandWidth(Parent: TControl; MinWidth: integer; Controls: array of TControl); }
procedure JvInterpreter_ExpandWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  ExpandWidth(V2O(Args.Values[0]) as TControl, Args.Values[1], Args.Values[2]);
end;
*)

{ function PanelBorder(Panel: TCustomPanel): integer; }

procedure JvInterpreter_PanelBorder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := PanelBorder(V2O(Args.Values[0]) as TCustomPanel);
end;

{ function Pixels(Control: TControl; APixels: integer): integer; }

procedure JvInterpreter_Pixels(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Pixels(V2O(Args.Values[0]) as TControl, Args.Values[1]);
end;

{ procedure SetChildPropOrd(Owner: TComponent; PropName: string; Value: Longint); }

procedure JvInterpreter_SetChildPropOrd(var Value: Variant; Args: TJvInterpreterArgs);
begin
  SetChildPropOrd(V2O(Args.Values[0]) as TComponent, Args.Values[1], Args.Values[2]);
end;

{ procedure Error(const Message: string); }

procedure JvInterpreter_Error(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Error(Args.Values[0]);
end;

{ procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean; var PlainItem: string; var Width: Integer; CalcWidth: Boolean); }

procedure JvInterpreter_ItemHtDrawEx(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPILER5_UP}
  ItemHtDrawEx(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Word(V2S(Args.Values[2]))),
    Args.Values[3], Args.Values[4], string(TVarData(Args.Values[5]).vString), TVarData(Args.Values[6]).vInteger,
    Args.Values[7]);
  {$ELSE}
  ItemHtDrawEx(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Byte(V2S(Args.Values[2]))),
    Args.Values[3], Args.Values[4], string(TVarData(Args.Values[5]).vString), TVarData(Args.Values[6]).vInteger,
    Args.Values[7]);
  {$ENDIF}
end;

{ function ItemHtDraw(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean): string; }

procedure JvInterpreter_ItemHtDraw(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPILER5_UP}
  Value := ItemHtDraw(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]),
    TOwnerDrawState(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
  {$ELSE}
  Value := ItemHtDraw(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]),
    TOwnerDrawState(Byte(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
  {$ENDIF}
end;

{ function ItemHtWidth(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean): Integer; }

procedure JvInterpreter_ItemHtWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPILER5_UP}
  Value := ItemHtWidth(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]),
    TOwnerDrawState(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
  {$ELSE}
  Value := ItemHtWidth(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]),
    TOwnerDrawState(Byte(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
  {$ENDIF}
end;

{ function ItemHtPlain(const Text: string): string; }

procedure JvInterpreter_ItemHtPlain(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ItemHtPlain(Args.Values[0]);
end;

{ procedure ClearList(List: TList); }

procedure JvInterpreter_ClearList(var Value: Variant; Args: TJvInterpreterArgs);
begin
  ClearList(V2O(Args.Values[0]) as TList);
end;

{ procedure MemStreamToClipBoard(MemStream: TMemoryStream; const Format: word); }

procedure JvInterpreter_MemStreamToClipBoard(var Value: Variant; Args: TJvInterpreterArgs);
begin
  MemStreamToClipBoard(V2O(Args.Values[0]) as TMemoryStream, Args.Values[1]);
end;

{ procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format: word); }

procedure JvInterpreter_ClipBoardToMemStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  ClipBoardToMemStream(V2O(Args.Values[0]) as TMemoryStream, Args.Values[1]);
end;

{ function GetPropType(Obj: TObject; const PropName: string): TTypeKind; }

procedure JvInterpreter_GetPropType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetPropType(V2O(Args.Values[0]), Args.Values[1]);
end;

{ function GetPropStr(Obj: TObject; const PropName: string): string; }

procedure JvInterpreter_GetPropStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetPropStr(V2O(Args.Values[0]), Args.Values[1]);
end;

{ function GetPropOrd(Obj: TObject; const PropName: string): Integer; }

procedure JvInterpreter_GetPropOrd(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetPropOrd(V2O(Args.Values[0]), Args.Values[1]);
end;

{ function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; }

procedure JvInterpreter_CompareMem(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CompareMem(V2P(Args.Values[0]), V2P(Args.Values[1]), Args.Values[2]);
end;

{ procedure ShowMenu(Form: TForm; MenuAni: TMenuAnimation); }

procedure JvInterpreter_ShowMenu(var Value: Variant; Args: TJvInterpreterArgs);
begin
  ShowMenu(V2O(Args.Values[0]) as TForm, Args.Values[1]);
end;

{****************************** RAUtilsW *******************************}
{ function GetWordOnPos(const S: string; const P: integer): string; }

procedure JvInterpreter_GetWordOnPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetWordOnPos(Args.Values[0], Args.Values[1]);
end;

{ function GetWordOnPosEx(const S: string; const P: integer; var iBeg, iEnd: integer): string; }

procedure JvInterpreter_GetWordOnPosEx(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetWordOnPosEx(Args.Values[0], Args.Values[1], TVarData(Args.Values[2]).vInteger,
    TVarData(Args.Values[3]).vInteger);
end;

{ function SubStr(const S: string; const index: integer; const Separator: string): string; }

procedure JvInterpreter_SubStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := SubStr(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function SubStrEnd(const S: string; const index: integer; const Separator: string): string; }

procedure JvInterpreter_SubStrEnd(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := SubStrEnd(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

(*
{ function SubWord(P: PChar; var P2: PChar): string; }
procedure JvInterpreter_SubWord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := SubWord(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])));
end;
*)

{ function NumberByWord(const N: longint): string; }

procedure JvInterpreter_NumberByWord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := NumberByWord(Args.Values[0]);
end;

{ function GetLineByPos(const S: string; const Pos: integer): integer; }

procedure JvInterpreter_GetLineByPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetLineByPos(Args.Values[0], Args.Values[1]);
end;

{ procedure GetXYByPos(const S: string; const Pos: integer; var X, Y: integer); }

procedure JvInterpreter_GetXYByPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  GetXYByPos(Args.Values[0], Args.Values[1], TVarData(Args.Values[2]).vInteger, TVarData(Args.Values[3]).vInteger);
end;

{ function ReplaceString(S: string; const OldPattern, NewPattern: string): string; }

procedure JvInterpreter_ReplaceString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReplaceString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ConcatSep(const S, S2, Separator: string): string; }

procedure JvInterpreter_ConcatSep(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ConcatSep(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ConcatLeftSep(const S, S2, Separator: string): string; }

procedure JvInterpreter_ConcatLeftSep(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ConcatLeftSep(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function MinimizeString(const S: string; const MaxLen: integer): string; }

procedure JvInterpreter_MinimizeString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := MinimizeString(Args.Values[0], Args.Values[1]);
end;

{ procedure Dos2Win(var S: string); }

procedure JvInterpreter_Dos2Win(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Dos2Win(string(TVarData(Args.Values[0]).vString));
end;

{ procedure Win2Dos(var S: string); }

procedure JvInterpreter_Win2Dos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Win2Dos(string(TVarData(Args.Values[0]).vString));
end;

{ function Dos2WinRes(const S: string): string; }

procedure JvInterpreter_Dos2WinRes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Dos2WinRes(Args.Values[0]);
end;

{ function Win2DosRes(const S: string): string; }

procedure JvInterpreter_Win2DosRes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Win2DosRes(Args.Values[0]);
end;

{ function Win2Koi(const S: string): string; }

procedure JvInterpreter_Win2Koi(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Win2Koi(Args.Values[0]);
end;

{ function Spaces(const N: integer): string; }

procedure JvInterpreter_Spaces(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Spaces(Args.Values[0]);
end;

{ function AddSpaces(const S: string; const N: integer): string; }

procedure JvInterpreter_AddSpaces(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := AddSpaces(Args.Values[0], Args.Values[1]);
end;

{ function LastDate(const Dat: TDateTime): string; }

procedure JvInterpreter_LastDate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := LastDate(Args.Values[0]);
end;

{ function CurrencyToStr(const Cur: currency): string; }

procedure JvInterpreter_CurrencyToStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CurrencyToStr(Args.Values[0]);
end;

{ function Cmp(const S1, S2: string): boolean; }

procedure JvInterpreter_Cmp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Cmp(Args.Values[0], Args.Values[1]);
end;

{ function StringCat(var S1: string; S2: string): string; }

procedure JvInterpreter_StringCat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := StringCat(string(TVarData(Args.Values[0]).vString), Args.Values[1]);
end;

{ function HasChar(const Ch: Char; const S: string): boolean; }

procedure JvInterpreter_HasChar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasChar(string(Args.Values[0])[1], Args.Values[1]);
end;

{ function HasAnyChar(const Chars: string; const S: string): boolean; }

procedure JvInterpreter_HasAnyChar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasAnyChar(Args.Values[0], Args.Values[1]);
end;

(*
{ function CharInSet(const Ch: Char; const SetOfChar: TSetOfChar): boolean; }
procedure JvInterpreter_CharInSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CharInSet(string(Args.Values[0])[1], Args.Values[1]);
end;
*)

{ function CountOfChar(const Ch: Char; const S: string): Integer; }

procedure JvInterpreter_CountOfChar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CountOfChar(string(Args.Values[0])[1], Args.Values[1]);
end;

{ function DefStr(const S: string; Default: string): string; }

procedure JvInterpreter_DefStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := DefStr(Args.Values[0], Args.Values[1]);
end;

{ function GetWinDir: TFileName; }

procedure JvInterpreter_GetWinDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetWinDir;
end;

{ function GetTempDir: string; }

procedure JvInterpreter_GetTempDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetTempDir;
end;

{ function GenTempFileName(FileName: string): string; }

procedure JvInterpreter_GenTempFileName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GenTempFileName(Args.Values[0]);
end;

{ function GenTempFileNameExt(FileName: string; const FileExt: string): string; }

procedure JvInterpreter_GenTempFileNameExt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GenTempFileNameExt(Args.Values[0], Args.Values[1]);
end;

{ function ClearDir(const Dir: string): boolean; }

procedure JvInterpreter_ClearDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ClearDir(Args.Values[0]);
end;

{ function DeleteDir(const Dir: string): boolean; }

procedure JvInterpreter_DeleteDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := DeleteDir(Args.Values[0]);
end;

{ function FileEquMask(FileName, Mask: TFileName): boolean; }

procedure JvInterpreter_FileEquMask(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FileEquMask(Args.Values[0], Args.Values[1]);
end;

{ function FileEquMasks(FileName, Masks: TFileName): boolean; }

procedure JvInterpreter_FileEquMasks(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FileEquMasks(Args.Values[0], Args.Values[1]);
end;

{ procedure DeleteFiles(const Folder: TFileName; const Masks: string); }

procedure JvInterpreter_DeleteFiles(var Value: Variant; Args: TJvInterpreterArgs);
begin
  DeleteFiles(Args.Values[0], Args.Values[1]);
end;

{ function LZFileExpand(const FileSource, FileDest: string): boolean; }

procedure JvInterpreter_LZFileExpand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := LZFileExpand(Args.Values[0], Args.Values[1]);
end;

{ function FileGetInfo(FileName: TFileName; var SearchRec: TSearchRec): boolean; }

procedure JvInterpreter_FileGetInfo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FileGetInfo(Args.Values[0], TSearchRec(V2R(Args.Values[1])^));
end;

{ function HasSubFolder(APath: TFileName): boolean; }

procedure JvInterpreter_HasSubFolder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasSubFolder(Args.Values[0]);
end;

{ function IsEmptyFolder(APath: TFileName): boolean; }

procedure JvInterpreter_IsEmptyFolder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := IsEmptyFolder(Args.Values[0]);
end;

{ procedure AddSlash(var Dir: TFileName); }

procedure JvInterpreter_AddSlash(var Value: Variant; Args: TJvInterpreterArgs);
begin
  AddSlash(TFileName(TVarData(Args.Values[0]).vString));
end;

{ function AddSlash2(const Dir: TFileName): string; }

procedure JvInterpreter_AddSlash2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := AddSlash2(Args.Values[0]);
end;

{ function AddPath(const FileName, Path: TFileName): TFileName; }

procedure JvInterpreter_AddPath(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := AddPath(Args.Values[0], Args.Values[1]);
end;

{ function BrowseForFolder(const Handle: HWnd; const Title: string; var Folder: string): boolean; }

{$IFNDEF BCB1}
procedure JvInterpreter_BrowseForFolder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := BrowseForFolder(Args.Values[0], Args.Values[1], string(TVarData(Args.Values[2]).vString));
end;
{$ENDIF BCB1}

{ function DeleteReadOnlyFile(const FileName: TFileName): boolean; }

procedure JvInterpreter_DeleteReadOnlyFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := DeleteReadOnlyFile(Args.Values[0]);
end;

{ function HasParam(const Param: string): boolean; }

procedure JvInterpreter_HasParam(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasParam(Args.Values[0]);
end;

{ function HasSwitch(const Param: string): boolean; }

procedure JvInterpreter_HasSwitch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasSwitch(Args.Values[0]);
end;

{ function Switch(const Param: string): string; }

procedure JvInterpreter_Switch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Switch(Args.Values[0]);
end;

{ function ExePath: TFileName; }

procedure JvInterpreter_ExePath(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ExePath;
end;

{ function CopyDir(const SourceDir, DestDir: TFileName): Boolean; }

procedure JvInterpreter_CopyDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CopyDir(Args.Values[0], Args.Values[1]);
end;

{ function TTFontSelected(const DC: HDC): boolean; }

procedure JvInterpreter_TTFontSelected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTFontSelected(Args.Values[0]);
end;

{ function TrueInflateRect(const R: TRect; const I: integer): TRect; }

procedure JvInterpreter_TrueInflateRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TrueInflateRect(Var2Rect(Args.Values[0]), Args.Values[1]));
end;

{ procedure SetWindowTop(const Handle: HWND; const Top: boolean); }

procedure JvInterpreter_SetWindowTop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  SetWindowTop(Args.Values[0], Args.Values[1]);
end;

{ function KeyPressed(VK: integer): boolean; }

procedure JvInterpreter_KeyPressed(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := KeyPressed(Args.Values[0]);
end;

{ function Max(x, y: integer): integer; }

procedure JvInterpreter_Max(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Max(Args.Values[0], Args.Values[1]);
end;

{ function Min(x, y: integer): integer; }

procedure JvInterpreter_Min(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Min(Args.Values[0], Args.Values[1]);
end;

{ procedure SwapInt(var Int1, Int2: Integer); }

procedure JvInterpreter_SwapInt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  SwapInt(TVarData(Args.Values[0]).vInteger, TVarData(Args.Values[1]).vInteger);
end;

{ function IntPower(Base, Exponent: integer): integer; }

procedure JvInterpreter_IntPower(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := IntPower(Args.Values[0], Args.Values[1]);
end;

{ function ChangeTopException(E: TObject): TObject; }

procedure JvInterpreter_ChangeTopException(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(ChangeTopException(V2O(Args.Values[0])));
end;

{ function MakeValidFileName(const FileName: TFileName; const ReplaceBadChar: Char): TFileName; }

procedure JvInterpreter_MakeValidFileName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := MakeValidFileName(Args.Values[0], string(Args.Values[1])[1]);
end;

{ function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer; }

procedure JvInterpreter_AnsiStrLIComp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := AnsiStrLIComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ function Var2Type(V: Variant; const VarType: integer): variant; }

procedure JvInterpreter_Var2Type(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Var2Type(Args.Values[0], Args.Values[1]);
end;

{ function VarToInt(V: Variant): Integer; }

procedure JvInterpreter_VarToInt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarToInt(Args.Values[0]);
end;

{ function GetParameter: string; }

procedure JvInterpreter_GetParameter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetParameter;
end;

{ function GetLongFileName(FileName: string): string; }

procedure JvInterpreter_GetLongFileName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetLongFileName(Args.Values[0]);
end;

{ function DirectoryExists(const Name: string): Boolean; }

procedure JvInterpreter_DirectoryExists(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := DirectoryExists(Args.Values[0]);
end;

{ procedure ForceDirectories(Dir: string); }

procedure JvInterpreter_ForceDirectories(var Value: Variant; Args: TJvInterpreterArgs);
begin
  ForceDirectories(Args.Values[0]);
end;

{ function FileNewExt(const FileName, NewExt: TFileName): TFileName; }

procedure JvInterpreter_FileNewExt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FileNewExt(Args.Values[0], Args.Values[1]);
end;

{ function GetComputerID: string; }

procedure JvInterpreter_GetComputerID(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetComputerID;
end;

{ function AddPaths(const PathList, Path: string): string; }

procedure JvInterpreter_AddPaths(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := AddPaths(Args.Values[0], Args.Values[1]);
end;

{ function ParentPath(const Path: TFileName): TFileName; }

procedure JvInterpreter_ParentPath(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ParentPath(Args.Values[0]);
end;

{ function FindInPath(const FileName, PathList: string): TFileName; }

procedure JvInterpreter_FindInPath(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FindInPath(Args.Values[0], Args.Values[1]);
end;

{ procedure PrepareIniSection(SS: TStrings); }

procedure JvInterpreter_PrepareIniSection(var Value: Variant; Args: TJvInterpreterArgs);
begin
  PrepareIniSection(V2O(Args.Values[0]) as TStrings);
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cJvUtils = 'JvUtils';
  cMath = 'Math';
  cJvStrUtil = 'JvStrUtil';
  cFileCtrl = 'FileCtrl';
begin
  with JvInterpreterAdapter do
  begin
    AddFun(cJvUtils, 'ReplaceAllStrings', JvInterpreter_ReplaceAllStrings, 3, [varString, varObject, varObject], varEmpty);
    AddFun(cJvUtils, 'ReplaceStrings', JvInterpreter_ReplaceStrings, 6, [varString, varInteger, varInteger, varObject,
      varObject, varInteger or varByRef], varEmpty);
    AddFun(cJvUtils, 'CountOfLines', JvInterpreter_CountOfLines, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'DeleteEmptyLines', JvInterpreter_DeleteEmptyLines, 1, [varObject], varEmpty);
    AddFun(cJvUtils, 'SQLAddWhere', JvInterpreter_SQLAddWhere, 2, [varObject, varString], varEmpty);
    AddFun(cJvUtils, 'ResSaveToFile', JvInterpreter_ResSaveToFile, 4, [varString, varString, varBoolean, varString],
      varEmpty);
    AddFun(cJvUtils, 'ResSaveToFileEx', JvInterpreter_ResSaveToFileEx, 5, [varEmpty, varEmpty, varEmpty, varBoolean,
      varString], varEmpty);
    AddFun(cJvUtils, 'ResSaveToString', JvInterpreter_ResSaveToString, 4, [varEmpty, varString, varString, varString or
      varByRef], varEmpty);
    AddFun(cJvUtils, 'Execute', JvInterpreter_Execute, 2, [varString, varString], varEmpty);
    AddFun(cJvUtils, 'IniReadSection', JvInterpreter_IniReadSection, 3, [varEmpty, varString, varObject], varEmpty);
    AddFun(cJvUtils, 'LoadTextFile', JvInterpreter_LoadTextFile, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'SaveTextFile', JvInterpreter_SaveTextFile, 2, [varEmpty, varString], varEmpty);
    AddFun(cJvUtils, 'ReadFolder', JvInterpreter_ReadFolder, 3, [varEmpty, varEmpty, varObject], varEmpty);
    AddFun(cJvUtils, 'ReadFolders', JvInterpreter_ReadFolders, 2, [varEmpty, varObject], varEmpty);
    {$IFDEF COMPILER3_UP}
    AddFun(cJvUtils, 'TargetFileName', JvInterpreter_TargetFileName, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'ResolveLink', JvInterpreter_ResolveLink, 3, [varEmpty, varEmpty, varEmpty or varByRef],
      varEmpty);
    {$ENDIF COMPILER3_UP}
    AddFun(cJvUtils, 'LoadIcoToImage', JvInterpreter_LoadIcoToImage, 3, [varObject, varObject, varString], varEmpty);
    AddFun(cJvUtils, 'RATextOut', JvInterpreter_RATextOut, 4, [varObject, varEmpty, varEmpty, varString], varEmpty);
    AddFun(cJvUtils, 'RATextOutEx', JvInterpreter_RATextOutEx, 5, [varObject, varEmpty, varEmpty, varString,
      varBoolean], varEmpty);
    AddFun(cJvUtils, 'RATextCalcHeight', JvInterpreter_RATextCalcHeight, 3, [varObject, varEmpty, varString],
      varEmpty);
    AddFun(cJvUtils, 'Roughed', JvInterpreter_Roughed, 3, [varObject, varEmpty, varBoolean], varEmpty);
    AddFun(cJvUtils, 'BitmapFromBitmap', JvInterpreter_BitmapFromBitmap, 4, [varObject, varInteger, varInteger,
      varInteger], varEmpty);
    AddFun(cJvUtils, 'TextWidth', JvInterpreter_TextWidth, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'DefineCursor', JvInterpreter_DefineCursor, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'FindFormByClassName', JvInterpreter_FindFormByClassName, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'FindByTag', JvInterpreter_FindByTag, 3, [varObject, varEmpty, varInteger], varEmpty);
    AddFun(cJvUtils, 'ControlAtPos2', JvInterpreter_ControlAtPos2, 3, [varObject, varInteger, varInteger], varEmpty);
    AddFun(cJvUtils, 'RBTag', JvInterpreter_RBTag, 1, [varObject], varEmpty);
    AddFun(cJvUtils, 'AppMinimized', JvInterpreter_AppMinimized, 0, [0], varEmpty);
    AddFun(cJvUtils, 'MsgDlg2', JvInterpreter_MsgDlg2, 6, [varString, varString, varEmpty, varEmpty, varInteger,
      varObject], varEmpty);
    AddFun(cJvUtils, 'MsgDlgDef', JvInterpreter_MsgDlgDef, 7, [varString, varString, varEmpty, varEmpty, varEmpty,
      varInteger, varObject], varEmpty);
    AddFun(cJvUtils, 'Delay', JvInterpreter_Delay, 1, [varEmpty], varEmpty);
    //AddFun(cJvUtils, 'CenterHor', JvInterpreter_CenterHor, 3, [varObject, varInteger, varEmpty], nil);
    AddFun(cJvUtils, 'EnableControls', JvInterpreter_EnableControls, 2, [varObject, varBoolean], varEmpty);
    AddFun(cJvUtils, 'EnableMenuItems', JvInterpreter_EnableMenuItems, 3, [varObject, varInteger, varBoolean],
      varEmpty);
    //AddFun(cJvUtils, 'ExpandWidth', JvInterpreter_ExpandWidth, 3, [varObject, varInteger, varEmpty], nil);
    AddFun(cJvUtils, 'PanelBorder', JvInterpreter_PanelBorder, 1, [varObject], varEmpty);
    AddFun(cJvUtils, 'Pixels', JvInterpreter_Pixels, 2, [varObject, varInteger], varEmpty);
    AddFun(cJvUtils, 'SetChildPropOrd', JvInterpreter_SetChildPropOrd, 3, [varObject, varString, varEmpty], varEmpty);
    AddFun(cJvUtils, 'Error', JvInterpreter_Error, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'ItemHtDrawEx', JvInterpreter_ItemHtDrawEx, 8, [varObject, varEmpty, varEmpty, varString,
      varBoolean, varString or varByRef, varInteger or varByRef, varBoolean], varEmpty);
    AddFun(cJvUtils, 'ItemHtDraw', JvInterpreter_ItemHtDraw, 5, [varObject, varEmpty, varEmpty, varString,
      varBoolean], varEmpty);
    AddFun(cJvUtils, 'ItemHtWidth', JvInterpreter_ItemHtWidth, 5, [varObject, varEmpty, varEmpty, varString,
      varBoolean], varEmpty);
    AddFun(cJvUtils, 'ItemHtPlain', JvInterpreter_ItemHtPlain, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'ClearList', JvInterpreter_ClearList, 1, [varObject], varEmpty);
    AddFun(cJvUtils, 'MemStreamToClipBoard', JvInterpreter_MemStreamToClipBoard, 2, [varObject, varSmallint],
      varEmpty);
    AddFun(cJvUtils, 'ClipBoardToMemStream', JvInterpreter_ClipBoardToMemStream, 2, [varObject, varSmallint],
      varEmpty);
    AddFun(cJvUtils, 'GetPropType', JvInterpreter_GetPropType, 2, [varObject, varString], varEmpty);
    AddFun(cJvUtils, 'GetPropStr', JvInterpreter_GetPropStr, 2, [varObject, varString], varEmpty);
    AddFun(cJvUtils, 'GetPropOrd', JvInterpreter_GetPropOrd, 2, [varObject, varString], varEmpty);
    AddFun(cJvUtils, 'CompareMem', JvInterpreter_CompareMem, 3, [varPointer, varPointer, varInteger], varEmpty);
    AddFun(cJvUtils, 'ShowMenu', JvInterpreter_ShowMenu, 2, [varObject, varEmpty], varEmpty);
    AddFun(cJvUtils, 'PrepareIniSection', JvInterpreter_PrepareIniSection, 1, [varObject], varEmpty);

    AddFun(cJvStrUtil, 'GetWordOnPos', JvInterpreter_GetWordOnPos, 2, [varString, varInteger], varEmpty);
    AddFun(cJvStrUtil, 'GetWordOnPosEx', JvInterpreter_GetWordOnPosEx, 4, [varString, varInteger, varInteger or
      varByRef, varInteger or varByRef], varEmpty);
    AddFun(cJvStrUtil, 'GetSubStr', JvInterpreter_SubStr, 3, [varString, varInteger, varEmpty], varEmpty);
    AddFun(cJvStrUtil, 'SubStr', JvInterpreter_SubStr, 3, [varString, varInteger, varString], varEmpty);
    AddFun(cJvStrUtil, 'SubStrEnd', JvInterpreter_SubStrEnd, 3, [varString, varInteger, varString], varEmpty);
    AddFun(cJvStrUtil, 'NumberByWord', JvInterpreter_NumberByWord, 1, [varEmpty], varEmpty);
    AddFun(cJvStrUtil, 'GetLineByPos', JvInterpreter_GetLineByPos, 2, [varString, varInteger], varEmpty);
    AddFun(cJvStrUtil, 'GetXYByPos', JvInterpreter_GetXYByPos, 4, [varString, varInteger, varInteger or varByRef,
      varInteger or varByRef], varEmpty);
    AddFun(cJvStrUtil, 'ReplaceString', JvInterpreter_ReplaceString, 3, [varString, varString, varString], varEmpty);
    AddFun(cJvStrUtil, 'ReplaceSokr1', JvInterpreter_ReplaceString, 3, [varString, varString, varString], varEmpty);
    AddFun(cJvStrUtil, 'ConcatSep', JvInterpreter_ConcatSep, 3, [varString, varString, varString], varEmpty);
    AddFun(cJvStrUtil, 'ConcatLeftSep', JvInterpreter_ConcatLeftSep, 3, [varString, varString, varString], varEmpty);
    AddFun(cJvStrUtil, 'MinimizeString', JvInterpreter_MinimizeString, 2, [varString, varInteger], varEmpty);
    AddFun(cJvStrUtil, 'Dos2Win', JvInterpreter_Dos2Win, 1, [varString or varByRef], varEmpty);
    AddFun(cJvStrUtil, 'Win2Dos', JvInterpreter_Win2Dos, 1, [varString or varByRef], varEmpty);
    AddFun(cJvStrUtil, 'Dos2WinRes', JvInterpreter_Dos2WinRes, 1, [varString], varEmpty);
    AddFun(cJvStrUtil, 'Win2DosRes', JvInterpreter_Win2DosRes, 1, [varString], varEmpty);
    AddFun(cJvStrUtil, 'Win2Koi', JvInterpreter_Win2Koi, 1, [varString], varString);
    AddFun(cJvStrUtil, 'Spaces', JvInterpreter_Spaces, 1, [varInteger], varEmpty);
    AddFun(cJvStrUtil, 'AddSpaces', JvInterpreter_AddSpaces, 2, [varString, varInteger], varEmpty);
    AddFun(cJvStrUtil, 'LastDate', JvInterpreter_LastDate, 1, [varEmpty], varEmpty);
    AddFun(cJvStrUtil, 'CurrencyToStr', JvInterpreter_CurrencyToStr, 1, [varEmpty], varEmpty);
    AddFun(cJvStrUtil, 'Cmp', JvInterpreter_Cmp, 2, [varString, varString], varEmpty);
    AddFun(cJvStrUtil, 'StringCat', JvInterpreter_StringCat, 2, [varString or varByRef, varString], varEmpty);
    AddFun(cJvStrUtil, 'HasChar', JvInterpreter_HasChar, 2, [varEmpty, varString], varEmpty);
    AddFun(cJvStrUtil, 'HasAnyChar', JvInterpreter_HasAnyChar, 2, [varString, varString], varEmpty);
    AddFun(cJvStrUtil, 'CountOfChar', JvInterpreter_CountOfChar, 2, [varEmpty, varString], varEmpty);
    AddFun(cJvStrUtil, 'DefStr', JvInterpreter_DefStr, 2, [varString, varString], varEmpty);
    AddFun(cJvUtils, 'GetWinDir', JvInterpreter_GetWinDir, 0, [0], varEmpty);
    AddFun(cJvUtils, 'GetTempDir', JvInterpreter_GetTempDir, 0, [0], varEmpty);
    AddFun(cJvUtils, 'GenTempFileName', JvInterpreter_GenTempFileName, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'GenTempFileNameExt', JvInterpreter_GenTempFileNameExt, 2, [varString, varString], varEmpty);
    AddFun(cJvUtils, 'ClearDir', JvInterpreter_ClearDir, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'DeleteDir', JvInterpreter_DeleteDir, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'FileEquMask', JvInterpreter_FileEquMask, 2, [varEmpty, varEmpty], varEmpty);
    AddFun(cJvUtils, 'FileEquMasks', JvInterpreter_FileEquMasks, 2, [varEmpty, varEmpty], varEmpty);
    AddFun(cJvUtils, 'DeleteFiles', JvInterpreter_DeleteFiles, 2, [varEmpty, varString], varEmpty);
    AddFun(cJvUtils, 'LZFileExpand', JvInterpreter_LZFileExpand, 2, [varString, varString], varEmpty);
    AddFun(cJvUtils, 'FileGetInfo', JvInterpreter_FileGetInfo, 2, [varEmpty, varEmpty or varByRef], varEmpty);
    AddFun(cJvUtils, 'HasSubFolder', JvInterpreter_HasSubFolder, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'IsEmptyFolder', JvInterpreter_IsEmptyFolder, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'AddSlash', JvInterpreter_AddSlash, 1, [varEmpty or varByRef], varEmpty);
    AddFun(cJvUtils, 'AddSlash2', JvInterpreter_AddSlash2, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'AddPath', JvInterpreter_AddPath, 2, [varEmpty, varEmpty], varEmpty);
    AddFun(cJvUtils, 'AddPaths', JvInterpreter_AddPaths, 2, [varString, varString], varEmpty);
    AddFun(cJvUtils, 'ParentPath', JvInterpreter_ParentPath, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'FindInPath', JvInterpreter_FindInPath, 2, [varString, varString], varEmpty);
    {$IFNDEF BCB1}
    AddFun(cJvUtils, 'BrowseForFolder', JvInterpreter_BrowseForFolder, 3, [varEmpty, varString, varString or
      varByRef], varEmpty);
    {$ENDIF BCB1}
    AddFun(cJvUtils, 'DeleteReadOnlyFile', JvInterpreter_DeleteReadOnlyFile, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'HasParam', JvInterpreter_HasParam, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'HasSwitch', JvInterpreter_HasSwitch, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'Switch', JvInterpreter_Switch, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'ExePath', JvInterpreter_ExePath, 0, [0], varEmpty);
    AddFun(cJvUtils, 'CopyDir', JvInterpreter_CopyDir, 2, [varEmpty, varEmpty], varEmpty);
    AddFun(cJvUtils, 'TTFontSelected', JvInterpreter_TTFontSelected, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'TrueInflateRect', JvInterpreter_TrueInflateRect, 2, [varEmpty, varInteger], varEmpty);
    AddFun(cJvUtils, 'SetWindowTop', JvInterpreter_SetWindowTop, 2, [varEmpty, varBoolean], varEmpty);
    AddFun(cJvUtils, 'KeyPressed', JvInterpreter_KeyPressed, 1, [varInteger], varEmpty);
    AddFun(cMath, 'Max', JvInterpreter_Max, 2, [varInteger, varInteger], varEmpty);
    AddFun(cMath, 'Min', JvInterpreter_Min, 2, [varInteger, varInteger], varEmpty);
    AddFun(cJvUtils, 'SwapInt', JvInterpreter_SwapInt, 2, [varInteger or varByRef, varInteger or varByRef],
      varEmpty);
    AddFun(cJvUtils, 'IntPower', JvInterpreter_IntPower, 2, [varInteger, varInteger], varEmpty);
    AddFun(cJvUtils, 'ChangeTopException', JvInterpreter_ChangeTopException, 1, [varObject], varEmpty);
    AddFun(cJvUtils, 'MakeValidFileName', JvInterpreter_MakeValidFileName, 2, [varEmpty, varEmpty], varEmpty);
    AddFun(cJvUtils, 'AnsiStrLIComp', JvInterpreter_AnsiStrLIComp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun(cJvUtils, 'Var2Type', JvInterpreter_Var2Type, 2, [varEmpty, varInteger], varEmpty);
    AddFun(cJvUtils, 'VarToInt', JvInterpreter_VarToInt, 1, [varEmpty], varEmpty);
    AddFun(cJvUtils, 'GetParameter', JvInterpreter_GetParameter, 0, [0], varEmpty);
    AddFun(cJvUtils, 'GetLongFileName', JvInterpreter_GetLongFileName, 1, [varString], varEmpty);
    AddFun(cFileCtrl, 'DirectoryExists', JvInterpreter_DirectoryExists, 1, [varString], varEmpty);
    AddFun(cFileCtrl, 'ForceDirectories', JvInterpreter_ForceDirectories, 1, [varString], varEmpty);
    AddFun(cJvUtils, 'FileNewExt', JvInterpreter_FileNewExt, 2, [varEmpty, varEmpty], varEmpty);
    AddFun(cJvUtils, 'GetComputerID', JvInterpreter_GetComputerID, 0, [0], varEmpty);
  end;
end;

end.

