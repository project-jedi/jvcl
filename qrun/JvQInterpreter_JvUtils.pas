{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_JvUtils.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQInterpreter_JvUtils;

interface

uses
  JvQInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  SysUtils, Classes, QGraphics, QForms, QControls, QStdCtrls, QExtCtrls,
  QDialogs, QMenus, Math,

  JvQJVCLUtils, JvQJCLUtils, JvQInterpreter_Windows;

{ function ReplaceAllStrings(S: string; Words, Frases: TStrings): string; }

procedure JvInterpreter_ReplaceAllStrings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReplaceAllStrings(Args.Values[0], V2O(Args.Values[1]) as TStrings, V2O(Args.Values[2]) as TStrings);
end;

{ function ReplaceStrings(S: string; PosBeg, Len: Integer; Words, Frases: TStrings; var NewSelStart: Integer): string; }

procedure JvInterpreter_ReplaceStrings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReplaceStrings(Args.Values[0], Args.Values[1], Args.Values[2], V2O(Args.Values[3]) as TStrings,
    V2O(Args.Values[4]) as TStrings, TVarData(Args.Values[5]).vInteger);
end;

{ function CountOfLines(const S: string): Integer; }

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

{ function ResSaveToFile(const Typ, Name: string; const Compressed: Boolean; const FileName: string): Boolean; }

procedure JvInterpreter_ResSaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ResSaveToFile(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ function ResSaveToFileEx(Instance: HINST; Typ, Name: PChar; const Compressed: Boolean; const FileName: string): Boolean; }

procedure JvInterpreter_ResSaveToFileEx(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ResSaveToFileEx(Args.Values[0], PChar(string(Args.Values[1])), PChar(string(Args.Values[2])),
    Args.Values[3], Args.Values[4]);
end;

{ function ResSaveToString(Instance: HINST; const Typ, Name: string; var S: string): Boolean; }

procedure JvInterpreter_ResSaveToString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ResSaveToString(Args.Values[0], Args.Values[1], Args.Values[2], string(TVarData(Args.Values[3]).vString));
end;

{ function Execute(const CommandLine, WorkingDirectory: string): Integer; }

procedure JvInterpreter_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Execute(Args.Values[0],Args.Values[1]);
end;

{ function IniReadSection(const IniFileName: TFileName; const Section: string; Ss: TStrings): Boolean; }

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

{ function ReadFolder(const Folder, Mask: TFileName; FileList: TStrings): Integer; }

procedure JvInterpreter_ReadFolder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReadFolder(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

procedure JvInterpreter_ReadFolders(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ReadFolders(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

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

{ procedure LoadIcoToImage(ALarge, ASmall: TImageList; const NameRes: string); }

procedure JvInterpreter_LoadIcoToImage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  LoadIcoToImage(V2O(Args.Values[0]) as TImageList, V2O(Args.Values[1]) as TImageList, Args.Values[2]);
end;

{ procedure RATextOut(Canvas: TCanvas; const R, RClip: TRect; const S: string); }

{ (rom) disabled because the functions drag JvClxUtils.pas into JvJCLUtils.pas
procedure JvInterpreter_RATextOut(var Value: Variant; Args: TJvInterpreterArgs);
begin
  RATextOut(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Var2Rect(Args.Values[2]), Args.Values[3]);
end;
}

{ function RATextOutEx(Canvas: TCanvas; const R, RClip: TRect; const S: string; const CalcHeight: Boolean): Integer; }

{ (rom) disabled because the functions drag JvClxUtils.pas into JvJCLUtils.pas
procedure JvInterpreter_RATextOutEx(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := RATextOutEx(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Var2Rect(Args.Values[2]),
    Args.Values[3], Args.Values[4]);
end;
}

{ function RATextCalcHeight(Canvas: TCanvas; const R: TRect; const S: string): Integer; }

{ (rom) disabled because the functions drag JvClxUtils.pas into JvJCLUtils.pas
procedure JvInterpreter_RATextCalcHeight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := RATextCalcHeight(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Args.Values[2]);
end;
}

{ procedure Roughed(ACanvas: TCanvas; const ARect: TRect; const AVert: Boolean); }

procedure JvInterpreter_Roughed(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Roughed(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Args.Values[2]);
end;

{ function BitmapFromBitmap(SrcBitmap: TBitmap; const AWidth, AHeight, index: Integer): TBitmap; }

procedure JvInterpreter_BitmapFromBitmap(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(BitmapFromBitmap(V2O(Args.Values[0]) as TBitmap, Args.Values[1], Args.Values[2], Args.Values[3]));
end;

{ function TextWidth(AStr: string): Integer; }

procedure JvInterpreter_TextWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TextWidth(Args.Values[0]);
end;

{ function DefineCursor(Identifier: PChar): TCursor; }

procedure JvInterpreter_DefineCursor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := DefineCursor(HInstance, PChar(string(Args.Values[0])));
end;

{ function FindFormByClassName(FormClassName: string): TForm; }

procedure JvInterpreter_FindFormByClassName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(FindFormByClassName(Args.Values[0]));
end;

{ function FindByTag(WinControl: TWinControl; ComponentClass: TComponentClass; const Tag: Integer): TComponent; }

procedure JvInterpreter_FindByTag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(FindByTag(V2O(Args.Values[0]) as TWinControl, TComponentClass(V2C(Args.Values[1])), Args.Values[2]));
end;

{ function ControlAtPos2(Parent: TWinControl; X, Y: Integer): TControl; }

procedure JvInterpreter_ControlAtPos2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(ControlAtPos2(V2O(Args.Values[0]) as TWinControl, Args.Values[1], Args.Values[2]));
end;

{ function RBTag(Parent: TWinControl): Integer; }

procedure JvInterpreter_RBTag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := RBTag(V2O(Args.Values[0]) as TWinControl);
end;

{ function AppMinimized: Boolean; }

procedure JvInterpreter_AppMinimized(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := AppMinimized;
end;

{ function MsgDlg2(const Msg, ACaption: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpContext: Integer; Control: TWinControl): Integer; }

procedure JvInterpreter_MsgDlg2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := MsgDlg2(Args.Values[0], Args.Values[1], Args.Values[2], TMsgDlgButtons(Word(V2S(Args.Values[3]))),
    Args.Values[4], V2O(Args.Values[5]) as TWinControl);
end;

{ function MsgDlgDef(const Msg, ACaption: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; HelpContext: Integer; Control: TWinControl): Integer; }

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
{ procedure CenterHor(Parent: TControl; MinLeft: Integer; Controls: array of TControl); }
procedure JvInterpreter_CenterHor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  CenterHor(V2O(Args.Values[0]) as TControl, Args.Values[1], Args.Values[2]);
end;
*)

{ procedure EnableControls(Control: TWinControl; const Enable: Boolean); }

procedure JvInterpreter_EnableControls(var Value: Variant; Args: TJvInterpreterArgs);
begin
  EnableControls(V2O(Args.Values[0]) as TWinControl, Args.Values[1]);
end;

{ procedure EnableMenuItems(MenuItem: TMenuItem; const Tag: Integer; const Enable: Boolean); }

procedure JvInterpreter_EnableMenuItems(var Value: Variant; Args: TJvInterpreterArgs);
begin
  EnableMenuItems(V2O(Args.Values[0]) as TMenuItem, Args.Values[1], Args.Values[2]);
end;

(*
{ procedure ExpandWidth(Parent: TControl; MinWidth: Integer; Controls: array of TControl); }
procedure JvInterpreter_ExpandWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  ExpandWidth(V2O(Args.Values[0]) as TControl, Args.Values[1], Args.Values[2]);
end;
*)

{ function PanelBorder(Panel: TCustomPanel): Integer; }

procedure JvInterpreter_PanelBorder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := PanelBorder(V2O(Args.Values[0]) as TCustomPanel);
end;

{ function Pixels(Control: TControl; APixels: Integer): Integer; }

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
  ItemHtDrawEx(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Word(V2S(Args.Values[2]))),
    Args.Values[3], Args.Values[4], string(TVarData(Args.Values[5]).vString), TVarData(Args.Values[6]).vInteger,
    Args.Values[7]);
end;

{ function ItemHtDraw(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean): string; }

procedure JvInterpreter_ItemHtDraw(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ItemHtDraw(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]),
    TOwnerDrawState(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
end;

{ function ItemHtWidth(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean): Integer; }

procedure JvInterpreter_ItemHtWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ItemHtWidth(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]),
    TOwnerDrawState(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
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
{ function GetWordOnPos(const S: string; const P: Integer): string; }

procedure JvInterpreter_GetWordOnPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetWordOnPos(Args.Values[0], Args.Values[1]);
end;

{ function GetWordOnPosEx(const S: string; const P: Integer; var iBeg, iEnd: Integer): string; }

procedure JvInterpreter_GetWordOnPosEx(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetWordOnPosEx(Args.Values[0], Args.Values[1], TVarData(Args.Values[2]).vInteger,
    TVarData(Args.Values[3]).vInteger);
end;

{ function SubStr(const S: string; const index: Integer; const Separator: string): string; }

procedure JvInterpreter_SubStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := SubStr(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function SubStrEnd(const S: string; const index: Integer; const Separator: string): string; }

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

{ function GetLineByPos(const S: string; const Pos: Integer): Integer; }

procedure JvInterpreter_GetLineByPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetLineByPos(Args.Values[0], Args.Values[1]);
end;

{ procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer); }

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

{ function MinimizeString(const S: string; const MaxLen: Integer): string; }

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

{ function Spaces(const N: Integer): string; }

procedure JvInterpreter_Spaces(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Spaces(Args.Values[0]);
end;

{ function AddSpaces(const S: string; const N: Integer): string; }

procedure JvInterpreter_AddSpaces(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := AddSpaces(Args.Values[0], Args.Values[1]);
end;

{ function LastDateRUS(const Dat: TDateTime): string; }

procedure JvInterpreter_LastDateRUS(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := LastDateRUS(Args.Values[0]);
end;

{ function CurrencyToStr(const Cur: currency): string; }

procedure JvInterpreter_CurrencyToStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CurrencyToStr(Args.Values[0]);
end;

{ function Cmp(const S1, S2: string): Boolean; }

procedure JvInterpreter_Cmp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Cmp(Args.Values[0], Args.Values[1]);
end;

{ function StringCat(var S1: string; S2: string): string; }

procedure JvInterpreter_StringCat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := StringCat(string(TVarData(Args.Values[0]).vString), Args.Values[1]);
end;

{ function HasChar(const Ch: Char; const S: string): Boolean; }

procedure JvInterpreter_HasChar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasChar(string(Args.Values[0])[1], Args.Values[1]);
end;

{ function HasAnyChar(const Chars: string; const S: string): Boolean; }

procedure JvInterpreter_HasAnyChar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasAnyChar(Args.Values[0], Args.Values[1]);
end;

(*
{ function CharInSet(const Ch: Char; const SetOfChar: TSetOfChar): Boolean; }
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

{ function ClearDir(const Dir: string): Boolean; }

procedure JvInterpreter_ClearDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ClearDir(Args.Values[0]);
end;

{ function DeleteDir(const Dir: string): Boolean; }

procedure JvInterpreter_DeleteDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := DeleteDir(Args.Values[0]);
end;

{ function FileEquMask(FileName, Mask: TFileName): Boolean; }

procedure JvInterpreter_FileEquMask(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FileEquMask(Args.Values[0], Args.Values[1]);
end;

{ function FileEquMasks(FileName, Masks: TFileName): Boolean; }

procedure JvInterpreter_FileEquMasks(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FileEquMasks(Args.Values[0], Args.Values[1]);
end;

{ procedure DeleteFiles(const Folder: TFileName; const Masks: string); }

procedure JvInterpreter_DeleteFiles(var Value: Variant; Args: TJvInterpreterArgs);
begin
  DeleteFiles(Args.Values[0], Args.Values[1]);
end;

{ function LZFileExpand(const FileSource, FileDest: string): Boolean; }

procedure JvInterpreter_LZFileExpand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := LZFileExpand(Args.Values[0], Args.Values[1]);
end;

{ function FileGetInfo(FileName: TFileName; var SearchRec: TSearchRec): Boolean; }

procedure JvInterpreter_FileGetInfo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FileGetInfo(Args.Values[0], TSearchRec(V2R(Args.Values[1])^));
end;

{ function HasSubFolder(APath: TFileName): Boolean; }

procedure JvInterpreter_HasSubFolder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasSubFolder(Args.Values[0]);
end;

{ function IsEmptyFolder(APath: TFileName): Boolean; }

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

{ function BrowseForFolderNative(const Handle: HWnd; const Title: string; var Folder: string): Boolean; }

{$IFNDEF BCB1}
procedure JvInterpreter_BrowseForFolderNative(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := BrowseForFolderNative(Args.Values[0], Args.Values[1], string(TVarData(Args.Values[2]).vString));
end;
{$ENDIF BCB1}

{ function DeleteReadOnlyFile(const FileName: TFileName): Boolean; }

procedure JvInterpreter_DeleteReadOnlyFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := DeleteReadOnlyFile(Args.Values[0]);
end;

{ function HasParam(const Param: string): Boolean; }

procedure JvInterpreter_HasParam(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := HasParam(Args.Values[0]);
end;

{ function HasSwitch(const Param: string): Boolean; }

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

{ function IsTTFontSelected(const DC: HDC): Boolean; }

procedure JvInterpreter_IsTTFontSelected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := IsTTFontSelected(Args.Values[0]);
end;

{ function TrueInflateRect(const R: TRect; const I: Integer): TRect; }

procedure JvInterpreter_TrueInflateRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TrueInflateRect(Var2Rect(Args.Values[0]), Args.Values[1]));
end;

{ procedure SetWindowTop(const Handle: HWND; const Top: Boolean); }

procedure JvInterpreter_SetWindowTop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  SetWindowTop(Args.Values[0], Args.Values[1]);
end;

{ function KeyPressed(VK: Integer): Boolean; }

procedure JvInterpreter_KeyPressed(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := KeyPressed(Args.Values[0]);
end;

{ function Max(x, y: Integer): Integer; }

procedure JvInterpreter_Max(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Max(Args.Values[0], Args.Values[1]);
end;

{ function Min(x, y: Integer): Integer; }

procedure JvInterpreter_Min(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Min(Args.Values[0], Args.Values[1]);
end;

{ procedure SwapInt(var Int1, Int2: Integer); }

procedure JvInterpreter_SwapInt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  SwapInt(TVarData(Args.Values[0]).vInteger, TVarData(Args.Values[1]).vInteger);
end;

{ function IntPower(Base, Exponent: Integer): Integer; }

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

{ function Var2Type(V: Variant; const VarType: Integer): Variant; }

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
    AddFunction(cJvUtils, 'ReplaceAllStrings', JvInterpreter_ReplaceAllStrings, 3, [varString, varObject, varObject], varEmpty);
    AddFunction(cJvUtils, 'ReplaceStrings', JvInterpreter_ReplaceStrings, 6, [varString, varInteger, varInteger, varObject,
      varObject, varInteger or varByRef], varEmpty);
    AddFunction(cJvUtils, 'CountOfLines', JvInterpreter_CountOfLines, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'DeleteEmptyLines', JvInterpreter_DeleteEmptyLines, 1, [varObject], varEmpty);
    AddFunction(cJvUtils, 'SQLAddWhere', JvInterpreter_SQLAddWhere, 2, [varObject, varString], varEmpty);
    AddFunction(cJvUtils, 'ResSaveToFile', JvInterpreter_ResSaveToFile, 4, [varString, varString, varBoolean, varString],
      varEmpty);
    AddFunction(cJvUtils, 'ResSaveToFileEx', JvInterpreter_ResSaveToFileEx, 5, [varEmpty, varEmpty, varEmpty, varBoolean,
      varString], varEmpty);
    AddFunction(cJvUtils, 'ResSaveToString', JvInterpreter_ResSaveToString, 4, [varEmpty, varString, varString, varString or
      varByRef], varEmpty);
    AddFunction(cJvUtils, 'Execute', JvInterpreter_Execute, 2, [varString, varString], varEmpty);
    AddFunction(cJvUtils, 'IniReadSection', JvInterpreter_IniReadSection, 3, [varEmpty, varString, varObject], varEmpty);
    AddFunction(cJvUtils, 'LoadTextFile', JvInterpreter_LoadTextFile, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'SaveTextFile', JvInterpreter_SaveTextFile, 2, [varEmpty, varString], varEmpty);
    AddFunction(cJvUtils, 'ReadFolder', JvInterpreter_ReadFolder, 3, [varEmpty, varEmpty, varObject], varEmpty);
    AddFunction(cJvUtils, 'ReadFolders', JvInterpreter_ReadFolders, 2, [varEmpty, varObject], varEmpty);
    AddFunction(cJvUtils, 'TargetFileName', JvInterpreter_TargetFileName, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'ResolveLink', JvInterpreter_ResolveLink, 3, [varEmpty, varEmpty, varEmpty or varByRef],
      varEmpty);
    AddFunction(cJvUtils, 'LoadIcoToImage', JvInterpreter_LoadIcoToImage, 3, [varObject, varObject, varString], varEmpty);
    { (rom) disabled because the functions drag JvClxUtils.pas into JvJCLUtils.pas
    AddFunction(cJvUtils, 'RATextOut', JvInterpreter_RATextOut, 4, [varObject, varEmpty, varEmpty, varString], varEmpty);
    AddFunction(cJvUtils, 'RATextOutEx', JvInterpreter_RATextOutEx, 5, [varObject, varEmpty, varEmpty, varString,
      varBoolean], varEmpty);
    AddFunction(cJvUtils, 'RATextCalcHeight', JvInterpreter_RATextCalcHeight, 3, [varObject, varEmpty, varString],
      varEmpty);
    }
    AddFunction(cJvUtils, 'Roughed', JvInterpreter_Roughed, 3, [varObject, varEmpty, varBoolean], varEmpty);
    AddFunction(cJvUtils, 'BitmapFromBitmap', JvInterpreter_BitmapFromBitmap, 4, [varObject, varInteger, varInteger,
      varInteger], varEmpty);
    AddFunction(cJvUtils, 'TextWidth', JvInterpreter_TextWidth, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'DefineCursor', JvInterpreter_DefineCursor, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'FindFormByClassName', JvInterpreter_FindFormByClassName, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'FindByTag', JvInterpreter_FindByTag, 3, [varObject, varEmpty, varInteger], varEmpty);
    AddFunction(cJvUtils, 'ControlAtPos2', JvInterpreter_ControlAtPos2, 3, [varObject, varInteger, varInteger], varEmpty);
    AddFunction(cJvUtils, 'RBTag', JvInterpreter_RBTag, 1, [varObject], varEmpty);
    AddFunction(cJvUtils, 'AppMinimized', JvInterpreter_AppMinimized, 0, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'MsgDlg2', JvInterpreter_MsgDlg2, 6, [varString, varString, varEmpty, varEmpty, varInteger,
      varObject], varEmpty);
    AddFunction(cJvUtils, 'MsgDlgDef', JvInterpreter_MsgDlgDef, 7, [varString, varString, varEmpty, varEmpty, varEmpty,
      varInteger, varObject], varEmpty);
    AddFunction(cJvUtils, 'Delay', JvInterpreter_Delay, 1, [varEmpty], varEmpty);
    //AddFunction(cJvUtils, 'CenterHor', JvInterpreter_CenterHor, 3, [varObject, varInteger, varEmpty], nil);
    AddFunction(cJvUtils, 'EnableControls', JvInterpreter_EnableControls, 2, [varObject, varBoolean], varEmpty);
    AddFunction(cJvUtils, 'EnableMenuItems', JvInterpreter_EnableMenuItems, 3, [varObject, varInteger, varBoolean],
      varEmpty);
    //AddFunction(cJvUtils, 'ExpandWidth', JvInterpreter_ExpandWidth, 3, [varObject, varInteger, varEmpty], nil);
    AddFunction(cJvUtils, 'PanelBorder', JvInterpreter_PanelBorder, 1, [varObject], varEmpty);
    AddFunction(cJvUtils, 'Pixels', JvInterpreter_Pixels, 2, [varObject, varInteger], varEmpty);
    AddFunction(cJvUtils, 'SetChildPropOrd', JvInterpreter_SetChildPropOrd, 3, [varObject, varString, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'Error', JvInterpreter_Error, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'ItemHtDrawEx', JvInterpreter_ItemHtDrawEx, 8, [varObject, varEmpty, varEmpty, varString,
      varBoolean, varString or varByRef, varInteger or varByRef, varBoolean], varEmpty);
    AddFunction(cJvUtils, 'ItemHtDraw', JvInterpreter_ItemHtDraw, 5, [varObject, varEmpty, varEmpty, varString,
      varBoolean], varEmpty);
    AddFunction(cJvUtils, 'ItemHtWidth', JvInterpreter_ItemHtWidth, 5, [varObject, varEmpty, varEmpty, varString,
      varBoolean], varEmpty);
    AddFunction(cJvUtils, 'ItemHtPlain', JvInterpreter_ItemHtPlain, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'ClearList', JvInterpreter_ClearList, 1, [varObject], varEmpty);
    AddFunction(cJvUtils, 'MemStreamToClipBoard', JvInterpreter_MemStreamToClipBoard, 2, [varObject, varSmallint],
      varEmpty);
    AddFunction(cJvUtils, 'ClipBoardToMemStream', JvInterpreter_ClipBoardToMemStream, 2, [varObject, varSmallint],
      varEmpty);
    AddFunction(cJvUtils, 'GetPropType', JvInterpreter_GetPropType, 2, [varObject, varString], varEmpty);
    AddFunction(cJvUtils, 'GetPropStr', JvInterpreter_GetPropStr, 2, [varObject, varString], varEmpty);
    AddFunction(cJvUtils, 'GetPropOrd', JvInterpreter_GetPropOrd, 2, [varObject, varString], varEmpty);
    AddFunction(cJvUtils, 'CompareMem', JvInterpreter_CompareMem, 3, [varPointer, varPointer, varInteger], varEmpty);
    AddFunction(cJvUtils, 'ShowMenu', JvInterpreter_ShowMenu, 2, [varObject, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'PrepareIniSection', JvInterpreter_PrepareIniSection, 1, [varObject], varEmpty);

    AddFunction(cJvStrUtil, 'GetWordOnPos', JvInterpreter_GetWordOnPos, 2, [varString, varInteger], varEmpty);
    AddFunction(cJvStrUtil, 'GetWordOnPosEx', JvInterpreter_GetWordOnPosEx, 4, [varString, varInteger, varInteger or
      varByRef, varInteger or varByRef], varEmpty);
    AddFunction(cJvStrUtil, 'SubStr', JvInterpreter_SubStr, 3, [varString, varInteger, varString], varEmpty);
    AddFunction(cJvStrUtil, 'SubStrEnd', JvInterpreter_SubStrEnd, 3, [varString, varInteger, varString], varEmpty);
    AddFunction(cJvStrUtil, 'GetLineByPos', JvInterpreter_GetLineByPos, 2, [varString, varInteger], varEmpty);
    AddFunction(cJvStrUtil, 'GetXYByPos', JvInterpreter_GetXYByPos, 4, [varString, varInteger, varInteger or varByRef,
      varInteger or varByRef], varEmpty);
    AddFunction(cJvStrUtil, 'ReplaceString', JvInterpreter_ReplaceString, 3, [varString, varString, varString], varEmpty);
    AddFunction(cJvStrUtil, 'ReplaceSokr1', JvInterpreter_ReplaceString, 3, [varString, varString, varString], varEmpty);
    AddFunction(cJvStrUtil, 'ConcatSep', JvInterpreter_ConcatSep, 3, [varString, varString, varString], varEmpty);
    AddFunction(cJvStrUtil, 'ConcatLeftSep', JvInterpreter_ConcatLeftSep, 3, [varString, varString, varString], varEmpty);
    AddFunction(cJvStrUtil, 'MinimizeString', JvInterpreter_MinimizeString, 2, [varString, varInteger], varEmpty);
    AddFunction(cJvStrUtil, 'Dos2Win', JvInterpreter_Dos2Win, 1, [varString or varByRef], varEmpty);
    AddFunction(cJvStrUtil, 'Win2Dos', JvInterpreter_Win2Dos, 1, [varString or varByRef], varEmpty);
    AddFunction(cJvStrUtil, 'Dos2WinRes', JvInterpreter_Dos2WinRes, 1, [varString], varEmpty);
    AddFunction(cJvStrUtil, 'Win2DosRes', JvInterpreter_Win2DosRes, 1, [varString], varEmpty);
    AddFunction(cJvStrUtil, 'Win2Koi', JvInterpreter_Win2Koi, 1, [varString], varString);
    AddFunction(cJvStrUtil, 'Spaces', JvInterpreter_Spaces, 1, [varInteger], varEmpty);
    AddFunction(cJvStrUtil, 'AddSpaces', JvInterpreter_AddSpaces, 2, [varString, varInteger], varEmpty);
    AddFunction(cJvStrUtil, 'LastDateRUS', JvInterpreter_LastDateRUS, 1, [varEmpty], varEmpty);
    AddFunction(cJvStrUtil, 'CurrencyToStr', JvInterpreter_CurrencyToStr, 1, [varEmpty], varEmpty);
    AddFunction(cJvStrUtil, 'Cmp', JvInterpreter_Cmp, 2, [varString, varString], varEmpty);
    AddFunction(cJvStrUtil, 'StringCat', JvInterpreter_StringCat, 2, [varString or varByRef, varString], varEmpty);
    AddFunction(cJvStrUtil, 'HasChar', JvInterpreter_HasChar, 2, [varEmpty, varString], varEmpty);
    AddFunction(cJvStrUtil, 'HasAnyChar', JvInterpreter_HasAnyChar, 2, [varString, varString], varEmpty);
    AddFunction(cJvStrUtil, 'CountOfChar', JvInterpreter_CountOfChar, 2, [varEmpty, varString], varEmpty);
    AddFunction(cJvStrUtil, 'DefStr', JvInterpreter_DefStr, 2, [varString, varString], varEmpty);
    AddFunction(cJvUtils, 'GetTempDir', JvInterpreter_GetTempDir, 0, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'GenTempFileName', JvInterpreter_GenTempFileName, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'GenTempFileNameExt', JvInterpreter_GenTempFileNameExt, 2, [varString, varString], varEmpty);
    AddFunction(cJvUtils, 'ClearDir', JvInterpreter_ClearDir, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'DeleteDir', JvInterpreter_DeleteDir, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'FileEquMask', JvInterpreter_FileEquMask, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'FileEquMasks', JvInterpreter_FileEquMasks, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'DeleteFiles', JvInterpreter_DeleteFiles, 2, [varEmpty, varString], varEmpty);
    AddFunction(cJvUtils, 'LZFileExpand', JvInterpreter_LZFileExpand, 2, [varString, varString], varEmpty);
    AddFunction(cJvUtils, 'FileGetInfo', JvInterpreter_FileGetInfo, 2, [varEmpty, varEmpty or varByRef], varEmpty);
    AddFunction(cJvUtils, 'HasSubFolder', JvInterpreter_HasSubFolder, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'IsEmptyFolder', JvInterpreter_IsEmptyFolder, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'AddSlash', JvInterpreter_AddSlash, 1, [varEmpty or varByRef], varEmpty);
    AddFunction(cJvUtils, 'AddSlash2', JvInterpreter_AddSlash2, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'AddPath', JvInterpreter_AddPath, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'AddPaths', JvInterpreter_AddPaths, 2, [varString, varString], varEmpty);
    AddFunction(cJvUtils, 'ParentPath', JvInterpreter_ParentPath, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'FindInPath', JvInterpreter_FindInPath, 2, [varString, varString], varEmpty);
    {$IFNDEF BCB1}
    AddFunction(cJvUtils, 'BrowseForFolderNative', JvInterpreter_BrowseForFolderNative, 3, [varEmpty, varString, varString or
      varByRef], varEmpty);
    {$ENDIF BCB1}
    AddFunction(cJvUtils, 'DeleteReadOnlyFile', JvInterpreter_DeleteReadOnlyFile, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'HasParam', JvInterpreter_HasParam, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'HasSwitch', JvInterpreter_HasSwitch, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'Switch', JvInterpreter_Switch, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'ExePath', JvInterpreter_ExePath, 0, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'CopyDir', JvInterpreter_CopyDir, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'IsTTFontSelected', JvInterpreter_IsTTFontSelected, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'TrueInflateRect', JvInterpreter_TrueInflateRect, 2, [varEmpty, varInteger], varEmpty);
    AddFunction(cJvUtils, 'SetWindowTop', JvInterpreter_SetWindowTop, 2, [varEmpty, varBoolean], varEmpty);
    AddFunction(cJvUtils, 'KeyPressed', JvInterpreter_KeyPressed, 1, [varInteger], varEmpty);
    AddFunction(cMath, 'Max', JvInterpreter_Max, 2, [varInteger, varInteger], varEmpty);
    AddFunction(cMath, 'Min', JvInterpreter_Min, 2, [varInteger, varInteger], varEmpty);
    AddFunction(cJvUtils, 'SwapInt', JvInterpreter_SwapInt, 2, [varInteger or varByRef, varInteger or varByRef],
      varEmpty);
    AddFunction(cJvUtils, 'IntPower', JvInterpreter_IntPower, 2, [varInteger, varInteger], varEmpty);
    AddFunction(cJvUtils, 'ChangeTopException', JvInterpreter_ChangeTopException, 1, [varObject], varEmpty);
    AddFunction(cJvUtils, 'MakeValidFileName', JvInterpreter_MakeValidFileName, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'AnsiStrLIComp', JvInterpreter_AnsiStrLIComp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'Var2Type', JvInterpreter_Var2Type, 2, [varEmpty, varInteger], varEmpty);
    AddFunction(cJvUtils, 'VarToInt', JvInterpreter_VarToInt, 1, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'GetParameter', JvInterpreter_GetParameter, 0, [varEmpty], varEmpty);
    AddFunction(cJvUtils, 'GetLongFileName', JvInterpreter_GetLongFileName, 1, [varString], varEmpty);
    AddFunction(cFileCtrl, 'DirectoryExists', JvInterpreter_DirectoryExists, 1, [varString], varEmpty);
    AddFunction(cFileCtrl, 'ForceDirectories', JvInterpreter_ForceDirectories, 1, [varString], varEmpty);
    AddFunction(cJvUtils, 'FileNewExt', JvInterpreter_FileNewExt, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cJvUtils, 'GetComputerID', JvInterpreter_GetComputerID, 0, [varEmpty], varEmpty);
  end;
end;

end.

