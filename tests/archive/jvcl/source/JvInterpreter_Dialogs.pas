{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Dialogs.PAS, released on 2002-07-04.

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

unit JvInterpreter_Dialogs;

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  Classes,
  {$IFDEF COMPLIB_VCL}
  Graphics, Controls, Dialogs,
  JvInterpreter_Windows;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Variants, Qt, QGraphics, QControls, QDialogs,
  JvInterpreter_Types;
  {$ENDIF COMPLIB_CLX}

{$IFDEF COMPLIB_CLX}
type
  TCommonDialog = TQtDialog;
{$ENDIF COMPLIB_CLX}

{ TCommonDialog }

{ property Read Handle: HWnd }

{$IFDEF COMPILER3_UP}
procedure TCommonDialog_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TCommonDialog(Args.Obj).Handle);
end;
{$ENDIF COMPILER3_UP}

{$IFDEF COMPLIB_VCL}

{ property Read Ctl3D: Boolean }

procedure TCommonDialog_Read_Ctl3D(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCommonDialog(Args.Obj).Ctl3D;
end;

{ property Write Ctl3D(Value: Boolean) }

procedure TCommonDialog_Write_Ctl3D(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCommonDialog(Args.Obj).Ctl3D := Value;
end;

{$ENDIF COMPLIB_VCL}

{ property Read HelpContext: THelpContext }

procedure TCommonDialog_Read_HelpContext(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCommonDialog(Args.Obj).HelpContext;
end;

{ property Write HelpContext(Value: THelpContext) }

procedure TCommonDialog_Write_HelpContext(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCommonDialog(Args.Obj).HelpContext := Value;
end;

{ TOpenDialog }

{ constructor Create(AOwner: TComponent) }

procedure TOpenDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TOpenDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function Execute: Boolean; }

procedure TOpenDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOpenDialog(Args.Obj).Execute;
end;

{$IFDEF COMPLIB_VCL}

{ property Read FileEditStyle: TFileEditStyle }

procedure TOpenDialog_Read_FileEditStyle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOpenDialog(Args.Obj).FileEditStyle;
end;

{ property Write FileEditStyle(Value: TFileEditStyle) }

procedure TOpenDialog_Write_FileEditStyle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TOpenDialog(Args.Obj).FileEditStyle := Value;
end;

{$ENDIF COMPLIB_VCL}

{ property Read Files: TStrings }

procedure TOpenDialog_Read_Files(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TOpenDialog(Args.Obj).Files);
end;

{ property Read HistoryList: TStrings }

procedure TOpenDialog_Read_HistoryList(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TOpenDialog(Args.Obj).HistoryList);
end;

{ property Write HistoryList(Value: TStrings) }

procedure TOpenDialog_Write_HistoryList(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TOpenDialog(Args.Obj).HistoryList := V2O(Value) as TStrings;
end;

{ property Read DefaultExt: string }

procedure TOpenDialog_Read_DefaultExt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOpenDialog(Args.Obj).DefaultExt;
end;

{ property Write DefaultExt(Value: string) }

procedure TOpenDialog_Write_DefaultExt(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TOpenDialog(Args.Obj).DefaultExt := Value;
end;

{ property Read FileName: TFileName }

procedure TOpenDialog_Read_FileName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOpenDialog(Args.Obj).FileName;
end;

{ property Write FileName(Value: TFileName) }

procedure TOpenDialog_Write_FileName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TOpenDialog(Args.Obj).FileName := Value;
end;

{ property Read Filter: string }

procedure TOpenDialog_Read_Filter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOpenDialog(Args.Obj).Filter;
end;

{ property Write Filter(Value: string) }

procedure TOpenDialog_Write_Filter(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TOpenDialog(Args.Obj).Filter := Value;
end;

{ property Read FilterIndex: Integer }

procedure TOpenDialog_Read_FilterIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOpenDialog(Args.Obj).FilterIndex;
end;

{ property Write FilterIndex(Value: Integer) }

procedure TOpenDialog_Write_FilterIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TOpenDialog(Args.Obj).FilterIndex := Value;
end;

{ property Read InitialDir: string }

procedure TOpenDialog_Read_InitialDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOpenDialog(Args.Obj).InitialDir;
end;

{ property Write InitialDir(Value: string) }

procedure TOpenDialog_Write_InitialDir(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TOpenDialog(Args.Obj).InitialDir := Value;
end;

{ property Read Options: TOpenOptions }

procedure TOpenDialog_Read_Options(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  Value := S2V(Integer(TOpenDialog(Args.Obj).Options));
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Value := S2V(Word(TOpenDialog(Args.Obj).Options));
  {$ENDIF COMPLIB_CLX}
end;

{ property Write Options(Value: TOpenOptions) }

procedure TOpenDialog_Write_Options(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  TOpenDialog(Args.Obj).Options := TOpenOptions(V2S(Value));
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  TOpenDialog(Args.Obj).Options := TOpenOptions(Word(V2S(Value)));
  {$ENDIF COMPLIB_CLX}
end;

{ property Read Title: string }

procedure TOpenDialog_Read_Title(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOpenDialog(Args.Obj).Title;
end;

{ property Write Title(Value: string) }

procedure TOpenDialog_Write_Title(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TOpenDialog(Args.Obj).Title := Value;
end;

{ TSaveDialog }

{ constructor Create(AOwner: TComponent) }

procedure TSaveDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSaveDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function Execute: Boolean; }

procedure TSaveDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSaveDialog(Args.Obj).Execute;
end;

{ TColorDialog }

{ constructor Create(AOwner: TComponent) }

procedure TColorDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColorDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function Execute: Boolean; }

procedure TColorDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColorDialog(Args.Obj).Execute;
end;

{ property Read Color: TColor }

procedure TColorDialog_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColorDialog(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure TColorDialog_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColorDialog(Args.Obj).Color := Value;
end;

{ property Read CustomColors: TStrings }

procedure TColorDialog_Read_CustomColors(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColorDialog(Args.Obj).CustomColors);
end;

{ property Write CustomColors(Value: TStrings) }

procedure TColorDialog_Write_CustomColors(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColorDialog(Args.Obj).CustomColors := V2O(Value) as TStrings;
end;

{$IFDEF COMPLIB_VCL}

{ property Read Options: TColorDialogOptions }

procedure TColorDialog_Read_Options(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TColorDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TColorDialogOptions) }

procedure TColorDialog_Write_Options(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColorDialog(Args.Obj).Options := TColorDialogOptions(Byte(V2S(Value)));
end;

{$ENDIF COMPLIB_VCL}

{ TFontDialog }

{ constructor Create(AOwner: TComponent) }

procedure TFontDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFontDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function Execute: Boolean; }

procedure TFontDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFontDialog(Args.Obj).Execute;
end;

{ property Read Font: TFont }

procedure TFontDialog_Read_Font(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFontDialog(Args.Obj).Font);
end;

{ property Write Font(Value: TFont) }

procedure TFontDialog_Write_Font(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFontDialog(Args.Obj).Font := V2O(Value) as TFont;
end;

{$IFDEF COMPLIB_VCL}

{ property Read Device: TFontDialogDevice }

procedure TFontDialog_Read_Device(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFontDialog(Args.Obj).Device;
end;

{ property Write Device(Value: TFontDialogDevice) }

procedure TFontDialog_Write_Device(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFontDialog(Args.Obj).Device := Value;
end;

{ property Read MinFontSize: Integer }

procedure TFontDialog_Read_MinFontSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFontDialog(Args.Obj).MinFontSize;
end;

{ property Write MinFontSize(Value: Integer) }

procedure TFontDialog_Write_MinFontSize(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFontDialog(Args.Obj).MinFontSize := Value;
end;

{ property Read MaxFontSize: Integer }

procedure TFontDialog_Read_MaxFontSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFontDialog(Args.Obj).MaxFontSize;
end;

{ property Write MaxFontSize(Value: Integer) }

procedure TFontDialog_Write_MaxFontSize(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFontDialog(Args.Obj).MaxFontSize := Value;
end;

{ property Read Options: TFontDialogOptions }

procedure TFontDialog_Read_Options(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Word(TFontDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TFontDialogOptions) }

procedure TFontDialog_Write_Options(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFontDialog(Args.Obj).Options := TFontDialogOptions(Word(V2S(Value)));
end;

{$ENDIF COMPLIB_VCL}

{$IFDEF COMPLIB_VCL}

{ TPrinterSetupDialog }

{ constructor Create(AOwner: TComponent) }

procedure TPrinterSetupDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPrinterSetupDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function Execute: Boolean; }

procedure TPrinterSetupDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPILER3_UP}
  Value := TPrinterSetupDialog(Args.Obj).Execute;
  {$ELSE}
  TPrinterSetupDialog(Args.Obj).Execute;
  {$ENDIF COMPILER3_UP}
end;

{ TPrintDialog }

{ constructor Create(AOwner: TComponent) }

procedure TPrintDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPrintDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function Execute: Boolean; }

procedure TPrintDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).Execute;
end;

{ property Read Collate: Boolean }

procedure TPrintDialog_Read_Collate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).Collate;
end;

{ property Write Collate(Value: Boolean) }

procedure TPrintDialog_Write_Collate(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).Collate := Value;
end;

{ property Read Copies: Integer }

procedure TPrintDialog_Read_Copies(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).Copies;
end;

{ property Write Copies(Value: Integer) }

procedure TPrintDialog_Write_Copies(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).Copies := Value;
end;

{ property Read FromPage: Integer }

procedure TPrintDialog_Read_FromPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).FromPage;
end;

{ property Write FromPage(Value: Integer) }

procedure TPrintDialog_Write_FromPage(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).FromPage := Value;
end;

{ property Read MinPage: Integer }

procedure TPrintDialog_Read_MinPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).MinPage;
end;

{ property Write MinPage(Value: Integer) }

procedure TPrintDialog_Write_MinPage(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).MinPage := Value;
end;

{ property Read MaxPage: Integer }

procedure TPrintDialog_Read_MaxPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).MaxPage;
end;

{ property Write MaxPage(Value: Integer) }

procedure TPrintDialog_Write_MaxPage(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).MaxPage := Value;
end;

{ property Read Options: TPrintDialogOptions }

procedure TPrintDialog_Read_Options(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TPrintDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TPrintDialogOptions) }

procedure TPrintDialog_Write_Options(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).Options := TPrintDialogOptions(Byte(V2S(Value)));
end;

{ property Read PrintToFile: Boolean }

procedure TPrintDialog_Read_PrintToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).PrintToFile;
end;

{ property Write PrintToFile(Value: Boolean) }

procedure TPrintDialog_Write_PrintToFile(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).PrintToFile := Value;
end;

{ property Read PrintRange: TPrintRange }

procedure TPrintDialog_Read_PrintRange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).PrintRange;
end;

{ property Write PrintRange(Value: TPrintRange) }

procedure TPrintDialog_Write_PrintRange(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).PrintRange := Value;
end;

{ property Read ToPage: Integer }

procedure TPrintDialog_Read_ToPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrintDialog(Args.Obj).ToPage;
end;

{ property Write ToPage(Value: Integer) }

procedure TPrintDialog_Write_ToPage(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TPrintDialog(Args.Obj).ToPage := Value;
end;

{$ENDIF COMPLIB_VCL}

{ TFindDialog }

{ constructor Create(AOwner: TComponent) }

procedure TFindDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFindDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure CloseDialog; }

{$IFDEF COMPLIB_VCL}
procedure TFindDialog_CloseDialog(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TFindDialog(Args.Obj).CloseDialog;
end;
{$ENDIF COMPLIB_VCL}

{ function Execute: Boolean; }

procedure TFindDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFindDialog(Args.Obj).Execute;
end;

{$IFDEF COMPLIB_VCL}

{ property Read Left: Integer }

procedure TFindDialog_Read_Left(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFindDialog(Args.Obj).Left;
end;

{ property Write Left(Value: Integer) }

procedure TFindDialog_Write_Left(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFindDialog(Args.Obj).Left := Value;
end;

{$ENDIF COMPLIB_VCL}

{ property Read Position: TPoint }

procedure TFindDialog_Read_Position(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Point2Var(TFindDialog(Args.Obj).Position);
end;

{ property Write Position(Value: TPoint) }

procedure TFindDialog_Write_Position(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFindDialog(Args.Obj).Position := Var2Point(Value);
end;

{$IFDEF COMPLIB_VCL}

{ property Read Top: Integer }

procedure TFindDialog_Read_Top(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFindDialog(Args.Obj).Top;
end;

{ property Write Top(Value: Integer) }

procedure TFindDialog_Write_Top(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFindDialog(Args.Obj).Top := Value;
end;

{$ENDIF COMPLIB_VCL}

{ property Read FindText: string }

procedure TFindDialog_Read_FindText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFindDialog(Args.Obj).FindText;
end;

{ property Write FindText(Value: string) }

procedure TFindDialog_Write_FindText(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFindDialog(Args.Obj).FindText := Value;
end;

{ property Read Options: TFindOptions }

procedure TFindDialog_Read_Options(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Word(TFindDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TFindOptions) }

procedure TFindDialog_Write_Options(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFindDialog(Args.Obj).Options := TFindOptions(Word(V2S(Value)));
end;

{ TReplaceDialog }

{ constructor Create(AOwner: TComponent) }

procedure TReplaceDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TReplaceDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read ReplaceText: string }

procedure TReplaceDialog_Read_ReplaceText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TReplaceDialog(Args.Obj).ReplaceText;
end;

{ property Write ReplaceText(Value: string) }

procedure TReplaceDialog_Write_ReplaceText(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TReplaceDialog(Args.Obj).ReplaceText := Value;
end;

{ function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TForm; }

{$IFDEF COMPLIB_VCL}
procedure JvInterpreter_CreateMessageDialog(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(CreateMessageDialog(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2])))));
end;
{$ENDIF COMPLIB_VCL}

{ function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; }

procedure JvInterpreter_MessageDlg(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  Value := MessageDlg(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3]);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Value := MessageDlg(VarToStr(Args.Values[0]), VarToStr(Args.Values[1]), TMsgDlgType(Byte(V2S(Args.Values[2]))),
    TMsgDlgButtons(Byte(V2S(Args.Values[3]))), Args.Values[4]);
  {$ENDIF COMPLIB_CLX}
end;

{ function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; }

procedure JvInterpreter_MessageDlgPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  Value := MessageDlgPos(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3],
    Args.Values[4], Args.Values[5]);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  Value := MessageDlgPos(Args.Values[0], Args.Values[1], TMsgDlgButtons(Byte(V2S(Args.Values[2]))), Args.Values[3],
    Args.Values[4], Args.Values[5]);
  {$ENDIF COMPLIB_CLX}
end;

{ function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; const HelpFileName: string): Integer; }

{$IFDEF COMPLIB_VCL}
{$IFDEF COMPILER3_UP}
procedure JvInterpreter_MessageDlgPosHelp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := MessageDlgPosHelp(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3],
    Args.Values[4], Args.Values[5], Args.Values[6]);
end;
{$ENDIF COMPILER3_UP}
{$ENDIF COMPLIB_VCL}

{ procedure ShowMessage(const Msg: string); }

procedure JvInterpreter_ShowMessage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPLIB_VCL}
  ShowMessage(Args.Values[0]);
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  ShowMessage(VarToStr(Args.Values[0]));
  {$ENDIF COMPLIB_CLX}
end;

{ procedure ShowMessageFmt(const Msg: string; Params: array of const); }

{$IFDEF COMPILER3_UP}
procedure JvInterpreter_ShowMessageFmt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Args.OpenArray(1);
  ShowMessageFmt(Args.Values[0], Slice(Args.OA^, Args.OAS));
end;
{$ENDIF COMPILER3_UP}

{ procedure ShowMessagePos(const Msg: string; X, Y: Integer); }

procedure JvInterpreter_ShowMessagePos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  ShowMessagePos(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function InputBox(const ACaption, APrompt, ADefault: string): string; }

procedure JvInterpreter_InputBox(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF MSWINDOWS}
  Value := InputBox(Args.Values[0], Args.Values[1], Args.Values[2]);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Value := InputBox(VarToStr(Args.Values[0]), VarToStr(Args.Values[1]), VarToStr(Args.Values[2]));
  {$ENDIF LINUX}
end;

{ function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean; }

procedure JvInterpreter_InputQuery(var Value: Variant; Args: TJvInterpreterArgs);
{$IFDEF LINUX}
var
  S: WideString;
{$ENDIF LINUX}
begin
  {$IFDEF MSWINDOWS}
  Value := InputQuery(Args.Values[0], Args.Values[1], string(TVarData(Args.Values[2]).vString));
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  S := Args.Values[2];
  Value := InputQuery(VarToStr(Args.Values[0]), VarToStr(Args.Values[1]), S);
  Args.Values[2] := S;
  {$ENDIF LINUX}
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cDialogs = 'Dialogs';
begin
  with JvInterpreterAdapter do
  begin
    { TCommonDialog }
    AddClass(cDialogs, TCommonDialog, 'TCommonDialog');
    {$IFDEF COMPILER3_UP}
    AddGet(TCommonDialog, 'Handle', TCommonDialog_Read_Handle, 0, [0], varEmpty);
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPLIB_VCL}
    AddGet(TCommonDialog, 'Ctl3D', TCommonDialog_Read_Ctl3D, 0, [0], varEmpty);
    AddSet(TCommonDialog, 'Ctl3D', TCommonDialog_Write_Ctl3D, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    AddGet(TCommonDialog, 'HelpContext', TCommonDialog_Read_HelpContext, 0, [0], varEmpty);
    AddSet(TCommonDialog, 'HelpContext', TCommonDialog_Write_HelpContext, 0, [0]);
    { TOpenOption }
    {$IFDEF COMPLIB_VCL}
    AddConst(cDialogs, 'ofReadOnly', Integer(ofReadOnly));
    {$ENDIF COMPLIB_VCL}
    AddConst(cDialogs, 'ofOverwritePrompt', Integer(ofOverwritePrompt));
    {$IFDEF COMPLIB_VCL}
    AddConst(cDialogs, 'ofHideReadOnly', Integer(ofHideReadOnly));
    AddConst(cDialogs, 'ofNoChangeDir', Integer(ofNoChangeDir));
    AddConst(cDialogs, 'ofShowHelp', Integer(ofShowHelp));
    AddConst(cDialogs, 'ofNoValidate', Integer(ofNoValidate));
    {$ENDIF COMPLIB_VCL}
    AddConst(cDialogs, 'ofAllowMultiSelect', Integer(ofAllowMultiSelect));
    AddConst(cDialogs, 'ofExtensionDifferent', Integer(ofExtensionDifferent));
    AddConst(cDialogs, 'ofPathMustExist', Integer(ofPathMustExist));
    AddConst(cDialogs, 'ofFileMustExist', Integer(ofFileMustExist));
    {$IFDEF COMPLIB_VCL}
    AddConst(cDialogs, 'ofCreatePrompt', Integer(ofCreatePrompt));
    AddConst(cDialogs, 'ofShareAware', Integer(ofShareAware));
    AddConst(cDialogs, 'ofNoReadOnlyReturn', Integer(ofNoReadOnlyReturn));
    AddConst(cDialogs, 'ofNoTestFileCreate', Integer(ofNoTestFileCreate));
    AddConst(cDialogs, 'ofNoNetworkButton', Integer(ofNoNetworkButton));
    AddConst(cDialogs, 'ofNoLongNames', Integer(ofNoLongNames));
    AddConst(cDialogs, 'ofOldStyleDialog', Integer(ofOldStyleDialog));
    AddConst(cDialogs, 'ofNoDereferenceLinks', Integer(ofNoDereferenceLinks));
    { TFileEditStyle }
    AddConst(cDialogs, 'fsEdit', Integer(fsEdit));
    AddConst(cDialogs, 'fsComboBox', Integer(fsComboBox));
    {$ENDIF COMPLIB_VCL}
    { TOpenDialog }
    AddClass(cDialogs, TOpenDialog, 'TOpenDialog');
    AddGet(TOpenDialog, 'Create', TOpenDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TOpenDialog, 'Execute', TOpenDialog_Execute, 0, [0], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TOpenDialog, 'FileEditStyle', TOpenDialog_Read_FileEditStyle, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'FileEditStyle', TOpenDialog_Write_FileEditStyle, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    AddGet(TOpenDialog, 'Files', TOpenDialog_Read_Files, 0, [0], varEmpty);
    AddGet(TOpenDialog, 'HistoryList', TOpenDialog_Read_HistoryList, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'HistoryList', TOpenDialog_Write_HistoryList, 0, [0]);
    AddGet(TOpenDialog, 'DefaultExt', TOpenDialog_Read_DefaultExt, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'DefaultExt', TOpenDialog_Write_DefaultExt, 0, [0]);
    AddGet(TOpenDialog, 'FileName', TOpenDialog_Read_FileName, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'FileName', TOpenDialog_Write_FileName, 0, [0]);
    AddGet(TOpenDialog, 'Filter', TOpenDialog_Read_Filter, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'Filter', TOpenDialog_Write_Filter, 0, [0]);
    AddGet(TOpenDialog, 'FilterIndex', TOpenDialog_Read_FilterIndex, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'FilterIndex', TOpenDialog_Write_FilterIndex, 0, [0]);
    AddGet(TOpenDialog, 'InitialDir', TOpenDialog_Read_InitialDir, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'InitialDir', TOpenDialog_Write_InitialDir, 0, [0]);
    AddGet(TOpenDialog, 'Options', TOpenDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'Options', TOpenDialog_Write_Options, 0, [0]);
    AddGet(TOpenDialog, 'Title', TOpenDialog_Read_Title, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'Title', TOpenDialog_Write_Title, 0, [0]);
    { TSaveDialog }
    AddClass(cDialogs, TSaveDialog, 'TSaveDialog');
    AddGet(TSaveDialog, 'Create', TSaveDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TSaveDialog, 'Execute', TSaveDialog_Execute, 0, [0], varEmpty);
    {$IFDEF COMPLIB_VCL}
    { TColorDialogOption }
    AddConst(cDialogs, 'cdFullOpen', Integer(cdFullOpen));
    AddConst(cDialogs, 'cdPreventFullOpen', Integer(cdPreventFullOpen));
    AddConst(cDialogs, 'cdShowHelp', Integer(cdShowHelp));
    AddConst(cDialogs, 'cdSolidColor', Integer(cdSolidColor));
    AddConst(cDialogs, 'cdAnyColor', Integer(cdAnyColor));
    {$ENDIF COMPLIB_VCL}
    { TColorDialog }
    AddClass(cDialogs, TColorDialog, 'TColorDialog');
    AddGet(TColorDialog, 'Create', TColorDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TColorDialog, 'Execute', TColorDialog_Execute, 0, [0], varEmpty);
    AddGet(TColorDialog, 'Color', TColorDialog_Read_Color, 0, [0], varEmpty);
    AddSet(TColorDialog, 'Color', TColorDialog_Write_Color, 0, [0]);
    AddGet(TColorDialog, 'CustomColors', TColorDialog_Read_CustomColors, 0, [0], varEmpty);
    AddSet(TColorDialog, 'CustomColors', TColorDialog_Write_CustomColors, 0, [0]);
    {$IFDEF COMPLIB_VCL}
    AddGet(TColorDialog, 'Options', TColorDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TColorDialog, 'Options', TColorDialog_Write_Options, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    {$IFDEF COMPLIB_VCL}
    { TFontDialogOption }
    AddConst(cDialogs, 'fdAnsiOnly', Integer(fdAnsiOnly));
    AddConst(cDialogs, 'fdTrueTypeOnly', Integer(fdTrueTypeOnly));
    AddConst(cDialogs, 'fdEffects', Integer(fdEffects));
    AddConst(cDialogs, 'fdFixedPitchOnly', Integer(fdFixedPitchOnly));
    AddConst(cDialogs, 'fdForceFontExist', Integer(fdForceFontExist));
    AddConst(cDialogs, 'fdNoFaceSel', Integer(fdNoFaceSel));
    AddConst(cDialogs, 'fdNoOEMFonts', Integer(fdNoOEMFonts));
    AddConst(cDialogs, 'fdNoSimulations', Integer(fdNoSimulations));
    AddConst(cDialogs, 'fdNoSizeSel', Integer(fdNoSizeSel));
    AddConst(cDialogs, 'fdNoStyleSel', Integer(fdNoStyleSel));
    AddConst(cDialogs, 'fdNoVectorFonts', Integer(fdNoVectorFonts));
    AddConst(cDialogs, 'fdShowHelp', Integer(fdShowHelp));
    AddConst(cDialogs, 'fdWysiwyg', Integer(fdWysiwyg));
    AddConst(cDialogs, 'fdLimitSize', Integer(fdLimitSize));
    AddConst(cDialogs, 'fdScalableOnly', Integer(fdScalableOnly));
    AddConst(cDialogs, 'fdApplyButton', Integer(fdApplyButton));
    { TFontDialogDevice }
    AddConst(cDialogs, 'fdScreen', Integer(fdScreen));
    AddConst(cDialogs, 'fdPrinter', Integer(fdPrinter));
    AddConst(cDialogs, 'fdBoth', Integer(fdBoth));
    {$ENDIF COMPLIB_VCL}
    { TFontDialog }
    AddClass(cDialogs, TFontDialog, 'TFontDialog');
    AddGet(TFontDialog, 'Create', TFontDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TFontDialog, 'Execute', TFontDialog_Execute, 0, [0], varEmpty);
    AddGet(TFontDialog, 'Font', TFontDialog_Read_Font, 0, [0], varEmpty);
    AddSet(TFontDialog, 'Font', TFontDialog_Write_Font, 0, [0]);
    {$IFDEF COMPLIB_VCL}
    AddGet(TFontDialog, 'Device', TFontDialog_Read_Device, 0, [0], varEmpty);
    AddSet(TFontDialog, 'Device', TFontDialog_Write_Device, 0, [0]);
    AddGet(TFontDialog, 'MinFontSize', TFontDialog_Read_MinFontSize, 0, [0], varEmpty);
    AddSet(TFontDialog, 'MinFontSize', TFontDialog_Write_MinFontSize, 0, [0]);
    AddGet(TFontDialog, 'MaxFontSize', TFontDialog_Read_MaxFontSize, 0, [0], varEmpty);
    AddSet(TFontDialog, 'MaxFontSize', TFontDialog_Write_MaxFontSize, 0, [0]);
    AddGet(TFontDialog, 'Options', TFontDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TFontDialog, 'Options', TFontDialog_Write_Options, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    {$IFDEF COMPLIB_VCL}
    { TPrinterSetupDialog }
    AddClass(cDialogs, TPrinterSetupDialog, 'TPrinterSetupDialog');
    AddGet(TPrinterSetupDialog, 'Create', TPrinterSetupDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TPrinterSetupDialog, 'Execute', TPrinterSetupDialog_Execute, 0, [0], varEmpty);
    { TPrintRange }
    AddConst(cDialogs, 'prAllPages', Integer(prAllPages));
    AddConst(cDialogs, 'prSelection', Integer(prSelection));
    AddConst(cDialogs, 'prPageNums', Integer(prPageNums));
    { TPrintDialogOption }
    AddConst(cDialogs, 'poPrintToFile', Integer(poPrintToFile));
    AddConst(cDialogs, 'poPageNums', Integer(poPageNums));
    AddConst(cDialogs, 'poSelection', Integer(poSelection));
    AddConst(cDialogs, 'poWarning', Integer(poWarning));
    AddConst(cDialogs, 'poHelp', Integer(poHelp));
    AddConst(cDialogs, 'poDisablePrintToFile', Integer(poDisablePrintToFile));
    { TPrintDialog }
    AddClass(cDialogs, TPrintDialog, 'TPrintDialog');
    AddGet(TPrintDialog, 'Create', TPrintDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TPrintDialog, 'Execute', TPrintDialog_Execute, 0, [0], varEmpty);
    AddGet(TPrintDialog, 'Collate', TPrintDialog_Read_Collate, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'Collate', TPrintDialog_Write_Collate, 0, [0]);
    AddGet(TPrintDialog, 'Copies', TPrintDialog_Read_Copies, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'Copies', TPrintDialog_Write_Copies, 0, [0]);
    AddGet(TPrintDialog, 'FromPage', TPrintDialog_Read_FromPage, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'FromPage', TPrintDialog_Write_FromPage, 0, [0]);
    AddGet(TPrintDialog, 'MinPage', TPrintDialog_Read_MinPage, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'MinPage', TPrintDialog_Write_MinPage, 0, [0]);
    AddGet(TPrintDialog, 'MaxPage', TPrintDialog_Read_MaxPage, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'MaxPage', TPrintDialog_Write_MaxPage, 0, [0]);
    AddGet(TPrintDialog, 'Options', TPrintDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'Options', TPrintDialog_Write_Options, 0, [0]);
    AddGet(TPrintDialog, 'PrintToFile', TPrintDialog_Read_PrintToFile, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'PrintToFile', TPrintDialog_Write_PrintToFile, 0, [0]);
    AddGet(TPrintDialog, 'PrintRange', TPrintDialog_Read_PrintRange, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'PrintRange', TPrintDialog_Write_PrintRange, 0, [0]);
    AddGet(TPrintDialog, 'ToPage', TPrintDialog_Read_ToPage, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'ToPage', TPrintDialog_Write_ToPage, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    { TFindOption }
    AddConst(cDialogs, 'frDown', Integer(frDown));
    AddConst(cDialogs, 'frFindNext', Integer(frFindNext));
    AddConst(cDialogs, 'frHideMatchCase', Integer(frHideMatchCase));
    AddConst(cDialogs, 'frHideWholeWord', Integer(frHideWholeWord));
    AddConst(cDialogs, 'frHideUpDown', Integer(frHideUpDown));
    AddConst(cDialogs, 'frMatchCase', Integer(frMatchCase));
    AddConst(cDialogs, 'frDisableMatchCase', Integer(frDisableMatchCase));
    AddConst(cDialogs, 'frDisableUpDown', Integer(frDisableUpDown));
    AddConst(cDialogs, 'frDisableWholeWord', Integer(frDisableWholeWord));
    AddConst(cDialogs, 'frReplace', Integer(frReplace));
    AddConst(cDialogs, 'frReplaceAll', Integer(frReplaceAll));
    AddConst(cDialogs, 'frWholeWord', Integer(frWholeWord));
    AddConst(cDialogs, 'frShowHelp', Integer(frShowHelp));
    { TFindDialog }
    AddClass(cDialogs, TFindDialog, 'TFindDialog');
    AddGet(TFindDialog, 'Create', TFindDialog_Create, 1, [varEmpty], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TFindDialog, 'CloseDialog', TFindDialog_CloseDialog, 0, [0], varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddGet(TFindDialog, 'Execute', TFindDialog_Execute, 0, [0], varEmpty);
    {$IFDEF COMPLIB_VCL}
    AddGet(TFindDialog, 'Left', TFindDialog_Read_Left, 0, [0], varEmpty);
    AddSet(TFindDialog, 'Left', TFindDialog_Write_Left, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    AddGet(TFindDialog, 'Position', TFindDialog_Read_Position, 0, [0], varEmpty);
    AddSet(TFindDialog, 'Position', TFindDialog_Write_Position, 0, [0]);
    {$IFDEF COMPLIB_VCL}
    AddGet(TFindDialog, 'Top', TFindDialog_Read_Top, 0, [0], varEmpty);
    AddSet(TFindDialog, 'Top', TFindDialog_Write_Top, 0, [0]);
    {$ENDIF COMPLIB_VCL}
    AddGet(TFindDialog, 'FindText', TFindDialog_Read_FindText, 0, [0], varEmpty);
    AddSet(TFindDialog, 'FindText', TFindDialog_Write_FindText, 0, [0]);
    AddGet(TFindDialog, 'Options', TFindDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TFindDialog, 'Options', TFindDialog_Write_Options, 0, [0]);
    { TReplaceDialog }
    AddClass(cDialogs, TReplaceDialog, 'TReplaceDialog');
    AddGet(TReplaceDialog, 'Create', TReplaceDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TReplaceDialog, 'ReplaceText', TReplaceDialog_Read_ReplaceText, 0, [0], varEmpty);
    AddSet(TReplaceDialog, 'ReplaceText', TReplaceDialog_Write_ReplaceText, 0, [0]);
    { TMsgDlgType }
    AddConst(cDialogs, 'mtWarning', Integer(mtWarning));
    AddConst(cDialogs, 'mtError', Integer(mtError));
    AddConst(cDialogs, 'mtInformation', Integer(mtInformation));
    AddConst(cDialogs, 'mtConfirmation', Integer(mtConfirmation));
    AddConst(cDialogs, 'mtCustom', Integer(mtCustom));
    { TMsgDlgBtn }
    AddConst(cDialogs, 'mbYes', Integer(mbYes));
    AddConst(cDialogs, 'mbNo', Integer(mbNo));
    AddConst(cDialogs, 'mbOK', Integer(mbOK));
    AddConst(cDialogs, 'mbCancel', Integer(mbCancel));
    AddConst(cDialogs, 'mbAbort', Integer(mbAbort));
    AddConst(cDialogs, 'mbRetry', Integer(mbRetry));
    AddConst(cDialogs, 'mbIgnore', Integer(mbIgnore));
    {$IFDEF COMPLIB_VCL}
    AddConst(cDialogs, 'mbAll', Integer(mbAll));
    {$IFDEF COMPILER3_UP}
    AddConst(cDialogs, 'mbNoToAll', Integer(mbNoToAll));
    AddConst(cDialogs, 'mbYesToAll', Integer(mbYesToAll));
    {$ENDIF COMPILER3_UP}
    AddConst(cDialogs, 'mbHelp', Integer(mbHelp));
    {$ENDIF COMPLIB_VCL}
    AddConst(cDialogs, 'mrNone', Integer(mrNone));
    AddConst(cDialogs, 'mrOk', Integer(mrOk));
    AddConst(cDialogs, 'mrCancel', Integer(mrCancel));
    AddConst(cDialogs, 'mrAbort', Integer(mrAbort));
    AddConst(cDialogs, 'mrRetry', Integer(mrRetry));
    AddConst(cDialogs, 'mrIgnore', Integer(mrIgnore));
    AddConst(cDialogs, 'mrYes', Integer(mrYes));
    AddConst(cDialogs, 'mrNo', Integer(mrNo));
    AddConst(cDialogs, 'mrAll', Integer(mrAll));
    {$IFDEF COMPILER3_UP}
    AddConst(cDialogs, 'mrNoToAll', Integer(mrNoToAll));
    AddConst(cDialogs, 'mrYesToAll', Integer(mrYesToAll));
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPLIB_VCL}
    AddFun(cDialogs, 'CreateMessageDialog', JvInterpreter_CreateMessageDialog, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    {$ENDIF COMPLIB_VCL}
    AddFun(cDialogs, 'MessageDlg', JvInterpreter_MessageDlg, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun(cDialogs, 'MessageDlgPos', JvInterpreter_MessageDlgPos, 6, [varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty, varEmpty], varEmpty);
    {$IFDEF COMPLIB_VCL}
    {$IFDEF COMPILER3_UP}
    AddFun(cDialogs, 'MessageDlgPosHelp', JvInterpreter_MessageDlgPosHelp, 7, [varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty, varEmpty, varEmpty], varEmpty);
    {$ENDIF COMPILER3_UP}
    {$ENDIF COMPLIB_VCL}
    AddFun(cDialogs, 'ShowMessage', JvInterpreter_ShowMessage, 1, [varEmpty], varEmpty);
    {$IFDEF COMPILER3_UP}
    AddFun(cDialogs, 'ShowMessageFmt', JvInterpreter_ShowMessageFmt, 2, [varEmpty, varEmpty], varEmpty);
    {$ENDIF COMPILER3_UP}
    AddFun(cDialogs, 'ShowMessagePos', JvInterpreter_ShowMessagePos, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun(cDialogs, 'InputBox', JvInterpreter_InputBox, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun(cDialogs, 'InputQuery', JvInterpreter_InputQuery, 3, [varEmpty, varEmpty, varByRef], varEmpty);
  end;
  RegisterClasses([TOpenDialog, TSaveDialog, TFontDialog, TColorDialog,
    {$IFDEF COMPLIB_VCL} TPrintDialog, TPrinterSetupDialog, {$ENDIF COMPLIB_VCL}
    TFindDialog, TReplaceDialog]);
end;

end.

