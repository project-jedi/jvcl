{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Dialogs.PAS, released on 2002-07-04.

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

unit JvInterpreter_Dialogs;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  Classes,
  {$IFDEF VCL}
  Graphics, Controls, Dialogs,
  JvInterpreter_Windows;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Variants, Qt, QGraphics, QControls, QDialogs,
  JvInterpreter_Types;
  {$ENDIF VisualCLX}

{$IFDEF VisualCLX}
type
  TCommonDialog = TQtDialog;
{$ENDIF VisualCLX}

{ TCommonDialog }

{ property Read Handle: HWnd }

procedure TCommonDialog_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TCommonDialog(Args.Obj).Handle);
end;

{$IFDEF VCL}

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

{$ENDIF VCL}

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

{$IFDEF VCL}

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

{$ENDIF VCL}

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
  {$IFDEF VCL}
  Value := S2V(Integer(TOpenDialog(Args.Obj).Options));
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Value := S2V(Word(TOpenDialog(Args.Obj).Options));
  {$ENDIF VisualCLX}
end;

{ property Write Options(Value: TOpenOptions) }

procedure TOpenDialog_Write_Options(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  TOpenDialog(Args.Obj).Options := TOpenOptions(V2S(Value));
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  TOpenDialog(Args.Obj).Options := TOpenOptions(Word(V2S(Value)));
  {$ENDIF VisualCLX}
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

{$IFDEF VCL}

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

{$ENDIF VCL}

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

{$IFDEF VCL}

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

{$ENDIF VCL}

{$IFDEF VCL}

{ TPrinterSetupDialog }

{ constructor Create(AOwner: TComponent) }

procedure TPrinterSetupDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPrinterSetupDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function Execute: Boolean; }

procedure TPrinterSetupDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPrinterSetupDialog(Args.Obj).Execute;
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

{$ENDIF VCL}

{ TFindDialog }

{ constructor Create(AOwner: TComponent) }

procedure TFindDialog_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFindDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure CloseDialog; }

{$IFDEF VCL}
procedure TFindDialog_CloseDialog(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TFindDialog(Args.Obj).CloseDialog;
end;
{$ENDIF VCL}

{ function Execute: Boolean; }

procedure TFindDialog_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFindDialog(Args.Obj).Execute;
end;

{$IFDEF VCL}

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

{$ENDIF VCL}

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

{$IFDEF VCL}

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

{$ENDIF VCL}

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

{$IFDEF VCL}
procedure JvInterpreter_CreateMessageDialog(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(CreateMessageDialog(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2])))));
end;
{$ENDIF VCL}

{ function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; }

procedure JvInterpreter_MessageDlg(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  Value := MessageDlg(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3]);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Value := MessageDlg(VarToStr(Args.Values[0]), VarToStr(Args.Values[1]), TMsgDlgType(Byte(V2S(Args.Values[2]))),
    TMsgDlgButtons(Byte(V2S(Args.Values[3]))), Args.Values[4]);
  {$ENDIF VisualCLX}
end;

{ function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; }

procedure JvInterpreter_MessageDlgPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  Value := MessageDlgPos(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3],
    Args.Values[4], Args.Values[5]);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Value := MessageDlgPos(Args.Values[0], Args.Values[1], TMsgDlgButtons(Byte(V2S(Args.Values[2]))), Args.Values[3],
    Args.Values[4], Args.Values[5]);
  {$ENDIF VisualCLX}
end;

{ function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; const HelpFileName: string): Integer; }

{$IFDEF VCL}
procedure JvInterpreter_MessageDlgPosHelp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := MessageDlgPosHelp(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3],
    Args.Values[4], Args.Values[5], Args.Values[6]);
end;
{$ENDIF VCL}

{ procedure ShowMessage(const Msg: string); }

procedure JvInterpreter_ShowMessage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  ShowMessage(Args.Values[0]);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  ShowMessage(VarToStr(Args.Values[0]));
  {$ENDIF VisualCLX}
end;

{ procedure ShowMessageFmt(const Msg: string; Params: array of const); }

procedure JvInterpreter_ShowMessageFmt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Args.OpenArray(1);
  ShowMessageFmt(Args.Values[0], Slice(Args.OA^, Args.OAS));
end;

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
  {$IFDEF UNIX}
  Value := InputBox(VarToStr(Args.Values[0]), VarToStr(Args.Values[1]), VarToStr(Args.Values[2]));
  {$ENDIF UNIX}
end;

{ function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean; }

procedure JvInterpreter_InputQuery(var Value: Variant; Args: TJvInterpreterArgs);
{$IFDEF UNIX}
var
  S: WideString;
{$ENDIF UNIX}
begin
  {$IFDEF MSWINDOWS}
  Value := InputQuery(Args.Values[0], Args.Values[1], string(TVarData(Args.Values[2]).vString));
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  S := Args.Values[2];
  Value := InputQuery(VarToStr(Args.Values[0]), VarToStr(Args.Values[1]), S);
  Args.Values[2] := S;
  {$ENDIF UNIX}
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cDialogs = 'Dialogs';
begin
  with JvInterpreterAdapter do
  begin
    { TCommonDialog }
    AddClass(cDialogs, TCommonDialog, 'TCommonDialog');
    AddGet(TCommonDialog, 'Handle', TCommonDialog_Read_Handle, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCommonDialog, 'Ctl3D', TCommonDialog_Read_Ctl3D, 0, [varEmpty], varEmpty);
    AddSet(TCommonDialog, 'Ctl3D', TCommonDialog_Write_Ctl3D, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TCommonDialog, 'HelpContext', TCommonDialog_Read_HelpContext, 0, [varEmpty], varEmpty);
    AddSet(TCommonDialog, 'HelpContext', TCommonDialog_Write_HelpContext, 0, [varEmpty]);
    { TOpenOption }
    {$IFDEF VCL}
    AddConst(cDialogs, 'ofReadOnly', Ord(ofReadOnly));
    {$ENDIF VCL}
    AddConst(cDialogs, 'ofOverwritePrompt', Ord(ofOverwritePrompt));
    {$IFDEF VCL}
    AddConst(cDialogs, 'ofHideReadOnly', Ord(ofHideReadOnly));
    AddConst(cDialogs, 'ofNoChangeDir', Ord(ofNoChangeDir));
    AddConst(cDialogs, 'ofShowHelp', Ord(ofShowHelp));
    AddConst(cDialogs, 'ofNoValidate', Ord(ofNoValidate));
    {$ENDIF VCL}
    AddConst(cDialogs, 'ofAllowMultiSelect', Ord(ofAllowMultiSelect));
    AddConst(cDialogs, 'ofExtensionDifferent', Ord(ofExtensionDifferent));
    AddConst(cDialogs, 'ofPathMustExist', Ord(ofPathMustExist));
    AddConst(cDialogs, 'ofFileMustExist', Ord(ofFileMustExist));
    {$IFDEF VCL}
    AddConst(cDialogs, 'ofCreatePrompt', Ord(ofCreatePrompt));
    AddConst(cDialogs, 'ofShareAware', Ord(ofShareAware));
    AddConst(cDialogs, 'ofNoReadOnlyReturn', Ord(ofNoReadOnlyReturn));
    AddConst(cDialogs, 'ofNoTestFileCreate', Ord(ofNoTestFileCreate));
    AddConst(cDialogs, 'ofNoNetworkButton', Ord(ofNoNetworkButton));
    AddConst(cDialogs, 'ofNoLongNames', Ord(ofNoLongNames));
    AddConst(cDialogs, 'ofOldStyleDialog', Ord(ofOldStyleDialog));
    AddConst(cDialogs, 'ofNoDereferenceLinks', Ord(ofNoDereferenceLinks));
    { TFileEditStyle }
    AddConst(cDialogs, 'fsEdit', Ord(fsEdit));
    AddConst(cDialogs, 'fsComboBox', Ord(fsComboBox));
    {$ENDIF VCL}
    { TOpenDialog }
    AddClass(cDialogs, TOpenDialog, 'TOpenDialog');
    AddGet(TOpenDialog, 'Create', TOpenDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TOpenDialog, 'Execute', TOpenDialog_Execute, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TOpenDialog, 'FileEditStyle', TOpenDialog_Read_FileEditStyle, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'FileEditStyle', TOpenDialog_Write_FileEditStyle, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TOpenDialog, 'Files', TOpenDialog_Read_Files, 0, [varEmpty], varEmpty);
    AddGet(TOpenDialog, 'HistoryList', TOpenDialog_Read_HistoryList, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'HistoryList', TOpenDialog_Write_HistoryList, 0, [varEmpty]);
    AddGet(TOpenDialog, 'DefaultExt', TOpenDialog_Read_DefaultExt, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'DefaultExt', TOpenDialog_Write_DefaultExt, 0, [varEmpty]);
    AddGet(TOpenDialog, 'FileName', TOpenDialog_Read_FileName, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'FileName', TOpenDialog_Write_FileName, 0, [varEmpty]);
    AddGet(TOpenDialog, 'Filter', TOpenDialog_Read_Filter, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'Filter', TOpenDialog_Write_Filter, 0, [varEmpty]);
    AddGet(TOpenDialog, 'FilterIndex', TOpenDialog_Read_FilterIndex, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'FilterIndex', TOpenDialog_Write_FilterIndex, 0, [varEmpty]);
    AddGet(TOpenDialog, 'InitialDir', TOpenDialog_Read_InitialDir, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'InitialDir', TOpenDialog_Write_InitialDir, 0, [varEmpty]);
    AddGet(TOpenDialog, 'Options', TOpenDialog_Read_Options, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'Options', TOpenDialog_Write_Options, 0, [varEmpty]);
    AddGet(TOpenDialog, 'Title', TOpenDialog_Read_Title, 0, [varEmpty], varEmpty);
    AddSet(TOpenDialog, 'Title', TOpenDialog_Write_Title, 0, [varEmpty]);
    { TSaveDialog }
    AddClass(cDialogs, TSaveDialog, 'TSaveDialog');
    AddGet(TSaveDialog, 'Create', TSaveDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TSaveDialog, 'Execute', TSaveDialog_Execute, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    { TColorDialogOption }
    AddConst(cDialogs, 'cdFullOpen', Ord(cdFullOpen));
    AddConst(cDialogs, 'cdPreventFullOpen', Ord(cdPreventFullOpen));
    AddConst(cDialogs, 'cdShowHelp', Ord(cdShowHelp));
    AddConst(cDialogs, 'cdSolidColor', Ord(cdSolidColor));
    AddConst(cDialogs, 'cdAnyColor', Ord(cdAnyColor));
    {$ENDIF VCL}
    { TColorDialog }
    AddClass(cDialogs, TColorDialog, 'TColorDialog');
    AddGet(TColorDialog, 'Create', TColorDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TColorDialog, 'Execute', TColorDialog_Execute, 0, [varEmpty], varEmpty);
    AddGet(TColorDialog, 'Color', TColorDialog_Read_Color, 0, [varEmpty], varEmpty);
    AddSet(TColorDialog, 'Color', TColorDialog_Write_Color, 0, [varEmpty]);
    AddGet(TColorDialog, 'CustomColors', TColorDialog_Read_CustomColors, 0, [varEmpty], varEmpty);
    AddSet(TColorDialog, 'CustomColors', TColorDialog_Write_CustomColors, 0, [varEmpty]);
    {$IFDEF VCL}
    AddGet(TColorDialog, 'Options', TColorDialog_Read_Options, 0, [varEmpty], varEmpty);
    AddSet(TColorDialog, 'Options', TColorDialog_Write_Options, 0, [varEmpty]);
    {$ENDIF VCL}
    {$IFDEF VCL}
    { TFontDialogOption }
    AddConst(cDialogs, 'fdAnsiOnly', Ord(fdAnsiOnly));
    AddConst(cDialogs, 'fdTrueTypeOnly', Ord(fdTrueTypeOnly));
    AddConst(cDialogs, 'fdEffects', Ord(fdEffects));
    AddConst(cDialogs, 'fdFixedPitchOnly', Ord(fdFixedPitchOnly));
    AddConst(cDialogs, 'fdForceFontExist', Ord(fdForceFontExist));
    AddConst(cDialogs, 'fdNoFaceSel', Ord(fdNoFaceSel));
    AddConst(cDialogs, 'fdNoOEMFonts', Ord(fdNoOEMFonts));
    AddConst(cDialogs, 'fdNoSimulations', Ord(fdNoSimulations));
    AddConst(cDialogs, 'fdNoSizeSel', Ord(fdNoSizeSel));
    AddConst(cDialogs, 'fdNoStyleSel', Ord(fdNoStyleSel));
    AddConst(cDialogs, 'fdNoVectorFonts', Ord(fdNoVectorFonts));
    AddConst(cDialogs, 'fdShowHelp', Ord(fdShowHelp));
    AddConst(cDialogs, 'fdWysiwyg', Ord(fdWysiwyg));
    AddConst(cDialogs, 'fdLimitSize', Ord(fdLimitSize));
    AddConst(cDialogs, 'fdScalableOnly', Ord(fdScalableOnly));
    AddConst(cDialogs, 'fdApplyButton', Ord(fdApplyButton));
    { TFontDialogDevice }
    AddConst(cDialogs, 'fdScreen', Ord(fdScreen));
    AddConst(cDialogs, 'fdPrinter', Ord(fdPrinter));
    AddConst(cDialogs, 'fdBoth', Ord(fdBoth));
    {$ENDIF VCL}
    { TFontDialog }
    AddClass(cDialogs, TFontDialog, 'TFontDialog');
    AddGet(TFontDialog, 'Create', TFontDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TFontDialog, 'Execute', TFontDialog_Execute, 0, [varEmpty], varEmpty);
    AddGet(TFontDialog, 'Font', TFontDialog_Read_Font, 0, [varEmpty], varEmpty);
    AddSet(TFontDialog, 'Font', TFontDialog_Write_Font, 0, [varEmpty]);
    {$IFDEF VCL}
    AddGet(TFontDialog, 'Device', TFontDialog_Read_Device, 0, [varEmpty], varEmpty);
    AddSet(TFontDialog, 'Device', TFontDialog_Write_Device, 0, [varEmpty]);
    AddGet(TFontDialog, 'MinFontSize', TFontDialog_Read_MinFontSize, 0, [varEmpty], varEmpty);
    AddSet(TFontDialog, 'MinFontSize', TFontDialog_Write_MinFontSize, 0, [varEmpty]);
    AddGet(TFontDialog, 'MaxFontSize', TFontDialog_Read_MaxFontSize, 0, [varEmpty], varEmpty);
    AddSet(TFontDialog, 'MaxFontSize', TFontDialog_Write_MaxFontSize, 0, [varEmpty]);
    AddGet(TFontDialog, 'Options', TFontDialog_Read_Options, 0, [varEmpty], varEmpty);
    AddSet(TFontDialog, 'Options', TFontDialog_Write_Options, 0, [varEmpty]);
    {$ENDIF VCL}
    {$IFDEF VCL}
    { TPrinterSetupDialog }
    AddClass(cDialogs, TPrinterSetupDialog, 'TPrinterSetupDialog');
    AddGet(TPrinterSetupDialog, 'Create', TPrinterSetupDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TPrinterSetupDialog, 'Execute', TPrinterSetupDialog_Execute, 0, [varEmpty], varEmpty);
    { TPrintRange }
    AddConst(cDialogs, 'prAllPages', Ord(prAllPages));
    AddConst(cDialogs, 'prSelection', Ord(prSelection));
    AddConst(cDialogs, 'prPageNums', Ord(prPageNums));
    { TPrintDialogOption }
    AddConst(cDialogs, 'poPrintToFile', Ord(poPrintToFile));
    AddConst(cDialogs, 'poPageNums', Ord(poPageNums));
    AddConst(cDialogs, 'poSelection', Ord(poSelection));
    AddConst(cDialogs, 'poWarning', Ord(poWarning));
    AddConst(cDialogs, 'poHelp', Ord(poHelp));
    AddConst(cDialogs, 'poDisablePrintToFile', Ord(poDisablePrintToFile));
    { TPrintDialog }
    AddClass(cDialogs, TPrintDialog, 'TPrintDialog');
    AddGet(TPrintDialog, 'Create', TPrintDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TPrintDialog, 'Execute', TPrintDialog_Execute, 0, [varEmpty], varEmpty);
    AddGet(TPrintDialog, 'Collate', TPrintDialog_Read_Collate, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'Collate', TPrintDialog_Write_Collate, 0, [varEmpty]);
    AddGet(TPrintDialog, 'Copies', TPrintDialog_Read_Copies, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'Copies', TPrintDialog_Write_Copies, 0, [varEmpty]);
    AddGet(TPrintDialog, 'FromPage', TPrintDialog_Read_FromPage, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'FromPage', TPrintDialog_Write_FromPage, 0, [varEmpty]);
    AddGet(TPrintDialog, 'MinPage', TPrintDialog_Read_MinPage, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'MinPage', TPrintDialog_Write_MinPage, 0, [varEmpty]);
    AddGet(TPrintDialog, 'MaxPage', TPrintDialog_Read_MaxPage, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'MaxPage', TPrintDialog_Write_MaxPage, 0, [varEmpty]);
    AddGet(TPrintDialog, 'Options', TPrintDialog_Read_Options, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'Options', TPrintDialog_Write_Options, 0, [varEmpty]);
    AddGet(TPrintDialog, 'PrintToFile', TPrintDialog_Read_PrintToFile, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'PrintToFile', TPrintDialog_Write_PrintToFile, 0, [varEmpty]);
    AddGet(TPrintDialog, 'PrintRange', TPrintDialog_Read_PrintRange, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'PrintRange', TPrintDialog_Write_PrintRange, 0, [varEmpty]);
    AddGet(TPrintDialog, 'ToPage', TPrintDialog_Read_ToPage, 0, [varEmpty], varEmpty);
    AddSet(TPrintDialog, 'ToPage', TPrintDialog_Write_ToPage, 0, [varEmpty]);
    {$ENDIF VCL}
    { TFindOption }
    AddConst(cDialogs, 'frDown', Ord(frDown));
    AddConst(cDialogs, 'frFindNext', Ord(frFindNext));
    AddConst(cDialogs, 'frHideMatchCase', Ord(frHideMatchCase));
    AddConst(cDialogs, 'frHideWholeWord', Ord(frHideWholeWord));
    AddConst(cDialogs, 'frHideUpDown', Ord(frHideUpDown));
    AddConst(cDialogs, 'frMatchCase', Ord(frMatchCase));
    AddConst(cDialogs, 'frDisableMatchCase', Ord(frDisableMatchCase));
    AddConst(cDialogs, 'frDisableUpDown', Ord(frDisableUpDown));
    AddConst(cDialogs, 'frDisableWholeWord', Ord(frDisableWholeWord));
    AddConst(cDialogs, 'frReplace', Ord(frReplace));
    AddConst(cDialogs, 'frReplaceAll', Ord(frReplaceAll));
    AddConst(cDialogs, 'frWholeWord', Ord(frWholeWord));
    AddConst(cDialogs, 'frShowHelp', Ord(frShowHelp));
    { TFindDialog }
    AddClass(cDialogs, TFindDialog, 'TFindDialog');
    AddGet(TFindDialog, 'Create', TFindDialog_Create, 1, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TFindDialog, 'CloseDialog', TFindDialog_CloseDialog, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TFindDialog, 'Execute', TFindDialog_Execute, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TFindDialog, 'Left', TFindDialog_Read_Left, 0, [varEmpty], varEmpty);
    AddSet(TFindDialog, 'Left', TFindDialog_Write_Left, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TFindDialog, 'Position', TFindDialog_Read_Position, 0, [varEmpty], varEmpty);
    AddSet(TFindDialog, 'Position', TFindDialog_Write_Position, 0, [varEmpty]);
    {$IFDEF VCL}
    AddGet(TFindDialog, 'Top', TFindDialog_Read_Top, 0, [varEmpty], varEmpty);
    AddSet(TFindDialog, 'Top', TFindDialog_Write_Top, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TFindDialog, 'FindText', TFindDialog_Read_FindText, 0, [varEmpty], varEmpty);
    AddSet(TFindDialog, 'FindText', TFindDialog_Write_FindText, 0, [varEmpty]);
    AddGet(TFindDialog, 'Options', TFindDialog_Read_Options, 0, [varEmpty], varEmpty);
    AddSet(TFindDialog, 'Options', TFindDialog_Write_Options, 0, [varEmpty]);
    { TReplaceDialog }
    AddClass(cDialogs, TReplaceDialog, 'TReplaceDialog');
    AddGet(TReplaceDialog, 'Create', TReplaceDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TReplaceDialog, 'ReplaceText', TReplaceDialog_Read_ReplaceText, 0, [varEmpty], varEmpty);
    AddSet(TReplaceDialog, 'ReplaceText', TReplaceDialog_Write_ReplaceText, 0, [varEmpty]);
    { TMsgDlgType }
    AddConst(cDialogs, 'mtWarning', Ord(mtWarning));
    AddConst(cDialogs, 'mtError', Ord(mtError));
    AddConst(cDialogs, 'mtInformation', Ord(mtInformation));
    AddConst(cDialogs, 'mtConfirmation', Ord(mtConfirmation));
    AddConst(cDialogs, 'mtCustom', Ord(mtCustom));
    { TMsgDlgBtn }
    AddConst(cDialogs, 'mbYes', Ord(mbYes));
    AddConst(cDialogs, 'mbNo', Ord(mbNo));
    AddConst(cDialogs, 'mbOK', Ord(mbOK));
    AddConst(cDialogs, 'mbCancel', Ord(mbCancel));
    AddConst(cDialogs, 'mbAbort', Ord(mbAbort));
    AddConst(cDialogs, 'mbRetry', Ord(mbRetry));
    AddConst(cDialogs, 'mbIgnore', Ord(mbIgnore));
    {$IFDEF VCL}
    AddConst(cDialogs, 'mbAll', Ord(mbAll));
    AddConst(cDialogs, 'mbNoToAll', Ord(mbNoToAll));
    AddConst(cDialogs, 'mbYesToAll', Ord(mbYesToAll));
    AddConst(cDialogs, 'mbHelp', Ord(mbHelp));
    {$ENDIF VCL}
    AddConst(cDialogs, 'mrNone', Ord(mrNone));
    AddConst(cDialogs, 'mrOk', Ord(mrOk));
    AddConst(cDialogs, 'mrCancel', Ord(mrCancel));
    AddConst(cDialogs, 'mrAbort', Ord(mrAbort));
    AddConst(cDialogs, 'mrRetry', Ord(mrRetry));
    AddConst(cDialogs, 'mrIgnore', Ord(mrIgnore));
    AddConst(cDialogs, 'mrYes', Ord(mrYes));
    AddConst(cDialogs, 'mrNo', Ord(mrNo));
    AddConst(cDialogs, 'mrAll', Ord(mrAll));
    AddConst(cDialogs, 'mrNoToAll', Ord(mrNoToAll));
    AddConst(cDialogs, 'mrYesToAll', Ord(mrYesToAll));
    {$IFDEF VCL}
    AddFunction(cDialogs, 'CreateMessageDialog', JvInterpreter_CreateMessageDialog, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    {$ENDIF VCL}
    AddFunction(cDialogs, 'MessageDlg', JvInterpreter_MessageDlg, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFunction(cDialogs, 'MessageDlgPos', JvInterpreter_MessageDlgPos, 6, [varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddFunction(cDialogs, 'MessageDlgPosHelp', JvInterpreter_MessageDlgPosHelp, 7, [varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty, varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddFunction(cDialogs, 'ShowMessage', JvInterpreter_ShowMessage, 1, [varEmpty], varEmpty);
    AddFunction(cDialogs, 'ShowMessageFmt', JvInterpreter_ShowMessageFmt, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cDialogs, 'ShowMessagePos', JvInterpreter_ShowMessagePos, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFunction(cDialogs, 'InputBox', JvInterpreter_InputBox, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFunction(cDialogs, 'InputQuery', JvInterpreter_InputQuery, 3, [varEmpty, varEmpty, varByRef], varEmpty);
  end;
  RegisterClasses([TOpenDialog, TSaveDialog, TFontDialog, TColorDialog,
    {$IFDEF VCL} TPrintDialog, TPrinterSetupDialog, {$ENDIF}
    TFindDialog, TReplaceDialog]);
end;

end.

