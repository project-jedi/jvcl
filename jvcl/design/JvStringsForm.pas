{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrLEdit.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  TStrings property editor originally from the Rx library (duplicated for internal use)

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvStringsForm;

{$I jvcl.inc}

interface

uses
  Classes,
  {$IFDEF VCL}
  Windows, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms, QControls, QDialogs, QStdCtrls, QExtCtrls, QWindows,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvComponent;

type
  TJvStrEditDlg = class(TJvForm)
    Memo: TMemo;
    LineCount: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    LoadBtn: TButton;
    SaveBtn: TButton;
    BevelBorder: TBevel;
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure UpdateStatus(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
  private
    // (rom) removed string[15] to increase flexibility
    SingleLine: string;
    MultipleLines: string;
    FFileName: string;
  end;

implementation

uses
  SysUtils,
  {$IFDEF VCL}
  LibHelp,
  {$ENDIF VCL}
  JvDsgnConsts;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

procedure TJvStrEditDlg.FileOpen(Sender: TObject);
begin
  with OpenDialog do
  begin
    Filter := RsTextFilter;
    FileName := FFileName;
    if Execute then
    begin
      FFileName := FileName;
      Memo.Lines.LoadFromFile(FileName);
    end;
  end;
end;

procedure TJvStrEditDlg.FileSave(Sender: TObject);
begin
  if SaveDialog.FileName = '' then
    SaveDialog.FileName := FFileName;
  with SaveDialog do
  begin
    Filter := RsTextFilter;
    if Execute then
      // FFileName := FileName;
      Memo.Lines.SaveToFile(FileName);
  end;
end;

procedure TJvStrEditDlg.UpdateStatus(Sender: TObject);
var
  Count: Integer;
begin
  Count := Memo.Lines.Count;
  if Count = 1 then
    LineCount.Caption := Format('%d %s', [Count, SingleLine])
  else
    LineCount.Caption := Format('%d %s', [Count, MultipleLines]);
end;

procedure TJvStrEditDlg.FormCreate(Sender: TObject);
begin
  {$IFDEF VCL}
  HelpContext := hcDStringListEditor;
  OpenDialog.HelpContext := hcDStringListLoad;
  SaveDialog.HelpContext := hcDStringListSave;
  {$ENDIF VCL}
  SingleLine := RsSingleLine;
  MultipleLines := RsMultipleLines;
  // set anchors
  BevelBorder.Anchors := [akLeft, akTop, akRight, akBottom];
  Memo.Anchors := [akLeft, akTop, akRight, akBottom];
  OKBtn.Anchors := [akRight, akBottom];
  CancelBtn.Anchors := [akRight, akBottom];
  HelpBtn.Anchors := [akRight, akBottom];
  LoadBtn.Anchors := [akLeft, akBottom];
  SaveBtn.Anchors := [akLeft, akBottom];
end;

procedure TJvStrEditDlg.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //if Key = VK_ESCAPE then
  if Ord(Key) = 27 then   //asn: With VisualCLX VK_ESCAPE <> 27
    CancelBtn.Click;
end;

procedure TJvStrEditDlg.HelpBtnClick(Sender: TObject);
begin
  {$IFDEF VCL}
  Application.HelpContext(HelpContext);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Application.ContextHelp(HelpContext);
  {$ENDIF VisualCLX}
end;

end.

