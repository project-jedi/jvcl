{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrLEdit.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvStringsForm;

{ TStrings property editor originally from the Rx library: duplicated for internal use) }

interface

uses
  Classes,
  {$IFDEF VCL}
  Windows, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF}
  {$IFDEF VisualCLX}
  QDialogs, QStdCtrls, QControls, QExtCtrls, QWindows,
  {$ENDIF}
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
    FFilename: string;
  end;

implementation

uses
  SysUtils,
  {$IFDEF VCL}
  LibHelp,
  {$ENDIF}
  JvDsgnConsts;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF}

procedure TJvStrEditDlg.FileOpen(Sender: TObject);
begin
  with OpenDialog do
  begin
    Filter := RsTextFilter;
    Filename := FFilename;
    if Execute then
    begin
      FFilename := Filename;
      Memo.Lines.LoadFromFile(FileName);
    end;
  end;
end;

procedure TJvStrEditDlg.FileSave(Sender: TObject);
begin
  if SaveDialog.FileName = '' then
    SaveDialog.FileName := FFilename;
  with SaveDialog do
  begin
    Filter := RsTextFilter;
    if Execute then
      // FFilename := Filename;
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
  HelpContext := hcDStringListEditor;
  OpenDialog.HelpContext := hcDStringListLoad;
  SaveDialog.HelpContext := hcDStringListSave;
  SingleLine := RsSingleLine;
  MultipleLines := RsMultipleLines;
end;

procedure TJvStrEditDlg.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    CancelBtn.Click;
end;

procedure TJvStrEditDlg.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.

