{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MainConfig.pas, released on 2003-10-01.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-12-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit MainConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JVCLConfiguration, CheckLst, Buttons, ExtCtrls,
  ImgList;

type
  TFormMainConfig = class(TForm)
    CheckListBox: TCheckListBox;
    ScrollBox: TScrollBox;
    LblComment: TLabel;
    BtnReload: TBitBtn;
    BtnQuit: TBitBtn;
    BtnSave: TBitBtn;
    BevelBorder: TBevel;
    TitlePanel: TPanel;
    imgProjectJEDI: TImage;
    Label4: TLabel;
    BevelHeader: TBevel;
    PanelSpace: TPanel;
    LabelTmp1: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckListBoxClick(Sender: TObject);
    procedure BtnReloadClick(Sender: TObject);
    procedure BtnQuitClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure CheckListBoxClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FConfig: TJVCLConfig;
    FFileName: string;
  public
    procedure UpdateCheckStates;
    property FileName: string read FFileName write FFileName;
    property Config: TJVCLConfig read FConfig;
  end;

var
  FormMainConfig: TFormMainConfig;
  Embedded: Boolean;

implementation

{$R *.dfm}

procedure TFormMainConfig.FormCreate(Sender: TObject);
var S: string;
begin
  if not Embedded then
  begin
    FFileName := ParamStr(1);
    if ParamCount <> 1 then
      S := ' '
    else
    if not FileExists(FFileName) then
      S := Format('File "%s" does not exist.'#10#10, [FFileName]);
    if S <> '' then
    begin
      MessageDlg(S + 'Syntax:  JVCLConfig.exe <FILE.INC>', mtError, [mbOK], 0);
      Application.ShowMainForm := False;
      Application.Terminate;
      Exit;
    end;
  end;

  FConfig := TJVCLConfig.Create;

  if not Embedded then
    BtnReload.Click;
end;

procedure TFormMainConfig.FormDestroy(Sender: TObject);
begin
  FConfig.Free;
end;

procedure TFormMainConfig.CheckListBoxClick(Sender: TObject);
begin
  LblComment.Caption := FConfig.Items[CheckListBox.ItemIndex].Comment;
  LblComment.Font.Color := clWindowText;
end;

procedure TFormMainConfig.BtnReloadClick(Sender: TObject);
begin
  LblComment.Caption := 'Select a compiler option.';
  LblComment.Font.Color := clGreen;

  FConfig.LoadFromFile(FFileName);

  UpdateCheckStates;
end;

procedure TFormMainConfig.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMainConfig.BtnSaveClick(Sender: TObject);
begin
  if FConfig.Modified then
    FConfig.SaveToFile(FFileName);
end;

procedure TFormMainConfig.CheckListBoxClickCheck(Sender: TObject);
begin
  FConfig.Items[CheckListBox.ItemIndex].Enabled :=
    CheckListBox.Checked[CheckListBox.ItemIndex];
end;

procedure TFormMainConfig.UpdateCheckStates;
var
  I: Integer;
begin
  CheckListBox.Clear;
  for I := 0 to FConfig.ItemCount - 1 do
  begin
    CheckListBox.Items.Add(FConfig.Items[I].Name);
    CheckListBox.Checked[I] := FConfig.Items[I].Enabled;
  end;
end;

procedure TFormMainConfig.FormShow(Sender: TObject);
begin
  ActiveControl := CheckListBox;
  if CheckListBox.Items.Count > 0 then
  begin
    CheckListBox.Selected[0] := True;
    CheckListBoxClick(CheckListBox);
  end;
end;

initialization
  Embedded := False;

end.
