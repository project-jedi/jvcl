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

{$I JVCL.INC}

unit MainConfig;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JVCLConfiguration, CheckLst, Buttons, ExtCtrls;

type
  TFormMainConfig = class(TForm)
    CheckListBox: TCheckListBox;
    ScrollBox: TScrollBox;
    LblComment: TLabel;
    BtnReload: TBitBtn;
    BtnQuit: TBitBtn;
    BtnSave: TBitBtn;
    BevelBorder: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckListBoxClick(Sender: TObject);
    procedure BtnReloadClick(Sender: TObject);
    procedure BtnQuitClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure CheckListBoxClickCheck(Sender: TObject);
  private
    { Private-Deklarationen }
    FConfig: TJVCLConfig;
    FFilename: string;
  public
    { Public-Deklarationen }
    procedure UpdateCheckStates;

    property Filename: string read FFilename write FFilename;
    property Config: TJVCLConfig read FConfig;
  end;

var
  FormMainConfig: TFormMainConfig;
  Embeded: Boolean;

implementation

{$R *.dfm}

procedure TFormMainConfig.FormCreate(Sender: TObject);
var S: string;
begin
  if not Embeded then
  begin
    FFilename := ParamStr(1);
    if ParamCount <> 1 then
      S := ' '
    else if not FileExists(FFilename) then
      S := Format('File "%s" does not exist.'#10#10, [FFilename]);
    if S <> '' then
    begin
      MessageDlg(S + 'Syntax:  JVCLConfig.exe <FILE.INC>', mtError, [mbOK], 0);
      Application.ShowMainForm := False;
      Application.Terminate;
      Exit;
    end;
  end;

  FConfig := TJVCLConfig.Create;

  if not Embeded then
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
  LblComment.Font.Color := clRed;

  FConfig.LoadFromFile(FFilename);

  UpdateCheckStates;
end;

procedure TFormMainConfig.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMainConfig.BtnSaveClick(Sender: TObject);
begin
  if FConfig.Modified then
    FConfig.SaveToFile(FFilename);
end;

procedure TFormMainConfig.CheckListBoxClickCheck(Sender: TObject);
begin
  FConfig.Items[CheckListBox.ItemIndex].Enabled :=
    CheckListBox.Checked[CheckListBox.ItemIndex];
end;

procedure TFormMainConfig.UpdateCheckStates;
var i: Integer;
begin
  CheckListBox.Clear;
  for i := 0 to FConfig.ItemCount - 1 do
  begin
    CheckListBox.Items.Add(FConfig.Items[i].Name);
    CheckListBox.Checked[i] := FConfig.Items[i].Enabled;
  end;
end;

initialization
  Embeded := False;

end.
