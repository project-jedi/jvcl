{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MainConfig.pas, released on 2003-10-01.

The Initial Developer of the Original Code is Andreas Hausladen
[Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2003-2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit MainConfig;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Buttons, ExtCtrls, ImgList,
  {$IFDEF USE_DXGETTEXT}
  JvGnugettext,
  {$ENDIF USE_DXGETTEXT}
  JvComponent,
  JVCLConfiguration;

type
  TFormJvclIncConfig = class(TJvForm)
    CheckListBox: TCheckListBox;
    ScrollBox: TScrollBox;
    LblComment: TLabel;
    BevelBorder: TBevel;
    TitlePanel: TPanel;
    imgProjectJEDI: TImage;
    Label4: TLabel;
    BevelHeader: TBevel;
    PanelSpace: TPanel;
    PaintBoxWhite: TPaintBox;
    BtnCancel: TButton;
    BtnOk: TButton;
    procedure CheckListBoxClick(Sender: TObject);
    procedure CheckListBoxClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxWhitePaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function GetCurrentItem: TJVCLConfigItem;
  private
    FConfig: TJVCLConfig;
    FSavedCaption: string;
    FFileName: string;
    property CurrentItem: TJVCLConfigItem read GetCurrentItem;
  public
    function Execute(const Version: String): Boolean;
    procedure UpdateCheckStates;

    property FileName: string read FFileName write FFileName;
    property Config: TJVCLConfig read FConfig;
  end;

var
  FormJvclIncConfig: TFormJvclIncConfig;

implementation

{$R *.dfm}

procedure TFormJvclIncConfig.CheckListBoxClick(Sender: TObject);
begin
  LblComment.Caption := CurrentItem.Comment;
  LblComment.Font.Color := clWindowText;
end;

procedure TFormJvclIncConfig.CheckListBoxClickCheck(Sender: TObject);
begin
  CurrentItem.Enabled := CheckListBox.Checked[CheckListBox.ItemIndex];
end;

procedure TFormJvclIncConfig.UpdateCheckStates;
var
  I, J: Integer;
begin
  CheckListBox.Clear;
  for I := 0 to FConfig.ItemCount - 1 do
  begin
    if not FConfig.Items[I].Hidden then
    begin
      J := CheckListBox.Items.AddObject(FConfig.Items[I].Name, FConfig.Items[I]);
      CheckListBox.Checked[J] := FConfig.Items[I].Enabled
    end;
  end;
end;

procedure TFormJvclIncConfig.FormShow(Sender: TObject);
begin
  ActiveControl := CheckListBox;
  if CheckListBox.Items.Count > 0 then
  begin
    CheckListBox.Selected[0] := True;
    CheckListBoxClick(CheckListBox);
  end;
end;

procedure TFormJvclIncConfig.PaintBoxWhitePaint(Sender: TObject);
begin
  // XP Theming makes the panel gray so we paint a white rectangle
  PaintBoxWhite.Canvas.Brush.Color := clWindow;
  PaintBoxWhite.Canvas.FillRect(PaintBoxWhite.ClientRect);
end;

procedure TFormJvclIncConfig.FormCreate(Sender: TObject);
begin
  {$IFDEF USE_DXGETTEXT}
  TranslateComponent(Self, 'JVCLInstall');
  {$ENDIF USE_DXGETTEXT}
  FConfig := TJVCLConfig.Create;
  FSavedCaption := Caption;
  LblComment.Caption := '';
end;

procedure TFormJvclIncConfig.FormDestroy(Sender: TObject);
begin
  FConfig.Free;
end;

function TFormJvclIncConfig.GetCurrentItem: TJVCLConfigItem;
begin
  if CheckListBox.ItemIndex >= 0 then
    Result := TJVCLConfigItem(CheckListBox.Items.Objects[CheckListBox.ItemIndex])
  else
    Result := nil;
end;

function TFormJvclIncConfig.Execute(const Version: String): Boolean;
begin
  if Version <> '' then
    Caption := FSavedCaption + ' - ' + Version
  else
    Caption := FSavedCaption;

  UpdateCheckStates;
  Result := ShowModal = mrOk;
end;

end.