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
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit MainConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JVCLConfiguration, CheckLst, Buttons, ExtCtrls,
  ImgList,
  {$IFDEF USE_DXGETTEXT}
  gnugettext,
  {$ENDIF USE_DXGETTEXT}
  JvComponent;

type
  TFormJvclIncConfig = class(TJvForm)
    CheckListBox: TCheckListBox;
    ScrollBox: TScrollBox;
    LblComment: TLabel;
    BtnClose: TBitBtn;
    BevelBorder: TBevel;
    TitlePanel: TPanel;
    imgProjectJEDI: TImage;
    Label4: TLabel;
    BevelHeader: TBevel;
    PanelSpace: TPanel;
    Label1: TLabel;
    PaintBoxWhite: TPaintBox;
    procedure CheckListBoxClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure CheckListBoxClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxWhitePaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FConfig: TJVCLConfig;
    FFileName: string;
  public
    procedure UpdateCheckStates;
    property FileName: string read FFileName write FFileName;
    property Config: TJVCLConfig read FConfig write FConfig;
  end;

var
  FormJvclIncConfig: TFormJvclIncConfig;

implementation

{$R *.dfm}

procedure TFormJvclIncConfig.CheckListBoxClick(Sender: TObject);
begin
  LblComment.Caption := FConfig.Items[CheckListBox.ItemIndex].Comment;
  LblComment.Font.Color := clWindowText;
end;

procedure TFormJvclIncConfig.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormJvclIncConfig.CheckListBoxClickCheck(Sender: TObject);
begin
  FConfig.Items[CheckListBox.ItemIndex].Enabled :=
    CheckListBox.Checked[CheckListBox.ItemIndex];
end;

procedure TFormJvclIncConfig.UpdateCheckStates;
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

procedure TFormJvclIncConfig.FormShow(Sender: TObject);
begin
  ActiveControl := CheckListBox;
  if CheckListBox.Items.Count > 0 then
  begin
    {$IFDEF COMPILER6_UP}
    CheckListBox.Selected[0] := True;
    CheckListBoxClick(CheckListBox);
    {$ENDIF COMPILER6_UP}
  end;
end;

procedure TFormJvclIncConfig.PaintBoxWhitePaint(Sender: TObject);
begin
  PaintBoxWhite.Canvas.Brush.Color := clWindow;
  PaintBoxWhite.Canvas.FillRect(PaintBoxWhite.ClientRect);
end;

procedure TFormJvclIncConfig.FormCreate(Sender: TObject);
begin
  {$IFDEF USE_DXGETTEXT}
  TranslateComponent(Label1, 'JVCLInstall');
  {$ENDIF USE_DXGETTEXT}
end;

end.
