{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:dejoy

 Contributor(s):dejoy

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{$I jvcl.inc}

unit Main;

interface

uses
  Classes, SysUtils,


  Types, QGraphics, QControls, QForms, QStdCtrls, QDialogs, QExtCtrls,

  JvQNetscapeSplitter, JvQOfficeColorButton, JvQOfficeColorPanel,
  JvQColorButton, JvQComponent, JvQExExtCtrls
  ;

type
  TColorDemoMainForm = class(TForm)
    Panel2: TPanel;
    GroupBox2: TGroupBox;
    chkPanelFlat: TCheckBox;
    chkPanelAuto: TCheckBox;
    chkPanelOther: TCheckBox;
    Panel3: TPanel;
    JvOfficeColorPanel1: TJvOfficeColorPanel;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    Panel5: TPanel;
    GroupBox1: TGroupBox;
    chkButtonFlat: TCheckBox;
    chkButtonAuto: TCheckBox;
    chkButtonOther: TCheckBox;
    chkButtonDrag: TCheckBox;
    chkButtonGlyph: TCheckBox;
    chkButtonEnabled: TCheckBox;
    Panel1: TPanel;
    JvOfficeColorButton1: TJvOfficeColorButton;
    procedure ButtonOptionClick(Sender: TObject);
    procedure JvOfficeColorPanel1ColorChange(Sender: TObject);
    procedure JvOfficeColorButton1ColorChange(Sender: TObject);
    procedure PanelOptionsClick(Sender: TObject);
  public
  end;

var
  ColorDemoMainForm: TColorDemoMainForm;

implementation

{$R Ico.res}




{$R *.xfm}


procedure TColorDemoMainForm.ButtonOptionClick(Sender: TObject);
begin
  with JvOfficeColorButton1 do
  begin
    Flat := chkButtonFlat.Checked;
    Properties.ShowAutoButton := chkButtonAuto.Checked;
    Properties.ShowOtherButton := chkButtonOther.Checked;
    Properties.ShowDragBar := chkButtonDrag.Checked;
    if chkButtonGlyph.Checked then

      Glyph.LoadFromResourceName(hInstance, 'FONTCOLOR')


    else
      Glyph := nil;
  end;
end;

procedure TColorDemoMainForm.JvOfficeColorPanel1ColorChange(
  Sender: TObject);
begin
  if not JvOfficeColorButton1.Enabled then
    JvOfficeColorButton1.Color := JvOfficeColorPanel1.Color;
  Panel3.Color := JvOfficeColorPanel1.Color;
  Panel3.Font.Color := ColorToRGB(Panel3.Color) xor $FFFFFF;
  Panel3.Caption := ColorToString(Panel3.Color);
end;

procedure TColorDemoMainForm.JvOfficeColorButton1ColorChange(
  Sender: TObject);
begin
  Panel1.Color := JvOfficeColorButton1.Color;
  Panel1.Font.Color := ColorToRGB(Panel1.Color) xor $FFFFFF;
  Panel1.Caption := ColorToString(Panel1.Color);

end;

procedure TColorDemoMainForm.PanelOptionsClick(Sender: TObject);
begin
  with JvOfficeColorPanel1 do
  begin
    Flat := chkPanelFlat.Checked;
    Properties.ShowAutoButton := chkPanelAuto.Checked;
    Properties.ShowOtherButton := chkPanelOther.Checked;
  end;
end;

end.

