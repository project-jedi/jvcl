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
{$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, Buttons, ExtCtrls,
{$ENDIF VCL}
{$IFDEF VisualCLX}
  Types, QGraphics, QControls, QForms, QStdCtrls, QDialogs, QExtCtrls,
{$ENDIF VisualCLX}
  JvNetscapeSplitter, JvOfficeColorButton, JvOfficeColorPanel,
  JvColorButton, JvComponent, JvExExtCtrls
  ;

type
  TColorDemoMainForm = class(TForm)
    pConfigurePanel: TPanel;
    GroupBoxPanelStyles: TGroupBox;
    chkPanelFlat: TCheckBox;
    chkPanelAuto: TCheckBox;
    chkPanelOther: TCheckBox;
    pPanelColor: TPanel;
    JvOfficeColorPanel: TJvOfficeColorPanel;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    pConfigureButton: TPanel;
    GroupBoxButtonStyles: TGroupBox;
    chkButtonFlat: TCheckBox;
    chkButtonAuto: TCheckBox;
    chkButtonOther: TCheckBox;
    chkButtonDrag: TCheckBox;
    chkButtonGlyph: TCheckBox;
    chkButtonEnabled: TCheckBox;
    pButtonColor: TPanel;
    JvOfficeColorButton: TJvOfficeColorButton;
    procedure ButtonOptionClick(Sender: TObject);
    procedure JvOfficeColorPanelColorChange(Sender: TObject);
    procedure JvOfficeColorButtonColorChange(Sender: TObject);
    procedure PanelOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
  end;

var
  ColorDemoMainForm: TColorDemoMainForm;

implementation

{$R Ico.res}

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}

{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

procedure TColorDemoMainForm.ButtonOptionClick(Sender: TObject);
begin
  with JvOfficeColorButton do
  begin
    Flat := chkButtonFlat.Checked;
    Properties.ShowAutoButton := chkButtonAuto.Checked;
    Properties.ShowOtherButton := chkButtonOther.Checked;
    Properties.ShowDragBar := chkButtonDrag.Checked;
    if chkButtonGlyph.Checked
      then Glyph.LoadFromResourceName(hInstance, 'FONTCOLOR')
      else Glyph := nil;
    Enabled := chkButtonEnabled.Checked;
  end;
end;

procedure TColorDemoMainForm.JvOfficeColorPanelColorChange(
  Sender: TObject);
begin
  if not JvOfficeColorButton.Enabled then
    JvOfficeColorButton.Color := JvOfficeColorPanel.Color;
  pPanelColor.Color := JvOfficeColorPanel.SelectedColor;
  pPanelColor.Font.Color := ColorToRGB(pPanelColor.Color) xor $FFFFFF;
  pPanelColor.Caption := ColorToString(pPanelColor.Color);
end;

procedure TColorDemoMainForm.JvOfficeColorButtonColorChange(
  Sender: TObject);
begin
  pButtonColor.Color := JvOfficeColorButton.SelectedColor;
  pButtonColor.Font.Color := ColorToRGB(pButtonColor.Color) xor $FFFFFF;
  pButtonColor.Caption := ColorToString(pButtonColor.Color);
end;

procedure TColorDemoMainForm.PanelOptionsClick(Sender: TObject);
begin
  with JvOfficeColorPanel do
  begin
    Flat := chkPanelFlat.Checked;
    Properties.ShowAutoButton := chkPanelAuto.Checked;
    Properties.ShowOtherButton := chkPanelOther.Checked;
  end;
end;

procedure TColorDemoMainForm.FormCreate(Sender: TObject);
begin
  JvOfficeColorButtonColorChange(nil);
  JvOfficeColorPanelColorChange(nil);
end;

end.

