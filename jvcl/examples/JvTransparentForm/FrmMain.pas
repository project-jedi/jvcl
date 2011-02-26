{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2010 Project JEDI

 Original author: Andreas Hausladen (Andreas dott Hausladen att gmx dott net)

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit FrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, StdCtrls, ComCtrls,
  JvComponentBase, JvFormTransparent;

type
  TFormDemo = class(TForm)
    ImageBackground: TImage;
    ButtonClose: TButton;
    JvTransparentForm: TJvTransparentForm;
    CheckBoxDontShowAgain: TCheckBox;
    TrackBarTransparency: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure TrackBarTransparencyChange(Sender: TObject);
  private
    { Private-Declarations }
  protected
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
  public
    { Public-Declarations }
  end;

var
  FormDemo: TFormDemo;

implementation

{$R *.dfm}

procedure TFormDemo.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormDemo.TrackBarTransparencyChange(Sender: TObject);
begin
  JvTransparentForm.LayeredAlphaValue := TrackBarTransparency.Position;
end;

procedure TFormDemo.WMShowWindow(var Message: TWMShowWindow);
begin
  // Put the "hat" out of the screen center
  if Message.Show then
    Top := (Screen.WorkAreaHeight - Height - 150) div 2;
  inherited;
end;

end.

