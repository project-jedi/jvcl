{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

unit uJvMouseGesture;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvMouseGesture, ExtCtrls, JvComponent, Menus,
  JvExStdCtrls, JvCheckBox, JvRadioButton;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    JvMouseGesture1: TJvMouseGesture;
    JvMouseGestureHook1: TJvMouseGestureHook;
    GroupBox1: TGroupBox;
    rbFormOnly: TJvRadioButton;
    rbAppEvents: TJvRadioButton;
    Label1: TLabel;
    cbMouseButton: TComboBox;
    chkNoPopup: TJvCheckBox;
    pnlGesture: TPanel;
    Label2: TLabel;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure cbMouseButtonChange(Sender: TObject);
    procedure Memo1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure JvMouseGestureHook1MouseGestureCustomInterpretation(
      Sender: TObject; const AGesture: String);
    procedure FormCreate(Sender: TObject);
    procedure rbFormOnlyClick(Sender: TObject);
  private
    procedure RefreshCaption;
  public
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

function BoolToStr(AValue:boolean):string;
const cBool:array[boolean] of PChar = ('false','true');
begin
  Result := cBool[AValue];
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not rbFormOnly.Checked then
    Exit;
  if Button = mbRight then
    JvMouseGesture1.StartMouseGesture(x,y);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if JvMouseGesture1.TrailActive then
    JvMouseGesture1.TrailMouseGesture(x,y);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if JvMouseGesture1.TrailActive then
  begin
    JvMouseGesture1.EndMouseGesture;
    Memo1.Lines.Add('Gesture = ' + JvMouseGesture1.Gesture);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  JvMouseGesture1.Active := True;
  JvMouseGestureHook1.Active := False;
  RefreshCaption;
end;

procedure TForm1.RefreshCaption;
begin
  Memo1.Clear;
  Caption := 'Panel Hooked = ' + BoolToStr(JvMouseGesture1.Active) +
             ', Application Hooked = '+ BoolToStr(JvMouseGestureHook1.Active);
  Memo1.Lines.Add(Caption);
end;


procedure TForm1.cbMouseButtonChange(Sender: TObject);
begin
  chkNoPopup.Enabled := false;
  case cbMouseButton.ItemIndex of
    0: JvMouseGestureHook1.MouseButton := mbLeft;
    1: JvMouseGestureHook1.MouseButton := mbMiddle;
    2:
    begin
      JvMouseGestureHook1.MouseButton := mbRight;
      chkNoPopup.Enabled := true;
    end;
  end;
end;

procedure TForm1.Memo1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  // if we have a gesture and the user wants to surpress the popup, return true
  Handled := chkNoPopup.Checked and rbAppEvents.Checked and (JvMouseGestureHook1.MouseGesture.Gesture <> '');
end;

procedure TForm1.JvMouseGestureHook1MouseGestureCustomInterpretation(
  Sender: TObject; const AGesture: String);
begin
  Memo1.Lines.Add('Gesture (via hook) = ' + AGesture);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cbMouseButton.ItemIndex := 2;
end;

procedure TForm1.rbFormOnlyClick(Sender: TObject);
begin
  if rbFormOnly.Checked then
  begin
    JvMouseGesture1.Active := true;
    JvMouseGestureHook1.Active := false;
  end
  else
  begin
    JvMouseGesture1.Active := false;
    JvMouseGestureHook1.Active := true;
  end;
  RefreshCaption;
end;

end.
