{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorForm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Color dorm for the @link(TJvColorButton) component }

unit JvColorForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, JvColorBox, JvColorBtn, JvComponent;

type
  TJvClrFrm = class(TForm)
    Bevel1: TBevel;
    OtherBtn: TSpeedButton;
    ColorSquare1: TJvColorSquare;
    ColorSquare2: TJvColorSquare;
    ColorSquare3: TJvColorSquare;
    ColorSquare4: TJvColorSquare;
    ColorSquare5: TJvColorSquare;
    ColorSquare6: TJvColorSquare;
    ColorSquare7: TJvColorSquare;
    ColorSquare8: TJvColorSquare;
    ColorSquare9: TJvColorSquare;
    ColorSquare10: TJvColorSquare;
    ColorSquare11: TJvColorSquare;
    ColorSquare12: TJvColorSquare;
    ColorSquare13: TJvColorSquare;
    ColorSquare14: TJvColorSquare;
    ColorSquare15: TJvColorSquare;
    ColorSquare16: TJvColorSquare;
    ColorSquare17: TJvColorSquare;
    ColorSquare18: TJvColorSquare;
    ColorSquare19: TJvColorSquare;
    ColorSquare20: TJvColorSquare;
    ColorSquare21: TJvColorSquare;
    procedure OtherBtnClick(Sender: TObject);
    procedure ColorSquare1Click(Sender: TObject);
    procedure ColorSquare21Change(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    FOwner:TControl;
    CDVisible:boolean;
    procedure ShowCD(Sender:TObject);
    procedure HideCD(Sender:TObject);
    procedure WMActivate(var Message:TWMActivate); message WM_ACTIVATE;
  protected
    procedure CreateWnd;override;
  public
    { Public declarations }
    SelectedColor:TColor;
    CD:TColorDialog;
    procedure SetButton(Button:TControl);
  end;

implementation

{$R *.DFM}

procedure TJvClrFrm.SetButton(Button:TControl);
begin
  FOwner := Button;
end;

procedure TJvClrFrm.ShowCD(Sender:TObject);
begin
  CDVisible := True;
end;

procedure TJvClrFrm.HideCD(Sender:TObject);
begin
  CDVisible := False;
end;

procedure TJvClrFrm.OtherBtnClick(Sender: TObject);
begin
  if Assigned(FOwner) and (FOwner is TJvColorButton) then
    TJvColorButton(FOwner).Color := SelectedColor;
  CD.Color := SelectedColor;
  if CD.Execute then
  begin
    ColorSquare21.Color := CD.Color;
    if FOwner is TJvColorButton then
    begin
      TJvColorButton(FOwner).CustomColors.Assign(CD.CustomColors);
      TJvColorButton(FOwner).Color := SelectedColor;
    end;
    ModalResult := mrOK;
  end
  else
    ModalResult := mrCancel;
  Hide;
end;

procedure TJvClrFrm.WMActivate(var Message:TWMActivate);
begin
  inherited;
  if (Message.Active = WA_INACTIVE) and not CDVisible then
  begin
    Hide;
    ModalResult := mrCancel;
  end;
end;

procedure TJvClrFrm.ColorSquare1Click(Sender: TObject);
begin
  if Sender is TJvColorSquare then
    SelectedColor := (Sender as TJvColorSquare).Color;
  if Assigned(FOwner) and (FOwner is TJvColorButton) then
    TJvColorButton(FOwner).Color := SelectedColor;
  Hide;
  ModalResult := mrOK;
end;

procedure TJvClrFrm.ColorSquare21Change(Sender: TObject);
begin
  SelectedColor := ColorSquare21.Color;
  if Assigned(FOwner) and (FOwner is TJvColorButton) then
    TJvColorButton(FOwner).Color := SelectedColor;
end;

procedure TJvClrFrm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Hide;
    ModalResult := mrCancel;
  end;
end;

procedure TJvClrFrm.FormCreate(Sender: TObject);
begin
  CD := TColorDialog.Create(self);
  CDVisible := False;
  CD.OnShow := ShowCD;
  CD.OnClose := HideCD;
end;

procedure TJvClrFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TJvClrFrm.CreateWnd;
begin
  inherited;
//  Hide;
  SetWindowLong(Handle,GWL_STYLE,
              GetWindowLong(Handle,GWL_STYLE) and not WS_CAPTION);
//  Show;
end;

procedure TJvClrFrm.FormActivate(Sender: TObject);
var R:TRect;aPoint:TPoint;
begin
  { set placement }
  if Assigned(FOwner) then
  begin
    R := FOwner.ClientRect;
    aPoint.X := R.Left;
    aPoint.Y := R.Top + R.Bottom;
    aPoint := FOwner.ClientToScreen(aPoint);
    Left := aPoint.X;
    Top :=  aPoint.Y;
    if FOwner is TJvColorButton then
      SelectedColor := TJvColorButton(FOwner).Color;
  end;
  ClientWidth := ColorSquare4.Left + ColorSquare4.Width;
  Height := OtherBtn.Top + OtherBtn.Height + 8;
end;

end.
