{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter THornqvist [peter3@peter3.com]

Last Modified: 2003-03-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Changes:
2003-03-24:
  JvHotLink merged into JvLabel: Set AutoOpenURL to true,
  modify HotTrackFont to fit and assign a URL (or file-path) to the URL property.
  JvAngleLabel merged into JvLabel: set Angle to <> 0 and font to a TrueTrype
  font to rotate the text // peter3

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, JVCLVer,
  JvTypes;

type
  TJvLabel = class(TLabel)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FHintColor: TColor;
    FHintSaved: TColor;
    FOver: Boolean;
    FAutoOpenURL: boolean;
    FURL: string;
    FAngle: TJvLabelRotateAngle;
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetAngle(const Value: TJvLabelRotateAngle);
    procedure DrawText(Flags: Word);
  protected
    procedure Paint; override;

    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Anchors;
    property Angle: TJvLabelRotateAngle read FAngle write SetAngle default 0;
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property URL: string read FURL write FURL;
    property AutoOpenURL: boolean read FAutoOpenURL write FAutoOpenURL;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation
uses
  JvFunctions;

constructor TJvLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  // (rom) needs better font handling
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHintColor := clInfoBk;
  FOver := False;
end;

destructor TJvLabel.Destroy;
begin
  FHotTrackFont.Free;
  FFontSave.Free;
  inherited Destroy;
end;

procedure TJvLabel.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvLabel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvLabel.CMMouseEnter(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FHintSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if HotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvLabel.CMMouseLeave(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FHintSaved;
    if HotTrack then
      Font.Assign(FFontSave);
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvLabel.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvLabel.CMDialogChar(var Msg: TCMDialogChar);
var
  Form: TCustomForm;
begin
  inherited;
  if Msg.Result = 1 then
  begin
    Form := GetParentForm(Self);
    if Assigned(Form) and Assigned(Form.ActiveControl) and not Form.ActiveControl.TabStop then
      PostMessage(Form.Handle, WM_NEXTDLGCTL, 0, 0);
  end;
end;

procedure TJvLabel.Click;
begin
  inherited;
  if AutoOpenURL and (URL <> '') then
    OpenObject(URL);
end;

procedure TJvLabel.SetAngle(const Value: TJvLabelRotateAngle);
begin
  if FAngle <> VAlue then
  begin
    FAngle := Value;
    Invalidate;
  end;
end;

procedure TJvLabel.DrawText(Flags: Word);
var
  Text: array[0..4096] of Char;
  LogFont, NewLogFont: TLogFont;
  NewFont: HFont;
  MRect: TRect;
  TextX, TextY: Integer;
  Phi: Real;
  Angle10: Integer;
begin
  Angle10 := Angle * 10;
  GetTextBuf(Text, SizeOf(Text));
  if (Flags and DT_CALCRECT <> 0) and ((Text[0] = #0) or ShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then
    StrCopy(Text, ' ');
  Canvas.Font := Font;
  if GetObject(Font.Handle, SizeOf(TLogFont), @LogFont) = 0 then
    PError('FONT');
  NewLogFont := LogFont;
  MRect := ClientRect;
  NewLogFont.lfEscapement := Angle10;
  NewFont := CreateFontIndirect(NewLogFont);
  {
    (p3) unnecessary
    OldFont := SelectObject(Canvas.Font.Handle, NewFont);
    DeleteObject(OldFont);
    ...this does the same thing:
  }
  Canvas.Font.Handle := NewFont;
  Phi := Angle10 * Pi / 1800;
  if not AutoSize then
  begin
    TextX := Trunc(0.5 * ClientWidth - 0.5 * Canvas.TextWidth(Text) * Cos(Phi) - 0.5 * Canvas.TextHeight(Text) *
      Sin(Phi));
    TextY := Trunc(0.5 * ClientHeight - 0.5 * Canvas.TextHeight(Text) * Cos(Phi) + 0.5 * Canvas.TextWidth(Text) *
      Sin(Phi));
  end
  else
  begin
    ClientWidth := 4 + Trunc(Canvas.TextWidth(Text) * Abs(Cos(Phi)) + Canvas.TextHeight(Text) * Abs(Sin(Phi)));
    ClientHeight := 4 + Trunc(Canvas.TextHeight(Text) * Abs(Cos(Phi)) + Canvas.TextWidth(Text) * Abs(Sin(Phi)));
    TextX := 2;
    if (Angle10 > 900) and (Angle10 < 2700) then
      TextX := TextX + Trunc(Canvas.TextWidth(Text) * Abs(Cos(Phi)));
    if Angle10 > 1800 then
      TextX := TextX + Trunc(Canvas.TextHeight(Text) * Abs(Sin(Phi)));
    TextY := 2;
    if Angle10 < 1800 then
      TextY := TextY + Trunc(Canvas.TextWidth(Text) * Abs(Sin(Phi)));
    if (Angle10 > 900) and (Angle10 < 2700) then
      TextY := TextY + Trunc(Canvas.TextHeight(Text) * Abs(Cos(Phi)));
  end;
  Canvas.TextOut(TextX, TextY, Text);
end;

procedure TJvLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  MRect: TRect;
begin
  if Angle <> 0 then
  begin
    with Canvas do
    begin
      if not Transparent then
      begin
        Brush.Color := Color;
        Brush.Style := bsSolid;
        FillRect(ClientRect);
      end;
      Brush.Style := bsClear;
      // (rom) what is MRect for?
      MRect := Rect(0, 0, ClientWidth, ClientHeight);
      DrawText(DT_EXPANDTABS or DT_WORDBREAK or Alignments[Alignment]);
    end;
  end
  else
    inherited;
end;

end.

