{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAngleLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvAngleLabel;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  JvLabel, JvTypes, JvFunctions;

type
  TJvAngleLabel = class(TJvLabel)
  private
    FAngle: TAngle;
    procedure SetAngle(const Value: TAngle);
    procedure DrawText(Flags: Word);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Angle: TAngle read FAngle write SetAngle default 0;
  end;

implementation

{**************************************************}

constructor TJvAngleLabel.Create(AOwner: TComponent);
begin
  inherited;
  // (rom) not a good idea to set the font here
  // (rom) this kills ParentFont and Kylix compatibility
  Font.Name := 'Arial';
  FAngle := 0;
end;

{**************************************************}

procedure TJvAngleLabel.DrawText(Flags: Word);
var
  Text: array[0..4096] of Char;
  LogFont, NewLogFont: TLogFont;
  NewFont, OldFont: HFont;
  MRect: TRect;
  TextX, TextY: Integer;
  Phi: Real;
begin
  FAngle := FAngle * 10;
  GetTextBuf(Text, SizeOf(Text));
  if (Flags and DT_CALCRECT <> 0) and ((Text[0] = #0) or ShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then
    StrCopy(Text, ' ');
  Canvas.Font := Font;
  if GetObject(Font.Handle, SizeOf(TLogFont), @LogFont) = 0 then
    PError('FONT');
  NewLogFont := LogFont;
  MRect := ClientRect;
  NewLogFont.lfEscapement := FAngle;
  NewFont := CreateFontIndirect(NewLogFont);
  OldFont := SelectObject(Canvas.Font.Handle, NewFont);
  DeleteObject(OldFont);
  Canvas.Font.Handle := NewFont;
  Phi := FAngle * Pi / 1800;
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
    if (FAngle > 900) and (FAngle < 2700) then
      TextX := TextX + Trunc(Canvas.TextWidth(Text) * Abs(Cos(Phi)));
    if (FAngle > 1800) then
      TextX := TextX + Trunc(Canvas.TextHeight(Text) * Abs(Sin(Phi)));
    TextY := 2;
    if FAngle < 1800 then
      TextY := TextY + Trunc(Canvas.TextWidth(Text) * Abs(Sin(Phi)));
    if (FAngle > 900) and (FAngle < 2700) then
      TextY := TextY + Trunc(Canvas.TextHeight(Text) * Abs(Cos(Phi)));
  end;
  Canvas.TextOut(TextX, TextY, Text);
  FAngle := FAngle div 10;
end;

{**************************************************}

procedure TJvAngleLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  MRect: TRect;
begin
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    // (rom) what is MRect for?
    MRect := Rect(0, 0, ClientWidth, ClientHeight);
    DrawText(DT_EXPANDTABS or DT_WORDBREAK or Alignments[Alignment]);
  end;
end;

{**************************************************}

procedure TJvAngleLabel.SetAngle(const Value: TAngle);
begin
  FAngle := Value;
  Invalidate;
end;

end.
