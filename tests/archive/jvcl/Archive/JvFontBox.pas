{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFontBox.PAS, released on 2001-02-28.

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

unit JvFontBox;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Buttons, Dialogs,
  JvCustomBox, JvTypes, JvCalculator;

type
  TJvFontBox = class(TJvCustomBox)
  private
    FLastValue: TFont;
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function ColorToStr(Value: TColor): string;
    function StrToColor(Value: string): TColor;
  protected
    procedure BtnClick(Sender: TObject); override;
    procedure PreEdit(Sender: TObject);
    procedure PostEdit(Sender: TObject);
    function FontToStr(const Font: TFont): string;
    function StrToFont(const Value: string): TFont;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value: TFont read GetFont write SetFont;
  end;

implementation

uses
  TypInfo;

{$R RES_FontBox.res}

{*****************************************************}

constructor TJvFontBox.Create(AOwner: TComponent);
begin
  inherited;
  Button.Glyph.LoadFromResourceName(HInstance, 'FONTBOX');
  FLastValue := TFont.Create;
  GetEdit.OnEnter := PreEdit;
  GetEdit.OnExit := PostEdit;
end;

{*****************************************************}

procedure TJvFontBox.SetFont(Value: TFont);
begin
  Edit.Text := FontToStr(Value);
end;

{*****************************************************}

function TJvFontBox.GetFont: TFont;
begin
  try
    Result := StrToFont(Edit.Text);
  except
    Result := FLastValue;
  end;
end;

{*****************************************************}

procedure TJvFontBox.BtnClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  begin
    Font.Assign(GetFont);
    if Execute then
      SetFont(Font);
    Free;
  end;
end;

{*****************************************************}

procedure TJvFontBox.PostEdit(Sender: TObject);
begin
  try
    FLastValue := StrToFont(Edit.Text);
  except
    SetFont(FLastValue);
    Beep;
  end;
end;

{*****************************************************}

procedure TJvFontBox.PreEdit(Sender: TObject);
begin
  FLastValue.Assign(GetFont);
end;

{*****************************************************}

function TJvFontBox.StrToColor(Value: string): TColor;
begin
  Value := UpperCase(Value);
  if Value = 'BLACK' then
    Result := clBlack
  else if Value = 'MAROON' then
    Result := clMaroon
  else if Value = 'GREEN' then
    Result := clGreen
  else if Value = 'OLIVE' then
    Result := clOlive
  else if Value = 'NAVY' then
    Result := clNavy
  else if Value = 'PURPLE' then
    Result := clPurple
  else if Value = 'TEAL' then
    Result := clTeal
  else if Value = 'GRAY' then
    Result := clGray
  else if Value = 'SILVER' then
    Result := clSilver
  else if Value = 'RED' then
    Result := clRed
  else if Value = 'LIME' then
    Result := clLime
  else if Value = 'YELLOW' then
    Result := clYellow
  else if Value = 'BLUE' then
    Result := clBlue
  else if Value = 'FUCHSIA' then
    Result := clFuchsia
  else if Value = 'AQUA' then
    Result := clAqua
  else if Value = 'WHITE' then
    Result := clWhite
  else
    Result := clBlack;
end;

{*****************************************************}

function TJvFontBox.ColorToStr(Value: TColor): string;
begin
  case Value of
    clBlack:
      Result := 'BLACK';
    clMaroon:
      Result := 'MAROON';
    clGreen:
      Result := 'GREEN';
    clOlive:
      Result := 'OLIVE';
    clNavy:
      Result := 'NAVY';
    clPurple:
      Result := 'PURPLE';
    clTeal:
      Result := 'TEAL';
    clGray:
      Result := 'GRAY';
    clSilver:
      Result := 'SILVER';
    clRed:
      Result := 'RED';
    clLime:
      Result := 'LIME';
    clYellow:
      Result := 'YELLOW';
    clBlue:
      Result := 'BLUE';
    clFuchsia:
      Result := 'FUCHSIA';
    clAqua:
      Result := 'AQUA';
    clWhite:
      Result := 'WHITE';
  else
    Result := 'BLACK';
  end;
end;

{*****************************************************}
  { FontName
  FontSize
  Color
  Attribs (Bold, Italic, Underline, StrikeOut)}

function TJvFontBox.FontToStr(const Font: TFont): string;
begin
  with TStringList.Create do
  begin
    Add(Font.Name);
    Add(IntToStr(Font.Size));
    Add(ColorToStr(Font.Color));
    if fsBold in Font.Style then
      Add('Bold');
    if fsItalic in Font.Style then
      Add('Italic');
    if fsUnderline in Font.Style then
      Add('Underline');
    if fsStrikeOut in Font.Style then
      Add('StrikeOut');
    Result := CommaText;
    Free;
  end;
end;

{*****************************************************}

function TJvFontBox.StrToFont(const Value: string): TFont;
var
  i: Integer;
begin
  Result := TFont.Create;
  with TStringList.Create do
  begin
    CommaText := Value;
    if Count = 0 then
    begin
      Free;
      Exit;
    end
    else if Count < 3 then
    begin
      Free;
      raise Exception.Create('');
    end;
    Result.Name := Strings[0];
    Result.Size := StrToIntDef(Strings[1], Result.Size);
    Result.Color := StrToColor(Strings[2]);
    for i := 3 to Count - 1 do
    begin
      Strings[i] := UpperCase(Strings[i]);
      if Strings[i] = 'BOLD' then
        Result.Style := Result.Style + [fsBold]
      else if Strings[i] = 'ITALIC' then
        Result.Style := Result.Style + [fsItalic]
      else if Strings[i] = 'UNDERLINE' then
        Result.Style := Result.Style + [fsUnderline]
      else if Strings[i] = 'STRIKEOUT' then
        Result.Style := Result.Style + [fsStrikeOut]
    end;
    Free;
  end;
end;

end.
