{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvReversedLabel.PAS, released on 2001-02-28.

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

unit JvReversedLabel;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvLabel;

type
  TJvReversedLabel = class(TJvLabel)
  private
    FFont: TFont;
  public
    constructor Create(AOwner: TComponent); override;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  published
  end;

implementation

{**************************************************}

constructor TJvReversedLabel.Create(AOwner: TComponent);
var
  ALogFont: TLogFont;
begin
  inherited;
  FFont := TFont.Create;
  ALogFont.lfHeight := Font.Height;
  ALogFont.lfWidth := 0;
  ALogFont.lfEscapement := 1800;
  ALogFont.lfOrientation := 1800;
  ALogFont.lfWeight := FW_DEMIBOLD;
  ALogFont.lfItalic := Ord(False);
  ALogFont.lfUnderline := Ord(False);
  ALogFont.lfStrikeOut := Ord(False);
  ALogFont.lfCharSet := ANSI_CHARSET;
  ALogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
  ALogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  ALogFont.lfQuality := DEFAULT_QUALITY;
  ALogFont.lfPitchAndFamily := DEFAULT_PITCH;
  StrCopy(ALogFont.lfFaceName, PChar('TEditRevFont' + IntToStr(GetTickCount)));
  FFont.Handle := CreateFontIndirect(ALogFont);
end;

{**************************************************}

procedure TJvReversedLabel.WMPaint(var Msg: TWMPaint);
var
  R: TRect;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Canvas.Font := FFont;
  R := ClientRect;
  Canvas.TextRect(R, Width - 5, Height - 2, Caption);
end;

end.
