{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSystemColors.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
                André Snepvangers [asn att xs4all dott nl] (VisualCLX compatible version)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQSystemColors;

interface

uses
  SysUtils, Classes,
  QGraphics, QForms,
  JvQTypes, JvQComponent;

type
  TJvSystemColors = class(TJvComponent)
  private
    procedure SetColor(Index: Integer; Value: TColor);
    function GetColor(Index: Integer): TColor;
  published
    property NormalForeground: TColor index 0 read GetColor write SetColor stored True;
    property NormalButton: TColor index 1 read GetColor write SetColor stored True;
    property NormalLight: TColor index 2 read GetColor write SetColor stored True;
    property NormalMidlight: TColor index 3 read GetColor write SetColor stored True;
    property NormalDark: TColor index 4 read GetColor write SetColor stored True;
    property NormalMid: TColor index 5 read GetColor write SetColor stored True;
    property NormalText: TColor index 6 read GetColor write SetColor stored True;
    property NormalBrightText: TColor index 7 read GetColor write SetColor stored True;
    property NormalButtonText: TColor index 8 read GetColor write SetColor stored True;
    property NormalBase: TColor index 9 read GetColor write SetColor stored True;
    property NormalBackground: TColor index 10 read GetColor write SetColor stored True;
    property NormalShadow: TColor index 11 read GetColor write SetColor stored True;
    property NormalHighlight: TColor index 12 read GetColor write SetColor stored True;
    property NormalHighlightedText: TColor index 13 read GetColor write SetColor stored True;
    property DisabledForeground: TColor index 14 read GetColor write SetColor stored True;
    property DisabledButton: TColor index 15 read GetColor write SetColor stored True;
    property DisabledLight: TColor index 16 read GetColor write SetColor stored True;
    property DisabledMidlight: TColor index 17 read GetColor write SetColor stored True;
    property DisabledDark: TColor index 18 read GetColor write SetColor stored True;
    property DisabledMid: TColor index 19 read GetColor write SetColor stored True;
    property DisabledText: TColor index 20 read GetColor write SetColor stored True;
    property DisabledBrightText: TColor index 21 read GetColor write SetColor stored True;
    property DisabledButtonText: TColor index 22 read GetColor write SetColor stored True;
    property DisabledBase: TColor index 23 read GetColor write SetColor stored True;
    property DisabledBackground: TColor index 24 read GetColor write SetColor stored True;
    property DisabledShadow: TColor index 25 read GetColor write SetColor stored True;
    property DisabledHighlight: TColor index 26 read GetColor write SetColor stored True;
    property DisabledHighlightedText: TColor index 27 read GetColor write SetColor stored True;
    property ActiveForeground: TColor index 28 read GetColor write SetColor stored True;
    property ActiveButton: TColor index 29 read GetColor write SetColor stored True;
    property ActiveLight: TColor index 30 read GetColor write SetColor stored True;
    property ActiveMidlight: TColor index 31 read GetColor write SetColor stored True;
    property ActiveDark: TColor index 32 read GetColor write SetColor stored True;
    property ActiveMid: TColor index 33 read GetColor write SetColor stored True;
    property ActiveText: TColor index 34 read GetColor write SetColor stored True;
    property ActiveBrightText: TColor index 35 read GetColor write SetColor stored True;
    property ActiveButtonText: TColor index 36 read GetColor write SetColor stored True;
    property ActiveBase: TColor index 37 read GetColor write SetColor stored True;
    property ActiveBackground: TColor index 38 read GetColor write SetColor stored True;
    property ActiveShadow: TColor index 39 read GetColor write SetColor stored True;
    property ActiveHighlight: TColor index 40 read GetColor write SetColor stored True;
    property ActiveHighlightedText: TColor index 41 read GetColor write SetColor stored True;
//    property HintColor: TColor index 42 read GetColor write SetColor stored True;
//    property DeskTopBackgroundColor:  TColor index 43 read GetColor write SetColor stored True;
    
  end;

implementation

const
  ColorArray: array[0..41] of TColor = (
    clNormalForeground, clNormalButton, clNormalLight, clNormalMidlight, clNormalDark, clNormalMid,
    clNormalText, clNormalBrightText, clNormalButtonText, clNormalBase, clNormalBackground,
    clNormalShadow, clNormalHighlight, clNormalHighlightedText,

    clDisabledForeground, clDisabledButton, clDisabledLight, clDisabledMidlight, clDisabledDark, clDisabledMid,
    clDisabledText, clDisabledBrightText, clDisabledButtonText, clDisabledBase, clDisabledBackground,
    clDisabledShadow, clDisabledHighlight, clDisabledHighlightedText,

    clActiveForeground, clActiveButton, clActiveLight, clActiveMidlight, clActiveDark, clActiveMid,
    clActiveText, clActiveBrightText, clActiveButtonText, clActiveBase, clActiveBackground,
    clActiveShadow, clActiveHighlight, clActiveHighlightedText
    {, clInfoBk, clDesktop} );

  ColorRoles: array[1..15] of TColorRole =(
    crForeground, crButton, crLight, crMidlight, crDark, crMid,
    crText, crBrightText, crButtonText, crBase, crBackground, crShadow,
    crHighlight, crHighlightText, crNoRole);

procedure TJvSystemColors.SetColor(Index: Integer; Value: TColor);
var
  Tmp: Integer;
begin
  Tmp := ColorArray[Index];
  with Application.Palette do
    if tmp < TColor(0)
    then
      case tmp of
      clNormalHighlightedText..clNormalForeground:
        SetColor(cgInactive, ColorRoles[-(tmp+cloNormal)], value);
      clDisabledHighlightedText..clDisabledForeground:
        SetColor(cgDisabled, ColorRoles[-(tmp+cloDisabled)], value);
      clActiveHighlightedText..clActiveForeground:
        SetColor(cgActive, ColorRoles[-(tmp+cloActive)], value);
    end;
end;

function TJvSystemColors.GetColor(Index: Integer): TColor;
begin
  with Application.Palette do
    Result:=  GetColor(ColorArray[Index]);
end;


end.

