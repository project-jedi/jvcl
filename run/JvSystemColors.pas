{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSystemColors.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].
                André Snepvangers [asn@xs4all.nl] (VisualCLX compatible version)

Last Modified: 2004-01-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSystemColors;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Graphics,
  {$ELSE}
  QWindows, QGraphics,
  {$ENDIF}
  JvTypes, JvComponent;

type
  TJvSystemColors = class(TJvComponent)
  private
    procedure SetColor(Index: Integer; Value: TColor);
    function GetColor(Index: Integer): TColor;
  published
    {$IFDEF VCL}
    property _3DHilight: TColor index 0 read GetColor write SetColor stored False;
    property _3DLight: TColor index 1 read GetColor write SetColor stored False;
    property _3DShadow: TColor index 2 read GetColor write SetColor stored False;
    property _3DdkShadow: TColor index 3 read GetColor write SetColor stored False;
    property _3DFace: TColor index 4 read GetColor write SetColor stored False;
    property ActiveBorder: TColor index 5 read GetColor write SetColor stored False;
    property ActiveCaption: TColor index 6 read GetColor write SetColor stored False;
    property AppWorkspace: TColor index 7 read GetColor write SetColor stored False;
    property Background: TColor index 8 read GetColor write SetColor stored False;
    property BtnFace: TColor index 9 read GetColor write SetColor stored False;
    property BtnText: TColor index 10 read GetColor write SetColor stored False;
    property CaptionText: TColor index 11 read GetColor write SetColor stored False;
    property GrayText: TColor index 12 read GetColor write SetColor stored False;
    property Highlight: TColor index 13 read GetColor write SetColor stored False;
    property HighlightText: TColor index 14 read GetColor write SetColor stored False;
    property InactiveBorder: TColor index 15 read GetColor write SetColor stored False;
    property InactiveCaption: TColor index 16 read GetColor write SetColor stored False;
    property InactiveCaptionText: TColor index 17 read GetColor write SetColor stored False;
    property InfoBk: TColor index 18 read GetColor write SetColor stored False;
    property InfoText: TColor index 19 read GetColor write SetColor stored False;
    property Menu: TColor index 20 read GetColor write SetColor stored False;
    property MenuText: TColor index 21 read GetColor write SetColor stored False;
    property ScrollBar: TColor index 22 read GetColor write SetColor stored False;
    property Window: TColor index 23 read GetColor write SetColor stored False;
    property WindowFrame: TColor index 24 read GetColor write SetColor stored False;
    property WindowText: TColor index 25 read GetColor write SetColor stored False;
    {$ENDIF}
    {$IFDEF VisualCLX}
    Property NormalForeground: TColor index 0 read GetColor write SetColor stored False;
    Property NormalButton: TColor index 1 read GetColor write SetColor stored False;
    Property NormalLight: TColor index 2 read GetColor write SetColor stored False;
    Property NormalMidlight: TColor index 3 read GetColor write SetColor stored False;
    Property NormalDark: TColor index 4 read GetColor write SetColor stored False;
    Property NormalMid: TColor index 5 read GetColor write SetColor stored False;
    Property NormalText: TColor index 6 read GetColor write SetColor stored False;
    Property NormalBrightText: TColor index 7 read GetColor write SetColor stored False;
    Property NormalButtonText: TColor index 8 read GetColor write SetColor stored False;
    Property NormalBase: TColor index 9 read GetColor write SetColor stored False;
    Property NormalBackground: TColor index 10 read GetColor write SetColor stored False;
    Property NormalShadow: TColor index 11 read GetColor write SetColor stored False;
    Property NormalHighlight: TColor index 12 read GetColor write SetColor stored False;
    Property NormalHighlightedText: TColor index 13 read GetColor write SetColor stored False;
    Property DisabledForeground: TColor index 14 read GetColor write SetColor stored False;
    Property DisabledButton: TColor index 15 read GetColor write SetColor stored False;
    Property DisabledLight: TColor index 16 read GetColor write SetColor stored False;
    Property DisabledMidlight: TColor index 17 read GetColor write SetColor stored False;
    Property DisabledDark: TColor index 18 read GetColor write SetColor stored False;
    Property DisabledMid: TColor index 19 read GetColor write SetColor stored False;
    Property DisabledText: TColor index 20 read GetColor write SetColor stored False;
    Property DisabledBrightText: TColor index 21 read GetColor write SetColor stored False;
    Property DisabledButtonText: TColor index 22 read GetColor write SetColor stored False;
    Property DisabledBase: TColor index 23 read GetColor write SetColor stored False;
    Property DisabledBackground: TColor index 24 read GetColor write SetColor stored False;
    Property DisabledShadow: TColor index 25 read GetColor write SetColor stored False;
    Property DisabledHighlight: TColor index 26 read GetColor write SetColor stored False;
    Property DisabledHighlightedText: TColor index 27 read GetColor write SetColor stored False;
    Property ActiveForeground: TColor index 28 read GetColor write SetColor stored False;
    Property ActiveButton: TColor index 29 read GetColor write SetColor stored False;
    Property ActiveLight: TColor index 30 read GetColor write SetColor stored False;
    Property ActiveMidlight: TColor index 31 read GetColor write SetColor stored False;
    Property ActiveDark: TColor index 32 read GetColor write SetColor stored False;
    Property ActiveMid: TColor index 33 read GetColor write SetColor stored False;
    Property ActiveText: TColor index 34 read GetColor write SetColor stored False;
    Property ActiveBrightText: TColor index 35 read GetColor write SetColor stored False;
    Property ActiveButtonText: TColor index 36 read GetColor write SetColor stored False;
    Property ActiveBase: TColor index 37 read GetColor write SetColor stored False;
    Property ActiveBackground: TColor index 38 read GetColor write SetColor stored False;
    Property ActiveShadow: TColor index 49 read GetColor write SetColor stored False;
    Property ActiveHighlight: TColor index 40 read GetColor write SetColor stored False;
    Property ActiveHighlightedText: TColor index 41 read GetColor write SetColor stored False;
    {$ENDIF VisualCLX}
  end;

implementation

const
  {$IFDEF VCL}
  ColorArray: array [0..25] of Integer = (COLOR_3DHILIGHT, COLOR_3DLIGHT, //0
    COLOR_3DSHADOW, COLOR_3DDKSHADOW, COLOR_3DFACE, COLOR_ACTIVEBORDER, //2
    COLOR_ACTIVECAPTION, COLOR_APPWORKSPACE, COLOR_BACKGROUND, //6
    COLOR_BTNFACE, COLOR_BTNTEXT, COLOR_CAPTIONTEXT, COLOR_GRAYTEXT, //9
    COLOR_HIGHLIGHT, COLOR_HIGHLIGHTTEXT, COLOR_INACTIVEBORDER, //13
    COLOR_INACTIVECAPTION, COLOR_INACTIVECAPTIONTEXT, COLOR_INFOBK, //16
    COLOR_INFOTEXT, COLOR_MENU, COLOR_MENUTEXT, COLOR_SCROLLBAR, //19
    COLOR_WINDOW, COLOR_WINDOWFRAME, COLOR_WINDOWTEXT); //23
  {$ENDIF}
  {$IFDEF VisualCLX}
  ColorArray: array[0..41] of TColor = (
    clNormalForeground, clNormalButton, clNormalLight, clNormalMidlight, clNormalDark, clNormalMid,
    clNormalText, clNormalBrightText, clNormalButtonText, clNormalBase, clNormalBackground,
    clNormalShadow, clNormalHighlight, clNormalHighlightedText,

    clDisabledForeground, clDisabledButton, clDisabledLight, clDisabledMidlight, clDisabledDark, clDisabledMid,
    clDisabledText, clDisabledBrightText, clDisabledButtonText, clDisabledBase, clDisabledBackground,
    clDisabledShadow, clDisabledHighlight, clDisabledHighlightedText,

    clActiveForeground, clActiveButton, clActiveLight, clActiveMidlight, clActiveDark, clActiveMid,
    clActiveText, clActiveBrightText, clActiveButtonText, clActiveBase, clActiveBackground,
    clActiveShadow, clActiveHighlight, clActiveHighlightedText);
  {$ENDIF}

procedure TJvSystemColors.SetColor(Index: Integer; Value: TColor);
var
  Tmp: Integer;
begin
  Tmp := ColorArray[Index];
  SetSysColors(1, Tmp, Value);
end;

function TJvSystemColors.GetColor(Index: Integer): TColor;
begin
  Result := GetSysColor(ColorArray[Index]);
end;

end.

