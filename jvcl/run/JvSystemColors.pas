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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSystemColors;

interface

uses
  Windows, SysUtils, Classes, Graphics,
  JvTypes, JvComponent;

type
  TJvSystemColors = class(TJvComponent)
  private
    procedure SetColor(Index: Integer; Value: TColor);
    function GetColor(Index: Integer): TColor;
  published
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
  end;

implementation

const
  ColorArray: array [0..25] of Integer = (COLOR_3DHILIGHT, COLOR_3DLIGHT, //0
    COLOR_3DSHADOW, COLOR_3DDKSHADOW, COLOR_3DFACE, COLOR_ACTIVEBORDER, //2
    COLOR_ACTIVECAPTION, COLOR_APPWORKSPACE, COLOR_BACKGROUND, //6
    COLOR_BTNFACE, COLOR_BTNTEXT, COLOR_CAPTIONTEXT, COLOR_GRAYTEXT, //9
    COLOR_HIGHLIGHT, COLOR_HIGHLIGHTTEXT, COLOR_INACTIVEBORDER, //13
    COLOR_INACTIVECAPTION, COLOR_INACTIVECAPTIONTEXT, COLOR_INFOBK, //16
    COLOR_INFOTEXT, COLOR_MENU, COLOR_MENUTEXT, COLOR_SCROLLBAR, //19
    COLOR_WINDOW, COLOR_WINDOWFRAME, COLOR_WINDOWTEXT); //23

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

