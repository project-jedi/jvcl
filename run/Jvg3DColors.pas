{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Jvg3DColors.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit Jvg3DColors;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
  TJvg3DLocalColors = class(TComponent)
  private
    FDkShadow: TColor;
    FHighlight: TColor;
    FShadow: TColor;
    FColorShadowShift: Byte;
    FColorHighlightShift: Byte;
    OldPointer: Pointer;
    procedure SetDefaults;
    procedure SetDkShadow(Value: TColor);
    procedure SetHighlight(Value: TColor);
    procedure SetShadow(Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateAuto3DColors(BaseColor: TColor);
    procedure MakeGlobal;
    procedure MakeLocal;
    property ColorShadowShift: Byte read FColorShadowShift write FColorShadowShift default 60;
    property ColorHighlightShift: Byte read FColorHighlightShift write FColorHighlightShift default 60;
  published
    property DkShadow: TColor read FDkShadow write SetDkShadow default cl3DDkShadow;
    property Highlight: TColor read FHighlight write SetHighlight default clBtnHighlight;
    property Shadow: TColor read FShadow write SetShadow default clBtnShadow;
  end;

  TJvg3DColors = class(TJvg3DLocalColors)
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

uses
  Math,
  {$IFDEF USEJVCL}
  JvConsts,
  JvResources,
  {$ENDIF USEJVCL}
  JvgUtils, JvgTypes;

{$IFNDEF USEJVCL}
resourcestring
  RsEOnlyOneInstanceOfTJvg3DLocalColors = 'Cannot create more than one instance of TJvg3DLocalColors component';
{$ENDIF USEJVCL}

//=== { TJvg3DLocalColors } ==================================================

constructor TJvg3DLocalColors.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefaults;
end;

destructor TJvg3DLocalColors.Destroy;
begin
  glGlobalData.lp3DColors := nil;
  inherited Destroy;
end;

procedure TJvg3DLocalColors.SetDefaults;
begin
  FDkShadow := cl3DDkShadow;
  FHighlight := clBtnHighlight;
  FShadow := clBtnShadow;
  FColorShadowShift := 60;
  FColorHighlightShift := 60;
end;

procedure TJvg3DLocalColors.CreateAuto3DColors(BaseColor: TColor);
var
  R, G, B: Byte;
begin
  if (BaseColor and $80000000) <> 0 then
    BaseColor := GetSysColor(BaseColor and $FF);
  B := GetRValue(BaseColor);
  G := GetGValue(BaseColor);
  R := GetBValue(BaseColor);
  FShadow := RGB(Max(R - ColorShadowShift, 0),
    Max(G - ColorShadowShift, 0), Max(B - ColorShadowShift, 0));
  FHighlight := RGB(Min(R + ColorHighlightShift, 255),
    Min(G + ColorHighlightShift, 255), Min(B + ColorHighlightShift, 255));
end;

procedure TJvg3DLocalColors.MakeGlobal;
begin
  OldPointer := glGlobalData.lp3DColors;
  glGlobalData.lp3DColors := Self;
end;

procedure TJvg3DLocalColors.MakeLocal;
begin
  glGlobalData.lp3DColors := OldPointer;
end;

procedure TJvg3DLocalColors.SetDkShadow(Value: TColor);
begin
  FDkShadow := Value; {TWinControl(Owner).Invalidate;}
end;

procedure TJvg3DLocalColors.SetHighlight(Value: TColor);
begin
  FHighlight := Value; {TWinControl(Owner).Invalidate;}
end;

procedure TJvg3DLocalColors.SetShadow(Value: TColor);
begin
  FShadow := Value; {TWinControl(Owner).Invalidate;}
end;

//=== { TJvg3DColors } =======================================================

constructor TJvg3DColors.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefaults;
  glGlobalData.lp3DColors := Self;
end;

procedure TJvg3DColors.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component <> Self) and (Operation = opInsert) and (Component is TJvg3DLocalColors) then
    raise Exception.CreateRes(@RsEOnlyOneInstanceOfTJvg3DLocalColors);
end;

end.

