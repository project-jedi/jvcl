{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Jvg3DColors.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15 

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit Jvg3DColors;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  Tgl3DLocalColors = class(TComponent)
  private
    FDkShadow,
    FHighlight,
    FShadow	         : TColor;
    FColorShadowShift    : byte;
    FColorHighlightShift : byte;
    OldPointer           : Pointer;
    procedure SetDefaults;
    procedure SetDkShadow( Value: TColor);
    procedure SetHighlight( Value: TColor);
    procedure SetShadow( Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateAuto3DColors(BaseColor: TColor);
    procedure MakeGlobal;
    procedure MakeLocal;

    property ColorShadowShift: byte read FColorShadowShift write FColorShadowShift
     default 60;
    property ColorHighlightShift: byte read FColorHighlightShift write FColorHighlightShift
     default 60;
  published
    property DkShadow: TColor read FDkShadow write SetDkShadow default cl3DDkShadow;
    property Highlight: TColor read FHighlight write SetHighlight default clBtnHighlight;
    property Shadow: TColor read FShadow write SetShadow default clBtnShadow;
  end;

  Tgl3DColors = class(Tgl3DLocalColors)
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation
uses JvgUtils, JvgTypes;
procedure Register;
begin
  RegisterComponents('Proba', [Tgl3DColors]);
end;

constructor Tgl3DColors.Create(AOwner: TComponent);
begin
  inherited;
  SetDefaults;
  glGlobalData.lp3DColors := self;
end;

procedure Tgl3DColors.Notification( Component: TComponent; Operation: TOperation );
begin
  if (Component <> Self)and(Operation = opInsert)and(Component is Tgl3DLocalColors ) then
    raise Exception.Create('Cannot create more than one instance of Tgl3DLocalColors component');
end;

constructor Tgl3DLocalColors.Create(AOwner: TComponent);
begin
  inherited;
  SetDefaults;
end;

destructor Tgl3DLocalColors.Destroy;
begin
  glGlobalData.lp3DColors := nil;
  inherited;
end;

procedure Tgl3DLocalColors.SetDefaults;
begin
  FDkShadow	:= cl3DDkShadow;
  FHighlight	:= clBtnHighlight;
  FShadow	:= clBtnShadow;
  FColorShadowShift    := 60;
  FColorHighlightShift := 60;
end;

procedure Tgl3DLocalColors.CreateAuto3DColors(BaseColor: TColor);
var
  r,g,b:byte;
begin
  if (BaseColor and $80000000)<>0 then
    BaseColor:=GetSysColor( BaseColor and $ff );
  b := (BaseColor and $00FF0000) shr 16;
  g := (BaseColor and $0000FF00) shr 8;
  r := BaseColor and $000000FF;
  FShadow := RGB(max(r-ColorShadowShift,0),max(g-ColorShadowShift,0),max(b-ColorShadowShift,0));
  FHighlight := RGB(min(r+ColorHighlightShift,255),min(g+ColorHighlightShift,255),min(b+ColorHighlightShift,255));
end;

procedure Tgl3DLocalColors.MakeGlobal;
begin
  OldPointer := glGlobalData.lp3DColors;
  glGlobalData.lp3DColors := self;
end;

procedure Tgl3DLocalColors.MakeLocal;
begin
  glGlobalData.lp3DColors := OldPointer;
end;

procedure Tgl3DLocalColors.SetDkShadow( Value: TColor);
begin FDkShadow := Value; {TWinControl(Owner).Invalidate;} end;

procedure Tgl3DLocalColors.SetHighlight( Value: TColor);
begin FHighlight := Value; {TWinControl(Owner).Invalidate;} end;

procedure Tgl3DLocalColors.SetShadow( Value: TColor);
begin FShadow := Value; {TWinControl(Owner).Invalidate;} end;

end.
