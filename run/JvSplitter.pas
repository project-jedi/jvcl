{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSplitter.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSplitter;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Forms, ExtCtrls, Controls,
  JvExExtCtrls;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvSplitter = class(TJvExSplitter)
  {$IFDEF JVCLThemesEnabled}
  protected
    procedure Paint; override;
  {$ENDIF JVCLThemesEnabled}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ShowHint;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvThemes;

constructor TJvSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvSplitter.Paint;
var
  Bmp: TBitmap;
  DC: THandle;
begin
  if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} then
  begin
//    DrawThemedBackground(Self, Canvas, ClientRect, Parent.Brush.Color);
    DC := Canvas.Handle;
    Bmp := TBitmap.Create;
    try
      Bmp.Width := ClientWidth;
      Bmp.Height := ClientHeight;
      Canvas.Handle := Bmp.Canvas.Handle;
      try
        inherited Paint;
      finally
        Canvas.Handle := DC;
      end;
      Bmp.Transparent := True;
      Bmp.TransparentColor := Color;
      Canvas.Draw(0, 0, Bmp);
    finally
      Bmp.Free;
    end;
  end
  else
    inherited Paint;
end;
{$ENDIF JVCLThemesEnabled}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
