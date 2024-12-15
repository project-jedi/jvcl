{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScreenSaver.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvScreenSaver;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes,
  JvTypes, JvComponentBase;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvScreenSaver = class(TJvComponent)
  private
    FOnStart: TNotifyEvent;
    FOnConfigure: TNotifyEvent;
    FOnPreview: TJvParentEvent;
    FOnPasswordChange: TJvParentEvent;
  public
    procedure Loaded; override;
  published
    property OnConfigure: TNotifyEvent read FOnConfigure write FOnConfigure;
    property OnPreview: TJvParentEvent read FOnPreview write FOnPreview;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnPasswordChange: TJvParentEvent read FOnPasswordChange write FOnPasswordChange;
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

procedure TJvScreenSaver.Loaded;
type
  TScreenSaverStyle = (ssConfigure, ssPassword, ssPreview, ssStart);
var
  S: string;
  Style: TScreenSaverStyle;
  H: THandle;
begin
  inherited Loaded;
  Style := ssConfigure;
  if ParamCount <> 0 then
  begin
    S := UpperCase(ParamStr(1));
    if S = 'C' then
      Style := ssConfigure
    else
    if S = 'A' then
      Style := ssPassword
    else
    if S = 'P' then
      Style := ssPreview
    else
      Style := ssStart;
  end;

  if Style in [ssPassword, ssPreview] then
    H := StrToInt(ParamStr(2))
  else
    H := 0;
  case Style of
    ssConfigure:
      if Assigned(FOnConfigure) then
        FOnConfigure(Self);
    ssPassword:
      if Assigned(FOnPasswordChange) then
        FOnPasswordChange(Self, H);
    ssPreview:
      if Assigned(FOnPreview) then
        FOnPreview(Self, H);
    ssStart:
      if Assigned(FOnStart) then
        FOnStart(Self);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
