{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimeLimit.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQTimeLimit;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,  
  QControls, QDialogs, QForms, QWindows, 
  JvQComponent, JvQTypes;

type
  TJvTimeLimit = class(TJvComponent)
  private
    FDate: TDate;
    FOnExpire: TNotifyEvent;
  protected
    procedure Loaded; override;
  published
    property EndDate: TDate read FDate write FDate;
    property OnExpire: TNotifyEvent read FOnExpire write FOnExpire;
  end;

implementation

uses
  JvQResources;

procedure TJvTimeLimit.Loaded;
begin
  if not (csDesigning in ComponentState) then
    if Date >= FDate then
    begin
      if Assigned(FOnExpire) then
        FOnExpire(Self)
      else
        ShowMessage(RsExpired);
      Application.Terminate;
    end;
end;

end.

