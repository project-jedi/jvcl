{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScreenSaver.PAS, released on 2001-02-28.

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

unit JvScreenSaver;

interface

uses
  Windows, SysUtils, Classes,
  JvTypes, JvComponent;

type
  TJvScreenSaver = class(TJvComponent)
  private
    FOnStart: TNotifyEvent;
    FOnConfigure: TNotifyEvent;
    FOnPreview: TJvParentEvent;
    FOnPasswordChange: TJvParentEvent;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnConfigure: TNotifyEvent read FOnConfigure write FOnConfigure;
    property OnPreview: TJvParentEvent read FOnPreview write FOnPreview;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnPasswordChange: TJvParentEvent read FOnPasswordChange write FOnPasswordChange;
  end;

implementation

constructor TJvScreenSaver.Create(AOwner: TComponent);
var
  St: string;
  Style: Integer;
  H: THandle;
begin
  inherited Create(AOwner);
  Style := 0;
  if ParamCount <> 0 then
  begin
    St := UpperCase(ParamStr(1));
    if St = 'C' then
      Style := 0
    else
    if St = 'A' then
      Style := 1
    else
    if St = 'P' then
      Style := 2
    else
      Style := 3;
  end;

  H := 0;
  if Style in [1, 2] then
    H := StrToInt(ParamStr(2));
  case Style of
    0:
      if Assigned(FOnConfigure) then
        FOnConfigure(Self);
    1:
      if Assigned(FOnPasswordChange) then
        FOnPasswordChange(Self, H);
    2:
      if Assigned(FOnPreview) then
        FOnPreview(Self, H);
    3:
      if Assigned(FOnStart) then
        FOnStart(Self);
  end;
end;

end.

