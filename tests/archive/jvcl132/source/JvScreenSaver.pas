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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvScreenSaver;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvScreenSaver = class(TJvComponent)
  private
    FStart: TNotifyEvent;
    FOnConfigure: TNotifyEvent;
    FOnPreview: TOnParent;
    FOnPasswordChange: TOnParent;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnConfigure: TNotifyEvent read FOnConfigure write FOnConfigure;
    property OnPreview: TOnParent read FOnPreview write FOnPreview;
    property OnStart: TNotifyEvent read FStart write FStart;
    property OnPasswordChange: TOnParent read FOnPasswordChange write FOnPasswordChange;
  end;

implementation

{**************************************************}

constructor TJvScreenSaver.Create(AOwner: TComponent);
var
  st: string;
  style: Integer;
  h: THandle;
begin
  inherited;
  style := 0;

  if ParamCount <> 0 then
  begin
    st := UpperCase(ParamStr(1));
    if st = 'C' then
      style := 0
    else if st = 'A' then
      style := 1
    else if st = 'P' then
      style := 2
    else
      style := 3;
  end;

  h := 0;
  if style in [1, 2] then
    h := StrToInt(paramstr(2));
  case style of
    0:
      if Assigned(FOnConfigure) then
        FOnConfigure(Self);
    1:
      if Assigned(FOnPasswordChange) then
        FOnPasswordChange(Self, h);
    2:
      if Assigned(FOnPreview) then
        FOnPreview(Self, h);
    3:
      if Assigned(FStart) then
        FStart(Self);
  end;
end;

end.
