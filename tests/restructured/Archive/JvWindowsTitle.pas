{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWindowsTitle.PAS, released on 2001-02-28.

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

unit JvWindowsTitle;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, JvTypes, JvComponent;

type
  TJvWindowsTitle = class(TJvComponent)
  private
    FOnList: TListEvent;
    FOnAfterList: TNotifyEvent;
    FOnBeforeList: TNotifyEvent;
  public
    procedure Update;
  published
    property OnBeforeList: TNotifyEvent read FOnBeforeList write FOnBeforeList;
    property OnList: TListEvent read FOnList write FOnList;
    property OnAfterList: TNotifyEvent read FOnAfterList write FOnAfterList;
  end;

implementation

{************************************************************}

function EnumCall(Handle: THandle; lParam: LPARAM): Boolean; stdcall;
var
  st: array[0..256] of Char;
  st2: string;
begin
  if IsWindowVisible(Handle) then
  begin
    GetWindowText(Handle, st, SizeOf(st));
    st2 := st;

    if (st2 <> '') then
      with TJvWindowsTitle(Pointer(lParam)) do
        if Assigned(OnList) then
          OnList(TJvWindowsTitle(Pointer(lParam)), st2, Handle);
  end;
  Result := True;
end;

{**************************************************}

procedure TJvWindowsTitle.Update;
begin
  if Assigned(FOnBeforeList) then
    FOnBeforeList(Self);
  if Assigned(FOnList) then
    EnumWindows(@EnumCall, Longint(Self));
  if Assigned(FOnAfterList) then
    FOnAfterList(Self);
end;

end.
