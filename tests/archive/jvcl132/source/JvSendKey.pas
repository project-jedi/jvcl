{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSendKey.PAS, released on 2001-02-28.

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

unit JvSendKey;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvSendKey = class(TJvComponent)
  private
  published
    function SendKey(AppName: string; Key: Char): Boolean;
  end;

implementation

{$WARNINGS OFF}

{****************************************************}

procedure SendShift(H: HWND; Down: Boolean);
var
  vKey, ScanCode: Word;
  lParam: Longint;
begin
  vKey := VK_SHIFT;
  ScanCode := MapVirtualKey(vKey, 0);
  lParam := Longint(ScanCode) shl 16 or 1;
  if not Down then
    lParam := lParam or $C0000000;
  SendMessage(H, WM_KEYDOWN, vKey, lParam);
end;

{****************************************************}

procedure SendCtrl(H: HWND; Down: Boolean);
var
  vKey, ScanCode: Word;
  lParam: Longint;
begin
  vKey := VK_CONTROL;
  ScanCode := MapVirtualKey(vKey, 0);
  lParam := Longint(ScanCode) shl 16 or 1;
  if not Down then
    lParam := lParam or $C0000000;
  SendMessage(H, WM_KEYDOWN, vKey, lParam);
end;

{****************************************************}

function TJvSendKey.SendKey(AppName: string; Key: Char): Boolean;
var
  vKey, ScanCode: Word;
  lParam, ConvKey: longint;
  Shift, Ctrl: Boolean;
  H: HWND;
begin
  H := FindWindow(PChar(AppName), nil);
  if H <> 0 then
  begin
    ConvKey := OemKeyScan(Ord(Key));
    Shift := (ConvKey and $00020000) <> 0;
    Ctrl := (ConvKey and $00040000) <> 0;
    ScanCode := ConvKey and $000000FF or $FF00;
    vKey := Ord(Key);
    lParam := Longint(ScanCode) shl 16 or 1;
    if Shift then
      SendShift(H, True);
    if Ctrl then
      SendCtrl(H, True);
    SendMessage(H, WM_KEYDOWN, vKey, lParam);
    SendMessage(H, WM_CHAR, vKey, lParam);
    lParam := lParam or $C0000000;
    SendMessage(H, WM_KEYUP, vKey, lParam);
    if Shift then
      SendShift(H, False);
    if Ctrl then
      SendCtrl(H, False);
    Result := True;
  end
  else
    Result := False;
end;

{$WARNINGS ON}

end.
