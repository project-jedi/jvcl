{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHotKey.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A component that allows the user to register an application wide hotkey combination.
    Set the HotKey property to a *unique* combination of Ctrl,Alt,Shift and a character.
    Set active to true to receive notifications when the hotkey is pressed. The OnHotKey
    event is called when the user presses the hotkey combination.}

unit JvAppHotKey;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  JvComponent;

type
  TJvApplicationHotKey = class(TJvComponent)
  private
    { Private declarations }
    FActive: boolean;
    FShortCut: TShortCut;
    FMods: word;
    FVirtKey: word;
    FOnHotKey: TNotifyEvent;
    FHandle: THandle;
    FID: integer;
    procedure GetKeys;
    procedure SetActive(Value: boolean);
    procedure SetShortCut(Value: TShortCut);
    function WndProc(var Msg: TMessage): boolean;
    procedure GetWndProc;
    procedure ResetWndProc;
  protected
    { Protected declarations }
    procedure DoHotKey; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Active: boolean read FActive write SetActive default false;
    property HotKey: TShortCut read FShortCut write SetShortCut;
    property OnHotKey: TNotifyEvent read FOnHotKey write FOnHotKey;
  end;

implementation
uses
  JvWndProcHook;

{ $R HotKey.dcr }

procedure TJvApplicationHotKey.GetKeys;
var aShift: TShiftState;
begin
  ShortCutToKey(FShortCut, FVirtKey, aShift);
  if ssCtrl in aShift then
    FMods := FMods or MOD_CONTROL;
  if ssShift in aShift then
    FMods := FMods or MOD_SHIFT;
  if ssAlt in aShift then
    FMods := FMods or MOD_ALT;
end;

procedure TJvApplicationHotKey.SetShortCut(Value: TShortCut);
var b: Boolean;
begin
  if FShortCut <> Value then
  begin
    b := FActive;
    SetActive(false);
    FShortCut := Value;
    SetActive(b);
  end;
end;

constructor TJvApplicationHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJvApplicationHotKey.Destroy;
begin
  ResetWndProc;
  inherited Destroy;
end;

procedure TJvApplicationHotKey.SetActive(Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if csDesigning in ComponentState then
      Exit;
    if FActive then
    begin
      GetWndProc;
      FID := GlobalAddAtom(PChar(Application.Exename));
      GetKeys;
      RegisterHotKey(FHandle, FID, FMods, FVirtKey);
    end
    else
    begin
      UnRegisterHotKey(FHandle, FID);
      ResetWndProc;
    end;
  end;
end;

procedure TJvApplicationHotKey.DoHotKey;
begin
  if Assigned(FOnHotKey) then
    FOnHotKey(self);
end;

procedure TJvApplicationHotKey.GetWndProc;
begin
  if (Owner is TWinControl) and (FHandle = 0) then
  begin
    FHandle := TWinControl(Owner).Handle;
    RegisterWndProcHook(TWinControl(Owner), WndProc, hoAfterMsg);
  end
  else
    SetActive(false);
end;

procedure TJvApplicationHotKey.ResetWndProc;
begin
  if FHandle <> 0 then
  begin
    UnregisterWndProcHook(TWinControl(Owner), WndProc, hoAfterMsg);
    GlobalDeleteAtom(FID);
    FHandle := 0;
  end
end;

function TJvApplicationHotKey.WndProc(var Msg: TMessage): boolean;
begin
  if Msg.Msg = WM_HOTKEY then
    DoHotKey;
  Result := false;
end;

end.

