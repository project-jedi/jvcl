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
  TJvHotKeyRegisterFailed = procedure (Sender:TObject; var HotKey:TShortCut) of object;
  TJvApplicationHotKey = class(TJvComponent)
  private
    { Private declarations }
    FActive: boolean;
    FShortCut: TShortCut;
    FOnHotKey: TNotifyEvent;
    FHandle: THandle;
    FID: integer;
    FHasRegistered:boolean;
    FOnHotKeyRegisterFailed: TJvHotKeyRegisterFailed;
    procedure SetActive(Value: boolean);
    procedure SetShortCut(Value: TShortCut);
    function WndProc(var Msg: TMessage): boolean;
    procedure GetWndProc;
    procedure ResetWndProc;
  protected
    { Protected declarations }
    procedure DoHotKey; virtual;
    function DoRegisterHotKey: boolean;dynamic;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property Active: boolean read FActive write SetActive default false;
    property HotKey: TShortCut read FShortCut write SetShortCut;
    property OnHotKey: TNotifyEvent read FOnHotKey write FOnHotKey;
    property OnHotKeyRegisterFailed:TJvHotKeyRegisterFailed read FOnHotKeyRegisterFailed write FOnHotKeyRegisterFailed;
  end;

implementation
uses
  JvWndProcHook;

{ $R HotKey.dcr }

var
  HotKeyInstances:integer = 0;

procedure GetHotKey(AShortCut:TShortCut;var VirtKey,Modifiers:Word);
var aShift: TShiftState;
begin
  ShortCutToKey(AShortCut, VirtKey, aShift);
  Modifiers := 0;
  if ssCtrl in aShift then
    Modifiers := Modifiers or MOD_CONTROL;
  if ssShift in aShift then
    Modifiers := Modifiers or MOD_SHIFT;
  if ssAlt in aShift then
    Modifiers := Modifiers or MOD_ALT;
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

function TJvApplicationHotKey.DoRegisterHotKey:boolean;
var AShortCut:TShortCut;FVirtKey,FMods:word;
begin
  Result := false;
  if FHandle = 0 then
  begin
    FHandle := TWinControl(Owner).Handle;
    GetHotKey(FShortCut,FVirtKey,FMods);
    while not RegisterHotKey(FHandle,FID,FMods,FVirtKey) do
    begin
      if Assigned(FOnHotKeyRegisterFailed) then
      begin
        AShortCut := FShortCut;
        FOnHotKeyRegisterFailed(self,FShortCut);
        // make sure we don't get stuck in a loop here:
        if AShortCut = FShortCut then Exit;
        GetHotKey(FShortCut,FVirtKey,FMods);
      end
      else
        Exit;
    end;
    Result := true;
  end;
end;

procedure TJvApplicationHotKey.SetActive(Value: boolean);
begin
  if FActive <> Value then
  begin
    if csDesigning in ComponentState then
    begin
      FActive := Value;
      Exit;
    end;
    if Value and not FHasRegistered then
    begin
      if IsLibrary then
        FID := GlobalAddAtom(PChar(Application.Exename))
      else
      begin
        FID := HotKeyInstances;
        Inc(HotKeyInstances);
      end;
      if not DoRegisterHotKey then Exit;
      GetWndProc;
    end
    else if FHasRegistered then
    begin
      UnRegisterHotKey(FHandle,FID);
      ResetWndProc;
      if IsLibrary then
        GlobalDeleteAtom (FID);
    end;
    FActive := Value;
  end;
end;

procedure TJvApplicationHotKey.DoHotKey;
begin
  if Assigned(FOnHotKey) then
    FOnHotKey(self);
end;

procedure TJvApplicationHotKey.GetWndProc;
begin
  if not FHasRegistered and (Owner is TWinControl) then
  begin
    RegisterWndProcHook(TWinControl(Owner), WndProc, hoAfterMsg);
    FHasRegistered := true;
  end
  else
    SetActive(false);
end;

procedure TJvApplicationHotKey.ResetWndProc;
begin
  if FHasRegistered and (Owner is TWinControl) then
  begin
    UnregisterWndProcHook(TWinControl(Owner), WndProc, hoAfterMsg);
    FHasRegistered := false;
  end;
  FHandle := 0;
end;

function TJvApplicationHotKey.WndProc(var Msg: TMessage): boolean;
begin
  if (Msg.Msg = WM_HOTKEY) and (FID = Msg.WParam) then
    DoHotKey;
  Result := false;
end;

end.

