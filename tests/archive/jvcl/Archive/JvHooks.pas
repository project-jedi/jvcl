{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHooks.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Windows hook manager for JVCL Controls

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvHooks;

interface

uses Windows, Messages, Classes, Forms, Controls;

type

  TJvWHook = class
  private
    FHandle : HWnd;
    OldWndProc : Pointer;
    NewWndProc : Pointer;
    FWndProc : TWndMethod;
    FEnabled : boolean;
    procedure SetEnabled(Value : boolean);
  public
    constructor Create(AHandle : Hwnd; AWndProc : TWndMethod);
    procedure HookWindow;
    procedure UnhookWindow;
    destructor Destroy; override;
    procedure CallOldProc(var Message: TMessage);
    property Enabled : boolean read FEnabled write SetEnabled;
  end;

implementation

var
  WindowHooks : TList = nil;

constructor TJvWHook.Create(AHandle : Hwnd; AWndProc : TWndMethod);
begin
  FHandle := AHandle;
  FWndProc := AWndProc;
  HookWindow;
end;

destructor TJvWHook.Destroy;
begin
  UnhookWindow;
  inherited Destroy;
end;

procedure TJvWHook.SetEnabled(Value : boolean);
begin
  if FEnabled <> Value then
    if FEnabled then
      HookWindow else
      UnhookWindow;
end;

procedure TJvWHook.HookWindow;
begin
  if not FEnabled then
  begin
    if WindowHooks = nil then
      WindowHooks := TList.Create;
    WindowHooks.Add(Self);
    OldWndProc := Pointer(GetWindowLong(FHandle, GWL_WNDPROC));
    NewWndProc := MakeObjectInstance(TWndMethod(FWndProc));
    SetWindowLong(FHandle, GWL_WNDPROC, LongInt(NewWndProc));
    FEnabled := true;
  end;  
end;

procedure TJvWHook.UnhookWindow;
var
  i : integer;
  Hook : TJvWHook;
  HookChanged : boolean;
begin
  if FEnabled then
  begin
    HookChanged := false;
    WindowHooks.Remove(Self);
    for i := 0 to WindowHooks.Count - 1 do
    begin
      Hook := TJvWHook(WindowHooks[i]);
      if Hook.OldWndProc = NewWndProc then
      begin
        Hook.OldWndProc := OldWndProc;
        SetWindowLong(FHandle, GWL_WNDPROC, LongInt(Hook.NewWndProc));
        HookChanged := true;
        break;
      end;
    end;
    if not HookChanged then
      SetWindowLong(FHandle, GWL_WNDPROC, LongInt(OldWndProc));
    if WindowHooks.Count = 0 then
    begin
      WindowHooks.Free;
      WindowHooks := nil; 
    end;
    if Assigned(NewWndProc) then
      FreeObjectInstance(NewWndProc);
    FEnabled := false;
  end;
end;

procedure TJvWHook.CallOldProc(var Message: TMessage);
begin
  with Message do
    Result := CallWindowProc(OldWndProc, FHandle, Msg, wParam, lParam);
end;

end.


