{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWndProcHook.PAS, released on 2002-11-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
Remko Bonte <remkobonte@myrealbox.com>

Last Modified: 2002-11-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Changes by Remko:
    * Renamed local vars that were named i,j to some more meaningful name :)
    * Added some helper functions
    * Changed the linked list a bit; removed FRoot; added FLast
    * Possibility to specify whether the hook must be before or after the
      handling of the message by the original owner
    * Hook can say he has handled the message; thus preventing the original
      control from handling the message
      - these last 2 seem necessary for some components
    * Added the component TJvWindowHook from JvHook

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvWndProcHook;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls,
  JvComponent;

type
  TJvControlHook = function(var Msg: TMessage): Boolean of object;
  TJvHookMessageEvent = procedure(Sender: TObject; var Msg: TMessage;
    var Handled: Boolean) of object;

  TJvHookOrder = (hoBeforeMsg, hoAfterMsg);

  TJvWindowHook = class(TJvComponent)
  private
    FActive: Boolean;
    FControl: TControl;
    FBeforeMessage: TJvHookMessageEvent;
    FAfterMessage: TJvHookMessageEvent;
    procedure SetActive(Value: Boolean);
    procedure SetControl(Value: TControl);
    function IsForm: Boolean;
    function NotIsForm: Boolean;
    procedure ReadForm(Reader: TReader);
    procedure WriteForm(Writer: TWriter);
    procedure SetAfterMessage(const Value: TJvHookMessageEvent);
    procedure SetBeforeMessage(const Value: TJvHookMessageEvent);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function DoAfterMessage(var Msg: TMessage): Boolean; dynamic;
    function DoBeforeMessage(var Msg: TMessage): Boolean; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HookControl;
    procedure UnHookControl;
  published
    property Active: Boolean read FActive write SetActive default True;
    property Control: TControl read FControl write SetControl stored NotIsForm;
    property BeforeMessage: TJvHookMessageEvent read FBeforeMessage write SetBeforeMessage;
    property AfterMessage: TJvHookMessageEvent read FAfterMessage write SetAfterMessage;
  end;

  { Maybe move to implementation section, and only declare a procedure, that
    calls release in the interface section ? See also remark in TJvHookInfos.WindowProc }
  TJvReleaser = class
  private
    FHandle: HWND;
    function GetHandle: HWND;
    procedure CMRelease(var Msg: TMessage); message CM_RELEASE;
    procedure WndProc(var Msg: TMessage);
  public
    destructor Destroy; override;
    procedure DefaultHandler(var Msg); override;
    class function Instance: TJvReleaser;
    procedure Release(AObject: TObject);
    property Handle: HWND read GetHandle;
  end;

function RegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean; overload;
function RegisterWndProcHook(AHandle: HWND; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean; overload;
function UnRegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean; overload;
function UnRegisterWndProcHook(AHandle: HWND; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean; overload;

implementation

uses
  Forms;

type
  PJvHookInfo = ^TJvHookInfo;
  TJvHookInfo = record
    Hook: TJvControlHook;
    Next: PJvHookInfo;
  end;

  TJvHookInfos = class(TObject)
  private
    FFirst: array [TJvHookOrder] of PJvHookInfo;
    FLast: array [TJvHookOrder] of PJvHookInfo;
    FHandle: HWND;
    FControl: TControl;
    FOldWndProc: TWndMethod;
    FHooked: Boolean;
  protected
    procedure WindowProc(var Msg: TMessage);
    procedure HookControl;
    procedure UnHookControl;
  public
    constructor Create(AControl: TControl); overload;
    constructor Create(AHandle: HWND); overload;
    destructor Destroy; override;
    procedure Add(const Order: TJvHookOrder; Hook: TJvControlHook);
    procedure Delete(const Order: TJvHookOrder; Hook: TJvControlHook);
    property Control: TControl read FControl;
    property Handle: HWND read FHandle;
  end;

  TJvWndProcHook = class(TComponent)
  private
    FHookInfos: TList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IndexOf(AControl: TControl): Integer; overload;
    function IndexOf(AHandle: HWND): Integer; overload;
    function Find(AControl: TControl): TJvHookInfos; overload;
    function Find(AHandle: HWND): TJvHookInfos; overload;
  public
    destructor Destroy; override;
    function RegisterWndProc(AControl: TControl; Hook: TJvControlHook;
      const Order: TJvHookOrder): Boolean; overload;
    function RegisterWndProc(AHandle: HWND; Hook: TJvControlHook;
      const Order: TJvHookOrder): Boolean; overload;
    function UnRegisterWndProc(AControl: TControl; Hook: TJvControlHook;
      const Order: TJvHookOrder): Boolean; overload;
    function UnRegisterWndProc(AHandle: HWND; Hook: TJvControlHook;
      const Order: TJvHookOrder): Boolean; overload;
  end;

var
  GJvWndProcHook: TJvWndProcHook = nil;
  GReleaser: TJvReleaser = nil;

function WndProcHook: TJvWndProcHook;
begin
  if GJvWndProcHook = nil then
    GJvWndProcHook := TJvWndProcHook.Create(nil);
  Result := GJvWndProcHook;
end;

function RegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean;
begin
  Result := WndProcHook.RegisterWndProc(AControl, Hook, Order);
end;

function RegisterWndProcHook(AHandle: HWND; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean;
begin
  Result := WndProcHook.RegisterWndProc(AHandle, Hook, Order);
end;

function UnRegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean;
begin
  Result := WndProcHook.UnRegisterWndProc(AControl, Hook, Order);
end;

function UnRegisterWndProcHook(AHandle: HWND; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean;
begin
  Result := WndProcHook.UnRegisterWndProc(AHandle, Hook, Order);
end;

//=== TJvWndProcHook =========================================================

destructor TJvWndProcHook.Destroy;
var
  I: Integer;
begin
  if FHookInfos <> nil then
  begin
    for I := 0 to FHookInfos.Count - 1 do
      TJvHookInfos(FHookInfos[I]).Free;
    FHookInfos.Free;
  end;
  inherited Destroy;
end;

function TJvWndProcHook.UnRegisterWndProc(AControl: TControl;
  Hook: TJvControlHook; const Order: TJvHookOrder): Boolean;
var
  HookInfos: TJvHookInfos;
begin
  Result := False;
  if not Assigned(AControl) or not Assigned(Hook) or
    not Assigned(FHookInfos) then
    Exit;
  // find the control:
  HookInfos := Find(AControl);
  Result := Assigned(HookInfos);
  if Result then
    // Maybe delete HookInfos if HookInfos.FFirst.. = nil?
    HookInfos.Delete(Order, Hook);
end;

function TJvWndProcHook.UnRegisterWndProc(AHandle: HWND;
  Hook: TJvControlHook; const Order: TJvHookOrder): Boolean;
var
  HookInfos: TJvHookInfos;
begin
  Result := False;
  if not Assigned(Hook) or not Assigned(FHookInfos) then
    Exit;
  // find the control:
  HookInfos := Find(AHandle);
  Result := Assigned(HookInfos);
  if Result then
    // Maybe delete HookInfos if HookInfos.FFirst.. = nil?
    HookInfos.Delete(Order, Hook);
end;

function TJvWndProcHook.Find(AControl: TControl): TJvHookInfos;
var
  I: Integer;
begin
  I := IndexOf(AControl);
  if I < 0 then
    Result := nil
  else
    Result := TJvHookInfos(FHookInfos[I]);
end;

function TJvWndProcHook.Find(AHandle: HWND): TJvHookInfos;
var
  I: Integer;
begin
  I := IndexOf(AHandle);
  if I < 0 then
    Result := nil
  else
    Result := TJvHookInfos(FHookInfos[I]);
end;

function TJvWndProcHook.IndexOf(AControl: TControl): Integer;
begin
  Result := 0;
  while (Result < FHookInfos.Count) and
    (TJvHookInfos(FHookInfos[Result]).Control <> AControl) do
    Inc(Result);
  if Result = FHookInfos.Count then
    Result := -1;
end;

function TJvWndProcHook.IndexOf(AHandle: HWND): Integer;
begin
  { The following code introduces a problem:

    The handle of a control may change (by a call to RecreateWnd for example)
    thus you may find a Ctrl by calling FindControl(AHandle) in RegisterWndProcHook
    and then it's possible to _not_ find the same control in UnRegisterWndProcHook,
    thus hooks may be left open unwanted.

    Maybe there is a better way to identify hooks than (Handle x Hook x Order) or
    ( Ctrl x Hook x Order ) (?)
  }

  {Ctrl := FindControl(AHandle);
  if Ctrl <> nil then
  begin
    Result := IndexOf(Ctrl);
    if Result >= 0 then
      Exit;
  end;}

  Result := 0;
  while (Result < FHookInfos.Count) and
    (TJvHookInfos(FHookInfos[Result]).Handle <> AHandle) do
    Inc(Result);
  if Result = FHookInfos.Count then
    Result := -1;
end;

procedure TJvWndProcHook.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FHookInfos <> nil) and (AComponent is TControl) then
  begin
    I := IndexOf(TControl(AComponent));
    if I >= 0 then
    begin
      TJvHookInfos(FHookInfos[I]).Free;
      FHookInfos.Delete(I);
    end;
  end;
end;

function TJvWndProcHook.RegisterWndProc(AControl: TControl;
  Hook: TJvControlHook; const Order: TJvHookOrder): Boolean;
var
  HookInfos: TJvHookInfos;
begin
  Result := False;
  if not Assigned(AControl) or not Assigned(Hook) then
    Exit;
  if FHookInfos = nil then
    FHookInfos := TList.Create;

  // find the control:
  HookInfos := Find(AControl);
  if not Assigned(HookInfos) then
  begin
    HookInfos := TJvHookInfos.Create(AControl);
    FHookInfos.Add(HookInfos);
    AControl.FreeNotification(Self);
  end;
  HookInfos.Add(Order, Hook);

  Result := True;
end;

function TJvWndProcHook.RegisterWndProc(AHandle: HWND;
  Hook: TJvControlHook; const Order: TJvHookOrder): Boolean;
var
  HookInfos: TJvHookInfos;
begin
  Result := False;
  if not Assigned(Hook) then
    Exit;
  if FHookInfos = nil then
    FHookInfos := TList.Create;

  // find the control:
  HookInfos := Find(AHandle);
  if not Assigned(HookInfos) then
  begin
    HookInfos := TJvHookInfos.Create(AHandle);
    FHookInfos.Add(HookInfos);
  end;
  HookInfos.Add(Order, Hook);

  Result := True;
end;

//=== TJvHookInfos ===========================================================

constructor TJvHookInfos.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
  FFirst[hoBeforeMsg] := nil;
  FFirst[hoAfterMsg] := nil;
  FLast[hoBeforeMsg] := nil;
  FLast[hoAfterMsg] := nil;
end;

constructor TJvHookInfos.Create(AHandle: HWND);
begin
  inherited Create;
  FHandle := AHandle;
  FFirst[hoBeforeMsg] := nil;
  FFirst[hoAfterMsg] := nil;
  FLast[hoBeforeMsg] := nil;
  FLast[hoAfterMsg] := nil;
end;

destructor TJvHookInfos.Destroy;
var
  HookInfo: PJvHookInfo;
begin
  // not quite sure about this: since the destructor is only ever called in the
  // finalization part of this unit, maybe the control has already disappeared?

  // Notification should take care of that.. (?) (Remko)

  UnHookControl;
  while FFirst[hoBeforeMsg] <> nil do
  begin
    HookInfo := FFirst[hoBeforeMsg];
    FFirst[hoBeforeMsg] := HookInfo.Next;
    Dispose(HookInfo);
  end;
  while FFirst[hoAfterMsg] <> nil do
  begin
    HookInfo := FFirst[hoAfterMsg];
    FFirst[hoAfterMsg] := HookInfo.Next;
    Dispose(HookInfo);
  end;
  inherited Destroy;
end;

procedure TJvHookInfos.Add(const Order: TJvHookOrder; Hook: TJvControlHook);
var
  HookInfo: PJvHookInfo;
begin
  New(HookInfo);
  HookInfo.Hook := Hook;
  HookInfo.Next := nil;

  { Some bookkeeping }
  if FFirst[Order] = nil then
    FFirst[Order] := HookInfo;

  if FLast[Order] <> nil then
    FLast[Order].Next := HookInfo;

  FLast[Order] := HookInfo;

  HookControl;
end;

procedure TJvHookInfos.Delete(const Order: TJvHookOrder; Hook: TJvControlHook);
var
  HookInfo: PJvHookInfo;
  PrevHookInfo: PJvHookInfo;
begin
  HookInfo := FFirst[Order];
  PrevHookInfo := nil;
  while (HookInfo <> nil) and
    ((TMethod(HookInfo.Hook).Code <> TMethod(Hook).Code) or
    (TMethod(HookInfo.Hook).Data <> TMethod(Hook).Data)) do
    {  This is unique: Code = the object whereto the method belongs
                       Data = identifies the method in the object }
  begin
    PrevHookInfo := HookInfo;
    HookInfo := HookInfo.Next;
  end;

  if not Assigned(HookInfo) then
    Exit;

  // patch up the hole (this is the reason for this entire unit!)
  if PrevHookInfo <> nil then
    PrevHookInfo.Next := HookInfo.Next;

  { Bookkeeping }
  if FLast[Order] = HookInfo then
    FLast[Order] := PrevHookInfo;
  if FFirst[Order] = HookInfo then
    FFirst[Order] := HookInfo.Next;

  Dispose(HookInfo);

  if (FFirst[hoBeforeMsg] = nil) and (FFirst[hoAfterMsg] = nil) then
    UnHookControl;
end;

procedure TJvHookInfos.HookControl;
begin
  if FHooked then
    Exit;
  if FControl <> nil then
  begin
    FOldWndProc := FControl.WindowProc;
    FControl.WindowProc := WindowProc;
    FHooked := True;
  end
  else
  begin
    TMethod(FOldWndProc).Data := nil;
    TMethod(FOldWndProc).Code := Pointer(GetWindowLong(FHandle, GWL_WNDPROC));
    {$IFDEF COMPILER6_UP}
    SetWindowLong(FHandle, GWL_WNDPROC, Integer(Classes.MakeObjectInstance(WindowProc)));
    {$ELSE}
    SetWindowLong(FHandle, GWL_WNDPROC, Integer(MakeObjectInstance(WindowProc)));
    {$ENDIF}
    FHooked := True;
  end;
end;

procedure TJvHookInfos.UnHookControl;
var
  Ptr: Pointer;
begin
  if not FHooked then
    Exit;
  if FControl <> nil then
  begin
    FControl.WindowProc := FOldWndProc;
    FHooked := False;
  end
  else
  begin
    Ptr := Pointer(GetWindowLong(FHandle, GWL_WNDPROC));
    SetWindowLong(FHandle, GWL_WNDPROC, Integer(TMethod(FOldWndProc).Code));
    FHooked := False;
    {$IFDEF COMPILER6_UP}
    Classes.FreeObjectInstance(Ptr);
    {$ELSE}
    FreeObjectInstance(Ptr);
    {$ENDIF}
  end;
end;

procedure TJvHookInfos.WindowProc(var Msg: TMessage);
var
  HookInfo, NextHookInfo: PJvHookInfo;
  I: Integer;
begin
  { An object can now report for every possible message that he has
    handled that message, thus preventing the original control from
    handling the message; this is probably not a good idea in the case
    of WM_DESTROY, WM_CLOSE etc. But that's the users responsibility,
    I think }

  Msg.Result := 0;

  HookInfo := FFirst[hoBeforeMsg];
  while HookInfo <> nil do
  begin
    { UnRegisterWndProc may be called from inside a hook procedure; if so
      HookInfo is no longer valid after calling HookInfo.Hook(Msg).
      Thus we need an auxiliary var. NextHookInfo }
    NextHookInfo := HookInfo.Next;
    if HookInfo.Hook(Msg) then
      Exit;
    HookInfo := NextHookInfo;
  end;

  { Maybe only exit here (before the original control handles the message),
    thus enabling all hooks to respond to the message? Otherwise if you
    have 2 components of the same class, that hook a control, then only 1 will
    get the message }

  if TMethod(FOldWndProc).Data <> nil then
    FOldWndProc(Msg)
  else
    Msg.Result := CallWindowProc(TMethod(FOldWndProc).Code, Handle, Msg.Msg,
      Msg.WParam, Msg.LParam);

  HookInfo := FFirst[hoAfterMsg];
  while HookInfo <> nil do
  begin
    NextHookInfo := HookInfo.Next;
    if HookInfo.Hook(Msg) then
      Exit;
    HookInfo := NextHookInfo;
  end;

  if (Control = nil) and (Msg.Msg = WM_DESTROY) then
  begin
    // Handle is being destroyed: remove all hooks on this window

    { This looks like a nice function candidate }
    with WndProcHook do
    begin
      I := FHookInfos.IndexOf(Self);
      if I >= 0 then
        FHookInfos.Delete(I);
    end;

    { We could unhook later (or never (?)) but now is a good time }
    UnhookControl;

    { Now must this object free _itself_, but that is kinda risky (?)

      Therefore I introduced TJvReleaser, if this is a silly idea please remove
      it :)
    }
    TJvReleaser.Instance.Release(Self);
  end;
end;

//=== TJvWindowHook ==========================================================

constructor TJvWindowHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
end;

destructor TJvWindowHook.Destroy;
begin
  Active := False;
  Control := nil;
  inherited Destroy;
end;

procedure TJvWindowHook.DefineProperties(Filer: TFiler);

  {$IFDEF WIN32}
  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := IsForm <> TJvWindowHook(Filer.Ancestor).IsForm
    else
      Result := IsForm;
  end;
  {$ENDIF}

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsForm', ReadForm, WriteForm,
    {$IFDEF WIN32} DoWrite {$ELSE} IsForm {$ENDIF});
end;

function TJvWindowHook.DoAfterMessage(var Msg: TMessage): Boolean;
begin
  Result := False;
  if Assigned(FAfterMessage) then
    FAfterMessage(Self, Msg, Result);
end;

function TJvWindowHook.DoBeforeMessage(var Msg: TMessage): Boolean;
begin
  Result := False;
  if Assigned(FBeforeMessage) then
    FBeforeMessage(Self, Msg, Result);
end;

procedure TJvWindowHook.HookControl;
begin
  SetActive(True);
end;

function TJvWindowHook.IsForm: Boolean;
begin
  Result := (Control <> nil) and ((Control = Owner) and (Owner is TCustomForm));
end;

procedure TJvWindowHook.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Control then
      Control := nil
        { Correct? }
    else
    if (Owner = AComponent) or (Owner = nil) then
      Control := nil;
  end;
end;

function TJvWindowHook.NotIsForm: Boolean;
begin
  { Correct? }
  Result := (Control <> nil) and not (Control is TCustomForm);
end;

procedure TJvWindowHook.ReadForm(Reader: TReader);
begin
  if Reader.ReadBoolean then
    if Owner is TCustomForm then
      Control := TControl(Owner);
end;

procedure TJvWindowHook.SetActive(Value: Boolean);
begin
  if FActive = Value then
    Exit;

  if not (csDesigning in ComponentState) then
  begin
    if Value then
    begin
      { Only register if assigned, to prevent unnecessarily overhead }
      if Assigned(FAfterMessage) then
        WndProcHook.RegisterWndProc(FControl, DoAfterMessage, hoAfterMsg);
      if Assigned(FBeforeMessage) then
        WndProcHook.RegisterWndProc(FControl, DoBeforeMessage, hoBeforeMsg);
    end
    else
    begin
      if Assigned(FAfterMessage) then
        WndProcHook.UnRegisterWndProc(FControl, DoAfterMessage, hoAfterMsg);
      if Assigned(FBeforeMessage) then
        WndProcHook.UnRegisterWndProc(FControl, DoBeforeMessage, hoBeforeMsg);
    end;
  end;
  FActive := Value;
end;

procedure TJvWindowHook.SetAfterMessage(const Value: TJvHookMessageEvent);
begin
  if Active and not (csDesigning in ComponentState) then
  begin
    { Only register if assigned, to prevent unnecessarily overhead }
    if Assigned(Value) and not Assigned(FAfterMessage) then
      WndProcHook.RegisterWndProc(FControl, DoAfterMessage, hoAfterMsg)
    else
      if not Assigned(Value) and Assigned(FAfterMessage) then
        WndProcHook.UnRegisterWndProc(FControl, DoAfterMessage, hoAfterMsg);
  end;
  FAfterMessage := Value;
end;

procedure TJvWindowHook.SetBeforeMessage(const Value: TJvHookMessageEvent);
begin
  if Active and not (csDesigning in ComponentState) then
  begin
    { Only register if assigned, to prevent unnecessarily overhead }
    if Assigned(Value) and not Assigned(FBeforeMessage) then
      WndProcHook.RegisterWndProc(FControl, DoBeforeMessage, hoBeforeMsg)
    else
      if not Assigned(Value) and Assigned(FBeforeMessage) then
        WndProcHook.UnRegisterWndProc(FControl, DoBeforeMessage, hoBeforeMsg);
  end;
  FBeforeMessage := Value;
end;

procedure TJvWindowHook.SetControl(Value: TControl);
var
  SavedActive: Boolean;
begin
  if Value <> Control then
  begin
    SavedActive := Active;
    Active := False;
    if FControl <> nil then
      FControl.RemoveFreeNotification(Self);

    FControl := Value;

    if FControl <> nil then
      FControl.FreeNotification(Self);

    Active := SavedActive;
  end;
end;

procedure TJvWindowHook.UnhookControl;
begin
  SetActive(False);
end;

procedure TJvWindowHook.WriteForm(Writer: TWriter);
begin
  Writer.WriteBoolean(IsForm);
end;

//=== TJvReleaser ============================================================

destructor TJvReleaser.Destroy;
begin
  if FHandle <> 0 then
    {$IFDEF COMPILER6_UP}
    Classes.DeallocateHWnd(FHandle);
    {$ELSE}
    DeallocateHWnd(FHandle);
    {$ENDIF}
end;

procedure TJvReleaser.CMRelease(var Msg: TMessage);
var
  Obj: TObject;
begin
  Obj := TObject(Msg.WParam);
  Obj.Free;
end;

procedure TJvReleaser.DefaultHandler(var Msg);
begin
  with TMessage(Msg) do
    if FHandle <> 0 then
      Result := CallWindowProc(@DefWindowProc, FHandle, Msg, wParam, lParam);
end;

function TJvReleaser.GetHandle: HWND;
begin
  if FHandle = 0 then
    {$IFDEF COMPILER6_UP}
    FHandle := Classes.AllocateHwnd(WndProc);
    {$ELSE}
    FHandle := AllocateHwnd(WndProc);
    {$ENDIF}
  Result := FHandle;
end;

class function TJvReleaser.Instance: TJvReleaser;
begin
  if GReleaser = nil then
    GReleaser := TJvReleaser.Create;
  Result := GReleaser;
end;

procedure TJvReleaser.Release(AObject: TObject);
begin
  PostMessage(Handle, CM_Release, Integer(AObject), 0);
end;

procedure TJvReleaser.WndProc(var Msg: TMessage);
begin
  try
    Dispatch(Msg);
  except
    {$IFDEF COMPILER6_UP}
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
    {$ELSE}
    Application.HandleException(Self);
    {$ENDIF}
  end;
end;

initialization

finalization
  GJvWndProcHook.Free;
  GReleaser.Free;
  
end.

