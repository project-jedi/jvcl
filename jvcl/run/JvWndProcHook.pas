{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWndProcHook.PAS, released on 2002-11-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
Remko Bonte <remkobonte att myrealbox dott com>

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  * (rb) object naming could be improved, for example
      TJvWndProcHook             -> TJvHookController
      TJvWndProcHook.FHookInfos  -> TJvHookController.Items
      TJvHookInfos               -> TJvHookItem, TJvHookInfo, TJvHook
      TJvHookInfo                -> TJvHookData
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvWndProcHook;

interface

uses
  Windows, Messages, SysUtils, Controls, Forms, Classes,
  JvComponent, JvFinalize;

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

function RegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean; overload;
function RegisterWndProcHook(AHandle: HWND; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean; overload;
function UnRegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean; overload;
function UnRegisterWndProcHook(AHandle: HWND; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean; overload;
procedure ReleaseObj(AObject: TObject);

implementation

const
  sUnitName = 'JvWndProcHook';

type
  PJvHookInfo = ^TJvHookInfo;
  TJvHookInfo = record
    Hook: TJvControlHook;
    Next: PJvHookInfo;
  end;

  PHookInfoList = ^THookInfoList;
  THookInfoList = array[0..MaxInt div 4 - 1] of PJvHookInfo;

  TJvWndProcHook = class;

  TJvHookInfos = class(TObject)
  private
    FFirst: array[TJvHookOrder] of PJvHookInfo;
    FLast: array[TJvHookOrder] of PJvHookInfo;
    { FStack is filled with HookInfos that are being processed in WindowProc
      procedures. On entrance of the WindowProc the size increases, on exit it
      decreases. Thus when a message is send inside a hook handler, the stack
      size increases.

      We use a stack to be able to register and unregister hooks inside hook
      handlers, see \dev\DUnit for some examples.

      The odd members in the stack are hoBeforeMsg hooks, the even members in
      the list are hoAftermsg hooks
    }
    FStack: PHookInfoList;
    FStackCapacity: Integer;
    FStackCount: Integer;

    FHandle: HWND;
    FControl: TControl;
    FControlDestroyed: Boolean;
    FOldWndProc: TWndMethod;
    FHooked: Boolean;
    FController: TJvWndProcHook;
    procedure SetController(const Value: TJvWndProcHook);
  protected
    procedure WindowProc(var Msg: TMessage);
    procedure HookControl;
    procedure UnHookControl;

    procedure IncDepth;
    procedure DecDepth;
  public
    constructor Create(AControl: TControl); overload;
    constructor Create(AHandle: HWND); overload;
    destructor Destroy; override;
    procedure Add(const Order: TJvHookOrder; Hook: TJvControlHook);
    procedure Delete(const Order: TJvHookOrder; Hook: TJvControlHook);
    procedure ControlDestroyed;
    property Control: TControl read FControl;
    { Prevent calls to WndProcHook by using property Controller;
      TJvHookInfos may live longer than WndProcHook }
    property Controller: TJvWndProcHook read FController write SetController;
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

    procedure Remove(AHookInfos: TJvHookInfos);
    procedure Add(AHookInfos: TJvHookInfos);
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

  TJvReleaser = class
  private
    FHandle: HWND;
    FReleasing: TList;
    function GetHandle: HWND;
    procedure CMRelease(var Msg: TMessage); message CM_RELEASE;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DefaultHandler(var Msg); override;
    class function Instance: TJvReleaser;
    procedure Release(AObject: TObject);
    property Handle: HWND read GetHandle;
  end;

var
  GJvWndProcHook: TJvWndProcHook = nil;
  GReleaser: TJvReleaser = nil;

function WndProcHook: TJvWndProcHook;
begin
  if GJvWndProcHook = nil then
  begin
    GJvWndProcHook := TJvWndProcHook.Create(nil);
    AddFinalizeObjectNil(sUnitName, TObject(GJvWndProcHook));
  end;
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

procedure ReleaseObj(AObject: TObject);
begin
  TJvReleaser.Instance.Release(AObject);
end;

//=== TJvWndProcHook =========================================================

procedure TJvWndProcHook.Add(AHookInfos: TJvHookInfos);
var
  I: Integer;
begin
  I := FHookInfos.IndexOf(AHookInfos);
  if I < 0 then
    FHookInfos.Add(AHookInfos);
end;

destructor TJvWndProcHook.Destroy;
begin
  if FHookInfos <> nil then
  begin
    while FHookInfos.Count > 0 do
      { If you free a hook info, it will remove itself from the list }
      TJvHookInfos(FHookInfos[0]).Free;

    FHookInfos.Free;
  end;
  inherited Destroy;
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

function TJvWndProcHook.IndexOf(AControl: TControl): Integer;
begin
  Result := 0;
  while (Result < FHookInfos.Count) and
    (TJvHookInfos(FHookInfos[Result]).Control <> AControl) do
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
      { Be careful because the TJvHookInfos object might be in it's
        WindowProc procedure, for example when hooking a form and handling
        a CM_RELEASE message. The TJvHookInfos object can't be destroyed then.

        General rule must be that only TJvHookInfos can destroy itself, and
        remove it from the TJvWndProcHook.FHookInfos list.
      }
      TJvHookInfos(FHookInfos[I]).ControlDestroyed;
  end;
end;

function TJvWndProcHook.RegisterWndProc(AControl: TControl;
  Hook: TJvControlHook; const Order: TJvHookOrder): Boolean;
var
  HookInfos: TJvHookInfos;
begin
  Result := False;
  if not Assigned(AControl) or (csDestroying in AControl.ComponentState)
    or not Assigned(Hook) then
    Exit;

  if FHookInfos = nil then
    FHookInfos := TList.Create;

  // find the control:
  HookInfos := Find(AControl);
  if not Assigned(HookInfos) then
  begin
    HookInfos := TJvHookInfos.Create(AControl);
    HookInfos.Controller := Self;
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
    HookInfos.Controller := Self;
  end;
  HookInfos.Add(Order, Hook);

  Result := True;
end;

procedure TJvWndProcHook.Remove(AHookInfos: TJvHookInfos);
var
  I: Integer;
begin
  I := FHookInfos.IndexOf(AHookInfos);
  if I >= 0 then
    FHookInfos.Delete(I);
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

function TJvWndProcHook.UnRegisterWndProc(AControl: TControl;
  Hook: TJvControlHook; const Order: TJvHookOrder): Boolean;
var
  HookInfos: TJvHookInfos;
begin
  Result := False;
  if not Assigned(AControl) or not Assigned(Hook) or not Assigned(FHookInfos) then
    Exit;
  // find the control:
  HookInfos := Find(AControl);
  Result := Assigned(HookInfos);
  if Result then
    // Maybe delete HookInfos if HookInfos.FFirst.. = nil?
    HookInfos.Delete(Order, Hook);
end;

//=== TJvHookInfos ===========================================================

procedure TJvHookInfos.Add(const Order: TJvHookOrder; Hook: TJvControlHook);
var
  HookInfo: PJvHookInfo;
  I: Integer;
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

  { Update the stack }
  if Order = hoBeforeMsg then
    I := 0
  else
    I := 1;
  while I < FStackCount * 2 do
  begin
    if FStack[I] = nil then
      FStack[I] := HookInfo;
    Inc(I, 2);
  end;

  HookControl;
end;

procedure TJvHookInfos.ControlDestroyed;
begin
  if FControlDestroyed then
    Exit;

  { This procedure is called when we get notified that the control we are hooking
    is destroyed. We can get this notification from TJvWindowHook.Notification
    or in TJvHookInfos.WindowProc.

    Problem is that the control might be destroyed when we are in the
    TJvHookInfos.WindowProc. This can occur for example with the CM_RELEASE
    message for a TCustomForm. In this case we have to be extra careful to not
    call destroyed components via HookInfo.Hook(Msg) etc. Also in that case
    we can't free the TJvHookInfos yet, thus we use ReleaseObj.
  }

  FControlDestroyed := True;
  FOldWndProc := nil;

  { Remove this TJvHookInfos object from the HookInfo list of Controller }
  Controller := nil;
  ReleaseObj(Self);
end;

constructor TJvHookInfos.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
  FillChar(FFirst, SizeOf(FFirst), 0);
  FillChar(FLast, SizeOf(FLast), 0);
  //FillChar(FStack, SizeOf(FStack), 0);
  //FillChar(FStackCapacity, SizeOf(FStackCapacity), 0);
  //FillChar(FStackCount, SizeOf(FStackCount), 0);
end;

constructor TJvHookInfos.Create(AHandle: HWND);
begin
  inherited Create;
  FHandle := AHandle;
  FillChar(FFirst, SizeOf(FFirst), 0);
  FillChar(FLast, SizeOf(FLast), 0);
  //FillChar(FStack, SizeOf(FStack), 0);
  //FillChar(FStackCapacity, SizeOf(FStackCapacity), 0);
  //FillChar(FStackCount, SizeOf(FStackCount), 0);
end;

procedure TJvHookInfos.DecDepth;
begin
  if FStackCount > 0 then
    Dec(FStackCount);
end;

procedure TJvHookInfos.Delete(const Order: TJvHookOrder; Hook: TJvControlHook);
var
  HookInfo: PJvHookInfo;
  PrevHookInfo: PJvHookInfo;
  I: Integer;
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

  { Update the stack }
  if Order = hoBeforeMsg then
    I := 0
  else
    I := 1;
  while I < FStackCount * 2 do
  begin
    if FStack[I] = HookInfo then
      FStack[I] := HookInfo.Next;
    Inc(I, 2);
  end;

  Dispose(HookInfo);

  if (FFirst[hoBeforeMsg] = nil) and (FFirst[hoAfterMsg] = nil) then
    { Could also call ReleaseObj(Self). Now this object stays in memory until
      the Control it was hooking will be destroyed. }

    UnHookControl;
end;

destructor TJvHookInfos.Destroy;
var
  HookInfo: PJvHookInfo;
  Order: TJvHookOrder;
begin
  { Remove this TJvHookInfos object from the list of Controller,
    Controller might already be set to nil (in ControlDestroyed) }
  Controller := nil;

  UnHookControl;

  for Order := Low(TJvHookOrder) to High(TJvHookOrder) do
    while FFirst[Order] <> nil do
    begin
      HookInfo := FFirst[Order];
      FFirst[Order] := HookInfo.Next;
      Dispose(HookInfo);
    end;
  FreeMem(FStack);

  inherited Destroy;
end;

procedure TJvHookInfos.HookControl;
begin
  if FHooked or FControlDestroyed then
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
    SetWindowLong(FHandle, GWL_WNDPROC, Integer(MakeObjectInstance(WindowProc)));
    FHooked := True;
  end;
end;

procedure TJvHookInfos.IncDepth;
begin
  if FStackCount >= FStackCapacity then
  begin
    { Upsize the stack }
    Inc(FStackCapacity);
    FStackCapacity := FStackCapacity * 2;
    ReallocMem(FStack, 2 * FStackCapacity * SizeOf(Pointer));
  end;
  Inc(FStackCount);
end;

procedure TJvHookInfos.SetController(const Value: TJvWndProcHook);
begin
  if Value <> FController then
  begin
    if Assigned(FController) then
      FController.Remove(Self);

    FController := Value;

    if Assigned(FController) then
      FController.Add(Self);
  end;
end;

procedure TJvHookInfos.UnHookControl;
var
  Ptr: Pointer;
begin
  if not FHooked or FControlDestroyed then
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
    FreeObjectInstance(Ptr);
  end;
end;

procedure TJvHookInfos.WindowProc(var Msg: TMessage);
var
  TmpHookInfo: PJvHookInfo;
  { FStack[Index] is used to travel through the hook infos;
    FStack[Index] points to the current hook info (and might be nil)
    Note that the address of FStack may change due to ReallocMem calls in
    IncDepth; thus we can't assign FStack[Index] to a local var.
  }
  Index: Integer;
begin
  { An object can now report for every possible message that he has
    handled that message, thus preventing the original control from
    handling the message; this is probably not a good idea in the case
    of WM_DESTROY, WM_CLOSE etc. But that's the users responsibility,
    I think }

  Msg.Result := 0;

  IncDepth;
  // (rb) Don't know what the performance impact of a try..finally is.
  try
    { The even members in the stack are hoBeforeMsg hooks }
    Index := 2 * (FStackCount - 1);
    FStack[Index] := FFirst[hoBeforeMsg];
    while Assigned(FStack[Index]) do
    begin
      { We retrieve the next hook info *before* the call to Hook(), because,
        see (I) }
      TmpHookInfo := FStack[Index];
      FStack[Index] := FStack[Index].Next;
      if TmpHookInfo.Hook(Msg) or FControlDestroyed then
        Exit;
      { FStack[Index] may now be changed because of register/unregister calls
        inside HookInfo.Hook(Msg). }
    end;

    { Maybe only exit here (before the original control handles the message),
      thus enabling all hooks to respond to the message? Otherwise if you
      have 2 components of the same class, that hook a control, then only 1 will
      get the message }

    if TMethod(FOldWndProc).Data <> nil then
      FOldWndProc(Msg)
    else
    if TMethod(FOldWndProc).Code <> nil then
      Msg.Result := CallWindowProc(TMethod(FOldWndProc).Code, Handle, Msg.Msg,
        Msg.WParam, Msg.LParam);

    if FControlDestroyed then
      Exit;

    { The odd members in the list are hoAftermsg hooks }
    Index := 2 * FStackCount - 1;
    FStack[Index] := FFirst[hoAfterMsg];
    while Assigned(FStack[Index]) do
    begin
      TmpHookInfo := FStack[Index];
      FStack[Index] := FStack[Index].Next;
      if TmpHookInfo.Hook(Msg) or FControlDestroyed then
        Exit;
    end;
  finally
    DecDepth;
    if (Control = nil) and (Msg.Msg = WM_DESTROY) then
      // Handle is being destroyed: remove all hooks on this window
      ControlDestroyed;
  end;

  { (I)
         HookInfos before                                HookInfos after
         call to Hook()                                  call to Hook()

        |----------|  If FStack[Index] point to A        |----------|
     -->| hook A   |  (arrow) and hook A deletes itself  | hook B   |<--
        |----------|  then after the call to Hook,       |----------|
        | hook B   |  FStack[Index] points to B. If we   | hook C   |
        |----------|  then call Next, FStack[Index]      |----------|
        | hook C   |  points to C (should be B)
        |----------|
      }
end;

//=== TJvWindowHook ==========================================================

constructor TJvWindowHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
end;

procedure TJvWindowHook.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := IsForm <> TJvWindowHook(Filer.Ancestor).IsForm
    else
      Result := IsForm;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsForm', ReadForm, WriteForm, DoWrite);
end;

destructor TJvWindowHook.Destroy;
begin
  Active := False;
  Control := nil;
  inherited Destroy;
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

    if Assigned(Value) and (csDestroying in Value.ComponentState) then
      { (rb) this should not happen in calls made by Jv components }
      FControl := nil
    else
    begin
      FControl := Value;

      if FControl <> nil then
        FControl.FreeNotification(Self);

      Active := SavedActive;
    end;
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

procedure TJvReleaser.CMRelease(var Msg: TMessage);
var
  Obj: TObject;
  Index: Integer;
begin
  Index := FReleasing.IndexOf(Pointer(Msg.WParam));
  if Index >= 0 then
    FReleasing.Delete(Index);

  Obj := TObject(Msg.WParam);
  Obj.Free;
end;

constructor TJvReleaser.Create;
begin
  inherited Create;
  FReleasing := TList.Create;
end;

procedure TJvReleaser.DefaultHandler(var Msg);
begin
  with TMessage(Msg) do
    if FHandle <> 0 then
      Result := CallWindowProc(@DefWindowProc, FHandle, Msg, WParam, LParam);
end;

destructor TJvReleaser.Destroy;
begin
  while FReleasing.Count > 0 do
  begin
    TObject(FReleasing[0]).Free;
    FReleasing.Delete(0);
  end;

  FReleasing.Free;
  if FHandle <> 0 then
    DeallocateHWnd(FHandle);

  inherited Destroy;
end;

function TJvReleaser.GetHandle: HWND;
begin
  if FHandle = 0 then
    FHandle := AllocateHWnd(WndProc);
  Result := FHandle;
end;

class function TJvReleaser.Instance: TJvReleaser;
begin
  if GReleaser = nil then
  begin
    GReleaser := TJvReleaser.Create;
  { Don't call FreeAndNil for GReleaser, it's (hypothetically) possible that
    objects need access to the GReleaser var (via call to ReleaseObj) during
    GReleaser.Destroy }
    AddFinalizeObjectNil(sUnitName, TObject(GReleaser));
  end;
  Result := GReleaser;
end;

procedure TJvReleaser.Release(AObject: TObject);
begin
  { Make sure we're not already releasing this object }
  if FReleasing.IndexOf(AObject) < 0 then
  begin
    FReleasing.Add(AObject);
    PostMessage(Handle, CM_RELEASE, Integer(AObject), 0);
  end;
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
    {$ENDIF COMPILER6_UP}
  end;
end;

initialization

finalization
  FinalizeUnit(sUnitName);
  
end.

