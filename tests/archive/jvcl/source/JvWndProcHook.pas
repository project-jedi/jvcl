unit JvWndProcHook;

interface

uses
  SysUtils, Messages, Classes, Controls;

type
  { Changes:

    * Renamed local vars that were named i,j to some more meaningfull name :)
    * Added some helper functions
    * Changed the linked list a bit; removed FRoot; added FLast
    * Possibility to specify whether the hook must be before or after the
      handling of the message by the original owner
    * Hook can say he has handled the message; thus preventing the original
      control from handling the message
      - these last 2 seem necessairy for some components
    * Added the component TJvWindowHook from JvHook
  }

  TJvControlHook = function(var Message: TMessage): Boolean of object;
  TJvHookMessageEvent = procedure(Sender: TObject; var Msg: TMessage;
    var Handled: Boolean) of object;

  TJvHookOrder = (hoBeforeMsg, hoAfterMsg);

  TJvWindowHook = class(TComponent)
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
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HookControl;
    procedure UnHookControl;
  published
    property Active: Boolean read FActive write SetActive default True;
    property Control: TControl read FControl write SetControl
      stored NotIsForm;
    property BeforeMessage: TJvHookMessageEvent read FBeforeMessage write
      SetBeforeMessage;
    property AfterMessage: TJvHookMessageEvent read FAfterMessage write
      SetAfterMessage;
  end;

function RegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean;
function UnRegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean;

implementation

uses
  Forms;

type
  PJvHookInfo = ^TJvHookInfo;
  TJvHookInfo = record
    Hook: TJvControlHook;
    Next: PJvHookInfo;
  end;

  TJvHookInfos = class
  private
    FFirst: array[TJvHookOrder] of PJvHookInfo;
    FLast: array[TJvHookOrder] of PJvHookInfo;
    FControl: TControl;
    FOldWndProc: TWndMethod;
    FHooked: Boolean;
  protected
    procedure WindowProc(var Message: TMessage);
    procedure HookControl;
    procedure UnHookControl;
  public
    constructor Create(AControl: TControl);
    destructor Destroy; override;
    procedure Add(const Order: TJvHookOrder; Hook: TJvControlHook);
    procedure Delete(const Order: TJvHookOrder; Hook: TJvControlHook);
    property Control: TControl read FControl;
  end;

  TJvWndProcHook = class(TComponent)
  private
    FHookInfos: TList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function IndexOf(AControl: TControl): Integer;
    function Find(AControl: TControl): TJvHookInfos;
  public
    destructor Destroy; override;
    function RegisterWndProc(AControl: TControl; Hook: TJvControlHook;
      const Order: TJvHookOrder): Boolean;
    function UnRegisterWndProc(AControl: TControl; Hook: TJvControlHook;
      const Order: TJvHookOrder): Boolean;
  end;

var
  FJvWndProcHook: TJvWndProcHook = nil;

function WndProcHook: TJvWndProcHook;
begin
  if FJvWndProcHook = nil then
    FJvWndProcHook := TJvWndProcHook.Create(nil);
  Result := FJvWndProcHook;
end;

function RegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean;
begin
  Result := WndProcHook.RegisterWndProc(AControl, Hook, Order);
end;

function UnRegisterWndProcHook(AControl: TControl; Hook: TJvControlHook;
  const Order: TJvHookOrder): Boolean;
begin
  Result := WndProcHook.UnRegisterWndProc(AControl, Hook, Order);
end;

{ TJvWndProcHook }

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

destructor TJvWndProcHook.Destroy;
var
  I: Integer;
begin
  for I := 0 to FHookInfos.Count - 1 do
    TJvHookInfos(FHookInfos[I]).Free;
  FHookInfos.Free;
  inherited;
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
  inherited;
  if (Operation = opRemove) and (FHookInfos <> nil) and (AComponent is TControl)
    then
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

{ TJvHookInfos }

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

constructor TJvHookInfos.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
  FFirst[hoBeforeMsg] := nil;
  FFirst[hoAfterMsg] := nil;
  FLast[hoBeforeMsg] := nil;
  FLast[hoAfterMsg] := nil;
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
  inherited;
end;

procedure TJvHookInfos.HookControl;
begin
  if FHooked then
    Exit;
  FOldWndProc := FControl.WindowProc;
  FControl.WindowProc := WindowProc;
  FHooked := True;
end;

procedure TJvHookInfos.UnHookControl;
begin
  if not FHooked then
    Exit;
  FControl.WindowProc := FOldWndProc;
  FHooked := False;
end;

procedure TJvHookInfos.WindowProc(var Message: TMessage);
var
  HookInfo: PJvHookInfo;
begin
  { An object can now report for every possible message that he has
    handled that message, thus preventing the original control from
    handling the message; this is probably not a good idea in the case
    of WM_DESTROY, WM_CLOSE etc. But that's the users responsibility,
    I think }

  Message.Result := 0;

  HookInfo := FFirst[hoBeforeMsg];
  while HookInfo <> nil do
  begin
    if HookInfo.Hook(Message) then
      Exit;
    HookInfo := HookInfo.Next;
  end;

  { Maybe only exit here (before the original control handles the message),
    thus enabling all hooks to respond to the message? Otherwise if you
    have 2 components of the same class, that hook a control, then only 1 will
    get the message }

  FOldWndProc(Message);

  HookInfo := FFirst[hoAfterMsg];
  while HookInfo <> nil do
  begin
    if HookInfo.Hook(Message) then
      Exit;
    HookInfo := HookInfo.Next;
  end;
end;

{ TJvWindowHook }

constructor TJvWindowHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
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
{$IFDEF WIN32}DoWrite{$ELSE}IsForm{$ENDIF});
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
    else if (Owner = AComponent) or (Owner = nil) then
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
    else if not Assigned(Value) and Assigned(FAfterMessage) then
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
    else if not Assigned(Value) and Assigned(FBeforeMessage) then
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

initialization

finalization
  FJvWndProcHook.Free;
end.

