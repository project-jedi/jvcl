{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxHook.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxHook;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, SysUtils, Classes, Controls, Forms, JvxConst{, JvxComponent};

type
  PClass = ^TClass;
  THookMessageEvent = procedure (Sender: TObject; var Msg: TMessage;
    var Handled: Boolean) of object;

  TJvxWindowHook = class(TComponent)
  private
    FActive: Boolean;
    FControl: TWinControl;
    FControlHook: TObject;
    FBeforeMessage: THookMessageEvent;
    FAfterMessage: THookMessageEvent;
    function GetWinControl: TWinControl;
    function GetHookHandle: HWnd;
    procedure SetActive(Value: Boolean);
    procedure SetWinControl(Value: TWinControl);
    function IsForm: Boolean;
    function NotIsForm: Boolean;
    function DoUnhookControl: Pointer;
    procedure ReadForm(Reader: TReader);
    procedure WriteForm(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAfterMessage(var Msg: TMessage; var Handled: Boolean); dynamic;
    procedure DoBeforeMessage(var Msg: TMessage; var Handled: Boolean); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HookControl;
    procedure UnhookControl;
    property HookWindow: HWnd read GetHookHandle;
  published
    property Active: Boolean read FActive write SetActive default True;
    property WinControl: TWinControl read GetWinControl write SetWinControl
      stored NotIsForm;
    property BeforeMessage: THookMessageEvent read FBeforeMessage write FBeforeMessage;
    property AfterMessage: THookMessageEvent read FAfterMessage write FAfterMessage;
  end;

function GetVirtualMethodAddress(AClass: TClass; AIndex: Integer): Pointer;
function SetVirtualMethodAddress(AClass: TClass; AIndex: Integer;
  NewAddress: Pointer): Pointer;
function FindVirtualMethodIndex(AClass: TClass; MethodAddr: Pointer): Integer;

implementation

type
  TJvxHack = class(TWinControl);
  THookOrder = (hoBeforeMsg, hoAfterMsg);
{$IFNDEF Delphi3_Up}
  TCustomForm = TForm;
{$ENDIF}

{ TJvxControlHook }

  TJvxControlHook = class(TObject)
  private
    FControl: TWinControl;
    FNewWndProc: Pointer;
    FPrevWndProc: Pointer;
    FList: TList;
    FDestroying: Boolean;
    procedure SetWinControl(Value: TWinControl);
    procedure HookWndProc(var AMsg: TMessage);
    procedure NotifyHooks(Order: THookOrder; var Msg: TMessage;
      var Handled: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure HookControl;
    procedure UnhookControl;
    procedure AddHook(AHook: TJvxWindowHook);
    procedure RemoveHook(AHook: TJvxWindowHook);
    property WinControl: TWinControl read FControl write SetWinControl;
  end;

{ TJvxHookList }

  TJvxHookList = class(TList)
  private
    FHandle: HWnd;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    function FindControlHook(AControl: TWinControl): TJvxControlHook;
    function GetControlHook(AControl: TWinControl): TJvxControlHook;
    property Handle: HWnd read FHandle;
  end;

var
  HookList: TJvxHookList;

function GetHookList: TJvxHookList;
begin
  if HookList = nil then HookList := TJvxHookList.Create;
  Result := HookList;
end;

procedure DropHookList; far;
begin
  HookList.Free;
  HookList := nil;
end;

{ TJvxControlHook }

constructor TJvxControlHook.Create;
begin
  inherited Create;
  FList := TList.Create;
  FNewWndProc := Classes.MakeObjectInstance(HookWndProc);
  FPrevWndProc := nil;
  FControl := nil;
end;

destructor TJvxControlHook.Destroy;
begin
  FDestroying := True;
  if Assigned(HookList) then
    if HookList.IndexOf(Self) >= 0 then HookList.Remove(Self);
  while FList.Count > 0 do RemoveHook(TJvxWindowHook(FList.Last));
  FControl := nil;
  FList.Free;
  Classes.FreeObjectInstance(FNewWndProc);
  FNewWndProc := nil;
  inherited Destroy;
end;

procedure TJvxControlHook.AddHook(AHook: TJvxWindowHook);
begin
  if FList.IndexOf(AHook) < 0 then begin
    FList.Add(AHook);
    AHook.FControlHook := Self;
    WinControl := AHook.FControl;
  end;
  HookControl;
end;

procedure TJvxControlHook.RemoveHook(AHook: TJvxWindowHook);
begin
  AHook.FControlHook := nil;
  FList.Remove(AHook);
  if FList.Count = 0 then UnhookControl;
end;

procedure TJvxControlHook.NotifyHooks(Order: THookOrder; var Msg: TMessage;
  var Handled: Boolean);
var
  I: Integer;
begin
  if (FList.Count > 0) and Assigned(FControl) and
    not (FDestroying or (csDestroying in FControl.ComponentState)) then
    for I := FList.Count - 1 downto 0 do begin
      try
        if Order = hoBeforeMsg then
          TJvxWindowHook(FList[I]).DoBeforeMessage(Msg, Handled)
        else if Order = hoAfterMsg then
          TJvxWindowHook(FList[I]).DoAfterMessage(Msg, Handled);
      except
        Application.HandleException(Self);
      end;
      if Handled then Break;
    end;
end;

procedure TJvxControlHook.HookControl;
var
  P: Pointer;
begin
  if Assigned(FControl) and not ((csDesigning in FControl.ComponentState) or
    (csDestroying in FControl.ComponentState) or FDestroying) then
  begin
    FControl.HandleNeeded;
    P := Pointer(GetWindowLong(FControl.Handle, GWL_WNDPROC));
    if (P <> FNewWndProc) then begin
      FPrevWndProc := P;
      SetWindowLong(FControl.Handle, GWL_WNDPROC, LongInt(FNewWndProc));
    end;
  end;
end;

procedure TJvxControlHook.UnhookControl;
begin
  if Assigned(FControl) then begin
    if Assigned(FPrevWndProc) and FControl.HandleAllocated and
    (Pointer(GetWindowLong(FControl.Handle, GWL_WNDPROC)) = FNewWndProc) then
      SetWindowLong(FControl.Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
  end;
  FPrevWndProc := nil;
end;

procedure TJvxControlHook.HookWndProc(var AMsg: TMessage);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FControl) then begin
    if (AMsg.Msg <> WM_QUIT) then NotifyHooks(hoBeforeMsg, AMsg, Handled);
    with AMsg do begin
      if (not Handled) or (Msg = WM_DESTROY) then
        try
          if Assigned(FPrevWndProc) then
            Result := CallWindowProc(FPrevWndProc, FControl.Handle, Msg,
              WParam, LParam)
          else
            Result := CallWindowProc(TJvxHack(FControl).DefWndProc,
              FControl.Handle, Msg, WParam, LParam);
        finally
          NotifyHooks(hoAfterMsg, AMsg, Handled);
        end;
      if Msg = WM_DESTROY then begin
        UnhookControl;
        if Assigned(HookList) and not (FDestroying or
          (csDestroying in FControl.ComponentState)) then
          PostMessage(HookList.FHandle, CM_RECREATEWINDOW, 0, Longint(Self));
      end;
    end;
  end;
end;

procedure TJvxControlHook.SetWinControl(Value: TWinControl);
begin
  if Value <> FControl then begin
    UnhookControl;
    FControl := Value;
    if FList.Count > 0 then HookControl;
  end;
end;

{ TJvxHookList }

constructor TJvxHookList.Create;
begin
  inherited Create;
  FHandle := Classes.AllocateHWnd(WndProc);
end;

destructor TJvxHookList.Destroy;
begin
  while Count > 0 do TJvxControlHook(Last).Free;
  Classes.DeallocateHWnd(FHandle);
  inherited Destroy;
end;

procedure TJvxHookList.WndProc(var Msg: TMessage);
var
  Hook: TJvxControlHook;
begin
  try
    with Msg do begin
      if Msg = CM_RECREATEWINDOW then begin
        Hook := TJvxControlHook(LParam);
        if (Hook <> nil) and (IndexOf(Hook) >= 0) then
          Hook.HookControl;
      end
      else if Msg = CM_DESTROYHOOK then begin
        Hook := TJvxControlHook(LParam);
        if Assigned(Hook) and (IndexOf(Hook) >= 0) and
          (Hook.FList.Count = 0) then Hook.Free;
      end
      else Result := DefWindowProc(FHandle, Msg, wParam, lParam);
    end;
  except
    Application.HandleException(Self);
  end;
end;

function TJvxHookList.FindControlHook(AControl: TWinControl): TJvxControlHook;
var
  I: Integer;
begin
  if Assigned(AControl) then
    for I := 0 to Count - 1 do
      if (TJvxControlHook(Items[I]).WinControl = AControl) then begin
        Result := TJvxControlHook(Items[I]);
        Exit;
      end;
  Result := nil;
end;

function TJvxHookList.GetControlHook(AControl: TWinControl): TJvxControlHook;
begin
  Result := FindControlHook(AControl);
  if Result = nil then begin
    Result := TJvxControlHook.Create;
    try
      Add(Result);
      Result.WinControl := AControl;
    except
      Result.Free;
      raise;
    end;
  end;
end;

{ TJvxWindowHook }

constructor TJvxWindowHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
end;

destructor TJvxWindowHook.Destroy;
begin
  Active := False;
  WinControl := nil;
  inherited Destroy;
end;

procedure TJvxWindowHook.SetActive(Value: Boolean);
begin
  if FActive <> Value then
    if Value then HookControl else UnhookControl;
end;

function TJvxWindowHook.GetHookHandle: HWnd;
begin
  if Assigned(HookList) then Result := HookList.Handle
  else
{$IFDEF WIN32}
    Result := INVALID_HANDLE_VALUE;
{$ELSE}
    Result := 0;
{$ENDIF}
end;

procedure TJvxWindowHook.HookControl;
begin
  if Assigned(FControl) and not (csDestroying in ComponentState) then
    GetHookList.GetControlHook(FControl).AddHook(Self);
  FActive := True;
end;

function TJvxWindowHook.DoUnhookControl: Pointer;
begin
  Result := FControlHook;
  if Result <> nil then TJvxControlHook(Result).RemoveHook(Self);
  FActive := False;
end;

procedure TJvxWindowHook.UnhookControl;
begin
  DoUnhookControl;
  FActive := False;
end;

function TJvxWindowHook.NotIsForm: Boolean;
begin
  Result := (WinControl <> nil) and not (WinControl is TCustomForm);
end;

function TJvxWindowHook.IsForm: Boolean;
begin
  Result := (WinControl <> nil) and ((WinControl = Owner) and
    (Owner is TCustomForm));
end;

procedure TJvxWindowHook.ReadForm(Reader: TReader);
begin
  if Reader.ReadBoolean then
    if Owner is TCustomForm then WinControl := TWinControl(Owner);
end;

procedure TJvxWindowHook.WriteForm(Writer: TWriter);
begin
  Writer.WriteBoolean(IsForm);
end;

procedure TJvxWindowHook.DefineProperties(Filer: TFiler);
{$IFDEF WIN32}
  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := IsForm <> TJvxWindowHook(Filer.Ancestor).IsForm
    else Result := IsForm;
  end;
{$ENDIF}
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsForm', ReadForm, WriteForm,
    {$IFDEF WIN32} DoWrite {$ELSE} IsForm {$ENDIF});
end;

function TJvxWindowHook.GetWinControl: TWinControl;
begin
  if Assigned(FControlHook) then Result := TJvxControlHook(FControlHook).WinControl
  else Result := FControl;
end;

procedure TJvxWindowHook.DoAfterMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if Assigned(FAfterMessage) then FAfterMessage(Self, Msg, Handled);
end;

procedure TJvxWindowHook.DoBeforeMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if Assigned(FBeforeMessage) then FBeforeMessage(Self, Msg, Handled);
end;

procedure TJvxWindowHook.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = WinControl) and (Operation = opRemove) then
    WinControl := nil
  else if (Operation = opRemove) and ((Owner = AComponent) or
    (Owner = nil)) then WinControl := nil;
end;

procedure TJvxWindowHook.SetWinControl(Value: TWinControl);
var
  SaveActive: Boolean;
  Hook: TJvxControlHook;
begin
  if Value <> WinControl then begin
    SaveActive := FActive;
    Hook := TJvxControlHook(DoUnhookControl);
    FControl := Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
    if Assigned(Hook) and (Hook.FList.Count = 0) and Assigned(HookList) then
      PostMessage(HookList.Handle, CM_DESTROYHOOK, 0, Longint(Hook));
    if SaveActive then HookControl;
  end;
end;

{ SetVirtualMethodAddress procedure. Destroy destructor has index 0,
  first user defined virtual method has index 1. }

type
  PPointer = ^Pointer;

function GetVirtualMethodAddress(AClass: TClass; AIndex: Integer): Pointer;
var
  Table: PPointer;
begin
  Table := PPointer(AClass);
  Inc(Table, AIndex - 1);
  Result := Table^;
end;

function SetVirtualMethodAddress(AClass: TClass; AIndex: Integer;
  NewAddress: Pointer): Pointer;
{$IFDEF WIN32}
const
  PageSize = SizeOf(Pointer);
{$ENDIF}
var
  Table: PPointer;
{$IFDEF WIN32}
  SaveFlag: DWORD;
{$ELSE}
  Block: Pointer;
{$ENDIF}
begin
  Table := PPointer(AClass);
  Inc(Table, AIndex - 1);
  Result := Table^;
{$IFDEF WIN32}
  if VirtualProtect(Table, PageSize, PAGE_EXECUTE_READWRITE, @SaveFlag) then
  try
    Table^ := NewAddress;
  finally
    VirtualProtect(Table, PageSize, SaveFlag, @SaveFlag);
  end;
{$ELSE}
  PtrRec(Block).Ofs := PtrRec(Table).Ofs;
  PtrRec(Block).Seg := AllocCSToDSAlias(PtrRec(Table).Seg);
  try
    PPointer(Block)^ := NewAddress;
  finally
    FreeSelector(PtrRec(Block).Seg);
  end;
{$ENDIF}
end;

function FindVirtualMethodIndex(AClass: TClass; MethodAddr: Pointer): Integer;
begin
  Result := 0;
  repeat
    Inc(Result);
  until (GetVirtualMethodAddress(AClass, Result) = MethodAddr);
end;

initialization
  HookList := nil;
{$IFDEF WIN32}
finalization
  DropHookList;
{$ELSE}
  AddExitProc(DropHookList);
{$ENDIF}
end.
