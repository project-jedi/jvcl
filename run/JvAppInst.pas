{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppInst.pas, released on 2003-10-07.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-10-07

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvAppInst;

interface

uses
  Windows, Messages, SysUtils, Forms, Classes, Controls,
  { Classes must be after Forms for Delphi 5 compatibility. }
  JclAppInst;

type
  TJvAppInstDataKind = TJclAppInstDataKind; // = Integer

  TInstanceChangeEvent = procedure(Sender: TObject; ProcessId: Cardinal) of object;
  TUserNotifyEvent = procedure(Sender: TObject; Param: Integer) of object;
  TDataAvailableEvent = procedure(Sender: TObject; Kind: TJvAppInstDataKind;
    Data: Pointer; Size: Integer) of object;
    { Data contains the date and is released when the function returns }
  TCmdLineReceivedEvent = procedure(Sender: TObject; CmdLine: TStrings) of object;

  { TJvAppInstance encapsulates the TJclAppInstance class. To set a
    UniqueAppIdGuidStr you must call JclAppInst.JclAppInstances in the
    initialization section of a unit or before the forms are created (OnCreate
    is too late).
    This class is not thread safe. }
  TJvAppInstances = class(TComponent)
  private
    FHandle: THandle;
    FOnInstanceCreated: TInstanceChangeEvent;
    FOnInstanceDestroyed: TInstanceChangeEvent;
    FOnUserNotify: TUserNotifyEvent;
    FOnDataAvailable: TDataAvailableEvent;
    FOnCmdLineReceived: TCmdLineReceivedEvent;
    FOnRejected: TNotifyEvent;
    FAutoActivate: Boolean;
    FMaxInstances: Integer;
    FActive: Boolean;
    FSendCmdLine: Boolean;
    function GetAppInstances: TJclAppInstances;
  protected
    procedure Loaded; override;
    procedure WndProc(var Msg: TMessage); virtual;
    procedure DoInstanceCreated(ProcessId: Cardinal); virtual;
    procedure DoInstanceDestroyed(ProcessId: Cardinal); virtual;
    procedure DoUserNotify(Param: Integer); virtual;
    procedure DoDataAvailable(Kind: TJvAppInstDataKind; Data: Pointer; Size: Integer); virtual;
    procedure DoCmdLineReceived(CmdLine: TStrings); virtual;
    procedure DoRejected; virtual;
    property Handle: THandle read FHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Check;
    procedure UserNotify(Param: Integer);
    function SendData(DataKind: TJclAppInstDataKind;  Data: Pointer; Size: Integer): Boolean;
    property AppInstances: TJclAppInstances read GetAppInstances;
  published
    property Active: Boolean read FActive write FActive default True;
    property AutoActivate: Boolean read FAutoActivate write FAutoActivate default True;
     { AutoActivateFirst: True means that the first instance is brought to front
       by the second process instance. }
    property MaxInstances: Integer read FMaxInstances write FMaxInstances default 1;
     { MaxInstances: 0 means no restriction }
    property SendCmdLine: Boolean read FSendCmdLine write FSendCmdLine default True;
     { SendCmdLine: True means that the second process instance sends it's
       CmdLine to the first instance before it terminates. }
    property OnInstanceCreated: TInstanceChangeEvent read FOnInstanceCreated write FOnInstanceCreated;
    property OnInstanceDestroyed: TInstanceChangeEvent read FOnInstanceDestroyed write FOnInstanceDestroyed;
    property OnUserNotify: TUserNotifyEvent read FOnUserNotify write FOnUserNotify;
    property OnDataAvailable: TDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
    property OnCmdLineReceived: TCmdLineReceivedEvent read FOnCmdLineReceived write FOnCmdLineReceived;
    property OnRejected: TNotifyEvent read FOnRejected write FOnRejected;
  end;


function AllocateHWndEx(Method: TWndMethod; const AClassName: string = ''): HWND;
// AllocateHWndEx works like Classes.AllocateHWnd but does not use any virtual memory pages
procedure DeallocateHWndEx(Wnd: HWND);
// DeallocateHWndEx works like Classes.DeallocateHWnd but does not use any virtual memory pages

implementation

const
  sAppInstancesWindowClassName = 'JvAppInstances_WindowClass'; // do not localize

//=== AllocateHWndEx =========================================================

const
  cUtilWindowExClass: TWndClass = (
    style: 0;
    lpfnWndProc: nil;
    cbClsExtra: 0;
    cbWndExtra: SizeOf(TMethod);
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindowEx');

function StdWndProc(Window: HWND; Message, WParam: WPARAM;
  LParam: LPARAM): LRESULT; stdcall;
var
  Msg: TMessage;
  WndProc: TWndMethod;
begin
  TMethod(WndProc).Code := Pointer(GetWindowLong(Window, 0));
  TMethod(WndProc).Data := Pointer(GetWindowLong(Window, 4));
  if Assigned(WndProc) then
  begin
    Msg.Msg := Message;
    Msg.WParam := WParam;
    Msg.LParam := LParam;
    Msg.Result := 0;
    WndProc(Msg);
    Result := Msg.Result;
  end
  else
    Result := DefWindowProc(Window, Message, WParam, LParam);
end;

function AllocateHWndEx(Method: TWndMethod; const AClassName: string = ''): HWND;
var
  TempClass: TWndClass;
  UtilWindowExClass: TWndClass;
  ClassRegistered: Boolean;
begin
  UtilWindowExClass := cUtilWindowExClass;
  UtilWindowExClass.hInstance := HInstance;
  UtilWindowExClass.lpfnWndProc := @DefWindowProc;
  if AClassName <> '' then
    UtilWindowExClass.lpszClassName := PChar(AClassName);

  ClassRegistered := GetClassInfo(HInstance, UtilWindowExClass.lpszClassName,
     TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(UtilWindowExClass.lpszClassName, HInstance);
    Windows.RegisterClass(UtilWindowExClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowExClass.lpszClassName,
    '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);

  if Assigned(Method) then
  begin
    SetWindowLong(Result, 0, Longint(TMethod(Method).Code));
    SetWindowLong(Result, SizeOf(TMethod(Method).Code), Longint(TMethod(Method).Data));
    SetWindowLong(Result, GWL_WNDPROC, Longint(@StdWndProc));
  end;
end;

procedure DeallocateHWndEx(Wnd: HWND);
begin
  DestroyWindow(Wnd);
end;

//=== TJvAppInstances ========================================================

var
  FirstJvAppInstance: Boolean;

constructor TJvAppInstances.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then
  begin
    FHandle := AllocateHWndEx(WndProc, sAppInstancesWindowClassName);
    if FirstJvAppInstance then
    begin
      FirstJvAppInstance := False;
      AppInstances.CheckInstance($FFFF); // increase shared instance count
    end;
  end;

  FActive := True;
  FMaxInstances := 1;
  FAutoActivate := True;
  FSendCmdLine := True;
end;

destructor TJvAppInstances.Destroy;
begin
  if not (csDesigning in ComponentState) then
    DeallocateHWndEx(FHandle);
  inherited Destroy;
end;

procedure TJvAppInstances.Check;
begin
  if Active and not (csDesigning in ComponentState) then
    if FMaxInstances > 0 then
      if AppInstances.InstanceCount > FMaxInstances then
      begin
        DoRejected;
        if FAutoActivate then
          AppInstances.SwitchTo(0);
        if FSendCmdLine then
          AppInstances.SendCmdLineParams(sAppInstancesWindowClassName, Handle);

       // terminate this process (Form.OnCreate is not executed yet)

        { DoneApplication destroys all formulars in the Forms unit's
          finalization section. At that moment the OnDestroy events are fired.
          To prevent this we set the Application variable to nil. Because
          KillInstance uses halt() to terminate this does not raise any access
          violation. }
        Application := nil;
        AppInstances.KillInstance;
      end;
end;

procedure TJvAppInstances.DoCmdLineReceived(CmdLine: TStrings);
begin
  if Assigned(FOnCmdLineReceived) then
    FOnCmdLineReceived(Self, CmdLine);
end;

procedure TJvAppInstances.DoDataAvailable(Kind: TJvAppInstDataKind;
  Data: Pointer; Size: Integer);
begin
  if Assigned(FOnDataAvailable) then
    FOnDataAvailable(Self, Kind, Data, Size);
end;

procedure TJvAppInstances.DoInstanceCreated(ProcessId: Cardinal);
begin
  if Assigned(FOnInstanceCreated) then
    FOnInstanceCreated(Self, ProcessId);
end;

procedure TJvAppInstances.DoInstanceDestroyed(ProcessId: Cardinal);
begin
  if Assigned(FOnInstanceDestroyed) then
    FOnInstanceDestroyed(Self, ProcessId);
end;

procedure TJvAppInstances.DoUserNotify(Param: Integer);
begin
  if Assigned(FOnUserNotify) then
    FOnUserNotify(Self, Param);
end;

procedure TJvAppInstances.DoRejected;
begin
  if Assigned(FOnRejected) then
    FOnRejected(Self);
end;

function TJvAppInstances.GetAppInstances: TJclAppInstances;
begin
  if csDesigning in ComponentState then
    Result := nil
  else
    Result := JclAppInstances; // create AppInstance
end;

procedure TJvAppInstances.Loaded;
begin
  inherited Loaded;
  Check;
end;

procedure TJvAppInstances.WndProc(var Msg: TMessage);
var
  Kind: TJvAppInstDataKind;
  Data: Pointer;
  Size: Integer;
  CmdLine: TStringList;
begin
  if Active then
  begin
    try
      if Msg.Msg = AppInstances.MessageID then
      begin
        case Msg.WParam of
          AI_INSTANCECREATED:
            if Cardinal(Msg.LParam) <> GetCurrentProcessId then
              DoInstanceCreated(Cardinal(Msg.LParam));
          AI_INSTANCEDESTROYED:
            DoInstanceDestroyed(Cardinal(Msg.LParam));
          AI_USERMSG:
            DoUserNotify(Msg.LParam);
        end;
      end
      else
      begin
        Kind := ReadMessageCheck(Msg, Handle);
        case Kind of
          AppInstDataKindNoData:
            ; // do nothing
          AppInstCmdLineDataKind:
            begin
              if Assigned(FOnCmdLineReceived) then
              begin
                CmdLine := TStringList.Create;
                try
                  ReadMessageStrings(Msg, CmdLine);
                  DoCmdLineReceived(CmdLine);
                finally
                  CmdLine.Free;
                end;
              end;
              Exit;
            end;
        else
          if Assigned(FOnDataAvailable) then
          begin
            ReadMessageData(Msg, Data, Size);
            try
              DoDataAvailable(Kind, Data, Size);
            finally
              FreeMem(Data);
            end;
          end;
          Exit;
        end;
      end;
    except
      on E: Exception do
        Application.ShowException(E);
    end;
  end;

  with Msg do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

procedure TJvAppInstances.UserNotify(Param: Integer);
begin
  AppInstances.UserNotify(Param);
end;

function TJvAppInstances.SendData(DataKind: TJclAppInstDataKind;
  Data: Pointer; Size: Integer): Boolean;
begin
  Result := AppInstances.SendData(sAppInstancesWindowClassName, DataKind, Data,
    Size, Handle);
end;

initialization
  FirstJvAppInstance := True;

end.
