{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimerLst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTimerLst;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes{, JvComponent};

const
  DefaultInterval = 1000;
  HInvalidEvent = -1;

type
  TAllTimersEvent = procedure(Sender: TObject; Handle: Longint) of object;

  TJvTimerEvent = class;

  TJvTimerList = class(TComponent)
  private
    FEvents: TList;
    FWndHandle: hWnd;
    FActive: Boolean;
    FInterval: Longint;
    FSequence: Longint;
    FStartInterval: Longint;
    FOnFinish: TNotifyEvent;
    FOnTimers: TAllTimersEvent;
    procedure CalculateInterval(StartTicks: Longint);
    function CreateNewEvent: TJvTimerEvent;
    function GetCount: Integer;
    function GetEnabledCount: Integer;
    function ProcessEvents: Boolean;
    procedure RemoveItem(Item: TJvTimerEvent);
    procedure SetActive(Value: Boolean);
    procedure SetEvents(StartTicks: Longint);
    procedure Sort;
    procedure TimerWndProc(var Msg: TMessage);
    procedure UpdateTimer;
  protected
{$IFDEF WIN32}
    procedure GetChildren(Proc: TGetChildProc {$IFDEF Delphi3_Up};
      Root: TComponent {$ENDIF}); override;
{$ELSE}
    procedure WriteComponents(Writer: TWriter); override;
{$ENDIF WIN32}
    procedure DoTimer(Event: TJvTimerEvent); dynamic;
    function NextHandle: Longint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(AOnTimer: TNotifyEvent; AInterval: Longint;
      ACycled: Boolean): Longint; virtual;
    function AddItem(Item: TJvTimerEvent): Longint;
    procedure Clear;
    procedure Delete(AHandle: Longint); virtual;
    procedure Activate;
    procedure Deactivate;
    function ItemByHandle(AHandle: Longint): TJvTimerEvent;
    function ItemIndexByHandle(AHandle: Longint): Integer;
    property Count: Integer read GetCount;
    property EnabledCount: Integer read GetEnabledCount;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Events: TList read FEvents;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnTimers: TAllTimersEvent read FOnTimers write FOnTimers;
  end;

  TJvTimerEvent = class(TComponent)
  private
    FCycled: Boolean;
    FEnabled: Boolean;
    FExecCount: Integer;
    FHandle: Longint;
    FInterval: Longint;
    FLastExecute: Longint;
    FParentList: TJvTimerList;
    FRepeatCount: Integer;
    FOnTimer: TNotifyEvent;
    function GetAsSeconds: Cardinal;
    procedure SetAsSeconds(Value: Cardinal);
    procedure SetRepeatCount(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Longint);
{$IFNDEF WIN32}
    procedure SetParentList(Value: TJvTimerList);
{$ENDIF WIN32}
  protected
{$IFDEF WIN32}
    procedure SetParentComponent(Value: TComponent); override;
{$ELSE}
    procedure ReadState(Reader: TReader); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
{$IFDEF WIN32}
    function GetParentComponent: TComponent; override;
{$ENDIF}
    property AsSeconds: Cardinal read GetAsSeconds write SetAsSeconds;
    property Handle: Longint read FHandle;
    property ExecCount: Integer read FExecCount;
    property TimerList: TJvTimerList read FParentList;
  published
    property Cycled: Boolean read FCycled write FCycled default True;
    property RepeatCount: Integer read FRepeatCount write SetRepeatCount default 0;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Longint read FInterval write SetInterval default DefaultInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

uses Consts, Controls, Forms, SysUtils, JvVCLUtils, JvMaxMin;

const
  MinInterval = 100; { 0.1 sec }
{$IFDEF Delphi4_Up}
  MaxTimerInterval: Longint = High(Longint);
{$ELSE}
  MaxTimerInterval: Longint = High(Cardinal);
{$ENDIF}
{$IFNDEF WIN32}
  INVALID_HANDLE_VALUE = 0;
{$ENDIF}
  Registered: Boolean = False;

{ TJvTimerEvent }

constructor TJvTimerEvent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentList := nil;
  FCycled := True;
  FRepeatCount := 0;
  FEnabled := True;
  FExecCount := 0;
  FInterval := DefaultInterval;
  FLastExecute := GetTickCount;
  FHandle := HInvalidEvent;
end;

destructor TJvTimerEvent.Destroy;
begin
  FOnTimer := nil;
  inherited Destroy;
end;

{$IFNDEF WIN32}
procedure TJvTimerEvent.SetParentList(Value: TJvTimerList);
begin
  if FParentList <> nil then FParentList.RemoveItem(Self);
  if Value <> nil then Value.AddItem(Self);
end;
{$ENDIF}

function TJvTimerEvent.HasParent: Boolean;
begin
  Result := True;
end;

{$IFDEF WIN32}

function TJvTimerEvent.GetParentComponent: TComponent;
begin
  Result := FParentList;
end;

procedure TJvTimerEvent.SetParentComponent(Value: TComponent);
begin
  if FParentList <> nil then FParentList.RemoveItem(Self);
  if (Value <> nil) and (Value is TJvTimerList) then
    TJvTimerList(Value).AddItem(Self);
end;

{$ELSE}

procedure TJvTimerEvent.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TJvTimerList then
    SetParentList(TJvTimerList(Reader.Parent));
end;

{$ENDIF WIN32}

procedure TJvTimerEvent.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    if FEnabled then begin
      FExecCount := 0;
      FLastExecute := GetTickCount;
      if FParentList <> nil then
        with FParentList do begin
          CalculateInterval(GetTickCount);
          UpdateTimer;
        end;
    end;
  end;
end;

procedure TJvTimerEvent.SetInterval(Value: Longint);
begin
  if Value <> FInterval then begin
    FInterval := Value;
    if FParentList <> nil then
      with FParentList do begin
        CalculateInterval(GetTickCount);
        UpdateTimer;
      end;
  end;
end;

procedure TJvTimerEvent.SetRepeatCount(Value: Integer);
begin
  if FRepeatCount <> Value then begin
    Value := Max(Value, Integer(not FCycled));
    if not (csDesigning in ComponentState) then
      if FEnabled and (Value <= FExecCount) then Enabled := False;
    FRepeatCount := Value;
  end;
end;

function TJvTimerEvent.GetAsSeconds: Cardinal;
begin
  Result := Interval div 1000;
end;

procedure TJvTimerEvent.SetAsSeconds(Value: Cardinal);
begin
  Interval := Value * 1000;
end;

{ TJvTimerList }

constructor TJvTimerList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEvents := TList.Create;
  FWndHandle := INVALID_HANDLE_VALUE;
  FSequence := 0;
  FStartInterval := 0;
  Deactivate;
  if not Registered then begin
    RegisterClasses([TJvTimerEvent]);
    Registered := True;
  end;
end;

destructor TJvTimerList.Destroy;
begin
  OnFinish := nil;
  OnTimers := nil;
  Deactivate;
  Clear;
  FEvents.Free;
  inherited Destroy;
end;

procedure TJvTimerList.Activate;
begin
  Active := True;
end;

procedure TJvTimerList.Deactivate;
begin
  if not (csLoading in ComponentState) then Active := False;
end;

procedure TJvTimerList.SetEvents(StartTicks: Longint);
var
  I: Integer;
begin
  for I := 0 to FEvents.Count - 1 do
    if TJvTimerEvent(FEvents[I]).Enabled then
      TJvTimerEvent(FEvents[I]).FLastExecute := StartTicks;
end;

procedure TJvTimerList.SetActive(Value: Boolean);
var
  StartTicks: Longint;
begin
  if FActive <> Value then begin
    if not (csDesigning in ComponentState) then begin
      if Value then begin
        FWndHandle := {$IFDEF Delphi6_Up}Classes.{$ENDIF}AllocateHWnd(TimerWndProc);
        StartTicks := GetTickCount;
        SetEvents(StartTicks);
        CalculateInterval(StartTicks);
        Sort;
        UpdateTimer;
      end
      else begin
        KillTimer(FWndHandle, 1);
        {$IFDEF Delphi6_Up}Classes.{$ENDIF}DeallocateHWnd(FWndHandle);
        FWndHandle := INVALID_HANDLE_VALUE;
        if Assigned(FOnFinish) then FOnFinish(Self);
      end;
      FStartInterval := 0;
    end;
    FActive := Value;
  end;
end;

{$IFDEF WIN32}
procedure TJvTimerList.GetChildren(Proc: TGetChildProc {$IFDEF Delphi3_Up};
  Root: TComponent {$ENDIF});
var
  I: Integer;
begin
  inherited GetChildren(Proc {$IFDEF Delphi3_Up}, Root {$ENDIF});
  for I := 0 to FEvents.Count - 1 do
    Proc(TJvTimerEvent(FEvents[I]));
end;
{$ELSE}
procedure TJvTimerList.WriteComponents(Writer: TWriter);
var
  I: Integer;
  Item: TJvTimerEvent;
begin
  inherited WriteComponents(Writer);
  for I := 0 to FEvents.Count - 1 do begin
    Item := TJvTimerEvent(FEvents[I]);
    if Item.Owner = Writer.Root then Writer.WriteComponent(Item);
  end;
end;
{$ENDIF WIN32}

procedure TJvTimerList.Sort;
var
  I: Integer;
  ExitLoop: Boolean;
begin
  if not (csDesigning in ComponentState) then
    repeat
      ExitLoop := True;
      for I := 0 to Count - 2 do begin
        if TJvTimerEvent(FEvents[I]).Interval > TJvTimerEvent(FEvents[I + 1]).Interval then
        begin
          FEvents.Exchange(I, I + 1);
          ExitLoop := False;
        end;
      end;
    until ExitLoop;
end;

function TJvTimerList.NextHandle: Longint;
begin
  Inc(FSequence);
  Result := FSequence;
end;

function TJvTimerList.CreateNewEvent: TJvTimerEvent;
begin
  Result := TJvTimerEvent.Create(Owner);
end;

function TJvTimerList.AddItem(Item: TJvTimerEvent): Longint;
begin
  if FEvents.Add(Item) >= 0 then begin
    Item.FHandle := NextHandle;
    Item.FParentList := Self;
    Result := Item.FHandle;
    CalculateInterval(GetTickCount);
    Sort;
    UpdateTimer;
  end
  else Result := HInvalidEvent; { invalid handle }
end;

{ Create a new timer event and returns a handle }
function TJvTimerList.Add(AOnTimer: TNotifyEvent; AInterval: Longint;
  ACycled: Boolean): Longint;
var
  T: TJvTimerEvent;
begin
  T := CreateNewEvent;
  if (FEvents.Add(T) >= 0) then begin
    with T do begin
      OnTimer := AOnTimer;
      FParentList := Self;
      FHandle := NextHandle;
      Interval := AInterval;
      Cycled := ACycled;
      Result := FHandle;
    end;
    CalculateInterval(GetTickCount);
    Sort;
    UpdateTimer;
  end
  else begin
    T.Free;
    Result := HInvalidEvent; { invalid handle }
  end;
end;

function TJvTimerList.ItemIndexByHandle(AHandle: Longint): Integer;
begin
  for Result := 0 to FEvents.Count - 1 do
    if TJvTimerEvent(FEvents[Result]).Handle = AHandle then Exit;
  Result := -1;
end;

function TJvTimerList.ItemByHandle(AHandle: Longint): TJvTimerEvent;
var
  I: Integer;
begin
  I := ItemIndexByHandle(AHandle);
  if I >= 0 then Result := TJvTimerEvent(FEvents[I])
  else Result := nil;
end;

procedure TJvTimerList.Delete(AHandle: Longint);
var
  I: Integer;
  Item: TJvTimerEvent;
begin
  I := ItemIndexByHandle(AHandle);
  if I >= 0 then begin
    Item := TJvTimerEvent(FEvents[I]);
    RemoveItem(Item);
    if not (csDestroying in Item.ComponentState) then Item.Free;
    if Active then begin
      CalculateInterval(GetTickCount);
      UpdateTimer;
    end;
  end;
end;

function TJvTimerList.GetCount: Integer;
begin
  Result := FEvents.Count;
end;

function TJvTimerList.GetEnabledCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if TJvTimerEvent(FEvents[I]).Enabled then Inc(Result);
end;

procedure TJvTimerList.RemoveItem(Item: TJvTimerEvent);
begin
  FEvents.Remove(Item);
  Item.FParentList := nil;
end;

procedure TJvTimerList.Clear;
var
  I: Integer;
  Item: TJvTimerEvent;
begin
  for I := FEvents.Count - 1 downto 0 do begin
    Item := TJvTimerEvent(FEvents[I]);
    RemoveItem(Item);
    if not (csDestroying in Item.ComponentState) then Item.Free;
  end;
end;

procedure TJvTimerList.DoTimer(Event: TJvTimerEvent);
begin
  with Event do 
    if Assigned(FOnTimer) then FOnTimer(Event);
  if Assigned(FOnTimers) then FOnTimers(Self, Event.Handle);
end;

function TJvTimerList.ProcessEvents: Boolean;
var
  I: Integer;
  Item: TJvTimerEvent;
  StartTicks: Longint;
begin
  Result := False;
  if not (csDesigning in ComponentState) then begin
    StartTicks := GetTickCount;
    for I := Count - 1 downto 0 do begin
      Item := TJvTimerEvent(FEvents[I]);
      if (Item <> nil) and Item.Enabled then
        with Item do
          if (StartTicks - FLastExecute) >= (Interval - (MinInterval div 2)) then
          begin
            FLastExecute := StartTicks;
            Inc(FExecCount);
            Enabled := not ((not Cycled) and (FExecCount >= RepeatCount));
            if not Enabled then Result := True;
            DoTimer(Item);
          end;
    end;
  end;
end;

procedure TJvTimerList.TimerWndProc(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then begin
    with Msg do
      if Msg = WM_TIMER then
        try
          if (not (csDesigning in ComponentState)) and
            (FStartInterval = 0) and Active then 
          begin
            if ProcessEvents then begin
              if EnabledCount = 0 then Deactivate
              else begin
                CalculateInterval(GetTickCount);
                UpdateTimer;
              end;
            end;
          end else
            UpdateTimer;
        except
          Application.HandleException(Self);
        end
      else Result := DefWindowProc(FWndHandle, Msg, wParam, lParam);
  end;
end;

procedure TJvTimerList.CalculateInterval(StartTicks: Longint);
var
  I: Integer;
  ExitLoop: Boolean;
begin
  if not (csDesigning in ComponentState) then begin
    if Count = 0 then FInterval := 0
    else begin
      FStartInterval := 0;
      FInterval := MaxLongInt;
      for I := 0 to Count - 1 do
        with TJvTimerEvent(FEvents[I]) do
          if Enabled and (Interval > 0) then begin
            if Interval < Self.FInterval then Self.FInterval := Interval;
            if Self.FInterval > (Interval - (StartTicks - FLastExecute)) then
              Self.FInterval := (Interval - (StartTicks - FLastExecute));
          end;
      if FInterval < MinInterval then FInterval := MinInterval;
      if FInterval = MaxLongint then FInterval := 0
      else begin
        repeat
          ExitLoop := True;
          for I := 0 to Count - 1 do
            with TJvTimerEvent(FEvents[I]) do
              if (Interval mod Self.FInterval) <> 0 then begin
                Dec(Self.FInterval, Interval mod Self.FInterval);
                ExitLoop := False;
                Break;
              end;
        until ExitLoop or (FInterval <= MinInterval);
        if FInterval < MinInterval then FInterval := MinInterval;
      end;
    end;
  end;
end;

procedure TJvTimerList.UpdateTimer;
var
  FTimerInterval: Cardinal;
begin
  if not (csDesigning in ComponentState) then begin
    if FInterval <= MaxTimerInterval then FTimerInterval := FInterval
    else
      if (FInterval - FStartInterval) <= MaxTimerInterval then begin
        FTimerInterval := Cardinal(FInterval - FStartInterval);
        FStartInterval := 0;
      end
      else begin
        FTimerInterval := MaxTimerInterval;
        FStartInterval := FStartInterval + MaxTimerInterval;
      end;
    if not (csDesigning in ComponentState) and (FWndHandle <> INVALID_HANDLE_VALUE) then
    begin
      KillTimer(FWndHandle, 1);
      if EnabledCount = 0 then Deactivate
      else if FInterval > 0 then
        if SetTimer(FWndHandle, 1, FTimerInterval, nil) = 0 then begin
          Deactivate;
          raise EOutOfResources.Create(ResStr(SNoTimers));
        end;
    end;
  end;
end;

end.
