{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimerList.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Contributor(s):
- (p3) Rewritten to use TCollectionItem instead of TComponent for TJvTimerItem
  Change this in your own code:
  * CreateNewEvent -> Events.Add
  * AddItem -> Events.Add and then read the Item.Handle property
  * NextHandle -> Events.NextHandle
  * Delete() -> Events.DeleteByHandle()
  * IndexFromHandle -> Events.IndexFromHandle
  * ItemIndexByHandle -> Events.ItemIndexByHandle
  * Sort -> Events.Sort
  * EnabledCount -> Events.EnabledCount
  Additionally, if you cast Events[Index] to TComponent somewhere, you will have to
  change/remove it
  NOTE
    If you are using this component, the saved values in the dfm won't work. You can
    set them up again after loading the project, but you can also open your dfm
    in Notepad (assuming you have saved it as text, which you should), load the
    project into Delphi, ignore all warnings and then copy and paste from notepad
    to the Collection Editor for the Events property.

Known Issues:

-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQTimerList;

interface

uses
  Windows, Messages, SysUtils, Classes;

const
  DefaultInterval = 1000;

type
  TAllTimersEvent = procedure(Sender: TObject; Handle: Longint) of object;

  TJvTimerEvent = class;
  TJvTimerList = class; 

  // (rom) used THandle where needed
  TJvTimerEvents = class(TOwnedCollection)
  private
    FInterval: Longint;
    FStartInterval: Longint;
    FSequence: Longint;
    FParent: TJvTimerList;
    function GetEnabledCount: Integer;
    function GetItem(Index: Integer): TJvTimerEvent;
    procedure SetItem(Index: Integer; const Value: TJvTimerEvent);
  protected
    procedure CalculateInterval(StartTicks: Longint);
    procedure UpdateEvents(StartTicks: Longint);
    function ProcessEvents: Boolean;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);  override;  
  public
    constructor Create(AOwner: TPersistent);
    procedure Activate;
    function Add: TJvTimerEvent;
    procedure Deactivate;
    procedure DeleteByHandle(AHandle: THandle); virtual;
    function ItemByHandle(AHandle: THandle): TJvTimerEvent;
    function ItemIndexByHandle(AHandle: THandle): Integer;
    function IndexOfName(const AName:string):integer;
    function ItemByName(const AName:string): TJvTimerEvent;
    function NextHandle: THandle;
    procedure Sort;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvTimerEvent read GetItem write SetItem; default;
    property EnabledCount: Integer read GetEnabledCount;
  end;

  TJvTimerEvent = class(TCollectionItem)
  private
    FCycled: Boolean;
    FEnabled: Boolean;
    FExecCount: Integer;
    FHandle: THandle;
    FInterval: Longint;
    FLastExecute: Longint;
    FParentList: TJvTimerList;
    FRepeatCount: Integer;
    FOnTimer: TNotifyEvent;
    FName: string;
    function GetAsSeconds: Cardinal;
    procedure SetAsSeconds(Value: Cardinal);
    procedure SetRepeatCount(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Longint);
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property AsSeconds: Cardinal read GetAsSeconds write SetAsSeconds;
    property Handle: THandle read FHandle;
    property ExecCount: Integer read FExecCount;
    property TimerList: TJvTimerList read FParentList;
    procedure Assign(Source: TPersistent); override;
  published
    property Name:string read FName write FName;
    property Cycled: Boolean read FCycled write FCycled default True;
    property RepeatCount: Integer read FRepeatCount write SetRepeatCount default 0;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Longint read FInterval write SetInterval default DefaultInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

  TJvTimerList = class(TComponent)
  private
    FEvents: TJvTimerEvents;
    FWndHandle: HWND;
    FOnFinish: TNotifyEvent;
    FOnTimers: TAllTimersEvent;
    FActive: Boolean;
    FSorted: boolean;
    procedure TimerWndProc(var Msg: TMessage);
    procedure UpdateTimer;
    procedure SetEvents(const Value: TJvTimerEvents);
    procedure SetActive(Value: Boolean);
    procedure SetSorted(const Value: boolean);
  protected
    procedure DoTimer(Event: TJvTimerEvent); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(AOnTimer: TNotifyEvent; AInterval: Longint;
      ACycled: Boolean): THandle; virtual;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Events: TJvTimerEvents read FEvents write SetEvents;
    // NB! Setting sorted to true means that the index of the Events are changed!!!
    property Sorted:boolean read FSorted write SetSorted default False;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnTimers: TAllTimersEvent read FOnTimers write FOnTimers;
  end;

implementation

uses
  QConsts, QForms, // for Application.HandleException
  Math,
  JvQJVCLUtils, JvQResources, JvQTypes;

const
  MinInterval = 100; { 0.1 sec }
  MaxTimerInterval: Longint = High(Longint);

//=== TJvTimerEvent ==========================================================

constructor TJvTimerEvent.Create(ACollection: TCollection);
begin
  FHandle := INVALID_HANDLE_VALUE;
  inherited Create(ACollection);
  FCycled := True;
  FRepeatCount := 0;
  FEnabled := True;
  FExecCount := 0;
  FInterval := DefaultInterval;
  FLastExecute := GetTickCount;
end;

destructor TJvTimerEvent.Destroy;
begin
  FOnTimer := nil;
  inherited Destroy;
end;

procedure TJvTimerEvent.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      FExecCount := 0;
      FLastExecute := GetTickCount;
      if FParentList <> nil then
        with FParentList do
        begin
          Events.CalculateInterval(GetTickCount);
          UpdateTimer;
          Events.Activate;
        end;
    end;
  end;
end;

procedure TJvTimerEvent.SetInterval(Value: Longint);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    if FParentList <> nil then
      with FParentList do
      begin
        Events.CalculateInterval(GetTickCount);
        UpdateTimer;
      end;
  end;
end;

procedure TJvTimerEvent.SetRepeatCount(Value: Integer);
begin
  if FRepeatCount <> Value then
  begin
    Value := Max(Value, Integer(not FCycled));
    if not (csDesigning in FParentList.ComponentState) then
      if FEnabled and (Value <= FExecCount) then
        Enabled := False;
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

procedure TJvTimerEvent.Assign(Source: TPersistent);
begin
  if Source is TJvTimerEvent then
  begin
    if Source <> Self then
    begin
      Cycled := TJvTimerEvent(Source).Cycled;
      Enabled := TJvTimerEvent(Source).Enabled;
      Interval := TJvTimerEvent(Source).Interval;
      Name := TJvTimerEvent(Source).Name;
      RepeatCount := TJvTimerEvent(Source).RepeatCount;
    end;
  end
  else
    inherited Assign(Source);
end;

//=== TJvTimerList ===========================================================

constructor TJvTimerList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEvents := TJvTimerEvents.Create(Self);
  FWndHandle := INVALID_HANDLE_VALUE;
  Events.Deactivate;
end;

destructor TJvTimerList.Destroy;
begin
  OnFinish := nil;
  OnTimers := nil;
  Events.Deactivate;
  Events.Clear;
  FEvents.Free;
  inherited Destroy;
end;

{ Create a new timer event and returns a handle }

function TJvTimerList.Add(AOnTimer: TNotifyEvent; AInterval: Longint;
  ACycled: Boolean): THandle;
var
  T: TJvTimerEvent;
begin
  T := Events.Add;
  T.FParentList := Self;
  with T do
  begin
    OnTimer := AOnTimer;
    FParentList := Self;
    FHandle := Events.NextHandle;
    Interval := AInterval;
    Cycled := ACycled;
    Result := FHandle;
  end;
  Events.CalculateInterval(GetTickCount);
  if Sorted then
    Events.Sort;
  UpdateTimer;
end;

procedure TJvTimerList.TimerWndProc(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    with Msg do
      if Msg = WM_TIMER then
      try
        if (not (csDesigning in ComponentState)) and
          (Events.FStartInterval = 0) and Active then
        begin
          if Events.ProcessEvents then
          begin
            if Events.EnabledCount = 0 then
              Events.Deactivate
            else
            begin
              Events.CalculateInterval(GetTickCount);
              UpdateTimer;
            end;
          end;
        end
        else
          UpdateTimer;
      except  
        ApplicationHandleException(Self); 
      end
      else
        Result := DefWindowProc(FWndHandle, Msg, WParam, LParam);
  end;
end;

procedure TJvTimerList.UpdateTimer;
var
  TimerInterval: Cardinal;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Events.FInterval <= MaxTimerInterval then
      TimerInterval := Events.FInterval
    else
    if (Events.FInterval - Events.FStartInterval) <= MaxTimerInterval then
    begin
      TimerInterval := Cardinal(Events.FInterval - Events.FStartInterval);
      Events.FStartInterval := 0;
    end
    else
    begin
      TimerInterval := MaxTimerInterval;
      Events.FStartInterval := Events.FStartInterval + MaxTimerInterval;
    end;
    if not (csDesigning in ComponentState) and (FWndHandle <> INVALID_HANDLE_VALUE) then
    begin
      KillTimer(FWndHandle, 1);
      if Events.EnabledCount = 0 then
        Events.Deactivate
      else
      if Events.FInterval > 0 then
        if SetTimer(FWndHandle, 1, TimerInterval, nil) = 0 then
        begin
          Events.Deactivate;
          raise EOutOfResources.CreateRes(@SNoTimers);
        end;
    end;
  end;
end;

procedure TJvTimerList.SetEvents(const Value: TJvTimerEvents);
begin
  FEvents.Assign(Value);
end;

procedure TJvTimerList.SetActive(Value: Boolean);
var
  StartTicks: Longint;
begin
  if FActive <> Value then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Value then
      begin
        FWndHandle := AllocateHWndEx(TimerWndProc);
        StartTicks := GetTickCount;
        Events.UpdateEvents(StartTicks);
        Events.CalculateInterval(StartTicks);
        if Sorted then
          Events.Sort;
        UpdateTimer;
      end
      else
      begin
        KillTimer(FWndHandle, 1);
        DeallocateHWndEx(FWndHandle);
        FWndHandle := INVALID_HANDLE_VALUE;
        if Assigned(FOnFinish) then
          FOnFinish(Self);
      end;
      Events.FStartInterval := 0;
    end;
    FActive := Value;
  end;
end;

procedure TJvTimerList.DoTimer(Event: TJvTimerEvent);
begin
  with Event do
    if Assigned(FOnTimer) then
      FOnTimer(Event);
  if Assigned(FOnTimers) then
    FOnTimers(Self, Event.Handle);
end;

//===TJvTimerEvents ==========================================================

constructor TJvTimerEvents.Create(AOwner: TPersistent);
begin
  if not (AOwner is TJvTimerList) then
    raise EJVCLException.CreateRes(@RsEOwnerMustBeTJvTimerList);
  inherited Create(AOwner, TJvTimerEvent);
  FParent := TJvTimerList(AOwner);
end;



procedure TJvTimerEvents.Activate;
begin
  FParent.Active := True;
end;

function TJvTimerEvents.Add: TJvTimerEvent;
begin
  Result := TJvTimerEvent(inherited Add); 
end;

procedure TJvTimerEvents.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvTimerEvents then
  begin
    Clear;
    for I := 0 to TJvTimerEvents(Source).Count - 1 do
      Add.Assign(TJvTimerEvents(Source).Items[I]);
  end
  else
    inherited Assign(Source);
end;

procedure TJvTimerEvents.CalculateInterval(StartTicks: Integer);
var
  I: Integer;
  ExitLoop: Boolean;
begin
  if not (csDesigning in (Owner as TJvTimerList).ComponentState) then
  begin
    if Count = 0 then
      FInterval := 0
    else
    begin
      FStartInterval := 0;
      FInterval := MaxLongInt;
      for I := 0 to Count - 1 do
        with Items[I] do
          if Enabled and (Interval > 0) then
          begin
            if Interval < Self.FInterval then
              Self.FInterval := Interval;
            if Self.FInterval > (Interval - (StartTicks - FLastExecute)) then
              Self.FInterval := (Interval - (StartTicks - FLastExecute));
          end;
      if FInterval < MinInterval then
        FInterval := MinInterval;
      if FInterval = MaxLongint then
        FInterval := 0
      else
      begin
        repeat
          ExitLoop := True;
          for I := 0 to Count - 1 do
            with Items[I] do
              if (Interval mod Self.FInterval) <> 0 then
              begin
                Dec(Self.FInterval, Interval mod Self.FInterval);
                ExitLoop := False;
                Break;
              end;
        until ExitLoop or (FInterval <= MinInterval);
        if FInterval < MinInterval then
          FInterval := MinInterval;
      end;
    end;
  end;
end;

procedure TJvTimerEvents.Deactivate;
begin
  if not (csLoading in FParent.ComponentState) then
    FParent.Active := False;
end;

procedure TJvTimerEvents.DeleteByHandle(AHandle: THandle);
var
  I: Integer;
begin
  I := ItemIndexByHandle(AHandle);
  if I >= 0 then
    Delete(I);
  if FParent.Active then
  begin
    CalculateInterval(GetTickCount);
    FParent.UpdateTimer;
  end;
end;

function TJvTimerEvents.GetEnabledCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Enabled then
      Inc(Result);
end;

function TJvTimerEvents.GetItem(Index: Integer): TJvTimerEvent;
begin
  Result := TJvTimerEvent(inherited Items[Index]);
end;

function TJvTimerEvents.ItemByHandle(AHandle: THandle): TJvTimerEvent;
var
  I: Integer;
begin
  I := ItemIndexByHandle(AHandle);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TJvTimerEvents.ItemIndexByHandle(AHandle: THandle): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Handle = AHandle then
      Exit;
  Result := -1;
end;

function TJvTimerEvents.NextHandle: THandle;
begin
  Inc(FSequence);
  Result := FSequence;
end;

procedure TJvTimerEvents.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin 
  inherited Notify(Item, Action); 
  if Action = cnAdded then
    with TJvTimerEvent(Item) do
    begin
      FParentList := FParent;
      FHandle := NextHandle;
      FParentList := Self.FParent;
      CalculateInterval(GetTickCount);
      if FParent.Sorted then
        Sort;
      FParent.UpdateTimer;
    end;
end;

function TJvTimerEvents.ProcessEvents: Boolean;
var
  I: Integer;
  Item: TJvTimerEvent;
  StartTicks: Longint;
begin
  Result := False;
  if not (csDesigning in (Owner as TJvTimerList).ComponentState) then
  begin
    StartTicks := GetTickCount;
    for I := Count - 1 downto 0 do
    begin
      Item := Items[I];
      if (Item <> nil) and Item.Enabled then
        with Item do
          if (StartTicks - FLastExecute) >= (Interval - (MinInterval div 2)) then
          begin
            FLastExecute := StartTicks;
            Inc(FExecCount);
            Enabled := not ((not Cycled) and (FExecCount >= RepeatCount));
            if not Enabled then
              Result := True;
            FParent.DoTimer(Item);
          end;
    end;
  end;
end;

procedure TJvTimerEvents.SetItem(Index: Integer; const Value: TJvTimerEvent);
begin
  inherited Items[Index] := Value;
end;

procedure TJvTimerEvents.Sort;
var
  I: Integer;
  ExitLoop: Boolean;
begin
  if not (csDesigning in (Owner as TJvTimerList).ComponentState) then
    repeat
      ExitLoop := True;
      for I := 0 to Count - 2 do
      begin
        if Items[I].Interval > Items[I + 1].Interval then
        begin
          Items[i].Index := i + 1;
//          Items[i+1].Index := i;
          ExitLoop := False;
        end;
      end;
    until ExitLoop;
end;

procedure TJvTimerEvents.UpdateEvents(StartTicks: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Enabled then
      Items[I].FLastExecute := StartTicks;
end;

function TJvTimerEvent.GetDisplayName: String;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TJvTimerEvents.IndexOfName(const AName: string): integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiSameText(AName, Items[Result].Name) then
      Exit;
  Result := -1;
end;

function TJvTimerEvents.ItemByName(const AName: string): TJvTimerEvent;
var
  I: Integer;
begin
  I := IndexOfName(AName);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

procedure TJvTimerList.SetSorted(const Value: boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    if FSorted then Events.Sort;
  end;
end;

end.

