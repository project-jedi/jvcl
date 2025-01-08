{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppEvent.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Hofi
  Andreas Hausladen

Changes:
2004-10-07:
  * Added by Hofi
      TJvAppEvents
        property CancelDispatch
          gives a chance to break event dispatching in a particular event handler.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppEvent;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  SysUtils, Classes, Controls, Graphics, Forms, ActnList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.Types,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}  
  JvTypes, JvComponentBase;

const
  DefHintColor = clInfoBk;
  DefHintPause = 500;
  DefHintShortPause = DefHintPause div 10;
  DefHintHidePause = DefHintPause * 5;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvAppEvents = class(TJvComponent)
  private
    FChained: Boolean;
    FHintColor: TColor;
    FHintPause: Integer;
    FShowHint: Boolean;
    FUpdateFormatSettings: Boolean;
    FCancelDispatch: Boolean;
    FHintShortPause: Integer;
    FHintHidePause: Integer;
    FShowMainForm: Boolean;
    FHintShortCuts: Boolean;
    FMouseDragImmediate: Boolean;
    FMouseDragThreshold: Integer;
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnShortCut: TShortCutEvent;
    FUpdateMetricSettings: Boolean;
    FBiDiMode: TBiDiMode;
    FBiDiKeyboard: string;
    FNonBiDiKeyboard: string;
    FOnPaintIcon: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnMessage: TMessageEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShowHint: TShowHintEvent;
    {$IFDEF COMPILER7_UP}
    FOnModalBegin: TNotifyEvent;
    FOnModalEnd: TNotifyEvent;
    {$ENDIF COMPILER7_UP}
    FOnSettingsChanged: TNotifyEvent;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    procedure UpdateAppProps;
    function GetHintColor: TColor;
    function GetHintPause: Integer;
    function GetShowHint: Boolean;
    procedure SetHintColor(Value: TColor);
    procedure SetHintPause(Value: Integer);
    procedure SetShowHint(Value: Boolean);
    function GetUpdateFormatSettings: Boolean;
    procedure SetUpdateFormatSettings(Value: Boolean);
    function GetHintShortPause: Integer;
    function GetHintHidePause: Integer;
    function GetShowMainForm: Boolean;
    procedure SetHintShortPause(Value: Integer);
    procedure SetHintHidePause(Value: Integer);
    procedure SetShowMainForm(Value: Boolean);
    function GetHintShortCuts: Boolean;
    procedure SetHintShortCuts(Value: Boolean);
    function GetMouseDragImmediate: Boolean;
    function GetMouseDragThreshold: Integer;
    procedure SetMouseDragImmediate(Value: Boolean);
    procedure SetMouseDragThreshold(Value: Integer);
    function GetUpdateMetricSettings: Boolean;
    procedure SetUpdateMetricSettings(Value: Boolean);
    function GetBiDiMode: TBiDiMode;
    procedure SetBiDiMode(Value: TBiDiMode);
    function GetBiDiKeyboard: string;
    function GetNonBiDiKeyboard: string;
    procedure SetBiDiKeyboard(const Value: string);
    procedure SetNonBiDiKeyboard(const Value: string);
    procedure SetOnException(const Value: TExceptionEvent);
  protected
    procedure Loaded; override;
    procedure PaintIcon; virtual;
    procedure SettingsChanged; dynamic;
    function MessageHook(var Msg: TMessage): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelDispatch;
  published
    property Chained: Boolean read FChained write FChained default True;
    property HintColor: TColor read GetHintColor write SetHintColor default DefHintColor;
    property HintPause: Integer read GetHintPause write SetHintPause default DefHintPause;
    property ShowHint: Boolean read GetShowHint write SetShowHint default True;
    property UpdateFormatSettings: Boolean read GetUpdateFormatSettings write SetUpdateFormatSettings default True;
    property HintShortPause: Integer read GetHintShortPause write SetHintShortPause default DefHintShortPause;
    property HintHidePause: Integer read GetHintHidePause write SetHintHidePause default DefHintHidePause;
    property ShowMainForm: Boolean read GetShowMainForm write SetShowMainForm default True;
    property HintShortCuts: Boolean read GetHintShortCuts write SetHintShortCuts default True;
    property UpdateMetricSettings: Boolean read GetUpdateMetricSettings write SetUpdateMetricSettings default True;
    property BiDiMode: TBiDiMode read GetBiDiMode write SetBiDiMode default bdLeftToRight;
    property BiDiKeyboard: string read GetBiDiKeyboard write SetBiDiKeyboard;
    property NonBiDiKeyboard: string read GetNonBiDiKeyboard write SetNonBiDiKeyboard;
    property MouseDragImmediate: Boolean read GetMouseDragImmediate write SetMouseDragImmediate default True;
    property MouseDragThreshold: Integer read GetMouseDragThreshold write SetMouseDragThreshold default 5;
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write SetOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnPaintIcon: TNotifyEvent read FOnPaintIcon write FOnPaintIcon;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    {$IFDEF COMPILER7_UP}
    property OnModalBegin: TNotifyEvent read FOnModalBegin write FOnModalBegin;
    property OnModalEnd: TNotifyEvent read FOnModalEnd write FOnModalEnd;
    {$ENDIF COMPILER7_UP}
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnSettingsChanged: TNotifyEvent read FOnSettingsChanged write FOnSettingsChanged;
    property OnActiveControlChange: TNotifyEvent read FOnActiveControlChange write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent read FOnActiveFormChange write FOnActiveFormChange;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  AppEvnts;

type
  {$IFDEF COMPILER7} // In Delphi 7 they forgot to publish those events
  TApplicationEvents = class(AppEvnts.TApplicationEvents)
  published
    property OnModalBegin;
    property OnModalEnd;
  end;
  {$ENDIF COMPILER7}

  TJvAppEventList = class(TComponent)
  private
    FApplicationEvents: TApplicationEvents;
    FAppEvents: TList;
    FHooked: Boolean;
    FExceptionHandlerCount: Integer;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    procedure AddEvents(App: TJvAppEvents);
    procedure RemoveEvents(App: TJvAppEvents);
    procedure ClearEvents;
    function GetItem(Index: Integer): TJvAppEvents; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    procedure DoHint(Sender: TObject);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);
    function DoHelp(Command: Word; Data: {$IFDEF RTL230_UP}THelpEventData{$ELSE}Longint{$ENDIF}; var CallHelp: Boolean): Boolean;
    procedure DoMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DoShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure DoShowHint(var HintStr: THintString; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure DoActiveControlChange(Sender: TObject);
    procedure DoActiveFormChange(Sender: TObject);
    procedure DoActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: Boolean);
    {$IFDEF COMPILER7_UP}
    procedure DoModalBegin(Sender: TObject);
    procedure DoModalEnd(Sender: TObject);
    {$ENDIF COMPILER7_UP}
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    { OnException needs special treatment. Otherwise the first TJvAppEvents instance
      will steal the OnException handler. }
    procedure RegisterExceptionHandler;
    procedure UnregisterExceptionHandler;

    property Items[Index: Integer]: TJvAppEvents read GetItem;
  end;

//=== { TJvAppEventList } ====================================================

constructor TJvAppEventList.Create;
begin
  inherited Create(nil);
  FAppEvents := TList.Create;
end;

destructor TJvAppEventList.Destroy;
begin
  ClearEvents;
  FAppEvents.Free;
  inherited Destroy;
end;

procedure TJvAppEventList.ClearEvents;
begin
  if FHooked then
  begin
    FreeAndNil(FApplicationEvents);
    // Screen might get destroyed and set to nil before our finalization is called
    if Assigned(Screen) then
    begin
      {Screen.OnActiveControlChange := FOnActiveControlChange;
      Screen.OnActiveFormChange := FOnActiveFormChange;}
      Screen.OnActiveControlChange := nil;
      Screen.OnActiveFormChange := nil;
    end;
  end;
end;

procedure TJvAppEventList.AddEvents(App: TJvAppEvents);
begin
  if (App <> nil) and (FAppEvents.IndexOf(App) = -1) then
  begin
    FAppEvents.Add(App);
    if not (csDesigning in App.ComponentState) and (FAppEvents.Count = 1) then
    begin
      if FApplicationEvents = nil then
        FApplicationEvents := TApplicationEvents.Create(Self);

      FApplicationEvents.OnActionExecute := DoActionExecute;
      FApplicationEvents.OnActionUpdate := DoActionUpdate;
      FApplicationEvents.OnShortCut := DoShortCut;
      FApplicationEvents.OnActivate := DoActivate;
      FApplicationEvents.OnDeactivate := DoDeactivate;
      if FExceptionHandlerCount > 0 then
        FApplicationEvents.OnException := DoException;
      FApplicationEvents.OnIdle := DoIdle;
      FApplicationEvents.OnHelp := DoHelp;
      FApplicationEvents.OnHint := DoHint;
      FApplicationEvents.OnMessage := DoMessage;
      FApplicationEvents.OnMinimize := DoMinimize;
      FApplicationEvents.OnRestore := DoRestore;
      FApplicationEvents.OnShowHint := DoShowHint;
      {$IFDEF COMPILER7_UP}
      FApplicationEvents.OnModalBegin := DoModalBegin;
      FApplicationEvents.OnModalEnd := DoModalEnd;
      {$ENDIF COMPILER7_UP}

      if Screen <> nil then
      begin
        FOnActiveControlChange := Screen.OnActiveControlChange;
        FOnActiveFormChange := Screen.OnActiveFormChange;
        Screen.OnActiveControlChange := DoActiveControlChange;
        Screen.OnActiveFormChange := DoActiveFormChange;
      end;
      FHooked := True;
    end;
  end;
end;

procedure TJvAppEventList.RemoveEvents(App: TJvAppEvents);
begin
  if FAppEvents.IndexOf(App) >= 0 then
    FAppEvents.Remove(App);
  if not (csDesigning in App.ComponentState) and (FAppEvents.Count = 0) then
    ClearEvents;
end;

procedure TJvAppEventList.RegisterExceptionHandler;
begin
  Inc(FExceptionHandlerCount);
  if (FExceptionHandlerCount = 1) and (FApplicationEvents <> nil) then
    FApplicationEvents.OnException := DoException
end;

procedure TJvAppEventList.UnregisterExceptionHandler;
begin
  Dec(FExceptionHandlerCount);
  if (FExceptionHandlerCount = 0) and (FApplicationEvents <> nil) then
    FApplicationEvents.OnException := nil;
end;

procedure TJvAppEventList.DoActivate(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnActivate) then
      Items[I].FOnActivate(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoDeactivate(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnDeactivate) then
      Items[I].FOnDeactivate(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoException(Sender: TObject; E: Exception);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnException) then
      Items[I].FOnException(Sender, E);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoIdle(Sender: TObject; var Done: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnIdle) then
      Items[I].FOnIdle(Sender, Done);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

function TJvAppEventList.DoHelp(Command: Word; Data: {$IFDEF RTL230_UP}THelpEventData{$ELSE}Longint{$ENDIF}; var CallHelp: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnHelp) then
      Result := Items[I].FOnHelp(Command, Data, CallHelp);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoHint(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnHint) then
      Items[I].FOnHint(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoMessage(var Msg: TMsg; var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnMessage) then
      Items[I].FOnMessage(Msg, Handled);
    if not Items[I].Chained or Handled or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoMinimize(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnMinimize) then
      Items[I].FOnMinimize(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoRestore(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnRestore) then
      Items[I].FOnRestore(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoShowHint(var HintStr: THintString; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnShowHint) then
      Items[I].FOnShowHint(HintStr, CanShow, HintInfo);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

function TJvAppEventList.GetItem(Index: Integer): TJvAppEvents;
begin
  Result := TJvAppEvents(FAppEvents[Index]);
end;

procedure TJvAppEventList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FApplicationEvents) then
    FApplicationEvents := nil;
end;

procedure TJvAppEventList.DoActiveControlChange(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnActiveControlChange) then
      Items[I].FOnActiveControlChange(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
  if Assigned(FOnActiveControlChange) then
    FOnActiveControlChange(Sender);
end;

procedure TJvAppEventList.DoActiveFormChange(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnActiveFormChange) then
      Items[I].FOnActiveFormChange(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
  if Assigned(FOnActiveFormChange) then
    FOnActiveFormChange(Sender);
end;

procedure TJvAppEventList.DoActionExecute(Action: TBasicAction;
  var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnActionExecute) then
      Items[I].FOnActionExecute(Action, Handled);
    if not Items[I].Chained or Handled or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoActionUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnActionUpdate) then
      Items[I].FOnActionUpdate(Action, Handled);
    if not Items[I].Chained or Handled or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnShortCut) then
      Items[I].FOnShortCut(Msg, Handled);
    if not Items[I].Chained or Handled or Items[I].FCancelDispatch then
      Exit;
  end;
end;

{$IFDEF COMPILER7_UP}
procedure TJvAppEventList.DoModalBegin(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnModalBegin) then
      Items[I].FOnModalBegin(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;

procedure TJvAppEventList.DoModalEnd(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    Items[I].FCancelDispatch := False;
    if Assigned(Items[I].FOnModalEnd) then
      Items[I].FOnModalEnd(Sender);
    if not Items[I].Chained or Items[I].FCancelDispatch then
      Exit;
  end;
end;
{$ENDIF COMPILER7_UP}

//=== { TJvAppEvents } =======================================================

var
  AppList: TJvAppEventList = nil;

constructor TJvAppEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AppList = nil then
    AppList := TJvAppEventList.Create;

  FChained := True;
  FHintColor := DefHintColor;
  FHintPause := DefHintPause;
  FShowHint := True;
  FCancelDispatch := False;
  FHintShortPause := DefHintShortPause;
  FHintHidePause := DefHintHidePause;
  FShowMainForm := True;
  FHintShortCuts := True;
  FMouseDragImmediate := True;
  FMouseDragThreshold := 5;
  FUpdateFormatSettings := True;
  FUpdateMetricSettings := True;
  FBiDiMode := bdLeftToRight;
  if not (csDesigning in ComponentState) then
    Application.HookMainWindow(MessageHook);
  AppList.AddEvents(Self);
end;

destructor TJvAppEvents.Destroy;
begin
  if not (csDesigning in ComponentState) then
    Application.UnhookMainWindow(MessageHook);
  if (Self <> nil) and (AppList <> nil) then
  begin
    SetOnException(nil);
    AppList.RemoveEvents(Self);
  end;
  inherited Destroy;
end;

procedure TJvAppEvents.Loaded;
begin
  inherited Loaded;
  UpdateAppProps;
end;

procedure TJvAppEvents.CancelDispatch;
begin
  FCancelDispatch := True;
end;

procedure TJvAppEvents.PaintIcon;
var
  PS: TPaintStruct;
begin
  BeginPaint(Application.Handle, PS);
  try
    if PS.fErase then
      Windows.FillRect(PS.hdc, PS.rcPaint, HBRUSH(COLOR_BACKGROUND + 1));
    if Assigned(FOnPaintIcon) then
      FOnPaintIcon(Self);
  finally
    EndPaint(Application.Handle, PS);
  end;
end;

procedure TJvAppEvents.SettingsChanged;
begin
  if Assigned(FOnSettingsChanged) then
    FOnSettingsChanged(Self);
end;

function TJvAppEvents.MessageHook(var Msg: TMessage): Boolean;
begin
  Result := False;
  case Msg.Msg of
    WM_WININICHANGE:
      begin
        try
          SettingsChanged;
        except
          Application.HandleException(Self);
        end;
      end;
    WM_PAINT:
      if Assigned(FOnPaintIcon) and IsIconic(Application.Handle) then
      begin
        PaintIcon;
        Result := True;
      end;
  end;
end;

function TJvAppEvents.GetHintColor: TColor;
begin
  if csDesigning in ComponentState then
    Result := FHintColor
  else
    Result := Application.HintColor;
end;

function TJvAppEvents.GetHintPause: Integer;
begin
  if csDesigning in ComponentState then
    Result := FHintPause
  else
    Result := Application.HintPause;
end;

function TJvAppEvents.GetShowHint: Boolean;
begin
  if csDesigning in ComponentState then
    Result := FShowHint
  else
    Result := Application.ShowHint;
end;

procedure TJvAppEvents.SetHintColor(Value: TColor);
begin
  FHintColor := Value;
  if not (csDesigning in ComponentState) then
    Application.HintColor := Value;
end;

procedure TJvAppEvents.SetHintPause(Value: Integer);
begin
  FHintPause := Value;
  if not (csDesigning in ComponentState) then
    Application.HintPause := Value;
end;

procedure TJvAppEvents.SetShowHint(Value: Boolean);
begin
  FShowHint := Value;
  if not (csDesigning in ComponentState) then
    Application.ShowHint := Value;
end;

function TJvAppEvents.GetUpdateFormatSettings: Boolean;
begin
  if not (csDesigning in ComponentState) then
    Result := Application.UpdateFormatSettings
  else
    Result := FUpdateFormatSettings;
end;

procedure TJvAppEvents.SetUpdateFormatSettings(Value: Boolean);
begin
  FUpdateFormatSettings := Value;
  if not (csDesigning in ComponentState) then
    Application.UpdateFormatSettings := Value;
end;

function TJvAppEvents.GetHintShortPause: Integer;
begin
  if csDesigning in ComponentState then
    Result := FHintShortPause
  else
    Result := Application.HintShortPause;
end;

function TJvAppEvents.GetHintHidePause: Integer;
begin
  if csDesigning in ComponentState then
    Result := FHintHidePause
  else
    Result := Application.HintHidePause;
end;

function TJvAppEvents.GetShowMainForm: Boolean;
begin
  if csDesigning in ComponentState then
    Result := FShowMainForm
  else
    Result := Application.ShowMainForm;
end;

procedure TJvAppEvents.SetHintShortPause(Value: Integer);
begin
  FHintShortPause := Value;
  if not (csDesigning in ComponentState) then
    Application.HintShortPause := Value;
end;

procedure TJvAppEvents.SetHintHidePause(Value: Integer);
begin
  FHintHidePause := Value;
  if not (csDesigning in ComponentState) then
    Application.HintHidePause := Value;
end;

procedure TJvAppEvents.SetShowMainForm(Value: Boolean);
begin
  FShowMainForm := Value;
  if not (csDesigning in ComponentState) then
    Application.ShowMainForm := Value;
end;

function TJvAppEvents.GetUpdateMetricSettings: Boolean;
begin
  if csDesigning in ComponentState then
    Result := FUpdateMetricSettings
  else
    Result := Application.UpdateMetricSettings;
end;

procedure TJvAppEvents.SetUpdateMetricSettings(Value: Boolean);
begin
  FUpdateMetricSettings := Value;
  if not (csDesigning in ComponentState) then
    Application.UpdateMetricSettings := Value;
end;

function TJvAppEvents.GetHintShortCuts: Boolean;
begin
  if csDesigning in ComponentState then
    Result := FHintShortCuts
  else
    Result := Application.HintShortCuts;
end;

function TJvAppEvents.GetMouseDragImmediate: Boolean;
begin
  if (csDesigning in ComponentState) or (Mouse = nil) then
    Result := FMouseDragImmediate
  else
    Result := Mouse.DragImmediate;
end;

function TJvAppEvents.GetMouseDragThreshold: Integer;
begin
  if (csDesigning in ComponentState) or (Mouse = nil) then
    Result := FMouseDragThreshold
  else
    Result := Mouse.DragThreshold;
end;

procedure TJvAppEvents.SetMouseDragImmediate(Value: Boolean);
begin
  FMouseDragImmediate := Value;
  if not (csDesigning in ComponentState) and (Mouse <> nil) then
    Mouse.DragImmediate := Value;
end;

procedure TJvAppEvents.SetMouseDragThreshold(Value: Integer);
begin
  FMouseDragThreshold := Value;
  if not (csDesigning in ComponentState) and (Mouse <> nil) then
    Mouse.DragThreshold := Value;
end;

procedure TJvAppEvents.SetHintShortCuts(Value: Boolean);
begin
  FHintShortCuts := Value;
  if not (csDesigning in ComponentState) then
    Application.HintShortCuts := Value;
end;

function TJvAppEvents.GetBiDiMode: TBiDiMode;
begin
  if csDesigning in ComponentState then
    Result := FBiDiMode
  else
    Result := Application.BiDiMode;
end;

procedure TJvAppEvents.SetBiDiMode(Value: TBiDiMode);
begin
  FBiDiMode := Value;
  if not (csDesigning in ComponentState) then
    Application.BiDiMode := Value;
end;

function TJvAppEvents.GetBiDiKeyboard: string;
begin
  if csDesigning in ComponentState then
    Result := FBiDiKeyboard
  else
    Result := Application.BiDiKeyboard;
end;

function TJvAppEvents.GetNonBiDiKeyboard: string;
begin
  if csDesigning in ComponentState then
    Result := FNonBiDiKeyboard
  else
    Result := Application.NonBiDiKeyboard;
end;

procedure TJvAppEvents.SetBiDiKeyboard(const Value: string);
begin
  FBiDiKeyboard := Value;
  if not (csDesigning in ComponentState) then
    Application.BiDiKeyboard := Value;
end;

procedure TJvAppEvents.SetNonBiDiKeyboard(const Value: string);
begin
  FNonBiDiKeyboard := Value;
  if not (csDesigning in ComponentState) then
    Application.NonBiDiKeyboard := Value;
end;

procedure TJvAppEvents.SetOnException(const Value: TExceptionEvent);
begin
  if Assigned(FOnException) then
    AppList.UnregisterExceptionHandler;
  FOnException := Value;
  if Assigned(FOnException) then
    AppList.RegisterExceptionHandler;
end;

procedure TJvAppEvents.UpdateAppProps;
begin
  if not (csDesigning in ComponentState) then
  begin
    Application.HintColor := FHintColor;
    Application.HintPause := FHintPause;
    Application.ShowHint := FShowHint;
    Application.HintShortPause := FHintShortPause;
    Application.HintHidePause := FHintHidePause;
    Application.ShowMainForm := FShowMainForm;
    Application.HintShortCuts := FHintShortCuts;
    Application.UpdateFormatSettings := FUpdateFormatSettings;
    Application.UpdateMetricSettings := FUpdateMetricSettings;
    Application.BiDiMode := FBiDiMode;
    Application.BiDiKeyboard := FBiDiKeyboard;
    Application.NonBiDiKeyboard := FNonBiDiKeyboard;
    Mouse.DragImmediate := FMouseDragImmediate;
    Mouse.DragThreshold := FMouseDragThreshold;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(AppList);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
