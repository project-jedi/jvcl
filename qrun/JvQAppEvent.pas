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

The Original Code is: JvAppEvent.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQAppEvent;

// (rom) this definitely needs an explanation
{$C PRELOAD}

interface

uses 
  Qt, QTypes, 
  QWindows, QMessages, Types, QGraphics, QControls, QForms, QActnList,
  SysUtils, Classes,
  JvQTypes, JvQComponent, JvQFinalize;

const
  DefHintColor = clInfoBk;
  DefHintPause = 500;
  DefHintShortPause = DefHintPause div 10;
  DefHintHidePause = DefHintPause * 5;

type
  TJvAppEvents = class(TJvComponent)
  private
    FChained: Boolean;
    FHintColor: TColor;
    FHintPause: Integer;
    FShowHint: Boolean;
    FCanvas: TCanvas;
    FUpdateFormatSettings: Boolean;
    FHintShortPause: Integer;
    FHintHidePause: Integer;
    FShowMainForm: Boolean;
    FHintShortCuts: Boolean;
    FMouseDragImmediate: Boolean;
    FMouseDragThreshold: Integer;
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnShortCut: TShortCutEvent; 
    FOnPaintIcon: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;  
    FOnEvent : TEventEvent; 
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShowHint: TShowHintEvent; 
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    procedure UpdateAppProps;
    function GetCanvas: TCanvas;
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
  protected
    procedure Loaded; override; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas; { for painting the icon }
  published
    property Chained: Boolean read FChained write FChained default True;
    property HintColor: TColor read GetHintColor write SetHintColor default DefHintColor;
    property HintPause: Integer read GetHintPause write SetHintPause default DefHintPause;
    property ShowHint: Boolean read GetShowHint write SetShowHint default True;
    property UpdateFormatSettings: Boolean read GetUpdateFormatSettings
      write SetUpdateFormatSettings default True;
    property HintShortPause: Integer read GetHintShortPause write SetHintShortPause
      default DefHintShortPause;
    property HintHidePause: Integer read GetHintHidePause write SetHintHidePause
      default DefHintHidePause;
    property ShowMainForm: Boolean read GetShowMainForm write SetShowMainForm
      default True;
    property HintShortCuts: Boolean read GetHintShortCuts write SetHintShortCuts
      default True; 
    property MouseDragImmediate: Boolean read GetMouseDragImmediate
      write SetMouseDragImmediate default True;
    property MouseDragThreshold: Integer read GetMouseDragThreshold
      write SetMouseDragThreshold default 5;
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnPaintIcon: TNotifyEvent read FOnPaintIcon write FOnPaintIcon;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint; 
    property OnActiveControlChange: TNotifyEvent read FOnActiveControlChange write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent read FOnActiveFormChange write FOnActiveFormChange; 
    property OnEvent: TEventEvent read FOnEvent write FOnEvent; 
  end;

implementation

const
  sUnitName = 'JvAppEvent';

type
  TJvAppEventList = class(TObject)
  private
    FAppEvents: TList;
    FHooked: Boolean;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;  
    FOnEvent: TEventEvent; 
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShowHint: TShowHintEvent;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnShortCut: TShortCutEvent;
    procedure AddEvents(App: TJvAppEvents);
    procedure RemoveEvents(App: TJvAppEvents);
    procedure ClearEvents;
  protected
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    procedure DoHint(Sender: TObject);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);  
    function DoHelp(HelpType: THelpType; HelpContext: THelpContext;
      const HelpKeyword: String; const HelpFile: String;
      var Handled: Boolean): Boolean;
    procedure DoShortCut(Key: Integer; Shift: TShiftState; var Handled: Boolean);
    procedure DoEvent(Sender: QObjectH; Event: QEventH; var Handled: Boolean); 
    procedure DoShowHint(var HintStr: THintString; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure DoActiveControlChange(Sender: TObject);
    procedure DoActiveFormChange(Sender: TObject);
    procedure DoActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

//=== TJvAppEventList ========================================================

constructor TJvAppEventList.Create;
begin
  inherited Create;
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
    Application.OnActivate := nil;
    Application.OnDeactivate := nil;
    Application.OnException := nil; 
    Application.OnEvent := nil; 
    Application.OnIdle := nil;
    Application.OnHelp := nil;
    Application.OnHint := nil; 
    Application.OnMinimize := nil;
    Application.OnRestore := nil;
    Application.OnShowHint := nil;
    Application.OnActionExecute := nil;
    Application.OnActionUpdate := nil;
    Application.OnShortCut := nil;
    if Screen <> nil then
    begin
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
      FOnActivate := Application.OnActivate;
      FOnDeactivate := Application.OnDeactivate;
      FOnException := Application.OnException;
      FOnIdle := Application.OnIdle;
      FOnHelp := Application.OnHelp;
      FOnHint := Application.OnHint;  
      FOnEvent := Application.OnEvent; 
      FOnMinimize := Application.OnMinimize;
      FOnRestore := Application.OnRestore;
      FOnShowHint := Application.OnShowHint;
      FOnActionExecute := Application.OnActionExecute;
      FOnActionUpdate := Application.OnActionUpdate;
      FOnShortCut := Application.OnShortCut;
      Application.OnActionExecute := DoActionExecute;
      Application.OnActionUpdate := DoActionUpdate;
      Application.OnShortCut := DoShortCut;
      Application.OnActivate := DoActivate;
      Application.OnDeactivate := DoDeactivate;
      Application.OnException := DoException;
      Application.OnIdle := DoIdle;
      Application.OnHelp := DoHelp;
      Application.OnHint := DoHint;  
      Application.OnEvent := DoEvent; 
      Application.OnMinimize := DoMinimize;
      Application.OnRestore := DoRestore;
      Application.OnShowHint := DoShowHint;
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

procedure TJvAppEventList.DoActivate(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnActivate) then
      TJvAppEvents(FAppEvents[I]).FOnActivate(Sender);
    if not TJvAppEvents(FAppEvents[I]).Chained then
      Exit;
  end;
  if Assigned(FOnActivate) then
    FOnActivate(Sender);
end;

procedure TJvAppEventList.DoDeactivate(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnDeactivate) then
      TJvAppEvents(FAppEvents[I]).FOnDeactivate(Sender);
    if not TJvAppEvents(FAppEvents[I]).Chained then
      Exit;
  end;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(Sender);
end;

procedure TJvAppEventList.DoException(Sender: TObject; E: Exception);
var
  I: Integer;
  Handled: Boolean;
begin
  Handled := False;
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnException) then
    begin
      TJvAppEvents(FAppEvents[I]).FOnException(Sender, E);
      Handled := True;
    end;
    if not TJvAppEvents(FAppEvents[I]).Chained then
    begin
      if not Handled then
        Application.ShowException(E);
      Exit;
    end;
  end;
  if Assigned(FOnException) then
  begin
    FOnException(Sender, E);
    Handled := True;
  end;
  if not Handled then
    Application.ShowException(E);
end;

procedure TJvAppEventList.DoIdle(Sender: TObject; var Done: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnIdle) then
      TJvAppEvents(FAppEvents[I]).FOnIdle(Sender, Done);
    if not TJvAppEvents(FAppEvents[I]).Chained then
      Exit;
  end;
  if Assigned(FOnIdle) then
    FOnIdle(Sender, Done);
end;



function TJvAppEventList.DoHelp(HelpType: THelpType; HelpContext: THelpContext;
  const HelpKeyword: String; const HelpFile: String;
  var Handled: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnHelp) then
      Result := TJvAppEvents(FAppEvents[I]).FOnHelp(HelpType, HelpContext,
        HelpKeyword, HelpFile, Handled);
    if not TJvAppEvents(FAppEvents[I]).Chained or Handled then
      Exit;
  end;
  if Assigned(FOnHelp) then
    Result := FOnHelp(HelpType, HelpContext, HelpKeyword, HelpFile, Handled);
end;


procedure TJvAppEventList.DoHint(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnHint) then
      TJvAppEvents(FAppEvents[I]).FOnHint(Sender);
    if not TJvAppEvents(FAppEvents[I]).Chained then
      Exit;
  end;
  if Assigned(FOnHint) then
    FOnHint(Sender);
end;



procedure TJvAppEventList.DoEvent(Sender: QObjectH; Event: QEventH;
  var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnEvent) then
      TJvAppEvents(FAppEvents[I]).FOnEvent(Sender, Event, Handled);
    if not TJvAppEvents(FAppEvents[I]).Chained or Handled then
      Exit;
  end;
  if Assigned(FOnEvent) then
    FOnEvent(Sender, Event, Handled);
end;


procedure TJvAppEventList.DoMinimize(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnMinimize) then
      TJvAppEvents(FAppEvents[I]).FOnMinimize(Sender);
    if not TJvAppEvents(FAppEvents[I]).Chained then
      Exit;
  end;
  if Assigned(FOnMinimize) then
    FOnMinimize(Sender);
end;

procedure TJvAppEventList.DoRestore(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnRestore) then
      TJvAppEvents(FAppEvents[I]).FOnRestore(Sender);
    if not TJvAppEvents(FAppEvents[I]).Chained then
      Exit;
  end;
  if Assigned(FOnRestore) then
    FOnRestore(Sender);
end;

procedure TJvAppEventList.DoShowHint(var HintStr: THintString; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnShowHint) then
      TJvAppEvents(FAppEvents[I]).FOnShowHint(HintStr, CanShow, HintInfo);
    if not TJvAppEvents(FAppEvents[I]).Chained then
      Exit;
  end;
  if Assigned(FOnShowHint) then
    FOnShowHint(HintStr, CanShow, HintInfo);
end;

procedure TJvAppEventList.DoActiveControlChange(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnActiveControlChange) then
      TJvAppEvents(FAppEvents[I]).FOnActiveControlChange(Sender);
    if not TJvAppEvents(FAppEvents[I]).Chained then
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
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnActiveFormChange) then
      TJvAppEvents(FAppEvents[I]).FOnActiveFormChange(Sender);
    if not TJvAppEvents(FAppEvents[I]).Chained then
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
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnActionExecute) then
      TJvAppEvents(FAppEvents[I]).FOnActionExecute(Action, Handled);
    if not TJvAppEvents(FAppEvents[I]).Chained or Handled then
      Exit;
  end;
  if Assigned(FOnActionExecute) then
    FOnActionExecute(Action, Handled);
end;

procedure TJvAppEventList.DoActionUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnActionUpdate) then
      TJvAppEvents(FAppEvents[I]).FOnActionUpdate(Action, Handled);
    if not TJvAppEvents(FAppEvents[I]).Chained or Handled then
      Exit;
  end;
  if Assigned(FOnActionUpdate) then
    FOnActionUpdate(Action, Handled);
end;



procedure TJvAppEventList.DoShortCut(Key: Integer; Shift: TShiftState; var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do
  begin
    if Assigned(TJvAppEvents(FAppEvents[I]).FOnShortCut) then
      TJvAppEvents(FAppEvents[I]).FOnShortCut(Key, Shift, Handled);
    if not TJvAppEvents(FAppEvents[I]).Chained or Handled then
      Exit;
  end;
  if Assigned(FOnShortCut) then
    FOnShortCut(Key, Shift, Handled);
end;


//=== TJvAppEvents ===========================================================

// (rom) changed to var
var
  AppList: TJvAppEventList = nil;

constructor TJvAppEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AppList = nil then
  begin
    AppList := TJvAppEventList.Create;
    AddFinalizeObjectNil(sUnitName, TObject(AppList));
  end;

  FChained := True;
  FHintColor := DefHintColor;
  FHintPause := DefHintPause;
  FShowHint := True;
  FHintShortPause := DefHintShortPause;
  FHintHidePause := DefHintHidePause;
  FShowMainForm := True;
  FHintShortCuts := True;
  FMouseDragImmediate := True;
  FMouseDragThreshold := 5;
  FUpdateFormatSettings := True; 
  AppList.AddEvents(Self);
end;

destructor TJvAppEvents.Destroy;
begin 
  if (Self <> nil) and (AppList <> nil) then
    AppList.RemoveEvents(Self);
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvAppEvents.Loaded;
begin
  inherited Loaded;
  UpdateAppProps;
end;

function TJvAppEvents.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
    FCanvas := TCanvas.Create;
  Result := FCanvas;
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
    Result := FUpdateFormatSettings;
end;

procedure TJvAppEvents.SetUpdateFormatSettings(Value: Boolean);
begin
  FUpdateFormatSettings := Value; 
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



procedure TJvAppEvents.UpdateAppProps;
begin
  if not (csDesigning in ComponentState) then
  begin
    with Application do
    begin
      HintColor := FHintColor;
      HintPause := FHintPause;
      ShowHint := FShowHint;
      HintShortPause := FHintShortPause;
      HintHidePause := FHintHidePause;
      ShowMainForm := FShowMainForm;
      HintShortCuts := FHintShortCuts;
      UpdateFormatSettings := FUpdateFormatSettings; 
      with Mouse do
      begin
        DragImmediate := FMouseDragImmediate;
        DragThreshold := FMouseDragThreshold;
      end;
    end;
  end;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

