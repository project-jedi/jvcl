{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit fJvTracer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvRegAuto, iJvMTracer, Buttons, ExtCtrls, ShellAPI;

const
  Max_Timer = 10;

 { * Tray support }

 { Notify message for TrayIcon }
const
  CM_TRAYICON = CM_BASE + 63;

type

  TJvTrayForm  = class(TForm)
  private
    FIconData: TNotifyIconData;
    FhIcon : hIcon;
    FActive : boolean;
    FHint: string;
    FAdded  : boolean;

    procedure TrayActivate;
    procedure TrayDeactivate;
    procedure SetActive(Value: Boolean);
    procedure UpdateNotifyData;
    procedure SetIcon(Value: hIcon);
    procedure SetHint(const Value: string);
    procedure CMTrayIcon(var Message : TMessage); message CM_TRAYICON;
    procedure AppMinimize(Sender : TObject);
  public
    procedure HideTray;
    procedure ShowTray;
    procedure ChangeIcon;

    property TrayIcon : hIcon read FhIcon write SetIcon;
    property TrayHint : string read FHint write SetHint;
    property Active : boolean read FActive write SetActive;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TJvTracerMain  = class(TJvTrayForm )
    memOutput: TMemo;
    RegAuto1: TJvRegAuto;
    pnlToolBar: TPanel;
    sbClear: TSpeedButton;
    sbSaveLog: TSpeedButton;
    SaveDialog: TSaveDialog;
    pnlTimers: TPanel;
    lblTimer1: TLabel;
    Timer: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    sbTimers: TSpeedButton;
    sbLines: TSpeedButton;
    sbTop: TSpeedButton;
    procedure sbClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbSaveLogClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure sbTimersClick(Sender: TObject);
    procedure sbLinesClick(Sender: TObject);
    procedure sbTopClick(Sender: TObject);
  private
    { Private declarations }
    Capt : string;
    LastRemoteAppName : string;
    TimerTickBeg : array [1..Max_Timer] of longint;
    lblTimers : array [1..Max_Timer] of TLabel;

    procedure WMTracer(var Message : TWMTracer); message wm_Tracer;
    procedure TimerStart(Timer : integer);
    procedure TimerStop(Timer : integer);
    procedure RemoteAppClosed;
  public
    { Public declarations }
  end;

var
  TracerMain: TJvTracerMain ;

implementation

uses JvUtils;

{$R *.DFM}

procedure TJvTracerMain .WMTracer(var Message : TWMTracer);
var
  S : string;
begin
  if Tracer.Enabled then begin
    Caption := Capt +' - '+ Tracer.PDI.RemoteAppName;
    if Tracer.PDI.RemoteAppName <> LastRemoteAppName then
      memOutput.Lines.Add('--- '+Tracer.PDI.RemoteAppName+' ---');
    LastRemoteAppName := Tracer.PDI.RemoteAppName;
    if sbLines.Down then S := IntToStr(memOutput.Lines.Count+1)+'. ' else S := '';
    Message.Result := 1;
    case Message.Command of
      dc_Writeln : memOutput.Lines.Add(S + Tracer.PDI.S);
      dc_Clear   : sbClear.Click;
      dc_RemoteAppClosed : RemoteAppClosed;
      dc_TimerStart : TimerStart(Message.wParam);
      dc_TimerStop  : TimerStop(Message.wParam);
      else Message.Result := 0;
    end;
  end else Message.Result := 0;
end;

procedure TJvTracerMain .sbClearClick(Sender: TObject);
begin
  memOutput.Clear;
  memOutput.Lines.Add('------'+DateToStr(Date)+' '+TimeToStr(Time)+'-------');
  LastRemoteAppName := '';
end;

procedure TJvTracerMain .FormCreate(Sender: TObject);
begin
  TracerApp := true;
  Capt := Caption;
  sbClear.Click;
  lblTimers[1] := lblTimer1;
  Timer.Enabled := true;
  RegAuto1.Load;
  pnlTimers.Visible := sbTimers.Down;
  sbTimersClick(nil);
  sbLinesClick(nil);
  sbTopClick(nil);
  TrayHint := 'JVCL Tracer (waiting for double click)';
end;

procedure TJvTracerMain .sbSaveLogClick(Sender: TObject);
begin
  if SaveDialog.Execute then memOutput.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TJvTracerMain .TimerStart(Timer : integer);
begin
  memOutput.Lines.Add('Start timer'+IntToStr(Timer));
  TimerTickBeg[Timer] := GetTickCount;
end;

procedure TJvTracerMain .TimerStop(Timer : integer);
begin
  if TimerTickBeg[Timer] = 0 then exit;
  lblTimers[Timer].Caption := IntToStr(GetTickCount - TimerTickBeg[Timer]);
  TimerTickBeg[Timer] := 0;
  memOutput.Lines.Add('Stop timer'+IntToStr(Timer)+' = '+lblTimers[Timer].Caption+' ms');
end;

procedure TJvTracerMain .TimerTimer(Sender: TObject);
var
  i : integer;
begin
  for i := 1 to Max_Timer do
    if TimerTickBeg[i] <> 0 then begin
      lblTimers[i].Caption := IntToStr(GetTickCount - TimerTickBeg[i]);
    end;
end;

procedure TJvTracerMain .RemoteAppClosed;
var
  i : integer;
begin
  Caption := Capt;
  LastRemoteAppName := '';
  for i := 1 to Max_Timer do TimerStop(i);
end;

procedure TJvTracerMain .sbTimersClick(Sender: TObject);
begin
  pnlTimers.Visible := sbTimers.Down;
  if sbTimers.Down then sbTimers.Hint := 'Hide timers'
  else sbTimers.Hint := 'Show timers'
end;

procedure TJvTracerMain .sbLinesClick(Sender: TObject);
begin
  if sbLines.Down then sbLines.Hint := 'Hide line numbers'
  else sbLines.Hint := 'Show line numbers'
end;

procedure TJvTracerMain .sbTopClick(Sender: TObject);
begin
  SetWindowTop(Handle, sbTop.Down);
end;



{ *** Tray support }

{ Most of this code are given from RxLibrary.
  Use its component TTrayIcon and you don't need copy this code to your program. }

procedure TJvTrayForm .SetActive(Value: Boolean);
begin
  if (Value <> FActive) then begin
    FActive := Value;
    if Value then TrayActivate else TrayDeactivate;
  end;
end;

procedure TJvTrayForm .ShowTray;
begin
  Active := True;
end;

procedure TJvTrayForm .HideTray;
begin
  Active := False;
end;

procedure TJvTrayForm .UpdateNotifyData;
begin
  with FIconData do begin
    cbSize := SizeOf(TNotifyIconData);
    Wnd := Handle;
    uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    if FhIcon <> 0 then
      hIcon := FhIcon
    else
      hIcon := INVALID_HANDLE_VALUE;
    StrPLCopy(szTip, FHint, SizeOf(szTip) - 1);
    uCallbackMessage := CM_TRAYICON;
    uID := 0;
  end;
end;

procedure TJvTrayForm .ChangeIcon;
begin
  if FAdded then
    if FhIcon <> 0 then begin
      UpdateNotifyData;
      Shell_NotifyIcon(NIM_MODIFY, @FIconData);
    end else TrayDeactivate
  else
    if FActive then TrayActivate;
end;

procedure TJvTrayForm .TrayActivate;
begin
  Deactivate;
  UpdateNotifyData;
  FAdded := Shell_NotifyIcon(NIM_ADD, @FIconData);
  if (FHint = '') and FAdded then
    Shell_NotifyIcon(NIM_MODIFY, @FIconData);
end;

procedure TJvTrayForm .TrayDeactivate;
begin
  Shell_NotifyIcon(NIM_DELETE, @FIconData);
  FAdded := False;
end;

procedure TJvTrayForm .SetIcon(Value: hIcon);
begin
  if FhIcon <> Value then begin
    FhIcon := Value;
    ChangeIcon;
  end;
end;

procedure TJvTrayForm .SetHint(const Value: string);
begin
  if FHint <> Value then begin
    FHint := Value;
    ChangeIcon;
  end;
end;

constructor TJvTrayForm .Create(AOwner : TComponent);
begin
  FHint := Application.Title;
  // TrayIcon := Application.Icon.Handle;
  FhIcon := LoadImage(hInstance, PChar('MAINICON'), IMAGE_ICON, 16, 16, 0);
  inherited Create(AOwner);
  Application.OnMinimize := AppMinimize;
  ShowTray;
end;

destructor TJvTrayForm .Destroy;
begin
  inherited Destroy;
  HideTray;
end;

procedure TJvTrayForm .CMTrayIcon(var Message : TMessage);
begin
  case Message.lParam of
    WM_LBUTTONDBLCLK :
      begin
        ShowWindow(Application.Handle, SW_SHOW);
        Application.Restore;
        Application.BringToFront;
      end;
   // WM_LBUTTONUP     : ;
   // WM_RBUTTONUP     : ;
    //else inherited;
  end;
end;

procedure TJvTrayForm .AppMinimize(Sender : TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;

end.
