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

The Original Code is: JvHint.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvHint
description : Custom activated hint

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQHint;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,  
  QWindows, QControls, QForms, QExtCtrls, QGraphics, QTypes, Types, Qt, 
  JvQHTControls, JvQTypes;

type
  TJvHintWindow = class(THintWindow)
  public
    property Caption;
  end;
  TJvHintWindowClass = class of TJvHintWindow;

  TJvHint = class(TComponent)
  private
    FAutoHide: Boolean;
  protected
    // (rom) definitely needs cleanup here  bad structuring
    R: TRect;
    Area: TRect;
    State: (tmBeginShow, tmShowing, tmStopped);
    Txt: THintString;
    HintWindow: TJvHintWindow;
    TimerHint: TTimer;
    FDelay: Integer;
    procedure TimerHintTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(AArea: TRect; ATxt: THintString);
    procedure CancelHint;
  published
    property AutoHide: Boolean read FAutoHide write FAutoHide default True;
  end;

  TJvHTHintWindow = class(THintWindow)
  private
    HtLabel: TJvHTLabel;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer;
      const AHint: THintString; AData: Pointer): TRect; override;
  end;

procedure RegisterHtHints;

implementation

uses
  Math,
  JvQConsts, JvQResources;

//=== { TJvHint } ============================================================

constructor TJvHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TimerHint := TTimer.Create(Self);
  TimerHint.Enabled := False;
  TimerHint.Interval := 50;
  TimerHint.OnTimer := TimerHintTimer;
  HintWindow := TJvHintWindowClass.Create(Self);
  FAutoHide := True;
end;

destructor TJvHint.Destroy;
begin
  TimerHint.Free;
  HintWindow.Free;
  inherited Destroy;
end;

procedure TJvHint.ActivateHint(AArea: TRect; ATxt: THintString);
var
  P: TPoint;
begin
  GetCursorPos(P);
  Area := AArea;
  if ATxt = '' then
  begin
    CancelHint;
    Exit;
  end
  else
    Txt := ATxt;
  if not PtInRect(Area, P) then
  begin
    if IsWindowVisible(HintWindow.Handle) then
      ShowWindow(HintWindow.Handle, SW_HIDE);
    Exit;
  end;
  if HintWindow.Caption <> Txt then
  begin
    R := HintWindow.CalcHintRect(Screen.Width, Txt, nil);
    R.Top := P.Y + 20;
    R.Left := P.X;
    Inc(R.Bottom, R.Top);
    Inc(R.Right, R.Left);
    State := tmBeginShow;
    TimerHint.Enabled := True;
  end;
end;

procedure TJvHint.TimerHintTimer(Sender: TObject);
var
  P: TPoint;
  bPoint, bDelay: Boolean;
  Delay: Integer;
  HintPause: Integer;
begin
  HintWindow.Color := Application.HintColor;
  Delay := FDelay * Integer(TimerHint.Interval);
  case State of
    tmBeginShow:
      begin
        GetCursorPos(P);
        bPoint := not PtInRect(Area, P);
        if bPoint then
        begin
          State := tmStopped;
          Exit;
        end;
        if IsWindowVisible(HintWindow.Handle) then
          HintPause := Application.HintShortPause
        else
          HintPause := Application.HintPause;
        if Delay >= HintPause then
        begin
          HintWindow.ActivateHint(R, Txt);
          FDelay := 0;
          State := tmShowing;
        end
        else
          Inc(FDelay);
      end;
    tmShowing:
      begin
        GetCursorPos(P);
        bDelay := FAutoHide and (Delay > Application.HintHidePause);
        bPoint := not PtInRect(Area, P);
        if bPoint or bDelay then
        begin
          if IsWindowVisible(HintWindow.Handle) then
            ShowWindow(HintWindow.Handle, SW_HIDE);
          FDelay := 0;
          if bPoint then
            HintWindow.Caption := RsHintCaption;
          State := tmStopped;
        end
        else
          Inc(FDelay);
      end;
    tmStopped:
      begin
        FDelay := 0;
        GetCursorPos(P);
        bPoint := not PtInRect(Area, P);
        if IsWindowVisible(HintWindow.Handle) then
          ShowWindow(HintWindow.Handle, SW_HIDE);
        if bPoint then
        begin
          HintWindow.Caption := RsHintCaption;
          TimerHint.Enabled := False;
        end;
      end;
  end;
end;

procedure TJvHint.CancelHint;
begin
  if IsWindowVisible(HintWindow.Handle) then
    ShowWindow(HintWindow.Handle, SW_HIDE);
  HintWindow.Caption := '';
end;

//=== { TJvHTHintWindow } ====================================================

constructor TJvHTHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HtLabel := TJvHTLabel.Create(Self);
  HtLabel.Parent := Self;
  HtLabel.SetBounds(2, 2, 0, 0);
end;

procedure TJvHTHintWindow.Paint;
begin
end;

function TJvHTHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: THintString; AData: Pointer): TRect;
begin
  HtLabel.Caption := AHint;
  Result := Bounds(0, 0, HtLabel.Width + 6, HtLabel.Height + 2);
  if Application.HintHidePause > 0 then
    Application.HintHidePause :=
      Max(2500, // default
      Length(ItemHtPlain(AHint)) *
      (1000 div 20)); // 20 symbols per second
end;

procedure RegisterHtHints;
begin
  if Application.ShowHint then
  begin
    Application.ShowHint := False;
    HintWindowClass := TJvHTHintWindow;
    Application.ShowHint := True;
  end
  else
    HintWindowClass := TJvHTHintWindow;
end;

end.

