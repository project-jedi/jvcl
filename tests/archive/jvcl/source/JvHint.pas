{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHint.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvHint
description : Custom activated hint

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHint;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ExtCtrls,
  JvHtControls;

type

  TJvHint = class(TComponent)
  protected
    R: TRect;
    Area: TRect;
    Txt: string;
    State: (tmBeginShow, tmShowing, tmStopped);
    HintWindow: THintWindow;
    TimerHint: TTimer;
    FAutoHide: Boolean;
    FDelay: Integer;
    procedure TimerHintTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(AArea: TRect; ATxt: string);
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
    {$IFNDEF COMPILER3_UP}
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
      {$IFDEF COMPILER3_UP} override; {$ENDIF COMPILER3_UP}
  end;

procedure RegisterHtHints;

implementation

//=== TJvHint ================================================================

constructor TJvHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TimerHint := TTimer.Create(Self);
  TimerHint.Enabled := False;
  TimerHint.Interval := 50;
  TimerHint.OnTimer := TimerHintTimer;
  HintWindow := THintWindowClass.Create(Self);
  FAutoHide := True;
end;

destructor TJvHint.Destroy;
begin
  TimerHint.Free;
  HintWindow.Free;
  inherited Destroy;
end;

procedure TJvHint.ActivateHint(AArea: TRect; ATxt: string);
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
    {$IFDEF COMPILER2}
    R := Rect(0, 0, Screen.Width, 0);
    DrawText(HintWindow.Canvas.Handle, PChar(Txt), -1, R, DT_CALCRECT or DT_LEFT or
      DT_WORDBREAK or DT_NOPREFIX);
    Inc(R.Right, 6);
    Inc(R.Bottom, 2);
    {$ELSE}
    R := HintWindow.CalcHintRect(Screen.Width, Txt, nil);
    {$ENDIF COMPILER2}
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
            HintWindow.Caption := 'Jv hint';
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
          HintWindow.Caption := 'Jv hint';
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

//=== TJvHTHintWindow ========================================================

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

{$IFNDEF COMPILER3_UP}
procedure TJvHTHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  R: TRect;
begin
  R := CalcHintRect(Screen.Width, AHint, nil);
  inherited ActivateHint(Bounds(Rect.Left, Rect.Top, R.Right, R.Bottom + 4),
    AHint);
end;
{$ENDIF}

function TJvHTHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;

  function Max(X, Y: Integer): Integer;
  begin
    if X > Y then
      Result := X
    else
      Result := Y;
  end;

begin
  HtLabel.Caption := AHint;
  Result := Bounds(0, 0, HtLabel.Width + 6, HtLabel.Height + 2);
  if Application.HintHidePause > 0 then
    Application.HintHidePause := Max(2500 {default},
      Length(ItemHtPlain(AHint)) *
      (1000 div 20) { 20 symbols per second });
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

