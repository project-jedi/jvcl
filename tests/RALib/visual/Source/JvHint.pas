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

component   : TJvHint
description : Custom activated hint

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvHtControls;

type

  TJvHint = class(TComponent)
  protected
    R, Area : TRect;
    Txt : string;
    State : (tmBeginShow, tmShowing, tmStopped);
    HintWindow : THintWindow;
    timerHint : TTimer;
    FAutoHide : boolean;
    FDelay: integer;
    procedure timerHintTimer(Sender: TObject);
  public
    constructor Create(lOwner : TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(lArea : TRect; lTxt : string);
    procedure CancelHint;
  published
    property AutoHide : boolean read FAutoHide write FAutoHide default true;
  end;

  TJvHTHintWindow = class(THintWindow)
  private
    htLabel: TJvHTLabel;
  protected
    procedure Paint; override;
  public
   {$IFNDEF COMPILER3_UP}
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
   {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; {$IFDEF COMPILER3_UP} override; {$ENDIF COMPILER3_UP}
  end;

  procedure RegisterHtHints;

implementation


constructor TJvHint.Create(lOwner : TComponent);
begin
  inherited Create(lOwner);
  timerHint := TTimer.Create(self);
  timerHint.Enabled  := false;
  timerHint.Interval := 50;
  timerHint.OnTimer  := timerHintTimer;
  HintWindow := THintWindowClass.Create(Self);
  FAutoHide := true;
end;

destructor TJvHint.Destroy;
begin
  timerHint.Free;
  HintWindow.Free;
  inherited Destroy;
end;

procedure TJvHint.ActivateHint(lArea : TRect; lTxt : string);
var
  P : TPoint;
begin
  GetCursorPos(P);
  Area := lArea;
  if lTxt = '' then
  begin
    CancelHint;
    exit;
  end else
    Txt := lTxt;
  if not PtInRect(Area, P) then
  begin
    if IsWindowVisible(HintWindow.Handle) then
      ShowWindow(HintWindow.Handle, SW_HIDE);
    exit;
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
    R.Top  := P.Y + 20;
    R.Left := P.X;
    inc(R.Bottom, R.Top);
    inc(R.Right, R.Left);
    State := tmBeginShow;
    timerHint.Enabled := true;
  end;
end;

procedure TJvHint.timerHintTimer(Sender: TObject);
var
  P : TPoint;
  bPoint, bDelay : boolean;
  Delay : integer;
  HintPause : integer;
begin
  HintWindow.Color := Application.HintColor;
  Delay := FDelay * integer(timerHint.Interval);
  case State of
    tmBeginShow :
      begin
        GetCursorPos(P);
        bPoint := not PtInRect(Area, P);
        if bPoint then
        begin
          State := tmStopped;
          exit;
        end;
        if IsWindowVisible(HintWindow.Handle) then
          HintPause := Application.HintShortPause else
          HintPause := Application.HintPause;
        if Delay >= HintPause then
        begin
          HintWindow.ActivateHint(R, Txt);
          FDelay := 0;
          State := tmShowing;
        end else
          inc(FDelay);
      end;{tmBeginShow}
    tmShowing :
      begin
        GetCursorPos(P);
        bDelay := FAutoHide and (Delay > Application.HintHidePause);
        bPoint := not PtInRect(Area, P);
        if bPoint or bDelay then
        begin
          if IsWindowVisible(HintWindow.Handle) then
            ShowWindow(HintWindow.Handle, SW_HIDE);
          FDelay := 0;
          if bPoint then HintWindow.Caption := 'Это hint';
          State := tmStopped;
        end else
          inc(FDelay);
      end;{tmShowing}
    tmStopped:
      begin
        FDelay := 0;
        GetCursorPos(P);
        bPoint := not PtInRect(Area, P);
        if IsWindowVisible(HintWindow.Handle) then
          ShowWindow(HintWindow.Handle, SW_HIDE);
        if bPoint then
        begin
          HintWindow.Caption := 'Это hint';
          timerHint.Enabled := false;
        end;
      end;{tmStopped}
  end;{case}
end;

procedure TJvHint.CancelHint;
begin
  if IsWindowVisible(HintWindow.Handle) then
    ShowWindow(HintWindow.Handle, SW_HIDE);
  HintWindow.Caption := '';
end;


{ Color hints }

constructor TJvHTHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  htLabel := TJvHTLabel.Create(Self);
  htLabel.Parent := Self;
  htLabel.SetBounds(2, 2, 0, 0);
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

  function Max(x,y:integer):integer;
  begin
    if x > y then Result := x else Result := y;
  end;
  
begin
  htLabel.Caption := AHint;
  Result := Bounds(0, 0, htLabel.Width + 6, htLabel.Height + 2);
  if Application.HintHidePause > 0 then
    Application.HintHidePause := Max(2500 {default},
      Length(ItemHtPlain(AHint)) *
      (1000 div 20) { 20 symbols per second } );
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
