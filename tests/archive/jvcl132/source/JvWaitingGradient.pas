{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWaitingGradient.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvWaitingGradient;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, JVCLVer;

type
  TJvWaitingThread = class(TThread)
  protected
    procedure Draw;
    procedure Execute; override;
  public
    FDelay: Cardinal;
    FOnDraw: TNotifyEvent;
  end;

  TJvWaitingGradient = class(TGraphicControl)
  private
    FTimerTag: Integer;
    FBitmap: TBitmap;
    FLeft: Integer;
    FWidth: Integer;
    FStartColor: TColor;
    FEndColor: TColor;
    FStart: TColor;
    FEnd: TColor;
    FRect: TRect;
    FDRect: TRect;
    FLoading: Boolean;
    FInterval: Cardinal;
    FEnabled: Boolean;
    FScroll: TJvWaitingThread;
    FAboutJVCL: TJVCLAboutInfo;
    procedure Deplace(Sender: TObject);
    procedure CreateBitmap;
    procedure SetWidth(const Value: Integer);
    procedure SetEndColor(const Value: TColor);
    procedure SetStartColor(const Value: TColor);
    procedure SetInterval(const Value: Cardinal);
    procedure SetEnable(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Color;
    property GradientWidth: Integer read FWidth write SetWidth;
    property StartColor: TColor read FStart write SetStartColor default clBtnFace;
    property EndColor: TColor read Fend write SetEndColor default clBlack;
    property Interval: Cardinal read FInterval write SetInterval default 50;
    property Enabled: Boolean read FEnabled write SetEnable default True;
    property Visible;
    property Align;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvWaitingGradient
///////////////////////////////////////////////////////////

constructor TJvWaitingGradient.Create(AOwner: TComponent);
begin
  FLoading := True;
  inherited;
  Parent := TWinControl(AOwner);
  Color := clBtnFace;
  Width := 100;
  Height := 10;
  FInterval := 50;
  FEnabled := True;

  FStart := clBtnFace;
  FEnd := clBlack;
  FWidth := 50;
  FLeft := -FWidth;
  FBitmap := TBitmap.Create;
  CreateBitmap;
  FRect := Rect(0, 0, FWidth, Height);
  FDRect := Rect(0, 0, FWidth, Height);
  FTImertag := 0;

  FScroll := TJvWaitingThread.Create(True);
  FScroll.FreeOnTerminate := False;
  FScroll.FDelay := FInterval;
  FScroll.FOnDraw := Deplace;
  FScroll.Resume;

  FLoading := False;
end;

{**************************************************}

procedure TJvWaitingGradient.CreateBitmap;
var
  i: Integer;
  j: Real;
  Deltas: array[0..2] of Real; //R,G,B
  r: TRect;
  FSteps: Integer;
begin
  if FTimerTag = 0 then
  begin
    if FStart < 0 then
      FStartColor := GetSysColor(FStart and not $80000000)
    else
      FStartColor := FStart;
    if FEnd < 0 then
      FEndColor := GetSysColor(FEnd and not $80000000)
    else
      FEndColor := FEnd;
  end
  else
  begin
    if FStart < 0 then
      FEndColor := GetSysColor(FStart and not $80000000)
    else
      FEndColor := FStart;
    if FEnd < 0 then
      FStartColor := GetSysColor(FEnd and not $80000000)
    else
      FStartColor := FEnd;
  end;

  FBItmap.Width := FWidth;
  FBitmap.Height := Height;

  FSteps := FWidth;
  if FSteps > Width then
    FSteps := Width;
  Deltas[0] := (GetRValue(FEndColor) - GetRValue(FStartColor)) / FSteps;
  Deltas[1] := (GetGValue(FEndColor) - GetGValue(FStartColor)) / FSteps;
  Deltas[2] := (GetBValue(FEndColor) - GetBValue(FStartColor)) / FSteps;
  FBitmap.Canvas.Brush.Style := bsSolid;
  j := FWidth / FSteps;
  for i := 0 to FSteps do
  begin
    r.Top := 0;
    r.Bottom := Height;
    r.Left := Round(i * j);
    r.Right := Round((i + 1) * j);
    FBitmap.Canvas.Brush.Color := RGB(Round(GetRValue(FStartColor) + i * Deltas[0]), Round(GetGValue(FStartColor) + i *
      Deltas[1]), Round(GetBValue(FStartColor) + i * Deltas[2]));
    FBitmap.Canvas.FillRect(r);
  end;
end;

{**************************************************}

procedure TJvWaitingGradient.Deplace(Sender: TObject);
begin
  if FBitmap = nil then
    Exit;
  try
    if FTimerTag = 0 then
    begin
      if FLeft + FWidth >= Width then
      begin
        FTimerTag := 1;
        CreateBitmap;
        FLeft := Width;
      end
      else
        FLeft := FLeft + 2;
    end
    else
    begin
      if FLeft <= 0 then
      begin
        FTimerTag := 0;
        CreateBitmap;
        FLeft := -FWidth;
      end
      else
        FLeft := FLeft - 2;
    end;
    FDRect.Left := FLeft;
    FDRect.Right := FLeft + FWidth;

    Paint;
  except
  end;
end;

{**************************************************}

destructor TJvWaitingGradient.Destroy;
begin
  FBitmap.Free;
  FBitmap := nil;
  FScroll.FOnDraw := nil;
  FScroll.Terminate;
  FScroll.Free;
  inherited;
end;

{**************************************************}

procedure TJvWaitingGradient.Paint;
begin
  if Canvas <> nil then
  begin
    //paint
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(0, 0, FLeft, Height));
    Canvas.FillRect(Rect(FLeft + FBitmap.Width, 0, Width, Height));

    Canvas.CopyRect(FDRect, FBItmap.Canvas, FRect);
  end;
end;

{**************************************************}

procedure TJvWaitingGradient.Resize;
begin
  inherited;
  FRect := Rect(0, 0, FWidth, Height);
  FDRect := Rect(0, 0, FWidth, Height);
  if not FLoading then
    CreateBitmap;
end;

{**************************************************}

procedure TJvWaitingGradient.SetEnable(const Value: Boolean);
begin
  FEnabled := Value;
  if Value then
    FScroll.Resume
  else
    FScroll.Suspend;
end;

{**************************************************}

procedure TJvWaitingGradient.SetEndColor(const Value: TColor);
begin
  Fend := Value;
  if not FLoading then
    CreateBitmap;
end;

{**************************************************}

procedure TJvWaitingGradient.SetInterval(const Value: Cardinal);
begin
  FInterval := Value;
  FScroll.FDelay := Value;
end;

{**************************************************}

procedure TJvWaitingGradient.SetStartColor(const Value: TColor);
begin
  FStart := Value;
  if not FLoading then
    CreateBitmap;
end;

{**************************************************}

procedure TJvWaitingGradient.SetWidth(const Value: Integer);
begin
  if Value > 0 then
  begin
    FWidth := Value;
    FLeft := -FWidth;
    FRect := Rect(0, 0, FWidth, Height);
    FDRect := Rect(0, 0, FWidth, Height);
    if not FLoading then
      CreateBitmap;
  end
  else
    Beep;
end;

///////////////////////////////////////////////////////////
// TJvWaitingThread
///////////////////////////////////////////////////////////

procedure TJvWaitingThread.Draw;
begin
  if Assigned(FOnDraw) then
    FOnDraw(nil);
end;

{**************************************************}

procedure TJvWaitingThread.Execute;
begin
  while not Terminated and Assigned(FOnDraw) do
  begin
    Synchronize(Draw);
    Sleep(FDelay);
  end;
end;

end.
