{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWaitingGradient.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQWaitingGradient;

interface

uses
  SysUtils, Classes,
  
  
  Types, QGraphics, QControls, QWindows,
  
  JvQImageDrawThread, JvQComponent;

type
  TJvWaitingGradient = class(TJvGraphicControl)
  private
    FFromLeftToRight: Boolean; { Indicates direction }
    FBitmap: TBitmap;
    FLeftOffset: Integer;
    FGradientWidth: Integer;
    FStartColor: TColor;
    FEndColor: TColor;
    FSourceRect: TRect;
    FDestRect: TRect;
    FScroll: TJvImageDrawThread;
    FAlwaysRestart: Boolean;
    procedure Deplace(Sender: TObject);
    procedure UpdateBitmap;
    function GetActive: Boolean;
    function GetInterval: Cardinal;
    procedure SetGradientWidth(const Value: Integer);
    procedure SetEndColor(const Value: TColor);
    procedure SetStartColor(const Value: TColor);
    procedure SetInterval(const Value: Cardinal);
    procedure SetActive(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Restart;
  published
    // (rom) renamed Active
    property Active: Boolean read GetActive write SetActive default False;
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Cursor;
    
    property DragMode;
    property GradientWidth: Integer read FGradientWidth write SetGradientWidth;
    property Enabled;
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    property Height default 10;

    property Interval: Cardinal read GetInterval write SetInterval default 50;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property AlwaysRestart: Boolean read FAlwaysRestart write FAlwaysRestart default False;
    property StartColor: TColor read FStartColor write SetStartColor default clBtnFace;
    property Visible;
    property Width default 100;

    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnDblClick;
    property OnContextPopup;
  end;

implementation

uses
  JvQTypes;

constructor TJvWaitingGradient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {(rb) csOpaque included }
  ControlStyle := ControlStyle + [csOpaque];

  FBitmap := TBitmap.Create;

  FStartColor := clBtnFace;
  FEndColor := clBlack;
  FGradientWidth := 50;
  FLeftOffset := -FGradientWidth;
  FSourceRect := Rect(0, 0, FGradientWidth, Height);
  FDestRect := Rect(0, 0, FGradientWidth, Height);
  FFromLeftToRight := True;

  FScroll := TJvImageDrawThread.Create(True);
  FScroll.FreeOnTerminate := False;
  FScroll.Delay := 50;
  FScroll.OnDraw := Deplace;
  // (p3) don't set color: it will set ParentColor to False
//  Color := clBtnFace;

  { (rb) Set the size properties last; will trigger Resize }
  // (rom) also always set the default values
  Height := 10;
  Width := 100;
end;

destructor TJvWaitingGradient.Destroy;
begin
  FScroll.OnDraw := nil;
  FScroll.Terminate;
  //  FScroll.WaitFor;
  FreeAndNil(FScroll);

  FBitmap.Free;
  FBitmap := nil;
  inherited Destroy;
end;

procedure TJvWaitingGradient.Loaded;
begin
  inherited Loaded;
  UpdateBitmap;
  if Active then
    FScroll.Resume;
end;

procedure TJvWaitingGradient.UpdateBitmap;
var
  I: Integer;
  J: Real;
  Deltas: array [0..2] of Single; //R,G,B
  Rect: TRect;
  Steps: Integer;
  LStartColor, LEndColor: Longint;
begin
  if not Assigned(FBitmap) then
    Exit;
  if csLoading in ComponentState then
    Exit;

  if FFromLeftToRight then
  begin
    LStartColor := ColorToRGB(FStartColor);
    LEndColor := ColorToRGB(FEndColor);
  end
  else
  begin
    LStartColor := ColorToRGB(FEndColor);
    LEndColor := ColorToRGB(FStartColor);
  end;

  FBitmap.Width := FGradientWidth;
  FBitmap.Height := Height;

  Steps := FGradientWidth;
  if Steps > Width then
    Steps := Width;
  if Steps <= 0 then
    Exit;

  Deltas[0] := (GetRValue(LEndColor) - GetRValue(LStartColor)) / Steps;
  Deltas[1] := (GetGValue(LEndColor) - GetGValue(LStartColor)) / Steps;
  Deltas[2] := (GetBValue(LEndColor) - GetBValue(LStartColor)) / Steps;
  FBitmap.Canvas.Brush.Style := bsSolid;
  J := FGradientWidth / Steps;
  for I := 0 to Steps do
  begin
    Rect.Top := 0;
    Rect.Bottom := Height;
    Rect.Left := Round(I * J);
    Rect.Right := Round((I + 1) * J);
    FBitmap.Canvas.Brush.Color :=
      RGB(
        Round(GetRValue(LStartColor) + I * Deltas[0]),
        Round(GetGValue(LStartColor) + I * Deltas[1]),
        Round(GetBValue(LStartColor) + I * Deltas[2]));
    FBitmap.Canvas.FillRect(Rect);
  end;
end;

procedure TJvWaitingGradient.Deplace(Sender: TObject);
begin
  if FFromLeftToRight then
  begin
    if FLeftOffset + FGradientWidth >= Width then
    begin
      FFromLeftToRight := False;
      UpdateBitmap;
      FLeftOffset := Width;
    end
    else
      FLeftOffset := FLeftOffset + 2;
  end
  else
  begin
    if FLeftOffset <= 0 then
    begin
      FFromLeftToRight := True;
      UpdateBitmap;
      FLeftOffset := -FGradientWidth;
    end
    else
      FLeftOffset := FLeftOffset - 2;
  end;
  FDestRect.Left := FLeftOffset;
  FDestRect.Right := FLeftOffset + FGradientWidth;

  Repaint;
end;

procedure TJvWaitingGradient.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, FLeftOffset, Height));
  if not Assigned(FBitmap) then
    Exit;
  Canvas.FillRect(Rect(FLeftOffset + FBitmap.Width, 0, Width, Height));
  
  OffsetRect(FDestRect, Left, Top);
  
  Canvas.CopyRect(FDestRect, FBitmap.Canvas, FSourceRect);
  
  OffsetRect(FDestRect, -Left, -Top);
  
end;

procedure TJvWaitingGradient.Resize;
begin
  inherited Resize;
  FSourceRect := Rect(0, 0, FGradientWidth, Height);
  FDestRect := Rect(0, 0, FGradientWidth, Height);
  UpdateBitmap;
end;

function TJvWaitingGradient.GetActive: Boolean;
begin
  Result := not FScroll.Suspended;
end;

procedure TJvWaitingGradient.SetActive(const Value: Boolean);
begin
//  if csLoading in ComponentState then
//    Exit;
  if FScroll = nil then
    Exit;
  if Value and FScroll.Suspended then
  begin
    FScroll.Resume;
    if AlwaysRestart then
      Restart;
  end
  else
  if not Value and not FScroll.Suspended then
    FScroll.Suspend;
end;

procedure TJvWaitingGradient.SetEndColor(const Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    UpdateBitmap;
  end;
end;

function TJvWaitingGradient.GetInterval: Cardinal;
begin
  Result := FScroll.Delay;
end;

procedure TJvWaitingGradient.SetInterval(const Value: Cardinal);
begin
  FScroll.Delay := Value;
end;

procedure TJvWaitingGradient.SetStartColor(const Value: TColor);
begin
  if Value <> FStartColor then
  begin
    FStartColor := Value;
    UpdateBitmap;
  end;
end;

procedure TJvWaitingGradient.SetGradientWidth(const Value: Integer);
begin
  if Value > 0 then
  begin
    FGradientWidth := Value;
    FLeftOffset := -FGradientWidth;
    FSourceRect := Rect(0, 0, FGradientWidth, Height);
    FDestRect := Rect(0, 0, FGradientWidth, Height);
    UpdateBitmap;
  end;
end;

procedure TJvWaitingGradient.Restart;
begin
  FLeftOffset := -FGradientWidth;
  UpdateBitmap;
end;

end.

