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

The Original Code is: JvSplit.PAS, released on 2002-07-04.

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

unit JvQSplit;

interface

uses  
  QControls, QExtCtrls, QForms, QGraphics, Types, QWindows, 
  SysUtils, Classes,
  JvQComponent;

type
  TSplitterStyle = (spUnknown, spHorizontalFirst, spHorizontalSecond,
    spVerticalFirst, spVerticalSecond);
  TInverseMode = (imNew, imClear, imMove);
  TSplitterMoveEvent = procedure(Sender: TObject; X, Y: Integer;
    var AllowChange: Boolean) of object;

  TJvxSplitter = class(TJvCustomPanel)
  private
    FControlFirst: TControl;
    FControlSecond: TControl;
    FSizing: Boolean;
    FStyle: TSplitterStyle;
    FPrevOrg: TPoint;
    FOffset: TPoint;
    FNoDropCursor: Boolean;
    FLimitRect: TRect;
    FTopLeftLimit: Integer;
    FBottomRightLimit: Integer;
    FForm: TCustomForm;
    FActiveControl: TWinControl;
    FAppShowHint: Boolean;
    FOldKeyDown: TKeyEvent;
    FOnPosChanged: TNotifyEvent;
    FOnPosChanging: TSplitterMoveEvent;
    function FindControl: TControl;
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StartInverseRect;
    procedure EndInverseRect(X, Y: Integer; AllowChange, Apply: Boolean);
    function GetAlign: TAlign;
    procedure MoveInverseRect(X, Y: Integer; AllowChange: Boolean);
    procedure ShowInverseRect(X, Y: Integer; Mode: TInverseMode);
    procedure DrawSizingLine(Split: TPoint);
    function GetStyle: TSplitterStyle;
    function GetCursor: TCursor;
    procedure SetControlFirst(Value: TControl);
    procedure SetControlSecond(Value: TControl);
    procedure SetAlign(Value: TAlign);
    procedure StopSizing(X, Y: Integer; Apply: Boolean);
    procedure CheckPosition(var X, Y: Integer);
    procedure ReadOffset(Reader: TReader);
    procedure WriteOffset(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Changed; dynamic;
    procedure Changing(X, Y: Integer; var AllowChange: Boolean); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateState;
  published
    property ControlFirst: TControl read FControlFirst write SetControlFirst;
    property ControlSecond: TControl read FControlSecond write SetControlSecond;
    property Align: TAlign read GetAlign write SetAlign default alNone;
    property Constraints;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property Enabled;
    property Color; 
    property Cursor read GetCursor stored False;
    property TopLeftLimit: Integer read FTopLeftLimit write FTopLeftLimit default 20;
    property BottomRightLimit: Integer read FBottomRightLimit write FBottomRightLimit default 20;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnPosChanged: TNotifyEvent read FOnPosChanged write FOnPosChanged;
    property OnPosChanging: TSplitterMoveEvent read FOnPosChanging write FOnPosChanging;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

implementation

const
  InverseThickness = 2;
  DefWidth = 3;

function CToC(C1, C2: TControl; P: TPoint): TPoint;
begin
  Result := C1.ScreenToClient(C2.ClientToScreen(P));
end;

type
  TJvHack = class(TWinControl);

constructor TJvxSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents,
    csOpaque, csDoubleClicks];  // csAcceptsControls
  Width := 185;
  Height := DefWidth;
  FSizing := False;
  FTopLeftLimit := 20;
  FBottomRightLimit := 20;
  FControlFirst := nil;
  FControlSecond := nil; 
end;

procedure TJvxSplitter.Loaded;
begin
  inherited Loaded;
  UpdateState;
end;

procedure TJvxSplitter.DefineProperties(Filer: TFiler); { for backward compatibility }
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('LimitOffset', ReadOffset, WriteOffset, False);
end;

procedure TJvxSplitter.ReadOffset(Reader: TReader);
var
  I: Integer;
begin
  I := Reader.ReadInteger;
  FTopLeftLimit := I;
  FBottomRightLimit := I;
end;

procedure TJvxSplitter.WriteOffset(Writer: TWriter);
begin
  Writer.WriteInteger(FTopLeftLimit);
end;

procedure TJvxSplitter.UpdateState;
begin
  inherited Cursor := Cursor;
end;

function TJvxSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft:
      Dec(P.X);
    alRight:
      Inc(P.X, Width);
    alTop:
      Dec(P.Y);
    alBottom:
      Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if PtInRect(Result.BoundsRect, P) then
      Exit;
  end;
  Result := nil;
end;

procedure TJvxSplitter.CheckPosition(var X, Y: Integer);
begin
  if X - FOffset.X < FLimitRect.Left then
    X := FLimitRect.Left + FOffset.X
  else
  if X - FOffset.X + Width > FLimitRect.Right then
    X := FLimitRect.Right - Width + FOffset.X;
  if Y - FOffset.Y < FLimitRect.Top then
    Y := FLimitRect.Top + FOffset.Y
  else
  if Y - FOffset.Y + Height > FLimitRect.Bottom then
    Y := FLimitRect.Bottom + FOffset.Y - Height;
end;

procedure TJvxSplitter.StartInverseRect;
var
  R: TRect;
  W: Integer;
begin
  if Parent = nil then
    Exit;
  R := Parent.ClientRect;
  FLimitRect.TopLeft := CToC(Self, Parent, Point(R.Left + FTopLeftLimit,
    R.Top + FTopLeftLimit));
  FLimitRect.BottomRight := CToC(Self, Parent, Point(R.Right - R.Left -
    FBottomRightLimit, R.Bottom - R.Top - FBottomRightLimit));
  FNoDropCursor := False;
  FForm := ValidParentForm(Self); 
  with FForm.Canvas do
  begin
    Pen.Color := clWhite;
    if FStyle in [spHorizontalFirst, spHorizontalSecond] then
      W := Height
    else
      W := Width;
    if W > InverseThickness + 1 then
      W := W - InverseThickness
    else
      W := InverseThickness;
    Pen.Width := W;
    Pen.Mode := pmXOR;
  end;
  ShowInverseRect(Width div 2, Height div 2, imNew);
end;

procedure TJvxSplitter.EndInverseRect(X, Y: Integer; AllowChange, Apply: Boolean);
const
  DecSize = 3;
var
  NewSize: Integer;
  Rect: TRect;
  W, H: Integer; 
  P: TPoint;
begin
  if FForm <> nil then
  begin
    ShowInverseRect(0, 0, imClear); 
    FForm := nil;
  end;
  FNoDropCursor := False;
  if Parent = nil then
    Exit;
  Rect := Parent.ClientRect;
  H := Rect.Bottom - Rect.Top - Height;
  W := Rect.Right - Rect.Left - Width;
  if not AllowChange then
  begin
    P := ScreenToClient(FPrevOrg);
    X := P.X + FOffset.X - Width div 2;
    Y := P.Y + FOffset.Y - Height div 2
  end;
  if not Apply then
    Exit;
  CheckPosition(X, Y);
  if (ControlFirst.Align = alRight) or
    ((ControlSecond <> nil) and (ControlSecond.Align = alRight)) then
  begin
    X := -X;
    FOffset.X := -FOffset.X;
  end;
  if (ControlFirst.Align = alBottom) or
    ((ControlSecond <> nil) and (ControlSecond.Align = alBottom)) then
  begin
    Y := -Y;
    FOffset.Y := -FOffset.Y;
  end;
  Parent.DisableAlign;
  try
    if FStyle = spHorizontalFirst then
    begin
      NewSize := ControlFirst.Height + Y - FOffset.Y;
      if NewSize <= 0 then
        NewSize := 1;
      if NewSize >= H then
        NewSize := H - DecSize;
      ControlFirst.Height := NewSize;
    end
    else
    if FStyle = spHorizontalSecond then
    begin
      NewSize := ControlSecond.Height + Y - FOffset.Y;
      if NewSize <= 0 then
        NewSize := 1;
      if NewSize >= H then
        NewSize := H - DecSize;
      ControlSecond.Height := NewSize;
    end
    else
    if FStyle = spVerticalFirst then
    begin
      NewSize := ControlFirst.Width + X - FOffset.X;
      if NewSize <= 0 then
        NewSize := 1;
      if NewSize >= W then
        NewSize := W - DecSize;
      ControlFirst.Width := NewSize;
    end
    else
    if FStyle = spVerticalSecond then
    begin
      NewSize := ControlSecond.Width + X - FOffset.X;
      if NewSize <= 0 then
        NewSize := 1;
      if NewSize >= W then
        NewSize := W - DecSize;
      ControlSecond.Width := NewSize;
    end;
  finally
    Parent.EnableAlign;
  end;
end;

procedure TJvxSplitter.MoveInverseRect(X, Y: Integer; AllowChange: Boolean);
var
  P: TPoint;
  NoDrop: Boolean;
begin
  if not AllowChange then
  begin
    SetCursor(Screen.Cursors[crNoDrop]);
    Exit;
  end;
  P := Point(X, Y);
  CheckPosition(X, Y);
  NoDrop := not AllowChange or (((X <> P.X) and (FStyle in [spVerticalFirst,
    spVerticalSecond])) or ((Y <> P.Y) and (FStyle in [spHorizontalFirst,
      spHorizontalSecond])));
  if NoDrop <> FNoDropCursor then
  begin
    FNoDropCursor := NoDrop;
    if NoDrop then
      SetCursor(Screen.Cursors[crNoDrop])
    else
      SetCursor(Screen.Cursors[Cursor]);
  end;
  ShowInverseRect(X - FOffset.X + Width div 2, Y - FOffset.Y + Height div 2,
    imMove);
end;

procedure TJvxSplitter.ShowInverseRect(X, Y: Integer; Mode: TInverseMode);
var
  P: TPoint;
  MaxRect: TRect;
  Horiz: Boolean;
begin
  P := Point(0, 0);
  if FStyle in [spHorizontalFirst, spHorizontalSecond] then
  begin
    P.Y := Y;
    Horiz := True;
  end
  else
  begin
    P.X := X;
    Horiz := False;
  end;
  MaxRect := Parent.ClientRect;
  P := ClientToScreen(P);
  with P, MaxRect do
  begin
    TopLeft := Parent.ClientToScreen(TopLeft);
    BottomRight := Parent.ClientToScreen(BottomRight);
    if X < Left then
      X := Left;
    if X > Right then
      X := Right;
    if Y < Top then
      Y := Top;
    if Y > Bottom then
      Y := Bottom;
  end;
  if Mode = imMove then
    if ((P.X = FPrevOrg.X) and not Horiz) or
      ((P.Y = FPrevOrg.Y) and Horiz) then
      Exit;
  if Mode in [imClear, imMove] then
    DrawSizingLine(FPrevOrg);
  if Mode in [imNew, imMove] then
  begin
    DrawSizingLine(P);
    FPrevOrg := P;
  end;
end;

procedure TJvxSplitter.DrawSizingLine(Split: TPoint);
var
  P: TPoint;
begin
  if FForm <> nil then
  begin
    P := FForm.ScreenToClient(Split);
    with FForm.Canvas do
    begin
      MoveTo(P.X, P.Y);
      if FStyle in [spHorizontalFirst, spHorizontalSecond] then
        LineTo(CToC(FForm, Self, Point(Width, 0)).X, P.Y)
      else
        LineTo(P.X, CToC(FForm, Self, Point(0, Height)).Y);
    end;
  end;
end;

function TJvxSplitter.GetStyle: TSplitterStyle;
begin
  Result := spUnknown;
  if ControlFirst <> nil then
  begin
    if ((ControlFirst.Align = alTop) and ((ControlSecond = nil) or
      (ControlSecond.Align = alClient))) or
      ((ControlFirst.Align = alBottom) and ((ControlSecond = nil) or
      (ControlSecond.Align = alClient))) then
      Result := spHorizontalFirst
    else
    if ((ControlFirst.Align = alClient) and (ControlSecond <> nil) and
      (ControlSecond.Align = alBottom)) or
      ((ControlFirst.Align = alClient) and (ControlSecond <> nil) and
      (ControlSecond.Align = alTop)) then
      Result := spHorizontalSecond
    else
    if ((ControlFirst.Align = alLeft) and ((ControlSecond = nil) or
      (ControlSecond.Align = alClient))) or
      ((ControlFirst.Align = alRight) and ((ControlSecond = nil) or
      (ControlSecond.Align = alClient))) then
      Result := spVerticalFirst
    else
    if ((ControlFirst.Align = alClient) and (ControlSecond <> nil) and
      (ControlSecond.Align = alRight)) or
      ((ControlFirst.Align = alClient) and (ControlSecond <> nil) and
      (ControlSecond.Align = alLeft)) then
      Result := spVerticalSecond;
    case Result of
      spHorizontalFirst, spVerticalFirst:
        if Align <> FControlFirst.Align then
          Result := spUnknown;
      spHorizontalSecond, spVerticalSecond:
        if Align <> FControlSecond.Align then
          Result := spUnknown;
    end;
  end;
end;

procedure TJvxSplitter.SetAlign(Value: TAlign);
begin
  if not (Align in [alTop, alBottom, alLeft, alRight]) then
  begin
    inherited Align := Value;
    if not (csReading in ComponentState) then
    begin
      if Value in [alTop, alBottom] then
        Height := DefWidth
      else
      if Value in [alLeft, alRight] then
        Width := DefWidth;
    end;
  end
  else
    inherited Align := Value;
  if (ControlFirst = nil) and (ControlSecond = nil) then
    ControlFirst := FindControl;
end;

function TJvxSplitter.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

function TJvxSplitter.GetCursor: TCursor;
begin
  Result := crDefault;
  case GetStyle of
    spHorizontalFirst, spHorizontalSecond:
      Result := crVSplit;
    spVerticalFirst, spVerticalSecond:
      Result := crHSplit;
  end;
end;

procedure TJvxSplitter.SetControlFirst(Value: TControl);
begin
  if Value <> FControlFirst then
  begin
    if (Value = Self) or (Value is TForm) then
      FControlFirst := nil
    else
    begin
      FControlFirst := Value;
      if Value <> nil then
        Value.FreeNotification(Self);
    end;
    UpdateState;
  end;
end;

procedure TJvxSplitter.SetControlSecond(Value: TControl);
begin
  if Value <> FControlSecond then
  begin
    if (Value = Self) or (Value is TForm) then
      FControlSecond := nil
    else
    begin
      FControlSecond := Value;
      if Value <> nil then
        Value.FreeNotification(Self);
    end;
    UpdateState;
  end;
end;

procedure TJvxSplitter.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = ControlFirst then
      ControlFirst := nil
    else
    if AComponent = ControlSecond then
      ControlSecond := nil;
  end;
end;

procedure TJvxSplitter.Changed;
begin
  if Assigned(FOnPosChanged) then
    FOnPosChanged(Self);
end;

procedure TJvxSplitter.Changing(X, Y: Integer; var AllowChange: Boolean);
begin
  if Assigned(FOnPosChanging) then
    FOnPosChanging(Self, X, Y, AllowChange);
end;

procedure TJvxSplitter.StopSizing(X, Y: Integer; Apply: Boolean);
var
  AllowChange: Boolean;
begin
  if FSizing then
  begin
    ReleaseCapture;
    AllowChange := Apply;
    if Apply then
      Changing(X, Y, AllowChange);
    EndInverseRect(X, Y, AllowChange, Apply);
    FSizing := False;
    Application.ShowHint := FAppShowHint;
    if Assigned(FActiveControl) then
    begin
      TJvHack(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
    if Apply then
      Changed;
  end;
end;

procedure TJvxSplitter.ControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
  StopSizing(0, 0, False);
end;

procedure TJvxSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not (csDesigning in ComponentState) and (Button = mbLeft) then
  begin
    FStyle := GetStyle;
    if FStyle <> spUnknown then
    begin
      FSizing := True;
      FAppShowHint := Application.ShowHint;
      SetCapture(Handle);
      with ValidParentForm(Self) do
      begin
        if ActiveControl <> nil then
          FActiveControl := ActiveControl
        else
          FActiveControl := GetParentForm(Self);
        FOldKeyDown := TJvHack(FActiveControl).OnKeyDown;
        TJvHack(FActiveControl).OnKeyDown := ControlKeyDown;
      end;
      Application.ShowHint := False;
      FOffset := Point(X, Y);
      StartInverseRect;
    end;
  end;
end;

procedure TJvxSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AllowChange: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if (GetCapture = Handle) and FSizing then
  begin
    AllowChange := True;
    Changing(X, Y, AllowChange);
    MoveInverseRect(X, Y, AllowChange);
  end;
end;

procedure TJvxSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopSizing(X, Y, True);
  inherited MouseUp(Button, Shift, X, Y);
end;

end.

