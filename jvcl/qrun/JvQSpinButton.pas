{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QSpinButtom.pas, released on 2003-09-25

The Initial Developer of the Original Code: André Snepvangers

Copyright (c) 2003 André Snepvangers (asn@xs4all.nl)

All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit JvQSpinButton;

interface

uses
  SysUtils, Classes,
  DateUtils, Types,
  QButtons, QControls, QStdCtrls, QExtCtrls, QGraphics, Qt,
  JvQSpeedButton, JvQComponent;

type
  TJvSpinButton = class (TJvWinControl)
  private
    FUpButton: TJvSpeedButton;
    FDownButton: TJvSpeedButton;
    FFocusedButton: TJvSpeedButton;
    FFocusControl: TWidgetControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    procedure SetFlat(value:boolean);
    function GetFlat: boolean;
    function CreateButton: TTimerSpeedButton;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    function GetUpNumGlyphs: TNumGlyphs;
    function GetDownNumGlyphs: TNumGlyphs;
    procedure SetUpNumGlyphs(Value: TNumGlyphs);
    procedure SetDownNumGlyphs(Value: TNumGlyphs);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TTimerSpeedButton);
    procedure AdjustToSize (var W, H: Integer);
  protected
    procedure AdjustSize ; override ;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
    procedure DoExit; override;
    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs: TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
    property DragMode;
    property Enabled;
    property Flat: Boolean read GetFlat write SetFlat;
    property FocusControl: TWidgetControl read FFocusControl write FFocusControl;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property UpNumGlyphs: TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;



implementation

{$R spin.res}

{ TSpinButton }

constructor TJvSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];
  FUpButton := CreateButton;
  FDownButton := CreateButton;
  UpGlyph := nil;
  DownGlyph := nil;
  InputKeys := [ikArrows];
  Width := 20;
  Height := 25;
  FFocusedButton := FUpButton;
end;

function TJvSpinButton.CreateButton: TJvSpeedButton;
begin
  Result := TJvSpeedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.AllowTimer := True;
  Result.Parent := Self;
end;

procedure TJvSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TJvSpinButton.AdjustToSize(var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then Exit;
  if W < 15 then W := 15;
  FUpButton.SetBounds(0, 0, W, H div 2);
  FDownButton.SetBounds(0, FUpButton.Height - 1, W, H - FUpButton.Height + 1);
end;

procedure TJvSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustToSize(W, H);
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TJvSpinButton.AdjustSize;
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustToSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
end;

procedure TJvSpinButton.DoEnter;
begin
  inherited;
//  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TJvSpinButton.DoExit;
begin
  inherited;
//  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TJvSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    Integer(Key_Up):
      begin
        SetFocusBtn (FUpButton);
        FUpButton.Click;
      end;
    Integer(Key_Down):
      begin
        SetFocusBtn (FDownButton);
        FDownButton.Click;
      end;
    Integer(Key_Space):
      FFocusedButton.Click;
  end;
end;

procedure TJvSpinButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn(TJvSpeedButton(Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and
        FFocusControl.CanFocus and ( not Focused ) then
      FFocusControl.SetFocus
    else if TabStop and (not Focused) and CanFocus then
      SetFocus;
  end;
end;

procedure TJvSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
  begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else
    if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TJvSpinButton.SetFocusBtn(Btn: TTimerSpeedButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
//    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if Focused then
    begin
//       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
       Invalidate;
    end;
  end;
end;

procedure TJvSpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustToSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

procedure TJvSpinButton.SetFlat(value:boolean);
begin
  if assigned(FDownButton)
  then
  begin
    FDownButton.Flat := value;
    FUpButton.Flat := value;
  end;
end;

function TJvSpinButton.GetFlat: boolean;
begin
  if assigned(FDownButton)
  then
    Result := FDownButton.Flat
  else
    Result := false;
end;

function TJvSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TJvSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpButton.Glyph := Value
  else
  begin
    FUpButton.Glyph.LoadFromResourceName(HInstance, 'SpinUp');
    FUpButton.NumGlyphs := 1;
    FUpButton.Invalidate;
  end;
end;

function TJvSpinButton.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

procedure TJvSpinButton.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

function TJvSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TJvSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownButton.Glyph := Value
  else
  begin
    FDownButton.Glyph.LoadFromResourceName(HInstance, 'SpinDown');
    FUpButton.NumGlyphs := 1;
    FDownButton.Invalidate;
  end;
end;

function TJvSpinButton.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

procedure TJvSpinButton.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;


(* asn: looks ugly at small sizes
procedure TTimerSpeedButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if tbFocusRect in FTimeBtnState then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then
      OffsetRect(R, 1, 1);
    Canvas.DrawFocusRect(R);
  end;
end;
*)

end.
