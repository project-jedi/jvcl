{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImage.PAS, released on 2001-02-28.

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

unit JvImage;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Graphics, ExtCtrls, Controls, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QExtCtrls, QControls, QForms,
  {$ENDIF VisualCLX}
  JvExExtCtrls;

type
  TPicState = (stDefault, stEntered, stClicked1, stClicked2, stDown);

  TJvPictures = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FPicClicked1: TPicture;
    FPicClicked2: TPicture;
    FPicDown: TPicture;
    FPicEnter: TPicture;
    procedure SetPicClicked(const Value: TPicture);
    procedure SetPicClicked2(const Value: TPicture);
    procedure SetPicDown(const Value: TPicture);
    procedure SetPicEnter(const Value: TPicture);
  protected
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property PicEnter: TPicture read FPicEnter write SetPicEnter;
    property PicClicked1: TPicture read FPicClicked1 write SetPicClicked;
    property PicClicked2: TPicture read FPicClicked2 write SetPicClicked2;
    property PicDown: TPicture read FPicDown write SetPicDown;
  end;

  TJvImage = class(TJvExImage)
  private
    FOnStateChanged: TNotifyEvent;
    FPictures: TJvPictures;
    FState: TPicState;
    FPicture: TPicture;
    FClickCount: Integer;
    FPictureChange: TNotifyEvent;
    procedure SetState(Value: TPicState);
    procedure PicturesChanged(Sender: TObject);
    procedure DoPictureChange(Sender: TObject);
    procedure DoOwnPictureChange(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
    procedure ApplyClick;
    function UsesPictures: Boolean;
  protected
    procedure Click; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    function HitTest(X, Y: Integer): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HintColor;
    property Pictures: TJvPictures read FPictures write FPictures;
    property Picture: TPicture read FPicture write SetPicture;
    property State: TPicState read FState write SetState default stDefault;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

implementation

//=== { TJvImage } ===========================================================

constructor TJvImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := stDefault;
  FPictures := TJvPictures.Create;
  FPictures.OnChanged := PicturesChanged;
  FPicture := TPicture.Create;
  FPicture.OnChange := DoOwnPictureChange;
  FPictureChange := inherited Picture.OnChange;
  inherited Picture.OnChange := DoPictureChange;
end;

destructor TJvImage.Destroy;
begin
  inherited Picture.OnChange := FPictureChange;
  FPictureChange := nil;
  FPictures.Free;
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvImage.ApplyClick;
begin
  case FClickCount of
    1:
      begin
        State := stClicked1;
        if State <> stClicked1 then
        begin
          FClickCount := 0;
          State := stDefault;
        end;
      end;
    2:
      begin
        State := stClicked2;
        if State <> stClicked2 then
        begin
          FClickCount := 0;
          State := stDefault;
        end;
      end;
    0, 3:
      begin
        State := stDefault;
        FClickCount := 0;
      end;
  end;
end;

procedure TJvImage.Click;
begin
  inherited Click;
  if UsesPictures then
  begin
    Inc(FClickCount);
    ApplyClick;
  end;
end;

procedure TJvImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if UsesPictures then
    State := stDown;
end;

procedure TJvImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not UsesPictures or (State = stClicked1) or (State = stClicked2) then
    Exit;
  if (X > 0) and (X < Width) and (Y > 0) and (Y < Height) then
  begin
    SetState(stEntered);
    if State <> stEntered then
      ApplyClick;
  end
  else
    ApplyClick;
end;

procedure TJvImage.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if UsesPictures then
      State := stEntered;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvImage.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    if UsesPictures then
      ApplyClick;
    inherited MouseLeave(Control);
  end;
end;

// (rom) improvement. now only non-transparent pixels are considered
// (rom) part of the clickable area
// (p3) NB!!! This only works if TGraphic is a TBitmap! For PNG and JPG images, the result is that
// the FPicture is cleared when "Assigned(Picture.Bitmap)" is called!
// A (somewhat) better solution would be to replace the test with
// "if Assigned(Picture) and (Picture.Graphic is TBitmap) and Transparent and"...
// but then the PicEnter image will be assigned as soon as the mouse enters the component as no
// transparency detection is possible (TGraphic doesn't have the necessary TransparentColor and Canvas.Pixels)
// (rom) improved

function TJvImage.HitTest(X, Y: Integer): Boolean;
begin
  Result := inherited HitTest(X, Y);
  if (not UsesPictures) and Assigned(Picture) and (Picture.Graphic is TBitmap) and
     Transparent and (X < Picture.Bitmap.Width) and (Y < Picture.Bitmap.Height) and
     (Picture.Bitmap.Canvas.Pixels[X, Y] = ColorToRGB(Picture.Bitmap.TransparentColor)) then
    Result := False;
end;

procedure TJvImage.DoOwnPictureChange(Sender: TObject);
begin
  inherited Picture.Assign(FPicture);
end;

procedure TJvImage.PicturesChanged(Sender: TObject);
begin
  if UsesPictures then
    SetState(State);
end;

procedure TJvImage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvImage.SetState(Value: TPicState);

  function NotEmpty(Value: TPicture): Boolean;
  begin
    Result := (Value <> nil) and (Value.Width > 0) and (Value.Height > 0);
  end;

begin
  case Value of
    stDefault:
      if NotEmpty(FPicture) then
      begin
        inherited Picture.Assign(FPicture);
        FState := Value;
      end;
    stEntered:
      if NotEmpty(Pictures.PicEnter) then
      begin
        inherited Picture.Assign(Pictures.PicEnter);
        FState := Value;
      end;
    stClicked1:
      if NotEmpty(Pictures.PicClicked1) then
      begin
        inherited Picture.Assign(Pictures.PicClicked1);
        FState := Value;
      end;
    stClicked2:
      if NotEmpty(Pictures.PicClicked2) then
      begin
        inherited Picture.Assign(Pictures.PicClicked2);
        FState := Value;
      end;
    stDown:
      if NotEmpty(Pictures.PicDown) then
      begin
        inherited Picture.Assign(Pictures.PicDown);
        FState := Value;
      end;
  end;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TJvImage.DoPictureChange(Sender: TObject);
begin
  Invalidate;
end;

function TJvImage.UsesPictures: Boolean;
begin
  Result := Assigned(Pictures.PicEnter.Graphic) or
    Assigned(Pictures.PicClicked1.Graphic) or
    Assigned(Pictures.PicClicked2.Graphic) or
    Assigned(Pictures.PicDown.Graphic);
end;

//=== { TJvPictures } ========================================================

constructor TJvPictures.Create;
begin
  inherited Create;
  FPicClicked1 := TPicture.Create;
  FPicClicked2 := TPicture.Create;
  FPicDown := TPicture.Create;
  FPicEnter := TPicture.Create;
end;

destructor TJvPictures.Destroy;
begin
  FPicClicked1.Free;
  FPicClicked2.Free;
  FPicDown.Free;
  FPicEnter.Free;
  inherited Destroy;
end;

procedure TJvPictures.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvPictures.SetPicClicked(const Value: TPicture);
begin
  FPicClicked1.Assign(Value);
  Changed;
end;

procedure TJvPictures.SetPicClicked2(const Value: TPicture);
begin
  FPicClicked2.Assign(Value);
  Changed;
end;

procedure TJvPictures.SetPicDown(const Value: TPicture);
begin
  FPicDown.Assign(Value);
  Changed;
end;

procedure TJvPictures.SetPicEnter(const Value: TPicture);
begin
  FPicEnter.Assign(Value);
  Changed;
end;

end.

