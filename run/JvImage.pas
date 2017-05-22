{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImage.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvImage;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, Graphics, ExtCtrls, Controls, Forms,
  JvExExtCtrls;

type
  TPicState = (stDefault, stEntered, stClicked1, stClicked2, stDown, stDisabled);

  TJvPictures = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FPicClicked1: TPicture;
    FPicClicked2: TPicture;
    FPicDown: TPicture;
    FPicEnter: TPicture;
    FPicDisabled: TPicture;
    procedure SetPicClicked(const Value: TPicture);
    procedure SetPicClicked2(const Value: TPicture);
    procedure SetPicDown(const Value: TPicture);
    procedure SetPicEnter(const Value: TPicture);
    procedure SetPicDisabled(const Value: TPicture);
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
    property PicDisabled:TPicture read FPicDisabled write SetPicDisabled;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvImage = class(TJvExImage)
  private
    FOnStateChanged: TNotifyEvent;
    FPictures: TJvPictures;
    FState: TPicState;
    FPicture: TPicture;
    FClickCount: Integer;
    FPictureChange: TNotifyEvent;
    FDrawing: Boolean;
    procedure SetState(Value: TPicState);
    procedure PicturesChanged(Sender: TObject);
    procedure DoPictureChange(Sender: TObject);
    procedure DoOwnPictureChange(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
    procedure ApplyClick;
    function UsesPictures: Boolean;
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    function HitTest(X, Y: Integer): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream); virtual;
  published
    property HintColor;
    property Pictures: TJvPictures read FPictures write FPictures;
    property Picture: TPicture read FPicture write SetPicture;
    property State: TPicState read FState write SetState default stDefault;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvJVCLUtils;

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

procedure TJvImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if UsesPictures then
    State := stDown;
end;

procedure TJvImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    if UsesPictures and Enabled then
      State := stEntered;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvImage.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    if UsesPictures and Enabled then
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
var
  G: TGraphic;
  D: TRect;
begin
  // All this code is required for Transparent, Center and other inherited
  // properties to work fine.
  G := Picture.Graphic;
  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := inherited Transparent;
    D := DestRect;
    if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
       (D.Right >= Width) and (D.Bottom >= Height) then
      ControlStyle := ControlStyle + [csOpaque]
    else  // picture might not cover entire clientrect
      ControlStyle := ControlStyle - [csOpaque];
    if DoPaletteChange and FDrawing then
     Update;
  end
  else ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then
    Invalidate;

  inherited Picture.Assign(FPicture);
end;

procedure TJvImage.Paint;
begin
  FDrawing := True;
  try
    inherited Paint;
  finally
    FDrawing := False;
  end;
end;

procedure TJvImage.PicturesChanged(Sender: TObject);
begin
  if UsesPictures then
    SetState(State);
end;

procedure TJvImage.SetEnabled(Value: Boolean);
begin
  if Value <> Enabled then
  begin
    if not Value then
      State := stDisabled
    else
      State := stDefault;
  end;

  inherited SetEnabled(Value);
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
    stDisabled:
      if NotEmpty(Pictures.PicDisabled) then
      begin
        inherited Picture.Assign(Pictures.PicDisabled);
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
    Assigned(Pictures.PicDown.Graphic) or
    Assigned(Pictures.PicDisabled.Graphic);
end;

procedure TJvImage.LoadFromStream(AStream: TStream);
var
  G: TGraphic;
begin
  G := GetGraphicObject(AStream);
  try
    Picture.Assign(G);
  finally
    G.Free;
  end;
end;

//=== { TJvPictures } ========================================================

constructor TJvPictures.Create;
begin
  inherited Create;
  FPicClicked1 := TPicture.Create;
  FPicClicked2 := TPicture.Create;
  FPicDown := TPicture.Create;
  FPicEnter := TPicture.Create;
  FPicDisabled := TPicture.Create;
end;

destructor TJvPictures.Destroy;
begin
  FPicClicked1.Free;
  FPicClicked2.Free;
  FPicDown.Free;
  FPicEnter.Free;
  FPicDisabled.Free;
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

procedure TJvPictures.SetPicDisabled(const Value: TPicture);
begin
  FPicDisabled.Assign(Value);
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
