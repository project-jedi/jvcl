{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgButton.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(S):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI'S JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls,  Imglist,
  JvComponent, JvgTypes, JvgUtils, JvgCommClasses;

type
  TDrawMode = (dmUseImageList, dmAutoCtrl3D, dmAutoFlat, dmAutoShadow);

  TglButtonOption = (boBlinkWhenActive, boBlinkWhenInactive,
    boBlinkWhenPushed, boChangeColorWhenActive,
    boChangeColorWhenPushed, boDelicateInactive,
    boDrawPushedAsFlat, boRaisedInactive,
    boRefreshOnActivate, boRefreshOnPush,
    boShadowSurround, boShiftMouseOnPush);

  TglButtonOptions = set of TglButtonOption;

  TglBtnState = (fbsOriginal, fbsInactive, fbsActive, fbsPushed, fbsDisabled);

  TJvgGlyphsIndexes = class(TPersistent)
  private
    FInactive: Integer;
    FPushed: Integer;
    FActive: Integer;
    FDisabled: Integer;
    FMask: Integer;
    FOnChanged: TNotifyEvent;
    procedure SetInactive(Value: Integer);
    procedure SetPushed(Value: Integer);
    procedure SetActive(Value: Integer);
    procedure SetDisabled(Value: Integer);
    procedure SetMask(Value: Integer);
  protected
    procedure DoChanged; virtual;
  public
    constructor Create;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged stored False;
  published
    property Inactive: Integer read FInactive write SetInactive default 0;
    property Pushed: Integer read FPushed write SetPushed default 1;
    property Active: Integer read FActive write SetActive default 2;
    property Disabled: Integer read FDisabled write SetDisabled default -1;
    property Mask: Integer read FMask write SetMask default 3;
  end;

  TJvgBtnGlyphs = class(TPersistent)
  private
    FGlyphInactive: TBitmap;
    FGlyphMask: TBitmap;
    FGlyphPushed: TBitmap;
    FGlyphActive: TBitmap;
    FGlyphDisabled: TBitmap;
    procedure SetGlyphInactive(Value: TBitmap);
    procedure SetGlyphMask(Value: TBitmap);
    procedure SetGlyphPushed(Value: TBitmap);
    procedure SetGlyphActive(Value: TBitmap);
    procedure SetGlyphDisabled(Value: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property GlyphInactive: TBitmap read FGlyphInactive write SetGlyphInactive;
    property GlyphMask: TBitmap read FGlyphMask write SetGlyphMask;
    property GlyphPushed: TBitmap read FGlyphPushed write SetGlyphPushed;
    property GlyphActive: TBitmap read FGlyphActive write SetGlyphActive;
    property GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
  end;

  TJvgButton = class(TJvGraphicControl)
  private
    FGlyph: TBitmap;
    FGlyphs: TJvgBtnGlyphs;
    FDrawMode: TDrawMode;
    FGlyphsList: TImageList;
    FTransparentColor: TColor;
    FNumGlyphs: Integer;
    FShiftMaskWhenPushed: TJvgPointClass;
    FEnabled: Boolean;
    FOptions: TglButtonOptions;
    FShadowDepth: Word;
    FGlyphsIndexes: TJvgGlyphsIndexes;
    FColorHighlight: TColor;
    FColorShadow: TColor;
    FColorDarkShadow: TColor;
    FDisabledMaskColor: TColor;
    FChangeColorOnActivate: TJvgTwainColors;
    FChangeColorOnPush: TJvgTwainColors;
    FAutoTrColor: TglAutoTransparentColor;
    FBlinkTimer: TTimer;
    FOnClick: TNotifyEvent;

    TmpBMP: TBitmap;
    Img: TBitmap;
    DefaultGlyphsList: TImageList;
    FBitmapsCreated: Boolean;
    FMouseInControl: Boolean;
    FPushed: Boolean;
    FShowingAsPushedNow: Boolean;
    FActiveNow: Boolean;
    FLoaded: Boolean;
    FBlinked: Boolean;
    FNeedBlink: Boolean;
    MShift: TPoint;
    FTestMode: Boolean;

    procedure SetGlyph(Value: TBitmap);
    procedure SetDrawMode(Value: TDrawMode);
    procedure SetGlyphsList(Value: TImageList);
    procedure SetNumGlyphs(Value: Integer);
    procedure SetTransparentColor(Value: TColor);
    procedure SetShadowDepth(Value: Word);
    procedure SetColorHighlight(Value: TColor);
    procedure SetColorShadow(Value: TColor);
    procedure SetColorDarkShadow(Value: TColor);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetOptions(Value: TglButtonOptions);
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetBlinkTimer(Value: TTimer);
    function GetBlinkTimer: TTimer;
    procedure SetTestMode(Value: Boolean);

    function IsMouseInControl: Boolean;
    procedure GetBitmaps;
    procedure CreateBitmaps; //...based on Inactive Glyph
    procedure GetBitmap_(Index: Integer; var Bmp: TBitmap);
    procedure SmthChanged(Sender: TObject);
    procedure ApplicateGlyph(var TargetBMP: TBitmap; State: TglBtnState;
      DrawState: TglDrawState; S: Integer);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure Paint; override;
    procedure Paint_;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(Control: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnBlinkTimer(Sender: TObject);
  published
    property ShowHint default True;
    property Glyphs: TJvgBtnGlyphs read FGlyphs write FGlyphs;
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode;
    property GlyphsList: TImageList read FGlyphsList write SetGlyphsList;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property HintColor;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor default clOlive;
    property ShiftMaskWhenPushed: TJvgPointClass read FShiftMaskWhenPushed write FShiftMaskWhenPushed;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property GlyphsIndexes: TJvgGlyphsIndexes read FGlyphsIndexes write FGlyphsIndexes;
    property ShadowDepth: Word read FShadowDepth write SetShadowDepth default 5;
    property ColorHighlight: TColor read FColorHighlight write SetColorHighlight default clBtnHighlight;
    property ColorShadow: TColor read FColorShadow write SetColorShadow default clBtnShadow;
    property ColorDarkShadow: TColor read FColorDarkShadow write SetColorDarkShadow default clBlack;
    property DisabledMaskColor: TColor read FDisabledMaskColor write SetDisabledMaskColor default clBlack;
    property Options: TglButtonOptions read FOptions write SetOptions;
    property ChangeColorOnActivate: TJvgTwainColors read FChangeColorOnActivate write FChangeColorOnActivate;
    property ChangeColorOnPush: TJvgTwainColors read FChangeColorOnPush write FChangeColorOnPush;
    property AutoTransparentColor: TglAutoTransparentColor read FAutoTrColor write SetAutoTrColor default ftcUser;
    property BlinkTimer: TTimer read GetBlinkTimer write SetBlinkTimer;
    property TestMode: Boolean read FTestMode write SetTestMode default False;
    property OnParentColorChange;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvConsts, JvJCLUtils, JvThemes;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvgButton.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvgButton.res}
{$ENDIF LINUX}

{$IFNDEF USEJVCL}
resourcestring
  RsEErrorDuringAccessGlyphsListOrGlyphP = 'Error during access GlyphsList or Glyph property';
{$ENDIF USEJVCL}

//=== { TJvgBtnGlyphs } ======================================================

constructor TJvgBtnGlyphs.Create;
begin
  inherited Create;
  FGlyphInactive := TBitmap.Create;
  FGlyphMask := TBitmap.Create;
  FGlyphPushed := TBitmap.Create;
  FGlyphActive := TBitmap.Create;
  FGlyphDisabled := TBitmap.Create;
end;

destructor TJvgBtnGlyphs.Destroy;
begin
  FGlyphInactive.Free;
  FGlyphMask.Free;
  FGlyphPushed.Free;
  FGlyphActive.Free;
  FGlyphDisabled.Free;
  inherited Destroy;
end;

procedure TJvgBtnGlyphs.SetGlyphInactive(Value: TBitmap);
begin
  GlyphInactive.Assign(Value);
end;

procedure TJvgBtnGlyphs.SetGlyphMask(Value: TBitmap);
begin
  GlyphMask.Assign(Value);
end;

procedure TJvgBtnGlyphs.SetGlyphPushed(Value: TBitmap);
begin
  GlyphPushed.Assign(Value);
end;

procedure TJvgBtnGlyphs.SetGlyphActive(Value: TBitmap);
begin
  GlyphActive.Assign(Value);
end;

procedure TJvgBtnGlyphs.SetGlyphDisabled(Value: TBitmap);
begin
  GlyphDisabled.Assign(Value);
end;

//=== { TJvgButton } =========================================================

constructor TJvgButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  IncludeThemeStyle(Self, [csParentBackground]);
  FGlyph := TBitmap.Create;
  FGlyphs := TJvgBtnGlyphs.Create;
  DefaultGlyphsList := TImageList.CreateSize(30, 30);
  Img := TBitmap.Create;
  TmpBMP := TBitmap.Create;
  FShiftMaskWhenPushed := TJvgPointClass.Create;
  FGlyphsIndexes := TJvgGlyphsIndexes.Create;
  FChangeColorOnActivate := TJvgTwainColors.Create;
  FChangeColorOnPush := TJvgTwainColors.Create;
  FGlyphsIndexes.OnChanged := SmthChanged;
  //...set defaults
  FShiftMaskWhenPushed.X := 0;
  FShiftMaskWhenPushed.Y := 0;
  FEnabled := True;
  FGlyphsList := nil;
  FNumGlyphs := 3;
  FDrawMode := dmUseImageList;
  FShadowDepth := 5;
  FColorHighlight := clBtnHighlight;
  FColorShadow := clBtnShadow;
  FColorDarkShadow := clBlack;
  FDisabledMaskColor := clBlack;
  FTestMode := False;
  ShowHint := True;
  FOptions := [boRaisedInactive, boShadowSurround, boShiftMouseOnPush,
    boChangeColorWhenActive, boChangeColorWhenPushed,
    boBlinkWhenActive];
  if DefaultGlyphsList.ResourceLoad(rtBitmap, 'FRDEFBUTTON', clNone) then
  begin
    FGlyphsList := DefaultGlyphsList;
    GetBitmaps;
  end;
  FPushed := False;
  FChangeColorOnActivate.FromColor := clBlack;
  FChangeColorOnActivate.ToColor := clBlack;
  FChangeColorOnPush.FromColor := clBlack;
  FChangeColorOnPush.ToColor := clBlack;
  FTransparentColor := clOlive;
  FAutoTrColor := {ftcLeftBottomPixel;} ftcUser;
  Width := 20;
  Height := 20;
  FLoaded := False;
end;

destructor TJvgButton.Destroy;
begin
  FGlyphsList := nil;
  FGlyphs.Free;
  FGlyph.Free;
  DefaultGlyphsList.Free;
  Img.Free;
  TmpBMP.Free;
  FShiftMaskWhenPushed.Free;
  FGlyphsIndexes.Free;
  FChangeColorOnActivate.Free;
  FChangeColorOnPush.Free;
  if not (csDestroying in Owner.ComponentState) then
    SetBlinkTimer(nil);
  inherited Destroy;
end;

procedure TJvgButton.Loaded;
begin
  inherited Loaded;
  FLoaded := True;
end;

procedure TJvgButton.Paint;
var
  DrawState: TglDrawState;
  I: Word;
begin
  with Glyphs do
  begin
    if not FLoaded then
    begin
      FLoaded := True;
      GetBitmaps;
    end;
    Width := FGlyphInactive.Width + 1;
    Height := FGlyphInactive.Height + 1;
    FShowingAsPushedNow := FPushed and FMouseInControl;
    FActiveNow := True;

    with Img do
    begin
      Width := Self.Width;
      Height := Self.Height;
      Canvas.Brush.Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(ClientRect);
    end;
    GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
      Img.Canvas.Handle);

    if boDelicateInactive in FOptions then
      DrawState := fdsDelicate
    else
      DrawState := fdsDefault;

    if FEnabled then
    begin
      if FMouseInControl then
      begin
        if FPushed then
        begin
          if (boDrawPushedAsFlat in FOptions) and (FDrawMode <>
            dmUseImageList) then
            ApplicateGlyph(Img, fbsOriginal {fbsPushed}, fdsDefault, 3)
          else
          begin
            if FDrawMode = dmAutoFlat then
              I := 2
            else
              I := 0;
            ApplicateGlyph(Img, fbsPushed, fdsDefault, I);
          end;
        end
        else
        begin
          FActiveNow := False;
          if (FDrawMode = dmAutoFlat) then
            I := 1
          else
            I := 0;
          ApplicateGlyph(Img, fbsActive, fdsDefault, I);
        end;
      end
      else
      begin
        if (FDrawMode = dmAutoFlat) and
          ({FPushed or }(not (boRaisedInactive in FOptions))) then
          ApplicateGlyph(Img, fbsOriginal, DrawState, 2)
        else
        begin
          if (FDrawMode = dmAutoFlat) then
            I := 1
          else
            I := 0;
          ApplicateGlyph(Img, fbsInactive, DrawState, I);
        end;
      end;
    end
    else
    begin
      if (FDrawMode = dmAutoFlat) and (boRaisedInactive in Options) then
        I := 1
      else
        I := 0;
      if DrawMode <> dmUseImageList then //...auto disabled
        ApplicateGlyph(Img, fbsDisabled, fdsDisabled, I)
      else
      begin //...user'S disabled
        if FGlyphsIndexes.Disabled = -1 then
          CreateBitmapExt(Img.Canvas.Handle, FGlyphInactive, ClientRect, 0,
            0,
            fwoNone, fdsDisabled, True, FTransparentColor,
            DisabledMaskColor)
        else
          CreateBitmapExt(Img.Canvas.Handle, FGlyphDisabled, ClientRect, 0,
            0,
            fwoNone, fdsDefault, True, FTransparentColor,
            DisabledMaskColor);
      end;
    end;
    Img.Transparent := True;
    Img.TransparentColor := clBtnFace;
    Canvas.Draw(0, 0, Img);
  end;
end;

procedure TJvgButton.Paint_;
begin
  if not Enabled then
    Exit;
  if FChangeColorOnActivate.FromColor <> FChangeColorOnActivate.ToColor then
  begin
    Repaint;
    Exit;
  end;
  if ((FDrawMode = dmAutoCtrl3D) or (FDrawMode = dmAutoShadow)) and
    (not FShowingAsPushedNow) and (not FPushed) and (not (boDelicateInactive in FOptions)) then
    Exit;
  if (FDrawMode = dmAutoFlat) and (not FShowingAsPushedNow) and (not FPushed) and
    (boRaisedInactive in FOptions) and (not (boDelicateInactive in FOptions)) then
    Exit;

  Repaint;
  Exit;

  // (rom) unused code
  if (FDrawMode = dmAutoFlat) and
    (FShowingAsPushedNow or (not (boRaisedInactive in FOptions))) then
  begin
    Repaint;
    Exit;
  end;
  if FPushed then
  begin
    if (boRefreshOnPush in FOptions) or (FDrawMode = dmAutoShadow) then
      Repaint
    else
      Paint;
  end
  else
  if boRefreshOnActivate in FOptions then
    Repaint
  else
    Paint;
end;

procedure TJvgButton.ApplicateGlyph(var TargetBMP: TBitmap; State: TglBtnState;
  DrawState: TglDrawState; S: Integer);
var
  I, J: Integer;
  fChangeColor, fCanBlink: Boolean;
  DrawState2: TglDrawState;
begin
  with Glyphs do
  begin
    I := 1;
    J := 1;
    fChangeColor := False;
    fCanBlink := False;
    if DrawState = fdsDisabled then
    begin
      DrawState := fdsDefault;
      DrawState2 := fdsDisabled //DrawState;
    end
    else
      DrawState2 := DrawState;
    case FDrawMode of
      dmAutoCtrl3D:
        if State = fbsPushed then
        begin
          I := 2;
          J := 2;
        end;
      dmUseImageList:
        begin
          I := 0;
          J := 0;
          S := 0;
        end;
    end;

    case State of
      fbsOriginal:
        begin
          CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyph, ClientRect, S,
            S,
            fwoNone, DrawState, True, FTransparentColor,
            DisabledMaskColor);
          Exit;
        end;
      fbsInactive, fbsDisabled:
        begin
          if (DrawMode = dmAutoFlat) and (boRaisedInactive in FOptions) then
            CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphActive,
              ClientRect, S, S,
              fwoNone, DrawState, True, FTransparentColor,
              DisabledMaskColor)
          else
            CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphInactive,
              ClientRect, 0, 0,
              fwoNone, DrawState, True, FTransparentColor,
              DisabledMaskColor);

          if State = fbsDisabled then
          begin
            I := 0;
            J := 0;
          end;
          fCanBlink := boBlinkWhenInactive in Options;
        end;
      fbsActive:
        begin
          if (FDrawMode = dmAutoCtrl3D) or (DrawMode = dmAutoShadow) then
            CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphInactive,
              ClientRect, S, S,
              fwoNone, DrawState, True, FTransparentColor,
              DisabledMaskColor)
          else
            CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphActive,
              ClientRect, S, S,
              fwoNone, DrawState, True, FTransparentColor,
              DisabledMaskColor);
          fChangeColor := boChangeColorWhenActive in Options;
          fCanBlink := boBlinkWhenActive in Options;
        end;
      fbsPushed:
        begin
          CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphPushed,
            ClientRect, S, S,
            fwoNone, DrawState, True, FTransparentColor,
            DisabledMaskColor);
          fChangeColor := boChangeColorWhenPushed in Options;
          fCanBlink := boBlinkWhenPushed in Options;
        end;
    end;
    GetBitmap_(FGlyphsIndexes.Inactive, TmpBMP);
    fCanBlink := fCanBlink and FNeedBlink;
    if fCanBlink then
      FBlinked := not FBlinked
    else
    if State = fbsActive then
      FBlinked := FChangeColorOnActivate.FromColor <> FChangeColorOnActivate.ToColor
    else
      FBlinked := FChangeColorOnPush.FromColor <> FChangeColorOnPush.ToColor;

    if fCanBlink then
    begin
      if FBlinked then
        if State = fbsPushed then
          with FChangeColorOnPush do
            ChangeBitmapColor(TmpBMP, FromColor, ToColor)
        else
          with FChangeColorOnActivate do
            ChangeBitmapColor(TmpBMP, FromColor, ToColor);
    end
    else
    if fChangeColor and (FDrawMode <> dmUseImageList) then
      if State = fbsActive then
        with FChangeColorOnActivate do
          ChangeBitmapColor(TmpBMP, FromColor, ToColor)
      else
        with FChangeColorOnPush do
          ChangeBitmapColor(TmpBMP, FromColor, ToColor);
    FNeedBlink := False;
    if (DrawMode = dmAutoShadow) and (State = fbsPushed) or
      (FDrawMode = dmUseImageList) then
      Exit;

    if DrawState2 = fdsDisabled then
    begin
      TmpBMP.Canvas.Brush.Color := FTransparentColor;
      TmpBMP.Canvas.Font.Color := clBtnFace;
      //    SetBkColor(TmpBMP.Canvas.Handle, FTransparentColor);
      TmpBMP.Monochrome := True;
      TmpBMP.Monochrome := False;
      CreateBitmapExt(TargetBMP.Canvas.Handle, TmpBMP, Rect(0, 0,
        TmpBMP.Width, TmpBMP.Height),
        I + S, J + S, fwoNone, fdsDefault, True, FTransparentColor,
        FDisabledMaskColor);
      GetBitmap_(FGlyphsIndexes.Inactive, TmpBMP);
    end;

    CreateBitmapExt(TargetBMP.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width,
      TmpBMP.Height),
      I + S, J + S, fwoNone, DrawState2, True, FTransparentColor,
      FDisabledMaskColor);
  end;
end;

procedure TJvgButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
  MouseInControl: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  Pt.X := X;
  Pt.Y := Y;
  if PtInRectExclusive(ClientRect, Pt) then
  begin
    MouseInControl := IsMouseInControl;
    if MouseInControl <> FMouseInControl then
    begin
      if FMouseInControl then
        if Assigned(OnMouseEnter) then
          OnMouseEnter(Self)
        else
        if Assigned(OnMouseLeave) then
          OnMouseLeave(Self);
      FMouseInControl := MouseInControl;
      Paint_;
    end;
  end;
end;

procedure TJvgButton.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    inherited MouseLeave(Control);
    FMouseInControl := False;
    Paint_;
  end;
end;

procedure TJvgButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or (not Enabled) or (not IsMouseInControl) then
    Exit;

  if boShiftMouseOnPush in FOptions then
  begin
    GetCursorPos(Pt);
    SetCursorPos(Pt.X + MShift.X, Pt.Y + MShift.Y);
  end;
  FPushed := True;
  Paint_;
end;

procedure TJvgButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FShowingAsPushedNow and Assigned(FOnClick) then
    FOnClick(Self);
  if (boShiftMouseOnPush in FOptions) and IsMouseInControl then
  begin
    GetCursorPos(Pt);
    SetCursorPos(Pt.X - MShift.X, Pt.Y - MShift.Y);
  end;
  FPushed := False;
  Paint_;
end;

procedure TJvgButton.GetBitmaps;
begin
  if not FLoaded then
    Exit;
  with Glyphs do
  begin
    FGlyphInactive.Width := 0;
    FGlyphPushed.Width := 0;
    FGlyphActive.Width := 0;
    FGlyphDisabled.Width := 0;
    FGlyphMask.Width := 0;
    if FDrawMode = dmUseImageList then
    begin
      if not Assigned(FGlyphsList) then
        Exit;
      with FGlyphsList, FGlyphsIndexes do
      begin
        if (Inactive < 0) and (Inactive > Count - 1) then
          Inactive := 0;
        if (Pushed < 0) and (Pushed > Count - 1) then
          Pushed := 1;
        if Active > Count - 1 then
          Active := -1;
        if Mask > Count - 1 then
          Mask := -1;

        if Inactive <> -1 then
          GetBitmap_(Inactive, FGlyphInactive);
        if Pushed <> -1 then
          GetBitmap_(Pushed, FGlyphPushed);
        if Active <> -1 then
          GetBitmap_(Active, FGlyphActive); //...optional bitmap
        if Disabled <> -1 then
          GetBitmap_(Disabled, FGlyphDisabled); //...optional bitmap
        if Mask <> -1 then
          GetBitmap_(Mask, FGlyphMask); //...optional bitmap
        FNumGlyphs := Count;
        FBitmapsCreated := not (FGlyphInactive.Empty or FGlyphPushed.Empty);
      end;
    end
    else
      CreateBitmaps;
    FBitmapsCreated := True;

    case FDrawMode of
      dmAutoShadow:
        if boDrawPushedAsFlat in FOptions then
        begin
          MShift.X := 1;
          MShift.Y := 1;
        end
        else
        begin
          MShift.X := FShadowDepth - 1;
          MShift.Y := FShadowDepth - 1;
        end;
      dmAutoCtrl3D:
        begin
          MShift.X := 2;
          MShift.Y := 2;
        end;
      dmAutoFlat:
        begin
          MShift.X := 1;
          MShift.Y := 1;
        end;
    else
      begin
        MShift.X := FShiftMaskWhenPushed.X;
        MShift.Y := FShiftMaskWhenPushed.Y;
      end;
    end;

    Width := FGlyphInactive.Width;
    Height := FGlyphInactive.Height;
  end;
end;

procedure TJvgButton.CreateBitmaps; //...based on Inactive Glyph
var
  MonoBMP, OldMonoBMP: HBITMAP;
  MonoDC: HDC;
  I: Word;

  procedure RemakeTmpBMP;
  begin
    SetBkColor(TmpBMP.Canvas.Handle, ColorToRGB(FTransparentColor));
    BitBlt(TmpBMP.Canvas.Handle, 0, 0, TmpBMP.Width, TmpBMP.Height, MonoDC, 0,
      0, SRCCOPY);
  end;

begin
  with FGlyphs, FGlyphsList, FGlyphsIndexes do
  begin
    FInactive := 0;
    FPushed := -1;
    FActive := -1;
    FDisabled := -1;
    FMask := -1;
    GetBitmap_(Inactive, TmpBMP);

    MonoDC := CreateCompatibleDC(TmpBMP.Canvas.Handle);
    MonoBMP := CreateBitmap(TmpBMP.Width, TmpBMP.Height, 1, 1, nil);
    OldMonoBMP := SelectObject(MonoDC, MonoBMP);
    //  SetMapMode( MonoDC, GetMapMode(TmpBMP.Canvas.Handle) );
    SetBkColor(TmpBMP.Canvas.Handle, ColorToRGB(FTransparentColor));
    BitBlt(MonoDC, 0, 0, TmpBMP.Width, TmpBMP.Height,
      TmpBMP.Canvas.Handle, 0, 0, SRCCOPY);
    //SetBkColor(TmpBMP.Canvas.Handle, OldBkColor);
    try
      if FDrawMode = dmAutoShadow then
      begin
        with FGlyphInactive do
        begin
          Width := TmpBMP.Width + FShadowDepth;
          Height := TmpBMP.Height + FShadowDepth;
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := FTransparentColor;
          Canvas.FillRect(Rect(0, 0, Width, Height));
        end;
        with FGlyphPushed do
        begin
          Width := TmpBMP.Width + FShadowDepth;
          Height := TmpBMP.Height + FShadowDepth;
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := FTransparentColor;
          Canvas.FillRect(Rect(0, 0, Width, Height));
        end;

        BitBlt(FGlyphPushed.Canvas.Handle, FShadowDepth, FShadowDepth,
          TmpBMP.Width, TmpBMP.Height, TmpBMP.Canvas.Handle, 0, 0,
          SRCCOPY);

        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          FShadowDepth, FShadowDepth, fwoNone, fdsDefault, True,
          FTransparentColor, FDisabledMaskColor);
        Exit;
      end;

      if FDrawMode = dmAutoCtrl3D then
        I := 3
      else
        I := 3;
      with FGlyphInactive do
      begin
        Width := TmpBMP.Width + I;
        Height := TmpBMP.Height + I;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := FTransparentColor;
        Canvas.FillRect(Rect(0, 0, Width, Height));
      end;
      if not (boDrawPushedAsFlat in FOptions) then
        with FGlyphPushed do
        begin
          Width := TmpBMP.Width + I;
          Height := TmpBMP.Height + I;
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := FTransparentColor;
          Canvas.FillRect(Rect(0, 0, Width, Height));
        end;
      with FGlyphActive do
      begin
        Width := TmpBMP.Width + I;
        Height := TmpBMP.Height + I;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := FTransparentColor;
        Canvas.FillRect(Rect(0, 0, Width, Height));
      end;

      if FDrawMode = dmAutoCtrl3D then //...add 3d border to inactive
      begin
        RemakeTmpBMP;
        if clBlack <> FColorDarkShadow then
          ChangeBitmapColor(TmpBMP, clBlack, FColorDarkShadow);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            0, 3, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            3, 0, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          3, 3, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);

        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          0, 0, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            2, 0, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            0, 2, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
        end;
        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            1, 2, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            2, 1, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          2, 2, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        if boDrawPushedAsFlat in FOptions then
          Exit;
        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            0, 3, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            3, 0, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          3, 3, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);

        RemakeTmpBMP;
        if clBlack <> FColorDarkShadow then
          ChangeBitmapColor(TmpBMP, clBlack, FColorDarkShadow);

        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          0, 0, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            2, 0, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            0, 2, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
        end;
        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            1, 2, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            2, 1, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          1, 1, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        RemakeTmpBMP;
        Exit;
      end;

      if FDrawMode = dmAutoFlat then
      begin
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          0, 0, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            0, 2, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
          CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            2, 0, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          2, 2, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
        CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          0, 0, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            1, 0, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
          CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            0, 1, fwoNone, fdsDefault, True, FTransparentColor,
            FDisabledMaskColor);
        end;
        RemakeTmpBMP;
      end;
      if boDrawPushedAsFlat in FOptions then
        Exit;
      RemakeTmpBMP;
      ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
      if boShadowSurround in FOptions then
      begin
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          0, 2, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          2, 0, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
      end;
      CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
        TmpBMP.Width, TmpBMP.Height),
        2, 2, fwoNone, fdsDefault, True, FTransparentColor,
        FDisabledMaskColor);
      RemakeTmpBMP;
      if clBlack <> FColorShadow then
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);

      CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
        TmpBMP.Width, TmpBMP.Height),
        0, 0, fwoNone, fdsDefault, True, FTransparentColor,
        FDisabledMaskColor);
      if boShadowSurround in FOptions then
      begin
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          1, 0, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
          TmpBMP.Width, TmpBMP.Height),
          0, 1, fwoNone, fdsDefault, True, FTransparentColor,
          FDisabledMaskColor);
      end;
      RemakeTmpBMP;
    finally
      DeleteObject(SelectObject(MonoDC, OldMonoBMP));
    end;
  end;
end;

function TJvgButton.IsMouseInControl: Boolean;
var
  Pt: TPoint;
  PixelColor: TColorRef;
begin
  GetCursorPos(Pt);
  Pt := ScreenToClient(Pt);
  if FShowingAsPushedNow and FPushed then
  begin
    Dec(Pt.X, FShiftMaskWhenPushed.X);
    Dec(Pt.Y, FShiftMaskWhenPushed.Y);
  end
  else
  if FDrawMode = dmAutoShadow then
  begin
    Inc(Pt.X, FShadowDepth);
    Inc(Pt.Y, FShadowDepth);
  end;

  Dec(Pt.X);
  Dec(Pt.Y);

  if FGlyphsIndexes.Mask = -1 then //...__mask is absent_
  begin
    with FGlyphs do
      case FDrawMode of
        dmAutoShadow:
          PixelColor := GetPixel(FGlyphPushed.Canvas.Handle, Pt.X, Pt.Y);
        dmAutoFlat:
          PixelColor := GetPixel(FGlyphActive.Canvas.Handle, Pt.X, Pt.Y);
      else
        PixelColor := GetPixel(FGlyphInactive.Canvas.Handle, Pt.X, Pt.Y);
      end;
    Result := (PixelColor <> TColorRef(FTransparentColor)) and (PixelColor <> DWORD(-1));
  end
  else //...__use mask_
  begin
    with FGlyphs do
      PixelColor := GetPixel(FGlyphMask.Canvas.Handle, Pt.X, Pt.Y);
    Result := (PixelColor = TColorRef(clWhite)) and (PixelColor <> DWORD(-1));
  end;
end;

procedure TJvgButton.SmthChanged(Sender: TObject);
begin
  GetBitmaps;
  Invalidate;
end;

procedure TJvgButton.GetBitmap_(Index: Integer; var Bmp: TBitmap);
begin
  try
    if FDrawMode = dmUseImageList then
      FGlyphsList.GetBitmap(Index, Bmp)
    else
    begin
      if Assigned(Bmp) then
      begin
        Bmp.Free;
        Bmp := TBitmap.Create;
      end;
      Bmp.Assign(Glyph);
    end;
  except
    MessageDlg(RsEErrorDuringAccessGlyphsListOrGlyphP, mtError, [mbOk], 0);
    raise;
  end;
end;

procedure TJvgButton.OnBlinkTimer(Sender: TObject);
var
  ParentForm: TForm;
  I: Integer;

  procedure Blink(FreeButton: TJvgButton);
  begin
    with FreeButton do
    begin
      FNeedBlink := False;
      if FShowingAsPushedNow then
        with FChangeColorOnPush do
          if (boBlinkWhenPushed in Options) and (FromColor <> ToColor) then
          begin
            FNeedBlink := True;
            Repaint;
            Exit;
          end
          else
            Exit;
      if FMouseInControl then
        with FChangeColorOnActivate do
          if (boBlinkWhenActive in Options) and (FromColor <> ToColor) then
          begin
            FNeedBlink := True;
            Repaint;
            Exit;
          end
          else
            Exit;
      if not FMouseInControl then
        with FChangeColorOnActivate do
          if (boBlinkWhenInactive in Options) and (FromColor <> ToColor) then
          begin
            FNeedBlink := True;
            Repaint;
            Exit;
          end
          else
            Exit;
    end;
  end;

begin
  if (not TestMode) and (csDesigning in ComponentState) then
    Exit;
  ParentForm := GetParentForm(Self);
  for I := 0 to ParentForm.ComponentCount - 1 do
    if (ParentForm.Components[I] is TJvgButton) and
      (TJvgButton(ParentForm.Components[I]).BlinkTimer = FBlinkTimer) then
      with ParentForm.Components[I] as TJvgButton do
        Blink(TJvgButton(ParentForm.Components[I]));
end;

procedure TJvgButton.SetDrawMode(Value: TDrawMode);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetGlyphsList(Value: TImageList);
begin
  if Assigned(Value) then
  begin
    FGlyphsList := Value;
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetGlyph(Value: TBitmap);
begin
  if Assigned(FGlyph) then
  begin
    FGlyph.Free;
    FGlyph := TBitmap.Create;
  end;
  FGlyph.Assign(Value);
  GetBitmaps;
  Invalidate;
  AutoTransparentColor := AutoTransparentColor;
end;

procedure TJvgButton.SetNumGlyphs(Value: Integer);
begin
  if (Value >= 2) or (Value <= 4) then
    FNumGlyphs := Value;
end;

procedure TJvgButton.SetTransparentColor(Value: TColor);
begin
  if (FAutoTrColor <> ftcUser) or (FTransparentColor = Value) then
    Exit;
  FTransparentColor := Value;
  GetBitmaps;
  Invalidate;
end;

procedure TJvgButton.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Repaint;
  end;
end;

procedure TJvgButton.SetShadowDepth(Value: Word);
begin
  if FShadowDepth <> Value then
  begin
    FShadowDepth := Value;
    if FDrawMode = dmAutoShadow then
    begin
      GetBitmaps;
      Invalidate;
    end;
  end;
end;

procedure TJvgButton.SetColorHighlight(Value: TColor);
begin
  if FColorHighlight <> Value then
  begin
    FColorHighlight := Value;
    if FDrawMode <> dmUseImageList then
    begin
      GetBitmaps;
      Invalidate;
    end;
  end;
end;

procedure TJvgButton.SetColorShadow(Value: TColor);
begin
  if FColorShadow <> Value then
  begin
    FColorShadow := Value;
    if FDrawMode <> dmUseImageList then
    begin
      GetBitmaps;
      Invalidate;
    end;
  end;
end;

procedure TJvgButton.SetColorDarkShadow(Value: TColor);
begin
  if FColorDarkShadow <> Value then
  begin
    FColorDarkShadow := Value;
    if FDrawMode <> dmUseImageList then
    begin
      GetBitmaps;
      Invalidate;
    end;
  end;
end;

procedure TJvgButton.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor <> Value then
  begin
    FDisabledMaskColor := Value;
    if FDrawMode <> dmUseImageList then
    begin
      GetBitmaps;
      Invalidate;
    end;
  end;
end;

procedure TJvgButton.SetOptions(Value: TglButtonOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if FDrawMode <> dmUseImageList then
    begin
      GetBitmaps;
      Invalidate;
    end;
  end;
end;

procedure TJvgButton.SetAutoTrColor(Value: TglAutoTransparentColor);
var
  X, Y: Integer;
  TmpBmp_: TBitmap;
begin
  FAutoTrColor := Value;
  TmpBmp_ := nil;
  if {(FAutoTrColor=ftcUser)or}(FGlyph.Width = 0) or (FGlyph.Height = 0) then
    Exit;
  try
    with FGlyph do
      case FAutoTrColor of
        ftcLeftTopPixel:
          begin
            X := 0;
            Y := 0;
          end;
        ftcLeftBottomPixel:
          begin
            X := 0;
            Y := Height - 1;
          end;
        ftcRightTopPixel:
          begin
            X := Width - 1;
            Y := 0;
          end;
        ftcRightBottomPixel:
          begin
            X := Width - 1;
            Y := Height - 1;
          end;
      else
        Exit;
      end;
    TmpBmp_ := TBitmap.Create;
    TmpBmp_.Assign(FGlyph);
    FTransparentColor := GetPixel(TmpBmp_.Canvas.Handle, X, Y);
  finally
    TmpBmp_.Free;
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetBlinkTimer(Value: TTimer);
var
  ParentForm: TForm;
  I: Integer;
  p1, p2: TNotifyEvent;
  Timer: TTimer;
begin
  if FBlinkTimer = Value then
    Exit;
  if Assigned(FBlinkTimer) then
  begin
    p1 := FBlinkTimer.OnTimer;
    p2 := OnBlinkTimer;
    if @FBlinkTimer.OnTimer = @p2 then //...points at me
    begin
      ParentForm := GetParentForm(Self);
      for I := 0 to ParentForm.ComponentCount - 1 do
        if (ParentForm.Components[I] is TJvgButton) and
          (TJvgButton(ParentForm.Components[I]) <> Self) and
          (TJvgButton(ParentForm.Components[I]).BlinkTimer = FBlinkTimer) then
        begin
          Timer := FBlinkTimer;
          FBlinkTimer := nil;
          Timer.OnTimer := TJvgButton(ParentForm.Components[I]).OnBlinkTimer;
          Break;
        end;
      if Assigned(FBlinkTimer) and (@FBlinkTimer.OnTimer = @p2) then
        FBlinkTimer.OnTimer := nil;
    end;
  end
  else
    FBlinkTimer := nil;

  FBlinkTimer := Value;
  if Assigned(FBlinkTimer) then
    FBlinkTimer.OnTimer := OnBlinkTimer;
end;

function TJvgButton.GetBlinkTimer: TTimer;
begin
  Result := nil;
  try
    if Assigned(FBlinkTimer) then
      if Owner.Components[FBlinkTimer.ComponentIndex] = FBlinkTimer then
        Result := FBlinkTimer;
  except
  end;
end;

procedure TJvgButton.SetTestMode(Value: Boolean);
var
  ParentForm: TForm;
  I: Integer;
begin
  ParentForm := GetParentForm(Self);
  for I := 0 to ParentForm.ComponentCount - 1 do
    if (ParentForm.Components[I] is TJvgButton) then
      TJvgButton(ParentForm.Components[I]).FTestMode := Value;
end;

//=== { TJvgGlyphsIndexes } ==================================================

constructor TJvgGlyphsIndexes.Create;
begin
  inherited Create;
  FInactive := 0;
  FPushed := 1;
  FActive := 2;
  FDisabled := -1;
  FMask := 3;
end;

procedure TJvgGlyphsIndexes.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvgGlyphsIndexes.SetInactive(Value: Integer);
begin
  FInactive := Value;
  DoChanged;
end;

procedure TJvgGlyphsIndexes.SetPushed(Value: Integer);
begin
  FPushed := Value;
  DoChanged;
end;

procedure TJvgGlyphsIndexes.SetActive(Value: Integer);
begin
  FActive := Value;
  DoChanged;
end;

procedure TJvgGlyphsIndexes.SetDisabled(Value: Integer);
begin
  FDisabled := Value;
  DoChanged;
end;

procedure TJvgGlyphsIndexes.SetMask(Value: Integer);
begin
  FMask := Value;
  DoChanged;
end;

end.

