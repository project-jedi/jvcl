{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgButton.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  JvgTypes, JvgUtils, JvgCommClasses{$IFDEF COMPILER5_UP}, Imglist{$ENDIF};

type
  TDrawMode = (dmUseImageList, dmAutoCtrl3D, dmAutoFlat, dmAutoShadow);

  TglButtonOptions_ = (boBlinkWhenActive, boBlinkWhenInactive,
    boBlinkWhenPushed, boChangeColorWhenActive,
    boChangeColorWhenPushed, boDelicateInactive,
    boDrawPushedAsFlat, boRaisedInactive,
    boRefreshOnActivate, boRefreshOnPush,
    boShadowSurround, boShiftMouseOnPush);

  TglButtonOptions = set of TglButtonOptions_;
  TglBtnState = (fbsOriginal, fbsInactive, fbsActive, fbsPushed, fbsDisabled);

  //*************************************{ . TJvgGlyphsIndexes . }
  TJvgGlyphsIndexes = class(TPersistent)
  private
    FInactive: integer;
    FPushed: integer;
    FActive: integer;
    FDisabled: integer;
    FMask: integer;
    procedure SetInactive(Value: integer);
    procedure SetPushed(Value: integer);
    procedure SetActive(Value: integer);
    procedure SetDisabled(Value: integer);
    procedure SetMask(Value: integer);
  public
    OnChanged: TNotifyEvent;
    constructor Create;
  published
    property Inactive: integer read FInactive write SetInactive default 0;
    property Pushed: integer read FPushed write SetPushed default 1;
    property Active: integer read FActive write SetActive default 2;
    property Disabled: integer read FDisabled write SetDisabled default -1;
    property Mask: integer read FMask write SetMask default 3;
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
  //*************************************{ . TJvgButton . }
  TJvgButton = class(TGraphicControl)
  private
    FGlyph: TBitmap;
    FGlyphs: TJvgBtnGlyphs;
    FDrawMode: TDrawMode;
    FGlyphsList: TImageList;
    FTransparentColor: TColor;
    FNumGlyphs: integer;
    FShiftMaskWhenPushed: TJvgPointClass;
    FEnabled: boolean;
    FOptions: TglButtonOptions;
    FShadowDepth: word;
    FGlyphsIndexes: TJvgGlyphsIndexes;
    FColorHighlight: TColor;
    FColorShadow: TColor;
    FColorDarkShadow: TColor;
    FDisabledMaskColor: TColor;
    FChangeColorOnActivate: TJvgTwainColors;
    FChangeColorOnPush: TJvgTwainColors;
    FAutoTrColor: TglAutoTransparentColor;
    FBlinkTimer: TTimer;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnClick: TNotifyEvent;

    TmpBMP: TBitmap;
    Img: TBitmap;
    DefaultGlyphsList: TImageList;
    fBimapsCreated: boolean;
    fMouseInControl: boolean;
    fPushed: boolean;
    fShowingAsPushedNow: boolean;
    fActiveNow: boolean;
    fLoaded: boolean;
    fBlinked: boolean;
    fNeedBlink: boolean;
    MShift: TPoint;
    procedure SetGlyph(Value: TBitmap);
    procedure SetDrawMode(Value: TDrawMode);
    procedure SetGlyphsList(Value: TImageList);
    procedure SetNumGlyphs(Value: integer);
    procedure SetTransparentColor(Value: TColor);
    procedure SetEnabled(Value: boolean);
    //    procedure SetDelicateInactive( Value: boolean );
    //    procedure SetRaisedInactive( Value: boolean );
    procedure SetShadowDepth(Value: word);
    procedure SetColorHighlight(Value: TColor);
    procedure SetColorShadow(Value: TColor);
    procedure SetColorDarkShadow(Value: TColor);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetOptions(Value: TglButtonOptions);
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetBlinkTimer(Value: TTimer);
    function GetBlinkTimer: TTimer;
    procedure SetTestMode(Value: boolean);
    //    procedure SetShadowSurround( Value: boolean );
    //    procedure SetDrawPushedAsFlat( Value: boolean );

    function IsMouseInControl: boolean;
    procedure GetBitmaps;
    procedure CreateBitmaps; //...based on Inactive Glyph
    procedure GetBitmap_(Index: integer; var Bmp: TBitmap);
    procedure SmthChanged(Sender: TObject);
    procedure ApplicateGlyph(var TargetBMP: TBitmap; State: TglBtnState; DrawState: TglDrawState; s: integer);
  protected
    procedure Paint; override;
    procedure Paint_;
    procedure Loaded; override;
    //    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    //  procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    //    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  public
    FTestMode: boolean; //...placed hete to access from SetTestMode method
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnBlinkTimer(Sender: TObject);
  published
    property ShowHint default True;
    property Glyphs: TJvgBtnGlyphs read FGlyphs write FGlyphs;
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode;
    property GlyphsList: TImageList read FGlyphsList write SetGlyphsList;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property NumGlyphs: integer read FNumGlyphs write SetNumGlyphs;
    property TransparentColor: TColor
      read FTransparentColor write SetTransparentColor default clOlive;
    property ShiftMaskWhenPushed: TJvgPointClass read FShiftMaskWhenPushed write FShiftMaskWhenPushed;
    //    property RefreshOnActivate: boolean
    //     read FRefreshOnActivate write FRefreshOnActivate default false;
    //    property RefreshOnPush: boolean
    //     read FRefreshOnPush write FRefreshOnPush default true;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property GlyphsIndexes: TJvgGlyphsIndexes
      read FGlyphsIndexes write FGlyphsIndexes;
    property ShadowDepth: word read FShadowDepth write SetShadowDepth
      default 5;
    property ColorHighlight: TColor read FColorHighlight write SetColorHighlight
      default clBtnHighlight;
    property ColorShadow: TColor read FColorShadow write SetColorShadow
      default clBtnShadow;
    property ColorDarkShadow: TColor read FColorDarkShadow write SetColorDarkShadow
      default clBlack;
    property DisabledMaskColor: TColor read FDisabledMaskColor write SetDisabledMaskColor
      default clBlack;
    property Options: TglButtonOptions read FOptions write SetOptions;
    property ChangeColorOnActivate: TJvgTwainColors
      read FChangeColorOnActivate write FChangeColorOnActivate;
    property ChangeColorOnPush: TJvgTwainColors
      read FChangeColorOnPush write FChangeColorOnPush;
    property AutoTransparentColor: TglAutoTransparentColor
      read FAutoTrColor write SetAutoTrColor default ftcUser;
    property BlinkTimer: TTimer read GetBlinkTimer write SetBlinkTimer;
    property TestMode: boolean read FTestMode write SetTestMode default false;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;


implementation
{$R JvgButton.res}


constructor TJvgBtnGlyphs.Create;
begin
  inherited;
  FGlyphInactive := TBitmap.Create;
  FGlyphMask := TBitmap.Create;
  FGlyphPushed := TBitmap.Create;
  FGlyphActive := TBitmap.Create;
  FGlyphDisabled := TBitmap.Create;
end;

destructor TJvgBtnGlyphs.Destroy;
begin
  if Assigned(FGlyphInactive) then FGlyphInactive.Free;
  if Assigned(FGlyphMask) then FGlyphMask.Free;
  if Assigned(FGlyphPushed) then FGlyphPushed.Free;
  if Assigned(FGlyphActive) then FGlyphActive.Free;
  if Assigned(FGlyphDisabled) then FGlyphDisabled.Free;
  inherited;
end;

//procedure TJvgBtnGlyphs.SetGlyphInactive( Value: TBitmap );

procedure TJvgBtnGlyphs.SetGlyphInactive(Value: TBitmap);
begin
  if Assigned(GlyphInactive) then GlyphInactive.Free;
  GlyphInactive.Assign(value);
end;

procedure TJvgBtnGlyphs.SetGlyphMask(Value: TBitmap);
begin
  if Assigned(GlyphMask) then GlyphMask.Free;
  GlyphMask.Assign(value);
end;

procedure TJvgBtnGlyphs.SetGlyphPushed(Value: TBitmap);
begin
  if Assigned(GlyphPushed) then GlyphPushed.Free;
  GlyphPushed.Assign(value);
end;

procedure TJvgBtnGlyphs.SetGlyphActive(Value: TBitmap);
begin
  if Assigned(GlyphActive) then GlyphActive.Free;
  GlyphActive.Assign(value);
end;

procedure TJvgBtnGlyphs.SetGlyphDisabled(Value: TBitmap);
begin
  if Assigned(GlyphDisabled) then GlyphDisabled.Free;
  GlyphDisabled.Assign(value);
end;

//*************************************{ . TJvgButton methods. }

constructor TJvgButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
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
  FShiftMaskWhenPushed.x := 0;
  FShiftMaskWhenPushed.y := 0;
  FEnabled := true;
  FGlyphsList := nil;
  FNumGlyphs := 3;
  FDrawMode := dmUseImageList;
  FShadowDepth := 5;
  FColorHighlight := clBtnHighlight;
  FColorShadow := clBtnShadow;
  FColorDarkShadow := clBlack;
  FDisabledMaskColor := clBlack;
  FTestMode := false;
  ShowHint := true;
  FOptions := [boRaisedInactive, boShadowSurround, boShiftMouseOnPush,
    boChangeColorWhenActive, boChangeColorWhenPushed,
    boBlinkWhenActive];
  //  if (csDesigning in ComponentState)and not(csLoading in ComponentState) then
  if DefaultGlyphsList.ResourceLoad(rtBitmap, 'FRDEFBUTTON', clNone) then
  begin
    //    ShowMessage('qwerty');
    FGlyphsList := DefaultGlyphsList;
    GetBitmaps;
  end;
  fPushed := false;
  FChangeColorOnActivate.FromColor := clBlack;
  FChangeColorOnActivate.ToColor := clBlack;
  FChangeColorOnPush.FromColor := clBlack;
  FChangeColorOnPush.ToColor := clBlack;
  FTransparentColor := clOlive;
  FAutoTrColor := {ftcLeftBottomPixel;} ftcUser;
  Width := 20;
  Height := 20;
  fLoaded := false; //(csDesigning in ComponentState);

end;

destructor TJvgButton.Destroy;
begin
  FGlyphsList := nil;
  Glyphs.Free;
  FGlyph.Free;
  DefaultGlyphsList.Free;
  Img.Free;
  TmpBMP.Free;
  FShiftMaskWhenPushed.Free;
  FGlyphsIndexes.Free;
  FChangeColorOnActivate.Free;
  FChangeColorOnPush.Free;
  if not (csDestroying in Owner.ComponentState) then SetBlinkTimer(nil);
  inherited;
end;

procedure TJvgButton.Loaded;
begin
  inherited Loaded; //if csDesigning in ComponentState then exit;
  //AutoTransparentColor := FAutoTrColor;
  fLoaded := true; // GetBitmaps;
end;

procedure TJvgButton.Paint;
var
  DrawState: TglDrawState;
  i: word;
begin
  with Glyphs do
  begin
    if not fLoaded then
    begin
      fLoaded := true;
      GetBitmaps;
    end;
    //    if not fBimapsCreated then exit;
    Width := FGlyphInactive.Width + 1;
    Height := FGlyphInactive.Height + 1;
    //fMouseInControl:=IsMouseInControl;
    fShowingAsPushedNow := fPushed and fMouseInControl;
    fActiveNow := true;

    with Img do
    begin
      Width := Self.Width;
      Height := Self.Height;
      Canvas.Brush.Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Bounds(0, 0, Width, Height));
    end;
    GetParentImageRect(self, Bounds(Left, Top, Width, Height), Img.Canvas.Handle);

    if boDelicateInactive in FOptions then
      DrawState := fdsDelicate
    else
      DrawState := fdsDefault;

    if FEnabled then //..._______________ENABLED_
    begin
      if fMouseInControl then
      begin
        if fPushed then //..._____________PUSHED_
        begin

          if (boDrawPushedAsFlat in FOptions) and (FDrawMode <> dmUseImageList) then
            ApplicateGlyph(Img, fbsOriginal {fbsPushed}, fdsDefault, 3)
          else
          begin
            if FDrawMode = dmAutoFlat then
              i := 2
            else
              i := 0;
            ApplicateGlyph(Img, fbsPushed, fdsDefault, i);
          end;
        end
        else
        begin //...__________________ACTIVE_
          fActiveNow := false;
          if (FDrawMode = dmAutoFlat) then
            i := 1
          else
            i := 0;
          //	  if {(FGlyphsIndexes.Active=-1)or}(FDrawMode=dmAutoCtrl3D) then
          //	     (FDrawMode=dmAutoShadow) then
          //	    //...__use INACTIVE as ACTIVE_
          //	    ApplicateGlyph( Img, fbsInactive, fdsDefault, 0 )
          //	  else
          ApplicateGlyph(Img, fbsActive, fdsDefault, i);
        end;
      end
      else
      begin //..._______________INACTIVE_

        if (FDrawMode = dmAutoFlat)
          and ({fPushed or }(not (boRaisedInactive in FOptions))) then
          ApplicateGlyph(Img, fbsOriginal, DrawState, 2)
        else
        begin
          if (FDrawMode = dmAutoFlat) then
            i := 1
          else
            i := 0;
          ApplicateGlyph(Img, fbsInactive, DrawState, i);
        end;
      end;
    end
    else
    begin //..._____________DISABLED_
      if (FDrawMode = dmAutoFlat) and (boRaisedInactive in Options) then
        i := 1
      else
        i := 0;
      if DrawMode <> dmUseImageList then //...auto disabled
        ApplicateGlyph(Img, fbsDisabled, fdsDisabled, i)
      else
      begin //...user's disabled
        if FGlyphsIndexes.Disabled = -1 then
          CreateBitmapExt(Img.Canvas.Handle, FGlyphInactive, ClientRect, 0, 0,
            fwoNone, fdsDisabled, true, FTransparentColor, DisabledMaskColor)
        else
          CreateBitmapExt(Img.Canvas.Handle, FGlyphDisabled, ClientRect, 0, 0,
            fwoNone, fdsDefault, true, FTransparentColor, DisabledMaskColor);
      end;
    end;
    Canvas.Draw(0, 0, Img);
    //BitBlt( Canvas.Handle, 0, 0, Height, Width, Img.Canvas.Handle, 0, 0, SRCCOPY );
  end;
end;

procedure TJvgButton.Paint_;
//var R: TRect;
begin
  //  r:=Bounds(left-1,top-1,Width+1,Height+1);
  //InvalidateRect( Parent.Handle, @R, false);
  if not Enabled then exit;
  if FChangeColorOnActivate.FromColor <> FChangeColorOnActivate.ToColor then
  begin
    rePaint;
    exit;
  end;
  if ((FDrawMode = dmAutoCtrl3D) or (FDrawMode = dmAutoShadow)) and (not fShowingAsPushedNow) and (not fPushed) and (not (boDelicateInactive in FOptions)) then exit;
  if (FDrawMode = dmAutoFlat)
    and (not fShowingAsPushedNow)
    and (not fPushed)
    and (boRaisedInactive in FOptions)
    and (not (boDelicateInactive in FOptions)) then exit;

  rePaint;
  exit;
  //Refresh;exit;

  if (FDrawMode = dmAutoFlat)
    and (fShowingAsPushedNow or (not (boRaisedInactive in FOptions))) then
  begin
    repaint;
    exit;
  end;
  if fPushed then
  begin
    if (boRefreshOnPush in FOptions) or (FDrawMode = dmAutoShadow) then
      Repaint
    else
      Paint;
  end
  else if boRefreshOnActivate in FOptions then
    Repaint
  else
    Paint;
end;

procedure TJvgButton.ApplicateGlyph(var TargetBMP: TBitmap;
  State: TglBtnState;
  DrawState: TglDrawState;
  s: integer);
var
  i, j: integer;
  fChangeColor, fCanBlink: boolean;
  DrawState2: TglDrawState;
begin
  with Glyphs do
  begin
    i := 1;
    j := 1;
    fChangeColor := false;
    fCanBlink := false;
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
          i := 2;
          j := 2;
        end;
      dmUseImageList:
        begin
          i := 0;
          j := 0;
          s := 0;
        end;
    end;

    case State of
      fbsOriginal:
        begin
          CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyph, ClientRect, s, s,
            fwoNone, DrawState, true, FTransparentColor, DisabledMaskColor);
          exit;
        end;
      fbsInactive, fbsDisabled:
        begin
          if (DrawMode = dmAutoFlat) and (boRaisedInactive in FOptions) then
            CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphActive, ClientRect, s, s,
              fwoNone, DrawState, true, FTransparentColor, DisabledMaskColor)
          else
            CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphInactive, ClientRect, 0, 0,
              fwoNone, DrawState, true, FTransparentColor, DisabledMaskColor);

          if State = fbsDisabled then
          begin
            i := 0;
            j := 0;
          end;
          fCanBlink := boBlinkWhenInactive in Options;
        end;
      fbsActive:
        begin
          if (FDrawMode = dmAutoCtrl3D) or (DrawMode = dmAutoShadow) then
            CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphInactive, ClientRect, s, s,
              fwoNone, DrawState, true, FTransparentColor, DisabledMaskColor)
          else
            CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphActive, ClientRect, s, s,
              fwoNone, DrawState, true, FTransparentColor, DisabledMaskColor);
          fChangeColor := boChangeColorWhenActive in Options;
          fCanBlink := boBlinkWhenActive in Options;
        end;
      fbsPushed:
        begin
          CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphPushed, ClientRect, s, s,
            fwoNone, DrawState, true, FTransparentColor, DisabledMaskColor);
          fChangeColor := boChangeColorWhenPushed in Options;
          fCanBlink := boBlinkWhenPushed in Options;
        end;
    end;
    GetBitmap_(FGlyphsIndexes.Inactive, TmpBMP);
    fCanBlink := fCanBlink and fNeedBlink;
    if fCanBlink then
      fBlinked := not fBlinked
    else if State = fbsActive then
      fBlinked := FChangeColorOnActivate.FromColor <> FChangeColorOnActivate.ToColor
    else
      fBlinked := FChangeColorOnPush.FromColor <> FChangeColorOnPush.ToColor;

    if fCanBlink then
    begin
      if fBlinked then
        if State = fbsPushed then
          with FChangeColorOnPush do
            ChangeBitmapColor(TmpBMP, FromColor, ToColor)
        else
          with FChangeColorOnActivate do
            ChangeBitmapColor(TmpBMP, FromColor, ToColor);
    end
    else if fChangeColor and (FDrawMode <> dmUseImageList) then
      if State = fbsActive then
        with FChangeColorOnActivate do
          ChangeBitmapColor(TmpBMP, FromColor, ToColor)
      else
        with FChangeColorOnPush do
          ChangeBitmapColor(TmpBMP, FromColor, ToColor);
    fNeedBlink := false;
    if (DrawMode = dmAutoShadow) and (State = fbsPushed)
      or (FDrawMode = dmUseImageList) then exit;

    if DrawState2 = fdsDisabled then
    begin
      TmpBMP.Canvas.Brush.Color := FTransparentColor;
      TmpBMP.Canvas.Font.Color := clBtnFace;
      //    SetBkColor(TmpBMP.Canvas.Handle, FTransparentColor);
      TmpBMP.Monochrome := true;
      TmpBMP.Monochrome := false;
      CreateBitmapExt(TargetBMP.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
        i + s, j + s, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
      GetBitmap_(FGlyphsIndexes.Inactive, TmpBMP);
    end;

    CreateBitmapExt(TargetBMP.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
      i + s, j + s, fwoNone, DrawState2, true, FTransparentColor, FDisabledMaskColor);
  end;
end;

{procedure TJvgButton.WMSize(var Message: TWMSize);
begin
  Width := FGlyphInactive.Width; Height := FGlyphInactive.Height;
end;}

procedure TJvgButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  fMouseInControl_: boolean;
begin
  inherited MouseMove(Shift, X, Y);
  pt.x := X;
  pt.y := Y;
  if IsPointInRect(pt, ClientRect) then
  begin
    fMouseInControl_ := IsMouseInControl;
    if fMouseInControl_ <> fMouseInControl then
    begin
      if fMouseInControl then
        if Assigned(FOnMouseEnter) then
          FOnMouseEnter(self)
        else if Assigned(FOnMouseLeave) then
          FOnMouseLeave(self);
      fMouseInControl := fMouseInControl_;
      Paint_;
    end;
  end;
end;

{procedure TJvgButton.CMMouseEnter(var Message: TMessage);
begin
  fMouseInControl:=true; Paint_;
end;}

procedure TJvgButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  //fMouseInControl:=IsMouseInControl;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(self);
  fMouseInControl := false;
  Paint_;
end;

procedure TJvgButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  pt: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or (not Enabled) or (not IsMouseInControl) then exit;

  if boShiftMouseOnPush in FOptions then
  begin
    GetCursorPos(pt);
    SetCursorPos(pt.x + MShift.x, pt.y + MShift.y);
  end;
  //  if not FMouseInControl then FMouseInControl := True;
  fPushed := true;
  Paint_;
end;

procedure TJvgButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  pt: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if fShowingAsPushedNow and Assigned(FOnClick) then FOnClick(self);
  if (boShiftMouseOnPush in FOptions) and IsMouseInControl then
  begin
    GetCursorPos(pt);
    SetCursorPos(pt.x - MShift.x, pt.y - MShift.y);
  end;
  fPushed := false;
  Paint_;
end;

procedure TJvgButton.GetBitmaps;
begin
  if not fLoaded then exit;
  with Glyphs do
  begin
    FGlyphInactive.Width := 0;
    FGlyphPushed.Width := 0;
    FGlyphActive.Width := 0;
    FGlyphDisabled.Width := 0;
    FGlyphMask.Width := 0;
    if FDrawMode = dmUseImageList then
    begin
      if not Assigned(FGlyphsList) then exit;
      with FGlyphsList, FGlyphsIndexes do
      begin
        if (Inactive < 0) and (Inactive > (Count - 1)) then Inactive := 0;
        if (Pushed < 0) and (Pushed > (Count - 1)) then Pushed := 1;
        if (Active > (Count - 1)) then Active := -1;
        if (Mask > (Count - 1)) then Mask := -1;

        if Inactive <> -1 then GetBitmap_(Inactive, FGlyphInactive);
        if Pushed <> -1 then GetBitmap_(Pushed, FGlyphPushed);
        if Active <> -1 then GetBitmap_(Active, FGlyphActive); //...optional bitmap
        if Disabled <> -1 then GetBitmap_(Disabled, FGlyphDisabled); //...optional bitmap
        if Mask <> -1 then GetBitmap_(Mask, FGlyphMask); //...optional bitmap
        FNumGlyphs := Count;
        fBimapsCreated := not (FGlyphInactive.Empty or FGlyphPushed.Empty);
      end;
    end
    else
      CreateBitmaps;
    fBimapsCreated := true;

    case FDrawMode of
      dmAutoShadow:
        if boDrawPushedAsFlat in FOptions then
        begin
          MShift.x := 1;
          MShift.y := 1;
        end
        else
        begin
          MShift.x := FShadowDepth - 1;
          MShift.y := FShadowDepth - 1;
        end;
      dmAutoCtrl3D:
        begin
          MShift.x := 2;
          MShift.y := 2;
        end;
      dmAutoFlat:
        begin
          MShift.x := 1;
          MShift.y := 1;
        end;
    else
      begin
        MShift.x := FShiftMaskWhenPushed.x;
        MShift.y := FShiftMaskWhenPushed.y;
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
  i: word;

  procedure RemakeTmpBMP;
  begin
    SetBkColor(TmpBMP.Canvas.Handle, ColorToRGB(FTransparentColor));
    BitBlt(TmpBMP.Canvas.Handle, 0, 0, TmpBMP.Width, TmpBMP.Height, MonoDC, 0, 0, SRCCOPY);
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
          TmpBMP.Width, TmpBMP.Height, TmpBMP.Canvas.Handle, 0, 0, SRCCOPY);

        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          FShadowDepth, FShadowDepth, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        exit;
      end;

      if FDrawMode = dmAutoCtrl3D then
        i := 3
      else
        i := 3;
      with FGlyphInactive do
      begin
        Width := TmpBMP.Width + i;
        Height := TmpBMP.Height + i;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := FTransparentColor;
        Canvas.FillRect(Rect(0, 0, Width, Height));
      end;
      if not (boDrawPushedAsFlat in FOptions) then
        with FGlyphPushed do
        begin
          Width := TmpBMP.Width + i;
          Height := TmpBMP.Height + i;
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := FTransparentColor;
          Canvas.FillRect(Rect(0, 0, Width, Height));
        end;
      with FGlyphActive do
      begin
        Width := TmpBMP.Width + i;
        Height := TmpBMP.Height + i;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := FTransparentColor;
        Canvas.FillRect(Rect(0, 0, Width, Height));
      end;

      //    TmpBMP.Width:=TmpBMP.Width+i;  TmpBMP.Height:=TmpBMP.Height+i;

      //===========================-AUTOCTRL3D-=================================

      if FDrawMode = dmAutoCtrl3D then //..._add 3d border to inactive BEGIN_
      begin
        //============================================.CTRL3D INACTIVE.=
        RemakeTmpBMP;
        //...__________________________________________________Dark Shadow
        if clBlack <> FColorDarkShadow then
          ChangeBitmapColor(TmpBMP, clBlack, FColorDarkShadow);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            0, 3, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            3, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          3, 3, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);

        RemakeTmpBMP;
        //...__________________________________________________Highlight
        ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          0, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            2, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            0, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        end;
        RemakeTmpBMP;
        //...__________________________________________________Shadow
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            1, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
          CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            2, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          2, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        //============================================.CTRL3D PUSHED.=
        if boDrawPushedAsFlat in FOptions then exit;
        RemakeTmpBMP;
        //...__________________________________________________Highlight
        ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            0, 3, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            3, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          3, 3, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);

        RemakeTmpBMP;
        //...__________________________________________________Dark Shadow
        if clBlack <> FColorDarkShadow then
          ChangeBitmapColor(TmpBMP, clBlack, FColorDarkShadow);

        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          0, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            2, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            0, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        end;
        RemakeTmpBMP;
        //...__________________________________________________Shadow
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            1, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
          CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            2, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          1, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        RemakeTmpBMP;
        exit;
      end;

      //===========================-AUTOFLAT-=================================

      if FDrawMode = dmAutoFlat then
      begin
        //============================================.FLAT INACTIVE.=
        CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          0, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        //============================================.FLAT ACTIVE.=
        RemakeTmpBMP;
        //...__________________________________________________Shadow
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            0, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
          CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            2, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        end;
        CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          2, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        //...__________________________________________________Highlight
        RemakeTmpBMP;
        ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
        CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          0, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        if boShadowSurround in FOptions then
        begin
          CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            1, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
          CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
            0, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        end;
        RemakeTmpBMP;
      end;
      //============================================.FLAT PUSHED.=
      if boDrawPushedAsFlat in FOptions then exit;
      RemakeTmpBMP;
      //...__________________________________________________Highlight
      ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
      if boShadowSurround in FOptions then
      begin
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          0, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          2, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
      end;
      CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
        2, 2, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
      RemakeTmpBMP;
      //...__________________________________________________Shadow
      if clBlack <> FColorShadow then
        ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);

      CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
        0, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
      if boShadowSurround in FOptions then
      begin
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          1, 0, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
        CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width, TmpBMP.Height),
          0, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor);
      end;
      RemakeTmpBMP;
    finally
      {      RemakeTmpBMP;
            ChangeBitmapColor( TmpBMP, clBlack, clBtnFace );
            CreateBitmapExt( FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0,0,TmpBMP.Width,TmpBMP.Height),
               1, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor );
            CreateBitmapExt( FGlyphActive.Canvas.Handle, TmpBMP, Rect(0,0,TmpBMP.Width,TmpBMP.Height),
               1, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor );
           } DeleteObject(SelectObject(MonoDC, OldMonoBMP));
    end;
  end;
end;

function TJvgButton.IsMouseInControl: boolean;
var
  pt: TPoint;
  PixelColor: TCOLORREF;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  if fShowingAsPushedNow and fPushed then
  begin
    dec(pt.x, FShiftMaskWhenPushed.x);
    dec(pt.y, FShiftMaskWhenPushed.y);
  end
  else if FDrawMode = dmAutoShadow then
  begin
    inc(pt.x, FShadowDepth);
    inc(pt.y, FShadowDepth);
  end;

  {  if (FDrawMode = dmAutoShadow)and(boDrawPushedAsFlat in Options) then
    begin
      dec( pt.x, FShadowDepth );
      dec( pt.y, FShadowDepth );
    end;}
  dec(pt.x);
  dec(pt.y);

  //	  (FDrawMode = dmAutoFlat)and(fPushed or FRaisedInactive
  if FGlyphsIndexes.Mask = -1 then //...__mask is absent_
  begin
    with FGlyphs do
      case FDrawMode of
        dmAutoShadow:
          PixelColor := GetPixel(FGlyphPushed.Canvas.Handle, pt.x, pt.y);
        dmAutoFlat:
          PixelColor := GetPixel(FGlyphActive.Canvas.Handle, pt.x, pt.y);
      else
        PixelColor := GetPixel(FGlyphInactive.Canvas.Handle, pt.x, pt.y);
      end;
    Result := (PixelColor <> FTransparentColor) and (PixelColor <> -1);
  end
  else //...__use mask_
  begin
    with FGlyphs do
      PixelColor := GetPixel(FGlyphMask.Canvas.Handle, pt.x, pt.y);
    Result := (PixelColor = clWhite) and (PixelColor <> -1);
  end;
end;

procedure TJvgButton.SmthChanged(Sender: TObject);
begin
  GetBitmaps;
  Invalidate;
end;

procedure TJvgButton.GetBitmap_(Index: integer; var Bmp: TBitmap);
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
    MessageDlg('Error during access GlyphsList or Glyph property',
      mtError, [mbOk], 0);
    raise;
  end;
end;

procedure TJvgButton.OnBlinkTimer(Sender: TObject);
var
  ParentForm: TForm;
  i: integer;

  procedure Blink(FreeButton: TJvgButton);
  begin
    with FreeButton do
    begin
      fNeedBlink := false;
      if fShowingAsPushedNow then
        with FChangeColorOnPush do
          if (boBlinkWhenPushed in Options) and (FromColor <> ToColor) then
          begin
            fNeedBlink := true;
            repaint;
            exit;
          end
          else
            exit;
      if fMouseInControl then
        with FChangeColorOnActivate do
          if (boBlinkWhenActive in Options) and (FromColor <> ToColor) then
          begin
            fNeedBlink := true;
            repaint;
            exit;
          end
          else
            exit;
      if not fMouseInControl then
        with FChangeColorOnActivate do
          if (boBlinkWhenInactive in Options) and (FromColor <> ToColor) then
          begin
            fNeedBlink := true;
            repaint;
            exit;
          end
          else
            exit;
    end;
  end;
begin //...main proc
  if (not FTestMode) and (csDesigning in ComponentState) then exit;
  ParentForm := GetParentForm(self);
  for i := 0 to ParentForm.ComponentCount - 1 do
    if (ParentForm.Components[i] is TJvgButton)
      and (TJvgButton(ParentForm.Components[i]).BlinkTimer = FBlinkTimer) then
      with ParentForm.Components[i] as TJvgButton do
      begin
        Blink(TJvgButton(ParentForm.Components[i]));
      end;

end;

//*************************************{ . TJvgButton properties methods. }

procedure TJvgButton.SetDrawMode(Value: TDrawMode);
begin
  if FDrawMode = Value then exit;
  //  if (Value=dmUseGlyphList)and(not Assigned(GlyphList)) then
  //  if (Value<>dmUseimageList)and(not Assigned(Glyph)) then
  //   FGlyph.Assign(FGlyphInactive);
  FDrawMode := Value;

  GetBitmaps;
  Invalidate;
end;

procedure TJvgButton.SetGlyphsList(Value: TImageList);
begin
  if (not Assigned(Value)) {or(Value.Count<2)} then exit;
  FGlyphsList := Value;
  GetBitmaps;
  Invalidate;
end;

procedure TJvgButton.SetGlyph(Value: TBitmap);
begin
  //  if (not Assigned(Value)) then exit;
  if Assigned(FGlyph) then
  begin
    FGlyph.Free;
    FGlyph := TBitmap.Create;
  end;
  FGlyph.Assign(Value);
  begin
    GetBitmaps;
    Invalidate;
  end;
  AutoTransparentColor := AutoTransparentColor;
  //if FDrawMode <> dmUseImageList then begin GetBitmaps; Invalidate; end;
end;

procedure TJvgButton.SetNumGlyphs(Value: integer);
begin
  if (Value < 2) or (Value > 4) then exit;
  FNumGlyphs := Value;
end;

procedure TJvgButton.SetTransparentColor(Value: TColor);
begin
  if (FAutoTrColor <> ftcUser) or (FTransparentColor = Value) then exit;
  FTransparentColor := Value;
  GetBitmaps;
  Invalidate;
end;

procedure TJvgButton.SetEnabled(Value: boolean);
begin
  if FEnabled = Value then exit;
  FEnabled := Value;
  Repaint;
end;

procedure TJvgButton.SetShadowDepth(Value: word);
begin
  if FShadowDepth = Value then exit;
  FShadowDepth := Value;
  if FDrawMode = dmAutoShadow then
  begin
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetColorHighlight(Value: TColor);
begin
  if FColorHighlight = Value then exit;
  FColorHighlight := Value;
  if FDrawMode <> dmUseImageList then
  begin
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetColorShadow(Value: TColor);
begin
  if FColorShadow = Value then exit;
  FColorShadow := Value;
  if FDrawMode <> dmUseImageList then
  begin
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetColorDarkShadow(Value: TColor);
begin
  if FColorDarkShadow = Value then exit;
  FColorDarkShadow := Value;
  if FDrawMode <> dmUseImageList then
  begin
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor = Value then exit;
  FDisabledMaskColor := Value;
  if FDrawMode <> dmUseImageList then
  begin
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetOptions(Value: TglButtonOptions);
begin
  if FOptions = Value then exit;
  FOptions := Value;
  if FDrawMode <> dmUseImageList then
  begin
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetAutoTrColor(Value: TglAutoTransparentColor);
var
  x, y: integer;
  TmpBmp_: TBitmap;
begin
  FAutoTrColor := Value;
  TmpBmp_ := nil;
  if {(FAutoTrColor=ftcUser)or}(FGlyph.Width = 0) or (FGlyph.Height = 0) then exit;
  try
    with FGlyph do
      case FAutoTrColor of
        ftcLeftTopPixel:
          begin
            x := 0;
            y := 0;
          end;
        ftcLeftBottomPixel:
          begin
            x := 0;
            y := Height - 1;
          end;
        ftcRightTopPixel:
          begin
            x := Width - 1;
            y := 0;
          end;
        ftcRightBottomPixel:
          begin
            x := Width - 1;
            y := Height - 1;
          end;
      else
        exit;
      end;
    TmpBmp_ := TBitmap.Create;
    TmpBmp_.Assign(FGlyph);
    //  if not (csDesigning in ComponentState) then
    FTransparentColor := GetPixel(TmpBmp_.Canvas.Handle, x, y);
  finally
    if Assigned(TmpBmp_) then TmpBmp_.Free;
    GetBitmaps;
    Invalidate;
  end;
end;

procedure TJvgButton.SetBlinkTimer(Value: TTimer);
var
  ParentForm: TForm;
  i: integer;
  p1, p2: TNotifyEvent;
  Timer: TTimer;
begin
  if FBlinkTimer = Value then exit;
  if Assigned(FBlinkTimer) then
  begin
    p1 := FBlinkTimer.OnTimer;
    p2 := OnBlinkTimer;
    if @FBlinkTimer.OnTimer = @p2 then //...points at me
    begin
      ParentForm := GetParentForm(self);
      for i := 0 to ParentForm.ComponentCount - 1 do
        if (ParentForm.Components[i] is TJvgButton)
          and (TJvgButton(ParentForm.Components[i]) <> self)
          and (TJvgButton(ParentForm.Components[i]).BlinkTimer = FBlinkTimer) then
        begin
          Timer := FBlinkTimer;
          FBlinkTimer := nil;
          Timer.OnTimer := TJvgButton(ParentForm.Components[i]).OnBlinkTimer;
          break;
        end;
      if Assigned(FBlinkTimer) and (@FBlinkTimer.OnTimer = @p2) then FBlinkTimer.OnTimer := nil;
    end;
  end
  else
  begin
    FBlinkTimer := nil;
  end;

  FBlinkTimer := Value;
  if Assigned(FBlinkTimer) then FBlinkTimer.OnTimer := OnBlinkTimer;
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

procedure TJvgButton.SetTestMode(Value: boolean);
var
  ParentForm: TForm;
  i: integer;
begin
  ParentForm := GetParentForm(self);
  for i := 0 to ParentForm.ComponentCount - 1 do
    if (ParentForm.Components[i] is TJvgButton) then
      TJvgButton(ParentForm.Components[i]).FTestMode := Value;
end;
//*************************************{ . TJvgGlyphsIndexes methods. }

constructor TJvgGlyphsIndexes.Create;
begin
  inherited Create;
  FInactive := 0;
  FPushed := 1;
  FActive := 2;
  FDisabled := -1;
  FMask := 3;
end;

procedure TJvgGlyphsIndexes.SetInactive(Value: integer);
begin
  FInactive := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgGlyphsIndexes.SetPushed(Value: integer);
begin
  FPushed := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgGlyphsIndexes.SetActive(Value: integer);
begin
  FActive := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgGlyphsIndexes.SetDisabled(Value: integer);
begin
  FDisabled := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgGlyphsIndexes.SetMask(Value: integer);
begin
  FMask := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

end.
