{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHint.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Rob den Braasem [rbraasem att xs4all dott nl].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgHint;

interface

uses
  Windows, Messages, SysUtils, Graphics, Controls, Classes, Forms,
  JvComponent, JvgCommClasses;

type
  TJvgHint = class(TJvComponent)
  private
    FOnShowHint: TShowHintEvent;
    FOnHint: TNotifyEvent;
    FActive: Boolean;
    FOnHintOld: TNotifyEvent;
    FOnShowHintOld: TShowHintEvent;
    FShowHint: Boolean;
    FHintWindow: THintWindow;
    FHintControl: TControl;
    FGlyph: TBitmap;
    FHintStyle: TJvgHintStyle;
    FSpacing: Integer;
    FGlyphAlign: TJvg2DAlign;
    FAlignment: TAlignment;
    procedure SetGlyph(const Value: TBitmap);
    procedure NewHint(Sender: TObject);
    procedure NewShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure InitHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowHintAt(X, Y: Integer; Caption: string);
  published
    property Active: Boolean read FActive write FActive default False;
    property ShowHint: Boolean read FShowHint write FShowHint default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Style: TJvgHintStyle read FHintStyle write FHintStyle;
    property Spacing: Integer read FSpacing write FSpacing default 0;
    property GlyphAlign: TJvg2DAlign read FGlyphAlign write FGlyphAlign;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
  end;

implementation

uses
  Math, ExtCtrls,
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvConsts,
  JvgTypes, JvgUtils;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvgHint.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvgHint.res}
{$ENDIF LINUX}

{$IFNDEF USEJVCL}
resourcestring
  RsEOnlyOneInstanceOfTJvgHint = 'Cannot create more than one instance of TJvgHint component';
{$ENDIF USEJVCL}

type
  TJvgHintWindow = class(THintWindow)
  private
    FHintComponent: TJvgHint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMKillFocus(var Msg: TMessage); message WM_ACTIVATE;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

var
  lpFrHintComponent: TJvgHint;

//=== TJvgHint ===============================================================

constructor TJvgHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TBitmap.Create;
  FHintStyle := TJvgHintStyle.Create;
  FGlyphAlign := TJvg2DAlign.Create;
  FActive := False;
  FShowHint := False;
  FSpacing := 0;
  FAlignment := taLeftJustify;
  FHintStyle.Color := clWindow;
  FHintStyle.Bevel.Inner := bvRaised;
  FHintStyle.Bevel.Outer := bvLowered;
  if not (csDesigning in ComponentState) then
    InitHint;
  Application.ShowHint := False;
  Application.ShowHint := True;
end;

destructor TJvgHint.Destroy;
begin
  FGlyph.Free;
  FHintStyle.Free;
  FGlyphAlign.Free;
  if Assigned(FOnHintOld) then
  begin
    Application.OnShowHint := FOnShowHintOld;
    Application.OnHint := FOnHintOld;
  end;
  inherited Destroy;
end;

procedure TJvgHint.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component <> Self) and (Operation = opInsert) and (Component is TJvgHint) then
    raise Exception.CreateRes(@RsEOnlyOneInstanceOfTJvgHint);
end;

procedure TJvgHint.InitHint;
begin
  with Application do
  begin
    FOnHintOld := OnHint;
    FOnShowHintOld := OnShowHint;
    OnShowHint := NewShowHint;
    OnHint := NewHint;
    HintWindowClass := TJvgHintWindow;
    lpFrHintComponent := Self;
  end;
  FShowHint := True;
end;

procedure TJvgHint.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and Active then
  begin
    InitHint;
    Application.ShowHint := False;
    Application.ShowHint := ShowHint;
  end;
  if Glyph.Empty then
    Glyph.LoadFromResourceName(hInstance, 'HELP');
end;

procedure TJvgHint.NewHint(Sender: TObject);
begin
  if Assigned(FOnHint) then
    FOnHint(Sender);
end;

procedure TJvgHint.NewShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  FHintControl := HintInfo.HintControl;
  if Assigned(FOnShowHint) then
    FOnShowHint(HintStr, CanShow, HintInfo);
  if CanShow then
    Self.ShowHintAt(HintInfo.CursorPos.X, HintInfo.CursorPos.Y, HintStr);
end;

procedure TJvgHint.ShowHintAt(X, Y: Integer; Caption: string);
var
  R: TRect;
  HW: TJvgHintWindow;
begin
  HW := TJvgHintWindow.Create(Application);
  R := Bounds(X, Y, 10, 10);

  DrawText(HW.Canvas.Handle, PChar(Caption), Length(Caption), R, DT_WORDBREAK or DT_CALCRECT);
  HW.ActivateHint(R, Caption);
end;

//=== TJvgHintWindow =========================================================

constructor TJvgHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintComponent := TJvgHint(lpFrHintComponent);
  try
    if Assigned(FHintComponent) then
      FHintComponent.FHintWindow := Self;
    with Canvas do
    begin
      Font.Assign(FHintComponent.Style.Font);
      {$IFDEF GL_RUS}
      Font.CharSet := RUSSIAN_CHARSET;
      {$ENDIF GL_RUS}
    end;
  except
  end;
end;

procedure TJvgHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_BORDER;
end;

procedure TJvgHintWindow.ActivateHint(Rect: TRect; const AHint: string);
begin
  Caption := AHint;
  BoundsRect := Rect;
  Tag := 1;
  Width := Width + 20;
  Height := Height + 1;
  if Rect.Top + Height > Screen.Height then
    Rect.Top := Screen.Height - Height;
  if Rect.Left + Width > Screen.Width then
    Rect.Left := Screen.Width - Width;
  if Rect.Left < 0 then
    Rect.Left := 0;
  if Rect.Bottom < 0 then
    Rect.Bottom := 0;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
    SWP_SHOWWINDOW or SWP_NOACTIVATE);
end;

procedure TJvgHintWindow.Paint;
const
  cAlignments: array [TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R: TRect;
  GlyphX, GlyphY: Integer;
begin
  R := ClientRect;
  Dec(R.Right);
  Dec(R.Bottom);
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := 0;
  GlyphX := 0;
  GlyphY := 0;

  with FHintComponent.Style do
  begin
    R := DrawBoxEx(Canvas.Handle, R, Bevel.Sides, Bevel.Inner, Bevel.Outer, Bevel.Bold, Color, Gradient.Active);
    if Gradient.Active then
    begin
      Inc(R.Right);
      Inc(R.Bottom);
      Gradient.Draw(Canvas.Handle, R, Integer(psSolid), 1);
      Dec(R.Right);
      Dec(R.Bottom);
    end;
  end;

  if Assigned(FHintComponent) then
  begin
    case FHintComponent.GlyphAlign.Vertical of
      fvaTop:
        GlyphY := R.Top;
      fvaCenter:
        GlyphY := (R.Bottom - R.Top - FHintComponent.Glyph.Height) div 2;
      fvaBottom:
        GlyphY := R.Bottom - FHintComponent.Glyph.Height;
    end;
    case FHintComponent.GlyphAlign.Horizontal of
      fhaLeft:
        GlyphX := R.Left + 1;
      fhaCenter:
        GlyphX := (R.Right - R.Left - FHintComponent.Glyph.Width) div 2;
      fhaRight:
        GlyphX := R.Right - FHintComponent.Glyph.Width - 2;
    end;

    CreateBitmapExt(Canvas.Handle, FHintComponent.Glyph, R,
      GlyphX, GlyphY, fwoNone, fdsDefault, True,
      GetTransparentColor(FHintComponent.Glyph, ftcLeftBottomPixel), 0);
    case FHintComponent.GlyphAlign.Horizontal of
      fhaLeft:
        Inc(R.Left, FHintComponent.Glyph.Width + FHintComponent.Spacing);
      fhaCenter:
        { nothing };
      fhaRight:
        Dec(R.Right, FHintComponent.Glyph.Width + FHintComponent.Spacing);
    end;

  end;

  SetBkMode(Canvas.Handle, TRANSPARENT);

  Canvas.Font.Assign(FHintComponent.Style.Font);
  InflateRect(R, -1, -1);
  if ClientRect.Bottom - ClientRect.Top > Canvas.TextHeight('Y') * 2 then
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
      DT_VCENTER or DT_WORDBREAK or cAlignments[FHintComponent.Alignment])
  else
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
      DT_VCENTER or DT_SINGLELINE or cAlignments[FHintComponent.Alignment]);
end;

procedure TJvgHintWindow.WMKillFocus(var Msg: TMessage);
begin
  Hide;
end;

function TJvgHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;
begin
  Canvas.Font.Assign(FHintComponent.Style.Font);
  Result := inherited CalcHintRect(MaxWidth, AHint, AData);
  if Assigned(FHintComponent.Glyph) and not FHintComponent.Glyph.Empty then
  begin
    Result.Bottom := Max(Result.Bottom, FHintComponent.Glyph.Height);
    Inc(Result.Right, FHintComponent.Glyph.Width + FHintComponent.Spacing);
  end;
  Inc(Result.Bottom, FHintComponent.Style.Bevel.BordersHeight);
  Inc(Result.Right, FHintComponent.Style.Bevel.BordersWidth);
end;

procedure TJvgHint.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

end.

