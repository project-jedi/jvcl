{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHint.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Rob den Braasem [rbraasem@xs4all.nl].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgHint;

interface

uses Windows,
  Messages,
  Controls,
  Classes,
  Forms,
  JVComponent,
  JvgCommClasses,
  SysUtils,
  Graphics;

var
  lpFrHintComponent: Pointer;

type
  TJvgHintWindow = class;

  TJvgHint = class(TComponent)
  private
    FOnShowHint 	: TShowHintEvent;
    FOnHint		: TNotifyEvent;
    FActive             : boolean;
    AOnHintOld		: TNotifyEvent;
    AOnShowHintOld	: TShowHintEvent;
    FShowHint : boolean;
    AHintWindow : THintWindow;
    AHintControl : TControl;
    FGlyph: TBitmap;
    FHintStyle: TJvgHintStyle;
    FSpacing: integer;
    FGlyphAlign: TJvg2DAlign;
    FAlignment: TAlignment;
    procedure SeTJvgyph(const Value: TBitmap);
  protected
    procedure Notification( Component: TComponent; Operation: TOperation ); override;
    procedure Loaded; override;
    procedure OnHintNew(Sender: TObject);
    procedure OnShowHintNew(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure InitHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowHintAt(x, y: integer; caption: string);
  published
    property Active : boolean read FActive write FActive;
    property ShowHint : boolean read FShowHint write FShowHint;
    property Glyph: TBitmap read FGlyph write SeTJvgyph;
    property Style: TJvgHintStyle read FHintStyle write FHintStyle;
    property Spacing: integer read FSpacing write FSpacing;
    property GlyphAlign: TJvg2DAlign read FGlyphAlign write FGlyphAlign;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property OnShowHint : TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnHint : TNotifyEvent read FOnHint write FOnHint;
//    property OnDraw : TDrawEvent read FOnDraw write FOnDraw;
//    property OnActivateHint : THintActivateEvent read FActivateHint write FActivateHint;
//    property OnHintHide : THintHideEvent read FOnHintHide write FOnHintHide;
  end;

  TJvgHintWindow = class(THintWindow)
  private
  protected
    HintComponent : TJvgHint;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMKillFocus(var Message: TMessage); message WM_ACTIVATE;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

implementation
uses JvgTypes, JvgUtils, ExtCtrls;
{$R ..\resources\GlHint.res}

constructor TJvgHint.Create(AOwner: TComponent);
begin
  inherited;
  FGlyph := TBitmap.Create;
  FHintStyle := TJvgHintStyle.Create;
  FGlyphAlign := TJvg2DAlign.Create;
  FHintStyle.Color := clWindow;
  FHintStyle.Bevel.Inner := bvRaised;
  FHintStyle.Bevel.Outer := bvLowered;
  if not (csDesigning in ComponentState) then InitHint;
  Application.ShowHint := false;
  Application.ShowHint := true;
end;

destructor TJvgHint.Destroy;
begin
  FGlyph.Free;
  FHintStyle.Free;
  FGlyphAlign.Free;
  if Assigned(AOnHintOld) then
  begin
    Application.OnShowHint := AOnShowHintOld;
    Application.OnHint := AOnHintOld;
  end;  
  inherited Destroy;
end;

procedure TJvgHint.Notification( Component: TComponent; Operation: TOperation );
begin
  if (Component <> Self)and(Operation = opInsert)and(Component is TJvgHint ) then
    raise Exception.Create('Cannot create more than one instance of TJvgHint component');
end;

procedure TJvgHint.InitHint;
begin
  with Application do begin
    AOnHintOld := OnHint;
    AOnShowHintOld := OnShowHint;
    OnShowHint := OnShowHintNew;
    OnHint := OnHintNew;
    HintWindowClass := TJvgHintWindow;
    lpFrHintComponent := Self;
  end;
  FShowHint := true;
end;

procedure TJvgHint.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState)and(Active) then
  begin
    InitHint;
    Application.ShowHint := false;
    Application.ShowHint := FShowHint;
  end;
  if FGlyph.Empty then
    FGlyph.LoadFromResourceName(hInstance, 'HELP');
end;

procedure TJvgHint.OnHintNew(Sender: TObject);
begin
  if Assigned(FOnHint) then FOnHint(Sender);
end;

procedure TJvgHint.OnShowHintNew(var HintStr: string;
	     var CanShow: Boolean; var HintInfo: THintInfo);
begin
  AHintControl := HintInfo.HintControl;
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo);
  if CanShow then
  self.ShowHintAt(HintInfo.CursorPos.X,
  HintInfo.CursorPos.Y,
  HintStr);
end;

procedure TJvgHint.ShowHintAt(x, y: integer; Caption: string);
var
  R: TRect;
  HW: TJvgHintWindow;
begin
  HW := TJvgHintWindow.Create(Application);
  R := Bounds(X, Y, 10, 10);

  DrawText( HW.Canvas.Handle, PChar(Caption), length(Caption), R, DT_WORDBREAK or DT_CALCRECT);
  HW.ActivateHint(R, Caption);
end;


constructor TJvgHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  HintComponent := TJvgHint(lpFrHintComponent);
  try
    if Assigned(HintComponent) then HintComponent.AHintWindow := Self;
    with Canvas do
    begin
      Font.Assign(HintComponent.Style.Font);
      {$IFDEF GL_RUS}
      Font.CharSet	    := RUSSIAN_CHARSET;
      {$ENDIF}
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
  tag := 1;
  Width := Width + 20;
  Height := Height + 1;
  if Rect.Top + Height > Screen.Height then
    Rect.Top := Screen.Height - Height;
  if Rect.Left + Width > Screen.Width then
    Rect.Left := Screen.Width - Width;
  if Rect.Left < 0 then Rect.Left := 0;
  if Rect.Bottom < 0 then Rect.Bottom := 0;

  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
	       SWP_SHOWWINDOW or SWP_NOACTIVATE );
end;

procedure TJvgHintWindow.Paint;
var
  R: TRect;
  glyphX, glyphY: integer;
const
  aAlignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
begin
  R := ClientRect;
  dec(R.right, 1); dec(R.Bottom, 1);
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := 0;

  with HintComponent.Style do
  begin
    R := DrawBoxEx( Canvas.Handle, R, Bevel.Sides, Bevel.Inner, Bevel.Outer, Bevel.Bold, Color, Gradient.Active );
    if Gradient.Active then
    begin
      inc(R.Right); inc(R.Bottom);
      Gradient.Draw(Canvas.Handle, R, integer(psSolid), 1);
      dec(R.Right); dec(R.Bottom);
    end;
  end;            

  if Assigned(HintComponent) then
  begin

    case HintComponent.GlyphAlign.Vertical of
      fvaTop:    glyphY := R.Top;
      fvaCenter: glyphY := (R.Bottom - R.Top - HintComponent.Glyph.Height) div 2;
      fvaBottom: glyphY := R.Bottom - HintComponent.Glyph.Height;
    end;
    case HintComponent.GlyphAlign.Horizontal of
      fhaLeft:   glyphX := R.Left + 1;
      fhaCenter: glyphX := (R.Right - R.Left - HintComponent.Glyph.Width) div 2;
      fhaRight:  glyphX := R.Right - HintComponent.Glyph.Width - 2;
    end;

    CreateBitmapExt(Canvas.Handle, HintComponent.Glyph, R, glyphX, glyphY, fwoNone, fdsDefault, true, GetTransparentColor(HintComponent.Glyph, ftcLeftBottomPixel), 0);
    case HintComponent.GlyphAlign.Horizontal of
      fhaLeft:   inc(R.Left, HintComponent.Glyph.Width + HintComponent.Spacing);
      fhaCenter: { nothing ;) };
      fhaRight:  dec(R.Right, HintComponent.Glyph.Width + HintComponent.Spacing);
    end;

  end;

  SetBkMode(Canvas.Handle, TRANSPARENT);

  Canvas.Font.Assign(HintComponent.Style.Font);
  InflateRect(R, -1, -1);
  if ClientRect.Bottom - ClientRect.Top > Canvas.TextHeight('Y')*2 then
    DrawText( Canvas.Handle, PChar(Caption), length(Caption), R, DT_VCENTER or DT_WORDBREAK or aAlignments[HintComponent.Alignment])
  else
    DrawText( Canvas.Handle, PChar(Caption), length(Caption), R, DT_VCENTER or DT_SINGLELINE or aAlignments[HintComponent.Alignment]);
end;



procedure TJvgHintWindow.WMKillFocus(var Message: TMessage);
begin
  hide;
end;


function TJvgHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
begin
  Canvas.Font.Assign(HintComponent.Style.Font);
  Result := inherited CalcHintRect(MaxWidth, AHint, AData);
  if Assigned(HintComponent.Glyph) then
  begin
    Result.Bottom := max(Result.Bottom, HintComponent.Glyph.Height);
    inc(Result.Right, HintComponent.Glyph.Width + HintComponent.Spacing);
  end;
  inc(Result.Bottom, HintComponent.Style.Bevel.BordersHeight);
  inc(Result.Right, HintComponent.Style.Bevel.BordersWidth);
end;

procedure TJvgHint.SeTJvgyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;



end.


