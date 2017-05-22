{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSwitch.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSwitch;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Menus,
  JvJCLUtils, JvComponent;

type
  TTextPos = (tpNone, tpLeft, tpRight, tpAbove, tpBelow);
  TSwitchBitmaps = set of Boolean;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvSwitch = class(TJvCustomControl)
  private
    FActive: Boolean;
    FBitmaps: array [Boolean] of TBitmap;
    FDisableBitmaps: array [Boolean] of TBitmap;
    FOnOn: TNotifyEvent;
    FOnOff: TNotifyEvent;
    FStateOn: Boolean;
    FTextPosition: TTextPos;
    FBorderStyle: TBorderStyle;
    FToggleKey: TShortCut;
    FShowFocus: Boolean;
    FUserBitmaps: TSwitchBitmaps;
    procedure GlyphChanged(Sender: TObject);
    procedure SetStateOn(Value: Boolean);
    procedure SetTextPosition(Value: TTextPos);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetSwitchGlyph(Index: Integer): TBitmap;
    procedure SetSwitchGlyph(Index: Integer; Value: TBitmap);
    function StoreBitmap(Index: Integer): Boolean;
    procedure SetShowFocus(Value: Boolean);
    procedure CreateDisabled(Index: Boolean);
    procedure ReadBinaryData(Stream: TStream);
    procedure WriteBinaryData(Stream: TStream);
  protected
    procedure FocusChanged(Control: TWinControl); override;
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; override;
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;
    procedure TextChanged; override;
    procedure EnabledChanged; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure DoOn; dynamic;
    procedure DoOff; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ToggleSwitch;
  published
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabled}
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Caption;
    property Color;
    property Cursor;
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Enabled;
    property Font;
    property GlyphOff: TBitmap index 0 read GetSwitchGlyph write SetSwitchGlyph stored StoreBitmap;
    property GlyphOn: TBitmap index 1 read GetSwitchGlyph write SetSwitchGlyph stored StoreBitmap;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property ToggleKey: TShortCut read FToggleKey write FToggleKey default Ord(' ');
    property ShowHint;
    property StateOn: Boolean read FStateOn write SetStateOn default False;
    property TabOrder;
    property TabStop default True;
    property TextPosition: TTextPos read FTextPosition write SetTextPosition default tpNone;
    property Anchors;
    property Constraints;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnContextPopup;
    property OnOn: TNotifyEvent read FOnOn write FOnOn;
    property OnOff: TNotifyEvent read FOnOff write FOnOff;
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
  JvJVCLUtils, JvThemes;

{$R JvSwitch.Res}

constructor TJvSwitch.Create(AOwner: TComponent);
var
  I: Boolean;
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csSetCaption, csCaptureMouse,
    csOpaque, csDoubleClicks];
  IncludeThemeStyle(Self, [csParentBackground]);
  Width := 50;
  Height := 60;
  for I := False to True do
  begin
    FBitmaps[I] := TBitmap.Create;
    SetSwitchGlyph(Ord(I), nil);
    FBitmaps[I].OnChange := GlyphChanged;
  end;
  FUserBitmaps := [];
  FShowFocus := True;
  FStateOn := False;
  FTextPosition := tpNone;
  FBorderStyle := bsNone;
  FToggleKey := Ord(' ');
  TabStop := True;
end;

destructor TJvSwitch.Destroy;
var
  I: Boolean;
begin
  for I := False to True do
  begin
    FBitmaps[I].OnChange := nil;
    FDisableBitmaps[I].Free;
    FBitmaps[I].Free;
  end;
  inherited Destroy;
end;


procedure TJvSwitch.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of Longint = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style or CS_HREDRAW or CS_VREDRAW;
    Style := Style or Longword(BorderStyles[FBorderStyle]);
  end;
end;


procedure TJvSwitch.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := FUserBitmaps <> TJvSwitch(Filer.Ancestor).FUserBitmaps
    else
      Result := FUserBitmaps <> [];
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadBinaryData, WriteBinaryData,DoWrite);
end;


function TJvSwitch.GetPalette: HPALETTE;
begin
  if Enabled then
    Result := FBitmaps[FStateOn].Palette
  else
    Result := 0;
end;


procedure TJvSwitch.ReadBinaryData(Stream: TStream);
begin
  Stream.ReadBuffer(FUserBitmaps, SizeOf(FUserBitmaps));
end;

procedure TJvSwitch.WriteBinaryData(Stream: TStream);
begin
  Stream.WriteBuffer(FUserBitmaps, SizeOf(FUserBitmaps));
end;

function TJvSwitch.StoreBitmap(Index: Integer): Boolean;
begin
  Result := (Index <> 0) in FUserBitmaps;
end;

function TJvSwitch.GetSwitchGlyph(Index: Integer): TBitmap;
begin
  if csLoading in ComponentState then
    Include(FUserBitmaps, Index <> 0);
  Result := FBitmaps[Index <> 0];
end;

procedure TJvSwitch.CreateDisabled(Index: Boolean);
begin
  FreeAndNil(FDisableBitmaps[Index]);
  FDisableBitmaps[Index] :=
    CreateDisabledBitmap(FBitmaps[Index], clBlack);
end;

procedure TJvSwitch.GlyphChanged(Sender: TObject);
var
  I: Boolean;
begin
  for I := False to True do
    if Sender = FBitmaps[I] then
      CreateDisabled(I);
  Invalidate;
end;

procedure TJvSwitch.SetSwitchGlyph(Index: Integer; Value: TBitmap);
const
  ResName: array [Boolean] of PChar = ('JvSwitchOFF', 'JvSwitchON');
begin
  if Value <> nil then
  begin
    FBitmaps[Index <> 0].Assign(Value);
    Include(FUserBitmaps, Index <> 0);
  end
  else
  begin
    FBitmaps[Index <> 0].Assign(nil); // fixes GDI resource leak
    FBitmaps[Index <> 0].LoadFromResourceName(HInstance, ResName[Index <> 0]);
    Exclude(FUserBitmaps, Index <> 0);
  end;
end;

procedure TJvSwitch.FocusChanged(Control: TWinControl);
var
  Active: Boolean;
begin
  Active := (Control = Self);
  if Active <> FActive then
  begin
    FActive := Active;
    if FShowFocus then
      Invalidate;
  end;
  inherited FocusChanged(Control);
end;

procedure TJvSwitch.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

procedure TJvSwitch.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

function TJvSwitch.WantKey(Key: Integer; Shift: TShiftState): Boolean;
begin
  Result := IsAccel(Key, Caption) and CanFocus and (ssAlt in Shift);
  if Result then
    SetFocus;
end;

function TJvSwitch.DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean;
begin
  Result := True; // the component paints the background in Paint
end;

procedure TJvSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if TabStop and CanFocus then
      SetFocus;
    ToggleSwitch;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FToggleKey = ShortCut(Key, Shift) then
  begin
    ToggleSwitch;
    Key := 0;
  end;
end;

procedure TJvSwitch.Paint;
var
  ARect: TRect;
  Text: string;
  FontHeight: Integer;

  procedure DrawBitmap(Bmp: TBitmap);
  var
    IWidth, IHeight, X, Y: Integer;
    IRect: TRect;
  begin
    IWidth := Bmp.Width;
    IHeight := Bmp.Height;
    IRect := Rect(0, 0, IWidth, IHeight);
    X := 0;
    Y := 0;
    case FTextPosition of
      tpNone:
        begin
          X := ((Width - IWidth) div 2);
          Y := ((Height - IHeight) div 2);
        end;
      tpLeft:
        begin
          X := Width - IWidth;
          Y := ((Height - IHeight) div 2);
          Dec(ARect.Right, IWidth);
        end;
      tpRight:
        begin
          X := 0;
          Y := ((Height - IHeight) div 2);
          Inc(ARect.Left, IWidth);
        end;
      tpAbove:
        begin
          X := ((Width - IWidth) div 2);
          Y := Height - IHeight;
          Dec(ARect.Bottom, IHeight);
        end;
      tpBelow:
        begin
          X := ((Width - IWidth) div 2);
          Y := 0;
          Inc(ARect.Top, IHeight);
        end;
    end;
    Bmp.Transparent := True;
    Canvas.Draw(X, Y, Bmp);
    if Focused and FShowFocus and TabStop and not (csDesigning in ComponentState) then
      Canvas.DrawFocusRect(Rect(X, Y, X + IWidth, Y + IHeight));
  end;

begin
  ARect := GetClientRect;
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  DrawThemedBackground(Self, Canvas, ARect);
  if not Enabled and (FDisableBitmaps[FStateOn] <> nil) then
    DrawBitmap(FDisableBitmaps[FStateOn])
  else
    DrawBitmap(FBitmaps[FStateOn]);
  if FTextPosition <> tpNone then
  begin
    FontHeight := Canvas.TextHeight('W');
    ARect.Top := ((ARect.Bottom + ARect.Top) - FontHeight) shr 1;
    ARect.Bottom := ARect.Top + FontHeight;
    Text := Caption;
    DrawText(Canvas, Text, Length(Caption), ARect, DT_EXPANDTABS or DT_VCENTER or DT_CENTER);
  end;
end;

procedure TJvSwitch.DoOn;
begin
  if Assigned(FOnOn) then
    FOnOn(Self);
end;

procedure TJvSwitch.DoOff;
begin
  if Assigned(FOnOff) then
    FOnOff(Self);
end;

procedure TJvSwitch.ToggleSwitch;
begin
  StateOn := not StateOn;
end;

procedure TJvSwitch.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvSwitch.SetStateOn(Value: Boolean);
begin
  if FStateOn <> Value then
  begin
    FStateOn := Value;
    Invalidate;
    if Value then
      DoOn
    else
      DoOff;
  end;
end;

procedure TJvSwitch.SetTextPosition(Value: TTextPos);
begin
  if FTextPosition <> Value then
  begin
    FTextPosition := Value;
    Invalidate;
  end;
end;

procedure TJvSwitch.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    if not (csDesigning in ComponentState) then
      Invalidate;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
