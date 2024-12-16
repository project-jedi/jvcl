{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBitBtn.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBitBtn;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Menus, Types,
  JvTypes, JvExButtons;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvBitBtn = class(TJvExBitBtn)
  private
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FHotGlyph: TBitmap;
    FOldGlyph: TBitmap;
    FDropDown: TPopupMenu;
    FCanvas: TControlCanvas;
    FSimpleFrame: Boolean;
    function GetCanvas: TCanvas;
    procedure SetHotGlyph(Value: TBitmap);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    function IsHotTrackFontStored: Boolean;
  protected
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure FontChanged; override;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Canvas: TCanvas read GetCanvas;
  published
    property DropDownMenu: TPopupMenu read FDropDown write FDropDown;
    property HintColor;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont stored IsHotTrackFontStored;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;
    property HotGlyph: TBitmap read FHotGlyph write SetHotGlyph;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    {$IFDEF HAS_PROPERTY_STYLEELEMENTS}
    property StyleElements;
    {$ENDIF HAS_PROPERTY_STYLEELEMENTS}

    property SimpleFrame: Boolean read FSimpleFrame write FSimpleFrame default False;
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
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvJVCLUtils;

constructor TJvBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FHotTrack := False;
  FSimpleFrame := False;
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHotGlyph := TBitmap.Create;
  FOldGlyph := TBitmap.Create;
  FHotTrackFontOptions := DefaultTrackFontOptions;
end;

destructor TJvBitBtn.Destroy;
begin
  FHotTrackFont.Free;
  FFontSave.Free;
  FHotGlyph.Free;
  FOldGlyph.Free;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

function TJvBitBtn.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvBitBtn.IsHotTrackFontStored: Boolean;
begin
  Result := IsHotTrackFontDfmStored(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvBitBtn.Click;
begin
  inherited Click;
  if FDropDown <> nil then
  begin
    FDropDown.Popup(GetClientOrigin.X, GetClientOrigin.Y + Height);
    MouseLeave(Self);
  end;
end;

procedure TJvBitBtn.SetHotGlyph(Value: TBitmap);
begin
  FHotGlyph.Assign(Value);
end;

procedure TJvBitBtn.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvBitBtn.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;

procedure TJvBitBtn.MouseEnter(AControl: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if not FHotGlyph.Empty then
    begin
      FOldGlyph.Assign(Glyph);
      Glyph.Assign(FHotGlyph);
    end;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    inherited MouseEnter(AControl);
  end;
end;

procedure TJvBitBtn.MouseLeave(AControl: TControl);
begin
  if MouseOver then
  begin
    if not FHotGlyph.Empty then
      Glyph.Assign(FOldGlyph);
    if FHotTrack then
      Font.Assign(FFontSave);
    inherited MouseLeave(AControl);
  end;
end;

procedure TJvBitBtn.FontChanged;
begin
  inherited FontChanged;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvBitBtn.CNDrawItem(var Msg: TWMDrawItem);
begin
  inherited;
  DrawItem(Msg.DrawItemStruct^);
end;

procedure TJvBitBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown: Boolean;
  R: TRect;
begin
  if (csDestroying in ComponentState) or not FSimpleFrame then
    Exit;
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;
  IsDown := DrawItemStruct.itemState and ODS_SELECTED <> 0;
  if not MouseOver and not IsDown then
  begin
    with FCanvas do
    begin
      if not Focused and not Default then
      begin
        Pen.Color := clBtnFace;
        MoveTo(R.Left + 1, R.Top + 1);
        LineTo(R.Right - 1, R.Top + 1);
        MoveTo(R.Left + 1, R.Top + 1);
        LineTo(R.Left + 1, R.Bottom - 1);

        Pen.Color := (Parent as TWinControl).Brush.Color;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
        MoveTo(R.Right - 1, R.Top);
        LineTo(R.Right - 1, R.Bottom);

        Pen.Color := clBtnShadow;
        MoveTo(R.Left - 2, R.Bottom - 2);
        LineTo(R.Right - 1, R.Bottom - 2);
        MoveTo(R.Right - 2, R.Top);
        LineTo(R.Right - 2, R.Bottom - 1);
      end
      else
      begin
        Brush.Color := clBtnFace;
        FrameRect(Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2));
      end;
    end;
  end;
  FCanvas.Handle := 0;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
