{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

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

Last Modified: 2003-10-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQBitBtn;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  
  
  QGraphics, QControls, QButtons, QMenus,
  
  JvQTypes, JvQExButtons;

type
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
    function GetCanvas: TCanvas;
    procedure SetHotGlyph(Value: TBitmap);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
  protected
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure FontChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Canvas: TCanvas read GetCanvas;
  published
    property DropDownMenu: TPopupMenu read FDropDown write FDropDown;
    property HintColor;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;
    property HotGlyph: TBitmap read FHotGlyph write SetHotGlyph;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

uses
  JvQJVCLUtils;

constructor TJvBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FHotTrack := False;
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

end.

