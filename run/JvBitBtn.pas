{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBitBtn.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2003-10-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvBitBtn;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Buttons, Menus,
  {$ELSE}
  QGraphics, QControls, QForms, QButtons, QMenus,
  {$ENDIF VCL}
  JVCLVer, JvTypes, JvExButtons;

type
  TJvBitBtn = class(TJvExBitBtn)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FOnParentColorChanged: TNotifyEvent;
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
    procedure ParentColorChanged; override;
    procedure FontChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Canvas: TCanvas read GetCanvas;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property DropDownMenu: TPopupMenu read FDropDown write FDropDown;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;
    property HotGlyph: TBitmap read FHotGlyph write SetHotGlyph;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

uses
  JvJVCLUtils;

constructor TJvBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self; //...i can draw now! :)
  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHintColor := clInfoBk;
  FOver := False;
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
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
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
    FOver := True;
  end;
  inherited MouseEnter(AControl);
end;

procedure TJvBitBtn.MouseLeave(AControl: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    FOver := False;
    Application.HintColor := FSaved;
    if not FHotGlyph.Empty then
      Glyph.Assign(FOldGlyph);
    if FHotTrack then
      Font.Assign(FFontSave);
  end;
  inherited MouseLeave(AControl);
end;

procedure TJvBitBtn.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvBitBtn.FontChanged;
begin
  inherited FontChanged;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

end.

