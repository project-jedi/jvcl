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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBitBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Buttons, Menus,
  JVCLVer;

type
  TJvBitBtn = class(TBitBtn)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FSaved: TColor;
    FColor: TColor;
    FOver: Boolean;
    FGlyph: TBitmap;
    FOldGlyph: TBitmap;
    FDropDown: TPopupMenu;
    FHotTrack: Boolean;
    FHotFont: TFont;
    FFontSave: TFont;
    procedure SetGlyph(Value: TBitmap);
    procedure SetHotFont(const Value: TFont);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;
    property HotGlyph: TBitmap read FGlyph write SetGlyph;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property DropDownMenu: TPopupMenu read FDropDown write FDropDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

constructor TJvBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  FHotFont := TFont.Create;
  FFontSave := TFont.Create;
  FColor := clInfoBk;
  FOver := False;
  FGlyph := TBitmap.Create;
  FOldGlyph := TBitmap.Create;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

destructor TJvBitBtn.Destroy;
begin
  FHotFont.Free;
  FFontSave.Free;
  FGlyph.Free;
  FOldGlyph.Free;
  inherited Destroy;
end;

procedure TJvBitBtn.Click;
begin
  inherited Click;
  if FDropDown <> nil then
  begin
    FDropDown.Popup(GetClientOrigin.x, GetClientOrigin.y + Height);
    Perform(CM_MOUSELEAVE, 0, 0);
  end;
end;

procedure TJvBitBtn.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvBitBtn.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TJvBitBtn.CMMouseEnter(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if not FGlyph.Empty then
    begin
      FOldGlyph.Assign(Glyph);
      Glyph.Assign(FGlyph);
    end;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotFont);
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvBitBtn.CMMouseLeave(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
    if not FOldGlyph.Empty then
      Glyph.Assign(FOldGlyph);
    if FHotTrack then
      Font.Assign(FFontSave);
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvBitBtn.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

end.

