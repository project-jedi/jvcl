{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgShape.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgShape;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls,
  JvgTypes, JvgUtils;

type
  TJvgShape = class(TShape)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FWallpaper: TBitmap;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    function GetWallpaper: TBitmap;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Wallpaper: TBitmap read GetWallpaper write FWallpaper;
  published
    property PopupMenu;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnClick;
    property OnDblClick;
  end;

implementation

constructor TJvgShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks];
end;

destructor TJvgShape.Destroy;
begin
  FWallpaper.Free;
  inherited Destroy;
end;

procedure TJvgShape.CMMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvgShape.CMMouseLeave(var Msg: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TJvgShape.GetWallpaper: TBitmap;
begin
  if FWallpaper = nil then
    FWallpaper := TBitmap.Create;
  Result := FWallpaper;
end;

procedure TJvgShape.Paint;
var
  R: TRect;
  OldBrush: TBrushStyle;
begin
  if not Assigned(FWallpaper) or FWallpaper.Empty then
    inherited Paint
  else
  begin
    R := ClientRect;
    OldBrush := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen := Pen;
    Canvas.Rectangle(R);
    Canvas.Brush.Style := OldBrush;
    InflateRect(R, -1, -1);
    DrawBitmapExt(Canvas.Handle, FWallpaper, R, 0, 0, fwoTile, fdsDefault, False, 0, 0);
  end;
end;

end.

