{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvZoom.PAS, released on 2001-02-28.

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

unit JvZoom;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, JVCLVer;

type
  TJvZoom = class(TCustomControl)
  private
    FTimer: TTimer;
    FActive: Boolean;
    FZoom: Integer;
    FDelay: Cardinal;
    FZCanvas: TControlCanvas;
    FCanvas: TCanvas;
    FLastPoint: TPoint;
    FAboutJVCL: TJVCLAboutInfo;
    procedure PaintMe(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Active: Boolean read FActive write SetActive default True;
    property ZoomLevel: Integer read FZoom write FZoom default 100;
    property Delay: Cardinal read FDelay write SetDelay default 100;
    property OnMouseDown;
    property OnClick;
    property OnDblClick;
    property OnMouseUp;
    property OnResize;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;

implementation

{**************************************************}

constructor TJvZoom.Create(AOwner: TComponent);
begin
  inherited;
  Height := 100;
  Width := 100;
  FDelay := 100;
  FZoom := 100;
  FActive := True;

  //Create canvas to write to the control
  FZCanvas := TControlCanvas.Create;
  FZCanvas.Control := Self;

  //Create the canvas to retrieve informations
  FCanvas := TCanvas.Create;
  FCanvas.Handle := GetDC(HWND_DESKTOP);

  Ftimer := TTimer.Create(Self);
  FTimer.OnTimer := PaintMe;
  FTimer.Interval := 100;

  //Assign the Parent, or it's impossible to draw on it
  Parent := TWinControl(AOwner);

  FTimer.Enabled := True;
end;

{**************************************************}

destructor TJvZoom.Destroy;
begin
  ReleaseDc(HWND_DESKTOP, FCanvas.Handle);
  FCanvas.Free;
  FZCanvas.Free;
  inherited;
end;

{**************************************************}

procedure TJvZoom.Paint;
begin
  inherited;
  if csDesigning in ComponentState then
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

procedure TJvZoom.PaintMe(Sender: TObject);
var
  p: TPoint;
  x, y: Integer;
  t: TRect;
begin
  GetCursorPos(p);
  //Only draw if on a different position
  if (p.x <> FLastPoint.x) or (p.y <> FLastPoint.y) then
  begin
    //if it's the first time, get the DC of the control to be able
    //to draw on it.
    if FTimer.Tag = 0 then
    begin
      try
        FZCanvas.Handle := GetDC(Handle);
      except
        Exit;
      end;
      FTimer.Tag := 1;
    end;

    //Analyse the point
    FLastPoint := p;
    //Create the area to Copy
    x := (Width div 2) * FZoom div 100;
    y := (Height div 2) * FZoom div 100;
    if p.x < x then
      p.x := x
    else if p.x + x > Screen.Width then
      p.x := Screen.Width - x;
    if p.y < y then
      p.y := y
    else if p.y + y > Screen.Height then
      p.y := Screen.Height - y;
    t.Left := p.x - x;
    t.Top := p.y - y;
    t.Right := p.x + x;
    t.Bottom := p.y + y;

    //Draw the area around the mouse
    FZCanvas.CopyRect(Rect(0, 0, Width, Height), FCanvas, t);
  end;
end;

{**************************************************}

procedure TJvZoom.SetActive(const Value: Boolean);
begin
  FActive := Value;
  FTimer.Enabled := Value;
end;

{**************************************************}

procedure TJvZoom.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  FTimer.Interval := Value;
end;

{**************************************************}

procedure TJvZoom.WMSize(var Msg: TWMSize);
begin
  //On resize, refresh it
  inherited;
  PaintMe(Self);
end;

end.

