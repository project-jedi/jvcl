{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgScrollBox.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgScrollBox;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, Forms, ExtCtrls,
  {$IFDEF USEJVCL}
  JVCLVer,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses, JvgUtils;

type
  TOnEraseBkgndEvent = procedure(Sender: TObject; DC: HDC) of object;

  TJvgScrollBox = class(TScrollBox)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FBackground: TBitmap;
    FBuffer: TBitmap;
    FBufferedDraw: Boolean;
    FOnEraseBkgndEvent: TOnEraseBkgndEvent;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetBackground(Value: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyBuffer(DC: HDC);
  published
    {$IFDEF USEJVCL}
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    {$ENDIF USEJVCL}
    property Background: TBitmap read FBackground write SetBackground;
    property BufferedDraw: Boolean read FBufferedDraw write FBufferedDraw;
    property OnEraseBkgndEvent: TOnEraseBkgndEvent read FOnEraseBkgndEvent write FOnEraseBkgndEvent;
  end;

{$IFNDEF USEJVCL}
  {$UNDEF UNITVERSIONING}
{$ENDIF ~USEJVCL}

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvgScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackground := TBitmap.Create;
  { FBackground.Width := 8; FBackground.Height := 8;
    FBackground.Canvas.Brush.Color := clWhite;//clWindow;
    FBackground.Canvas.FillRect( Rect(0,0,8,8) );
    FBackground.Canvas.Pixels[7,7] := 0;}
end;

destructor TJvgScrollBox.Destroy;
begin
  FBackground.Free;
  FBuffer.Free;
  inherited Destroy;
end;

procedure TJvgScrollBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  DC: HDC;
  R: TRect;
  IHeight, IWidth, SavedIHeight, x_, y_, XOffset, YOffset, SavedYOffset: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  if BufferedDraw and (FBuffer = nil) then
    FBuffer := TBitmap.Create;

  if Assigned(FBuffer) then
  begin
    FBuffer.Width := Width;
    FBuffer.Height := Height;
  end;

  if BufferedDraw then
    DC := FBuffer.Canvas.Handle
  else
    DC := Msg.DC;

  try
    if FBackground.Empty then
      Exit;

    if FBackground.Width <= 8 then
      with TCanvas.Create do
        try
          Handle := Msg.DC;
          //    Pen.Color := clWindow;
          //    Brush.Color := clWindow;
          //    Brush.Style := bsCross;
          Brush.Bitmap := FBackground;
          FillRect(ClientRect);
          Handle := 0;
          Msg.Result := 1;
        finally
          Free;
        end
    else
    begin
      //  SendMessage(Self.Handle, WM_SETREDRAW, 0, 0);
      //    BitBlt( Msg.DC, x_, y_, 100, 100, FBackground.Canvas.Handle, 0, 0, SRCCOPY);
      R := ClientRect;
      x_ := R.Left;
      y_ := R.Top;
      IHeight := FBackground.Height;
      IWidth := FBackground.Width;
      SavedIHeight := IHeight;

      XOffset := HorzScrollBar.Position - Trunc(HorzScrollBar.Position / IWidth) * IWidth;
      YOffset := VertScrollBar.Position - Trunc(VertScrollBar.Position / IHeight) * IHeight;
      SavedYOffset := YOffset;
      while x_ < R.Right do
      begin
        //if x_+IWidth > R.right then IWidth := R.right-x_;
        while y_ < R.Bottom do
        begin
          IHeight := SavedIHeight;
          //if y_+IHeight-YOffset > R.bottom then IHeight := R.bottom-y_;
          BitBlt(DC, x_, y_, IWidth - XOffset, IHeight - YOffset,
            FBackground.Canvas.Handle, XOffset, YOffset, SRCCOPY);
          Inc(y_, IHeight - YOffset);
          YOffset := 0;
        end;
        Inc(x_, IWidth - XOffset);
        y_ := R.Top;
        XOffset := 0;
        YOffset := SavedYOffset;
      end;
    end;
  finally
    if Assigned(FOnEraseBkgndEvent) then
      FOnEraseBkgndEvent(Self, DC);
    if BufferedDraw then
      ApplyBuffer(Msg.DC);
  end;
end;

procedure TJvgScrollBox.SetBackground(Value: TBitmap);
begin
  FBackground.Assign(Value);
  Invalidate;
end;

procedure TJvgScrollBox.ApplyBuffer(DC: HDC);
begin
  BitBlt(DC, 0, 0, Width, Height, FBuffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

