{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStarfield.PAS, released on 2001-02-28.

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

unit JvStarfield;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, JvTypes,
  JvImageDrawThread, JVCLVer;

type
  TJvStars = record
    X: Integer;
    Y: Integer;
    Color: TColor;
    Speed: Integer;
  end;

  TJvStarfield = class(TGraphicControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FStarfield: array of TJvStars;
    FThread: TJvImageDrawThread;
    FActive: Boolean;
    FDelay: Cardinal;
    FStars: Word;
    FMaxSpeed: Byte;
    FBmp: TBitmap;
    procedure Refresh(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetStars(const Value: Word);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property Anchors;
    property Constraints;
    property Delay: Cardinal read FDelay write SetDelay default 50;
    property Active: Boolean read FActive write SetActive default False;
    property Stars: Word read FStars write SetStars default 100;
    property MaxSpeed: Byte read FMaxSpeed write FMaxSpeed default 10;
  end;

implementation

constructor TJvStarfield.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Randomize;

  FDelay := 50;
  FActive := False;
  FBmp := TBitmap.Create;

  FThread := TJvImageDrawThread.Create(True);
  FThread.FreeOnTerminate := False;
  FThread.Delay := 50;
  FThread.OnDraw := Refresh;
  Self.Width := 100;
  Self.Height := 100;
  FMaxSpeed := 10;

  Stars := 100;
end;

destructor TJvStarfield.Destroy;
begin
  SetLength(FStarfield, 0);
  FThread.OnDraw := nil;
  FThread.Terminate;
//  FThread.WaitFor;
  FreeAndNil(FThread);
  FBmp.Free;
  inherited Destroy;
end;

procedure TJvStarfield.Resize;
begin
  inherited Resize;
  FBmp.Width := Width;
  FBmp.Height := Height;
  Stars := FStars;
end;

procedure TJvStarfield.SetStars(const Value: Word);
var
  I, J: Integer;
begin
  FStars := Value;

  SetLength(FStarfield, Value);
  for I := 0 to FStars - 1 do
  begin
    FStarfield[I].X := Random(Width div 2) + Width;
    FStarfield[I].Y := Random(Height);
    FStarfield[I].Speed := Random(FMaxSpeed) + 1;
    J := Random(120) + 120;
    FStarfield[I].Color := RGB(J, J, J);
  end;
end;

procedure TJvStarfield.Refresh(Sender: TObject);
var
  I, J: Integer;
begin
  if (FBmp.Height <> Height) or (FBmp.Width <> Width) then
    Resize
  else
  begin
    FBmp.Canvas.Brush.Color := clBlack;
    FBmp.Canvas.Brush.Style := bsSolid;
    FBmp.Canvas.FillRect(Rect(0, 0, Width, Height));
    for I := 0 to FStars - 1 do
    begin
      if FStarfield[I].X < Width then
        FBmp.Canvas.Pixels[Fstarfield[I].X, FStarfield[I].Y] := FStarfield[I].Color;
      FStarfield[I].X := FStarfield[I].X - FStarfield[I].Speed;
      if FStarfield[I].X < 0 then
      begin
        FStarfield[I].X := Width;
        FStarfield[I].Y := Random(Height);
        FStarfield[I].Speed := Random(FMaxSpeed) + 1;
        J := Random(120) + 120;
        FStarfield[I].Color := RGB(J, J, J);
      end;
    end;
    Canvas.Lock;
    try
      Canvas.Draw(0, 0, FBmp);
    finally
      Canvas.Unlock;
    end;
  end;
end;

procedure TJvStarfield.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    if FActive then
      FThread.Resume
    else
      FThread.Suspend;
end;

procedure TJvStarfield.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  FThread.Delay := Value;
end;

procedure TJvStarfield.Paint;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(ClientRect);
  end;
end;

end.

