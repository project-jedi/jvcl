{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStarfield.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvStarfield;

{$I jvcl.inc}

interface

uses
  Windows, Graphics, Controls,
  SysUtils, Classes,
  JvTypes, JvImageDrawThread, JvComponent;

type
  TJvStars = record
    X: Integer;
    Y: Integer;
    Color: TColor;
    Speed: Integer;
  end;

  TJvStarfield = class(TJvGraphicControl)
  private
    FStarfield: array of TJvStars;
    FThread: TJvImageDrawThread;
    FActive: Boolean;
    FDelay: Cardinal;
    FStars: Word;
    FMaxSpeed: Byte;
    FBmp: TBitmap;
    FOnActiveChanged: TNotifyEvent;
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
    procedure Clear;
  published
    property Align;
    property Anchors;
    property Constraints;
    property ParentColor default False;
    property Color default clBlack;
    property Height default 100;
    property Width default 100;
    property Delay: Cardinal read FDelay write SetDelay default 50;
    property Active: Boolean read FActive write SetActive default False;
    property Stars: Word read FStars write SetStars default 100;
    property MaxSpeed: Byte read FMaxSpeed write FMaxSpeed default 10;
    property Visible;

    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnClick;
    property OnDblClick;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnActiveChanged:TNotifyEvent read FOnActiveChanged write FOnActiveChanged;


  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvStarfield.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := False;
  Color := clBlack;
  ControlStyle := ControlStyle + [csOpaque];
  FDelay := 50;
  FActive := False;
  FBmp := TBitmap.Create;

  FThread := TJvImageDrawThread.Create(True);
  FThread.FreeOnTerminate := False;
  FThread.Delay := FDelay;
  FThread.OnDraw := Refresh;
  Width := 100;
  Height := 100;
  FMaxSpeed := 10;

  Stars := 100;
end;

destructor TJvStarfield.Destroy;
begin
  SetLength(FStarfield, 0);
  FThread.OnDraw := nil;
  FThread.Terminate;
  //FThread.WaitFor;
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
  Randomize;
  FStars := Value;
  SetLength(FStarfield, FStars);
  for I := 0 to FStars - 1 do
  begin
    FStarfield[I].X := Random(Width div 2) + Width;
    FStarfield[I].Y := Random(Height);
    FStarfield[I].Speed := Random(FMaxSpeed) + 1;
    J := Random(120) + 120;
    FStarfield[I].Color := RGB(J, J, J);
  end;
end;

procedure TJvStarfield.SetActive(const Value: Boolean);
begin
  if (FActive <> Value) and Assigned(FOnActiveChanged) then
    FOnActiveChanged(Self);
  FActive := Value;
  if not (csDesigning in ComponentState)  then
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

procedure TJvStarfield.Refresh(Sender: TObject);
begin
  Paint;
end;

procedure TJvStarfield.Paint;
var
  I, J: Integer;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(ClientRect);
  end
  else
  begin
    if (FBmp.Height <> Height) or (FBmp.Width <> Width) then
      Resize
    else
    begin
      FBmp.Canvas.Brush.Color := Color;
      if Color =  clNone then
        FBmp.Canvas.Brush.Style := bsClear
      else
        FBmp.Canvas.Brush.Style := bsSolid;
      FBmp.Canvas.FillRect(ClientRect);
      for I := 0 to FStars - 1 do
      begin
        if FStarfield[I].X < Width then
            FBmp.Canvas.Pixels[FStarfield[I].X, FStarfield[I].Y] := FStarfield[I].Color;
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
        if Color =  clNone then
          Canvas.Brush.Style := bsClear
        else
          Canvas.Brush.Style := bsSolid;
        Canvas.Draw(0, 0, FBmp);
      finally
        Canvas.Unlock;
      end;
    end;
  end;
end;

procedure TJvStarfield.Clear;
begin
  if not Active then
  begin
    Canvas.Brush.Color := Color;
    if Color =  clNone then
      Canvas.Brush.Style := bsClear
    else
      Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
  end;
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

