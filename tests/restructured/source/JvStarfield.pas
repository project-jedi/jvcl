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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvStarfield;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JVCLVer;

type
  TJvStarfieldThread = class(TThread)
  private
    FTag: Integer;
  protected
    procedure Draw;
    procedure Execute; override;
  public
    FDelay: Cardinal;
    FOnDraw: TNotifyEvent;
    property Tag: Integer read FTag write FTag;
  end;

  TJvStarfield = class(TGraphicControl)
  private
    FStarfield: array of TStars;
    FThread: TJvStarfieldThread;
    FActive: Boolean;
    FDelay: Cardinal;
    FStars: Word;
    FMaxSpeed: Byte;
    FBmp: TBitmap;
    FAboutJVCL: TJVCLAboutInfo;
    procedure Refresh(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetStars(const Value: Word);
  protected
  public
    procedure Resize; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property Delay: Cardinal read FDelay write SetDelay default 50;
    property Active: Boolean read FActive write SetActive default False;
    property Stars: Word read FStars write SetStars default 100;
    property MaxSpeed: Byte read FMaxSpeed write FMaxSpeed default 10;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvStarfield
///////////////////////////////////////////////////////////

constructor TJvStarfield.Create(AOwner: TComponent);
begin
  inherited;
  Randomize;

  FDelay := 50;
  FActive := False;
  FBmp := TBitmap.Create;

  FThread := TJvStarfieldThread.Create(True);
  FThread.FreeOnTerminate := True;
  FThread.FDelay := 50;
  FThread.FOnDraw := Refresh;
  Self.Width := 100;
  Self.Height := 100;
  FMaxSpeed := 10;

  SetStars(100);
end;

{************************************************************}

procedure TJvStarfield.Resize;
begin
  inherited;
  FBmp.Width := Width;
  FBmp.Height := Height;
  SetStars(FStars);
end;

{************************************************************}

procedure TJvStarfield.SetStars(const Value: Word);
var
  i, j: Integer;
begin
  FStars := Value;

  SetLength(FStarfield, Value);
  for i := 0 to FStars - 1 do
  begin
    FStarfield[i].X := Random(Width div 2) + Width;
    FStarfield[i].Y := Random(Height);
    FStarfield[i].Speed := Random(FMaxSpeed) + 1;
    j := Random(120) + 120;
    FStarfield[i].Color := RGB(j, j, j);
  end;
end;

{************************************************************}

destructor TJvStarfield.Destroy;
begin
  SetLength(FStarfield, 0);
  FThread.Terminate;
  FBmp.Free;
  inherited;
end;

{************************************************************}

procedure TJvStarfield.Refresh(Sender: TObject);
var
  i, j: Integer;
begin
  if (FBmp.Height <> Height) or (FBmp.Width <> Width) then
    Resize
  else
  begin
    FBmp.Canvas.Brush.Color := clBlack;
    FBmp.Canvas.Brush.Style := bsSolid;
    FBmp.Canvas.FillRect(Rect(0, 0, Width, Height));
    for i := 0 to FStars - 1 do
    begin
      if FStarfield[i].x < Width then
        FBmp.Canvas.Pixels[Fstarfield[i].x, FStarfield[i].y] := FStarfield[i].Color;
      FStarfield[i].X := FStarfield[i].X - FStarfield[i].Speed;
      if FStarfield[i].x < 0 then
      begin
        FStarfield[i].x := Width;
        FStarfield[i].y := Random(Height);
        FStarfield[i].Speed := Random(FMaxSpeed) + 1;
        j := Random(120) + 120;
        FStarfield[i].Color := RGB(j, j, j);
      end;
    end;
    Canvas.Draw(0, 0, FBmp)
  end;
end;

{************************************************************}

procedure TJvStarfield.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    if FActive then
      FThread.Resume
    else
      FThread.Suspend;
end;

{************************************************************}

procedure TJvStarfield.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  FThread.FDelay := Value;
end;

///////////////////////////////////////////////////////////
// TJvStarfieldThread
///////////////////////////////////////////////////////////

procedure TJvStarfieldThread.Draw;
begin
  if Assigned(FOnDraw) then
    FOnDraw(Self);
end;

{************************************************************}

procedure TJvStarfieldThread.Execute;
begin
  while not Terminated do
  begin
    Synchronize(Draw);
    Sleep(FDelay);
  end;
end;

end.
