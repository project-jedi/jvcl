{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageRotate.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvImageRotate;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Graphics, Controls, ExtCtrls,
  JvImageDrawThread, JVCLVer, JvExExtCtrls;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvImageRotate = class(TJvExImage)
  private
    FRotated: TBitmap;
    FOriginal: TBitmap;
    FTimer: TJvImageDrawThread;
    FPosition: Integer;
    FInterval: Cardinal;
    FColor: TColor;
    FRotating: Boolean;
    procedure SetPicture(Value: TBitmap);
    procedure SetRotating(Value: Boolean);
    procedure Rotate(Sender: TObject);
    procedure SetInterval(Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Rotating: Boolean read FRotating write SetRotating default False;
    property StartImage: TBitmap read FOriginal write SetPicture;
    property Interval: Cardinal read FInterval write SetInterval default 20;
    property FillColor: TColor read FColor write FColor;
    procedure SetAngle(Value: Integer);
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
  JvTypes;

constructor TJvImageRotate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOriginal := TBitmap.Create;
  FRotated := TBitmap.Create;
  FTimer := TJvImageDrawThread.Create(True);
  FTimer.FreeOnTerminate := False;
  FTimer.Delay := 20;
  FTimer.OnDraw := Rotate;
  FPosition := 0;
  FInterval := 20;
  FRotating := False;
  FOriginal.Assign(Picture.Bitmap);
end;

destructor TJvImageRotate.Destroy;
begin
  FTimer.OnDraw := nil;
  FTimer.Terminate;
  FreeAndNil(FTimer);
  FreeAndNil(FOriginal);
  FreeAndNil(FRotated);
  inherited Destroy;
end;

procedure TJvImageRotate.SetPicture(Value: TBitmap);
begin
  Value.PixelFormat := pf24bit;
  FOriginal.PixelFormat := pf24bit;
  Picture.Bitmap.PixelFormat := pf24bit;
  Picture.Assign(Value);
  FOriginal.Assign(Value);
end;

procedure TJvImageRotate.SetRotating(Value: Boolean);
begin
  FRotating := Value;
  if not (csDesigning in ComponentState) then
  begin
    FTimer.Paused := not FRotating;
  end;
end;

procedure TJvImageRotate.Rotate(Sender: TObject);
begin
  // Must exit because we are "Synchronized" and our parent is already
  // partly destroyed. If we did not exit, we would get an AV.
  if csDestroying in ComponentState then
    Exit;

  if FTimer.Tag = 0 then //0 from Left to Right
  begin
    if FPosition = 181 then
      FTimer.Tag := 1
    else
    begin
      Dec(FPosition);
      if FPosition < 0 then
        FPosition := 359;
      SetAngle(FPosition);
    end;
  end
  else //from Right to Left
  begin
    if FPosition = 179 then
      Self.FTimer.Tag := 0
    else
    begin
      FPosition := (FPosition + 1) mod 360;
      SetAngle(FPosition);
    end;
  end;
end;

procedure TJvImageRotate.SetAngle(Value: Integer);
var
  i, j, iRotationAxis, iOriginal, iPrime, iPrimeRotated: Integer;
  jRotationAxis, jOriginal, jPrime, jPrimeRotated: Integer;
  RowOriginal: PJvRGBArray;
  RowRotated: PJvRGBArray;
  Theta, SinTheta, CosTheta: Double;
  R, G, B: Byte;
begin
  FRotated.Width := FOriginal.Width;
  FRotated.Height := FOriginal.Height;
  FRotated.PixelFormat := pf24bit;
  iRotationAxis := FOriginal.Width div 2;
  jRotationAxis := FOriginal.Height div 2;
  Theta := -Value * Pi / 180;
  SinTheta := Sin(Theta);
  CosTheta := Cos(Theta);
  B := FColor mod 256;
  G := Round((FColor div 256) mod 256);
  R := Round((FColor div 256) div 256);
  for j := FRotated.Height - 1 downto 0 do
  begin
    RowRotated := FRotated.Scanline[j];
    jPrime := 2 * (j - jRotationAxis) + 1;
    for i := FRotated.Width - 1 downto 0 do
    begin
      iPrime := 2 * (i - iRotationAxis) + 1;
      iPrimeRotated := Round(iPrime * CosTheta - jPrime * SinTheta);
      jPrimeRotated := Round(iPrime * SinTheta + jPrime * CosTheta);
      iOriginal := (iPrimeRotated - 1) div 2 + iRotationAxis;
      jOriginal := (jPrimeRotated - 1) div 2 + jRotationAxis;
      if (iOriginal >= 0) and (iOriginal <= FOriginal.Width - 1) and
        (jOriginal >= 0) and (jOriginal <= FOriginal.Height - 1) then
      begin
        RowOriginal := FOriginal.Scanline[jOriginal];
        RowRotated[i] := RowOriginal[iOriginal]
      end
      else
      begin
        RowRotated[i].rgbBlue := B;
        RowRotated[i].rgbGreen := G;
        RowRotated[i].rgbRed := R
      end;
    end;
  end;
  Picture.Assign(FRotated);
end;

procedure TJvImageRotate.SetInterval(Value: Cardinal);
begin
  FInterval := Value;
  FTimer.Delay := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
