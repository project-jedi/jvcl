{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormAnimation.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFormAnimation;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Controls, Forms,
  JvComponent;

type
  TJvFormAnimation = class(TJvComponent)
  private
    FForm: TCustomForm;
    FRegions: array of HRGN;
    // (rom) simplified
    procedure AnimateDisappear(N: Integer);
    procedure AnimateAppear(N: Integer);
    procedure DeleteRegions;
  public
    constructor Create(AOwner: TComponent); override;
  published
    procedure DisappearEllipse;
    procedure DisappearRectangle;
    procedure DisappearRoundedRectangle(EllipseX, EllipseY: Integer);
    procedure DisappearHorizontally;
    procedure DisappearVertically;
    procedure DisappearTelevision;
    procedure DisappearToBottom;
    procedure DisappearToTop;
    procedure AppearEllipse;
    procedure AppearRectangle;
    procedure AppearRoundedRectangle(EllipseX, EllipseY: Integer);
    procedure AppearHorizontally;
    procedure AppearVertically;
    procedure AppearTelevision;
    procedure AppearToTop;
    procedure AppearToBottom;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math;

constructor TJvFormAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := GetParentForm(TControl(AOwner));
end;

procedure TJvFormAnimation.AnimateDisappear(N: Integer);
var
  I: Integer;
begin
  FForm.Visible := True;
  for I := 0 to N do
  begin
    if SetWindowRgn(FForm.Handle, FRegions[I], True) <> 0 then
      FRegions[I] := NullHandle;
    FForm.Repaint;
    Sleep(10);
  end;
  FForm.Visible := False;
  SetWindowRgn(FForm.Handle, NullHandle, True);
  DeleteRegions;
end;

procedure TJvFormAnimation.AnimateAppear(N: Integer);
var
  I: Integer;
  Rgn: HRGN;
begin
  FForm.Visible := False;
  Rgn := CreateRectRgn(0, 0, 0, 0);
  SetWindowRgn(FForm.Handle, Rgn, True);
  FForm.Visible := True;
  for I := N downto 0 do
  begin
    if SetWindowRgn(FForm.Handle, FRegions[I], True) <> 0 then
      FRegions[I] := NullHandle;
    FForm.Repaint;
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, NullHandle, True);
  DeleteObject(Rgn);
  DeleteRegions;
end;

procedure TJvFormAnimation.DeleteRegions;
var
  I: Integer;
begin
  for I := Low(FRegions) to High(FRegions) do
    if FRegions[I] <> NullHandle then
      DeleteObject(FRegions[I]);
  SetLength(FRegions, 0);
end;

procedure TJvFormAnimation.DisappearEllipse;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if I < (FForm.Width div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateEllipticRgn(I, J, FForm.Width - I, FForm.Height - J);
      I := I + 2;
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateDisappear(L);
end;

procedure TJvFormAnimation.DisappearRectangle;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if I < (FForm.Width div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
      I := I + 2;
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateDisappear(L);
end;

procedure TJvFormAnimation.DisappearRoundedRectangle(EllipseX,
  EllipseY: Integer);
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if I < (FForm.Width div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRoundRectRgn(I, J, FForm.Width - I, FForm.Height - J, EllipseX, EllipseY);
      I := I + 2;
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateDisappear(L);
end;

procedure TJvFormAnimation.DisappearHorizontally;
var
  I, J, K, L: Integer;
begin
  J := 0;
  L := 0;
  I := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if I < (FForm.Width div 2) then
    begin
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
      I := I + 2;
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateDisappear(L);
end;

procedure TJvFormAnimation.DisappearVertically;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if J < (FForm.Height div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateDisappear(L);
end;

procedure TJvFormAnimation.DisappearTelevision;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if J + 2 < (FForm.Height div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
    end
    else
    if I + 6 < (FForm.Width div 2) then
    begin
      I := I + 8;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateDisappear(L);
end;

procedure TJvFormAnimation.DisappearToBottom;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if J < FForm.Height then
    begin
      J := J + 2;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width, FForm.Height);
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateDisappear(L);
end;

procedure TJvFormAnimation.DisappearToTop;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if J < FForm.Height then
    begin
      J := J + 2;
      FRegions[K] := CreateRectRgn(I, 0, FForm.Width, FForm.Height - J);
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateDisappear(L);
end;

procedure TJvFormAnimation.AppearEllipse;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if I < (FForm.Width div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateEllipticRgn(I, J, FForm.Width - I, FForm.Height - J);
      I := I + 2;
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateAppear(L);
end;

procedure TJvFormAnimation.AppearRectangle;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if I < (FForm.Width div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
      I := I + 2;
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateAppear(L);
end;

procedure TJvFormAnimation.AppearRoundedRectangle(EllipseX, EllipseY: Integer);
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if I < (FForm.Width div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRoundRectRgn(I, J, FForm.Width - I, FForm.Height - J, EllipseX, EllipseY);
      I := I + 2;
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateAppear(L);
end;

procedure TJvFormAnimation.AppearHorizontally;
var
  I, J, K, L: Integer;
begin
  J := 0;
  L := 0;
  I := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if I < (FForm.Width div 2) then
    begin
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
      I := I + 2;
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateAppear(L);
end;

procedure TJvFormAnimation.AppearVertically;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if J < (FForm.Height div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateAppear(L);
end;

procedure TJvFormAnimation.AppearTelevision;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if J + 2 < (FForm.Height div 2) then
    begin
      J := J + 2;
      if J > (FForm.Height div 2) then
        I := FForm.Width;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
    end
    else
    if I + 6 < (FForm.Width div 2) then
    begin
      I := I + 8;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width - I, FForm.Height - J);
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateAppear(L);
end;

procedure TJvFormAnimation.AppearToBottom;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if J < FForm.Height then
    begin
      J := J + 2;
      FRegions[K] := CreateRectRgn(I, J, FForm.Width, FForm.Height);
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateAppear(L);
end;

procedure TJvFormAnimation.AppearToTop;
var
  I, J, K, L: Integer;
begin
  J := 0;
  I := 0;
  L := 0;

  SetLength(FRegions, Max(FForm.Width, FForm.Height));
  for K := 0 to High(FRegions) do
  begin
    if J < FForm.Height then
    begin
      J := J + 2;
      FRegions[K] := CreateRectRgn(I, 0, FForm.Width, FForm.Height - J);
    end
    else
    begin
      L := K;
      Break;
    end;
  end;

  AnimateAppear(L);
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

