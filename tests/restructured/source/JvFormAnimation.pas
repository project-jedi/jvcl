{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormAnimation.PAS, released on 2001-02-28.

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

unit JvFormAnimation;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, JvComponent;

type
  TJvFormAnimation = class(TJvComponent)
  private
    FForm: TCustomForm;
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

{**************************************************}

constructor TJvFormAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FForm := GetParentForm(TControl(AOwner));
end;

{**************************************************}

procedure TJvFormAnimation.DisappearEllipse;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if i < (FForm.Width div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateEllipticRgn(i, j, FForm.Width - i, FForm.Height - j);
      i := i + 2;
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  for i := 0 to l do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
end;

{**************************************************}

procedure TJvFormAnimation.DisappearRectangle;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if i < (FForm.Width div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
      i := i + 2;
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  for i := 0 to l do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
end;

{**************************************************}

procedure TJvFormAnimation.DisappearRoundedRectangle(EllipseX,
  EllipseY: Integer);
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if i < (FForm.Width div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRoundRectRgn(i, j, FForm.Width - i, FForm.Height - j, EllipseX, EllipseY);
      i := i + 2;
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  for i := 0 to l do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
end;

{**************************************************}

procedure TJvFormAnimation.DisappearHorizontally;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  l := 0;
  i := 0;

  for k := 0 to 599 do
  begin
    if i < (FForm.Width div 2) then
    begin
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
      i := i + 2;
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  for i := 0 to l do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
end;

{**************************************************}

procedure TJvFormAnimation.DisappearVertically;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if j < (FForm.Height div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  for i := 0 to l do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
end;

{**************************************************}

procedure TJvFormAnimation.DisappearTelevision;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if j + 2 < (FForm.Height div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
    end
    else if i + 6 < (FForm.Width div 2) then
    begin
      i := i + 8;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  for i := 0 to l do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(5);
  end;
  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
end;

{**************************************************}

procedure TJvFormAnimation.DisappearToBottom;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if j < FForm.Height then
    begin
      j := j + 2;
      hs[k] := CreateRectRgn(i, j, FForm.Width, FForm.Height);
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  for i := 0 to l do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
end;

{**************************************************}

procedure TJvFormAnimation.DisappearToTop;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if j < FForm.Height then
    begin
      j := j + 2;
      hs[k] := CreateRectRgn(i, 0, FForm.Width, FForm.Height - j);
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  for i := 0 to l do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
end;

{**************************************************}

procedure TJvFormAnimation.AppearEllipse;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if i < (FForm.Width div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateEllipticRgn(i, j, FForm.Width - i, FForm.Height - j);
      i := i + 2;
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
  FForm.Visible := True;
  for i := l downto 0 do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, 0, True);
end;

{**************************************************}

procedure TJvFormAnimation.AppearRectangle;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if i < (FForm.Width div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
      i := i + 2;
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
  FForm.Visible := True;
  for i := l downto 0 do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, 0, True);
end;

{**************************************************}

procedure TJvFormAnimation.AppearRoundedRectangle(EllipseX,
  EllipseY: Integer);
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if i < (FForm.Width div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRoundRectRgn(i, j, FForm.Width - i, FForm.Height - j, EllipseX, EllipseY);
      i := i + 2;
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
  FForm.Visible := True;
  for i := l downto 0 do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, 0, True);
end;

{**************************************************}

procedure TJvFormAnimation.AppearHorizontally;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  l := 0;
  i := 0;

  for k := 0 to 599 do
  begin
    if i < (FForm.Width div 2) then
    begin
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
      i := i + 2;
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
  FForm.Visible := True;
  for i := l downto 0 do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, 0, True);
end;

{**************************************************}

procedure TJvFormAnimation.AppearVertically;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if j < (FForm.Height div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
  FForm.Visible := True;
  for i := l downto 0 do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, 0, True);
end;

{**************************************************}

procedure TJvFormAnimation.AppearTelevision;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if j + 2 < (FForm.Height div 2) then
    begin
      j := j + 2;
      if j > (FForm.Height div 2) then
        i := FForm.Width;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
    end
    else if i + 6 < (FForm.Width div 2) then
    begin
      i := i + 8;
      hs[k] := CreateRectRgn(i, j, FForm.Width - i, FForm.Height - j);
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
  FForm.Visible := True;
  for i := l downto 0 do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, 0, True);
end;

{**************************************************}

procedure TJvFormAnimation.AppearToBottom;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if j < FForm.Height then
    begin
      j := j + 2;
      hs[k] := CreateRectRgn(i, j, FForm.Width, FForm.Height);
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
  FForm.Visible := True;
  for i := l downto 0 do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, 0, True);
end;

{**************************************************}

procedure TJvFormAnimation.AppearToTop;
var
  i, j, k, l: Integer;
  hs: array[0..600] of HRGN;
begin
  j := 0;
  i := 0;
  l := 0;

  for k := 0 to 599 do
  begin
    if j < FForm.Height then
    begin
      j := j + 2;
      hs[k] := CreateRectRgn(i, 0, FForm.Width, FForm.Height - j);
    end
    else
    begin
      l := k;
      Break;
    end;
  end;

  SetWindowRgn(FForm.Handle, CreateRectRgn(0, 0, 0, 0), True);
  FForm.Visible := True;
  for i := l downto 0 do
  begin
    SetWindowRgn(FForm.Handle, hs[i], True);
    Sleep(10);
  end;
  SetWindowRgn(FForm.Handle, 0, True);
end;

end.
