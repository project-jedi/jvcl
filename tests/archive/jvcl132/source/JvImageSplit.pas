{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageSplit.PAS, released on 2001-02-28.

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

unit JvImageSplit;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvFunctions, JvTypes, JvComponent;

type
  TJvImageSplit = class(TJvComponent)
  private
  published
    function GetRBitmap(Value: TBitmap): TBitmap;
    function GetGBitmap(Value: TBitmap): TBitmap;
    function GetBBitmap(Value: TBitmap): TBitmap;
    function GetMonochromeBitmap(Value: TBitmap): TBitmap;
    function GetHueBitmap(Value: TBitmap): TBitmap;
    function GetSaturationBitmap(Value: TBitmap): TBitmap;
    function GetValueBitmap(Value: TBitmap): TBitmap;
  end;

implementation

{************************************************************}

function TJvImageSplit.GetRBitmap(Value: TBitmap): TBitmap;
var
  i, j: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowB := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[i].rgbtRed := rowRGB[i].rgbtRed;
      TRGBArray(rowB^)[i].rgbtGreen := 0;
      TRGBArray(rowB^)[i].rgbtBlue := 0;
    end;
  end;
end;

{************************************************************}

function TJvImageSplit.GetBBitmap(Value: TBitmap): TBitmap;
var
  i, j: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowB := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[i].rgbtRed := 0;
      TRGBArray(rowB^)[i].rgbtGreen := 0;
      TRGBArray(rowB^)[i].rgbtBlue := rowRGB[i].rgbtBlue;
    end;
  end;
end;

{************************************************************}

function TJvImageSplit.GetGBitmap(Value: TBitmap): TBitmap;
var
  i, j: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowB := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[i].rgbtRed := 0;
      TRGBArray(rowB^)[i].rgbtGreen := rowRGB[i].rgbtGreen;
      TRGBArray(rowB^)[i].rgbtBlue := 0;
    end;
  end;
end;

{************************************************************}

function TJvImageSplit.GetHueBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, i, j: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowS := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      with rowRGB[i] do
        RgbToHsv(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[i].rgbtBlue := h;
      rowS[i].rgbtGreen := h;
      rowS[i].rgbtRed := h;
    end;
  end;
end;

{************************************************************}

function TJvImageSplit.GetMonochromeBitmap(Value: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Assign(Value);
  Result.Monochrome := True;
end;

{************************************************************}

function TJvImageSplit.GetSaturationBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, i, j: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowS := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      with rowRGB[i] do
        RgbToHsv(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[i].rgbtBlue := s;
      rowS[i].rgbtGreen := s;
      rowS[i].rgbtRed := s;
    end;
  end;
end;

{************************************************************}

function TJvImageSplit.GetValueBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, i, j: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowS := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      with rowRGB[i] do
        RgbToHsv(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[i].rgbtBlue := v;
      rowS[i].rgbtGreen := v;
      rowS[i].rgbtRed := v;
    end;
  end;
end;

end.
