{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesingUtils.pas, released on 2005-08-21.

The Initial Developer of the Original Code is Scott J Miles
Portions created by Scott J Miles are Copyright (C) 2005 Scott J Miles.
All Rights Reserved.

Contributor(s): Olivier Sannier (JVCL Integration)

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
unit JvDesignUtils;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Windows, Classes, Controls, Graphics, Forms;

function DesignClientToParent(const inPt: TPoint;
  inControl, inParent: TControl): TPoint;

function DesignMin(inA, inB: Integer): Integer;
function DesignMax(inA, inB: Integer): Integer;

function DesignRectWidth(const inRect: TRect): Integer;
function DesignRectHeight(const inRect: TRect): Integer;
function DesignValidateRect(const inRect: TRect): TRect;

function DesignNameIsUnique(inOwner: TComponent; const inName: string): Boolean;
function DesignUniqueName(inOwner: TComponent; const inClassName: string): string;

procedure DesignPaintRubberbandRect(const inRect: TRect; inPenStyle: TPenStyle);
procedure DesignPaintGrid(inCanvas: TCanvas; const inRect: TRect;
  inBackColor: TColor = clBtnFace; inGridColor: TColor = clBlack;
  inDivPixels: Integer = 8);
procedure DesignPaintRules(inCanvas: TCanvas; const inRect: TRect;
  inDivPixels: Integer = 32; inSubDivs: Boolean = true);

procedure DesignSaveComponentToStream(inComp: TComponent; inStream: TStream);
function DesignLoadComponentFromStream(inComp: TComponent; inStream: TStream;
  inOnError: TReaderError): TComponent;

procedure DesignSaveComponentToFile(inComp: TComponent;
   const inFilename: string);
procedure DesignLoadComponentFromFile(inComp: TComponent;
  const inFilename: string; inOnError: TReaderError);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

function DesignClientToParent(const inPt: TPoint;
  inControl, inParent: TControl): TPoint;
begin
  Result := inPt;
  while (inControl <> inParent) and (inControl <> nil) do
  begin
    Inc(Result.X, inControl.Left);
    Inc(Result.Y, inControl.Top);
    inControl := inControl.Parent;
  end;
end;

function DesignMin(inA, inB: Integer): Integer;
begin
  if inB < inA then
    Result := inB
  else
    Result := inA;
end;

function DesignMax(inA, inB: Integer): Integer;
begin
  if inB > inA then
    Result := inB
  else
    Result := inA;
end;

function DesignRectWidth(const inRect: TRect): Integer;
begin
  Result := inRect.Right - inRect.Left;
end;

function DesignRectHeight(const inRect: TRect): Integer;
begin
  Result := inRect.Bottom - inRect.Top;
end;

function DesignValidateRect(const inRect: TRect): TRect;
begin
  with Result do
  begin
    if inRect.Right < inRect.Left then
    begin
      Left := inRect.Right;
      Right := inRect.Left;
    end
    else begin
      Left := inRect.Left;
      Right := inRect.Right;
    end;
    if inRect.Bottom < inRect.Top then
    begin
      Top := inRect.Bottom;
      Bottom := inRect.Top;
    end
    else begin
      Top := inRect.Top;
      Bottom := inRect.Bottom;
    end;
  end;
end;

function DesignNameIsUnique(inOwner: TComponent; const inName: string): Boolean;
begin
  Result := true;
  while Result and (inOwner <> nil) do
  begin
    Result := inOwner.FindComponent(inName) = nil;
    inOwner := inOwner.Owner;
  end;
end;

function DesignUniqueName(inOwner: TComponent; const inClassName: string): string;
var
  base: string;
  i: Integer;
begin
  base := Copy(inClassName, 2, MAXINT);
  i := 0;
  repeat
    Inc(i);
    Result := base + IntToStr(i);
  until DesignNameIsUnique(inOwner, Result);
end;

procedure DesignPaintRubberbandRect(const inRect: TRect; inPenStyle: TPenStyle);
var
  desktopWindow: HWND;
  dc: HDC;
  c: TCanvas;
begin
  desktopWindow := GetDesktopWindow;
  dc := GetDCEx(desktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
  try
    c := TCanvas.Create;
    with c do
    try
      Handle := dc;
      Pen.Style := inPenStyle;
      Pen.Color := clWhite;
      Pen.Mode := pmXor;
      Brush.Style := bsClear;
      Rectangle(inRect);
    finally
      c.Free;
    end;
  finally
    ReleaseDC(desktopWindow, dc);
  end;
end;

procedure DesignPaintRules(inCanvas: TCanvas; const inRect: TRect;
  inDivPixels: Integer; inSubDivs: Boolean);
var
  d, d2, w, h, i: Integer;
begin
  d := inDivPixels;
  d2 := d div 2;
  w := (inRect.Right - inRect.Left + d - 1) div d;
  h := (inRect.Bottom - inRect.Top + d - 1) div d;
  with inCanvas do
  begin
    Pen.Style := psDot;
    for i := 0 to w do
    begin
      Pen.Color := $DDDDDD;
      MoveTo(i * d, inRect.Top);
      LineTo(i * d, inRect.Bottom);
      if inSubDivs then
      begin
        Pen.Color := $F0F0F0;
        MoveTo(i * d + d2, inRect.Top);
        LineTo(i * d + d2, inRect.Bottom);
      end;
    end;
    for i := 0 to h do
    begin
      Pen.Color := $DDDDDD;
      MoveTo(inRect.Left, i * d);
      LineTo(inRect.Right, i * d);
      if inSubDivs then
      begin
        Pen.Color := $F0F0F0;
        MoveTo(inRect.Left, i * d + d2);
        LineTo(inRect.Right, i * d + d2);
      end;
    end;
  end;
end;

procedure DesignPaintGrid(inCanvas: TCanvas; const inRect: TRect;
  inBackColor, inGridColor: TColor; inDivPixels: Integer);
var
  b: TBitmap;
  i: Integer;
begin
  b := TBitmap.Create;
  try
    b.Height := DesignRectHeight(inRect);
    b.Width := inDivPixels;
    b.Canvas.Brush.Color := inBackColor;
    b.Canvas.FillRect(Rect(0, 0, b.Width, b.Height));
    //
    i := 0;
    repeat
      b.Canvas.Pixels[0, i] := inGridColor;
      inc(i, inDivPixels);
    until (i >= b.Height);
    //
    i := inRect.Left;
    repeat
      inCanvas.Draw(i, inRect.Top, b);
      Inc(i, inDivPixels);
    until (i >= inRect.Right);
  finally
    b.Free;
  end;
end;

procedure DesignSaveComponentToStream(inComp: TComponent; inStream: TStream);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.WriteComponent(inComp);
    ms.Position := 0;
    ObjectBinaryToText(ms, inStream);
  finally
    ms.Free;
  end;
end;

function DesignLoadComponentFromStream(inComp: TComponent; inStream: TStream;
  inOnError: TReaderError): TComponent;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ObjectTextToBinary(inStream, ms);
    ms.Position := 0;
    with TReader.Create(ms, 4096) do
    try
      OnError := inOnError;
      Result := ReadRootComponent(inComp);
    finally
      Free;
    end;
  finally
    ms.Free;
  end;
end;

procedure DesignSaveComponentToFile(inComp: TComponent;
  const inFilename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(inFilename, fmCreate);
  try
    DesignSaveComponentToStream(inComp, fs);
  finally
    fs.Free;
  end;
end;

procedure DesignLoadComponentFromFile(inComp: TComponent;
  const inFilename: string; inOnError: TReaderError);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(inFilename, fmOpenRead);
  try
    DesignLoadComponentFromStream(inComp, fs, inOnError);
  finally
    fs.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
