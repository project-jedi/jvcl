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
home page, located at http://jvcl.delphi-jedi.org

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

function DesignClientToParent(const APt: TPoint; AControl, AParent: TControl): TPoint;

function DesignMin(AA, AB: Integer): Integer;
function DesignMax(AA, AB: Integer): Integer;

function DesignRectWidth(const ARect: TRect): Integer;
function DesignRectHeight(const ARect: TRect): Integer;
function DesignValidateRect(const ARect: TRect): TRect;

function DesignNameIsUnique(AOwner: TComponent; const AName: string): Boolean;
function DesignUniqueName(AOwner: TComponent; const AClassName: string): string;

procedure DesignPaintRubberbandRect(AContainer: TWinControl; ARect: TRect; APenStyle: TPenStyle);
procedure DesignPaintGrid(ACanvas: TCanvas; const ARect: TRect;
  ABackColor: TColor = clBtnFace; AGridColor: TColor = clBlack;
  ADivPixels: Integer = 8);
procedure DesignPaintRules(ACanvas: TCanvas; const ARect: TRect;
  ADivPixels: Integer = 32; ASubDivs: Boolean = True);

procedure DesignSaveComponentToStream(AComp: TComponent; AStream: TStream);
function DesignLoadComponentFromStream(AComp: TComponent; AStream: TStream;
  AOnError: TReaderError): TComponent;

procedure DesignSaveComponentToFile(AComp: TComponent; const AFileName: string);
procedure DesignLoadComponentFromFile(AComp: TComponent;
  const AFileName: string; AOnError: TReaderError);

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

function DesignClientToParent(const APt: TPoint; AControl, AParent: TControl): TPoint;
begin
  Result := APt;
  while (AControl <> AParent) and (AControl <> nil) do
  begin
    Inc(Result.X, AControl.Left);
    Inc(Result.Y, AControl.Top);
    AControl := AControl.Parent;
  end;
end;

function DesignMin(AA, AB: Integer): Integer;
begin
  if AB < AA then
    Result := AB
  else
    Result := AA;
end;

function DesignMax(AA, AB: Integer): Integer;
begin
  if AB > AA then
    Result := AB
  else
    Result := AA;
end;

function DesignRectWidth(const ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

function DesignRectHeight(const ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function DesignValidateRect(const ARect: TRect): TRect;
begin
  if ARect.Right < ARect.Left then
  begin
    Result.Left := ARect.Right;
    Result.Right := ARect.Left;
  end
  else
  begin
    Result.Left := ARect.Left;
    Result.Right := ARect.Right;
  end;
  if ARect.Bottom < ARect.Top then
  begin
    Result.Top := ARect.Bottom;
    Result.Bottom := ARect.Top;
  end
  else
  begin
    Result.Top := ARect.Top;
    Result.Bottom := ARect.Bottom;
  end;
end;

function DesignNameIsUnique(AOwner: TComponent; const AName: string): Boolean;
begin
  Result := True;
  while Result and (AOwner <> nil) do
  begin
    Result := AOwner.FindComponent(AName) = nil;
    AOwner := AOwner.Owner;
  end;
end;

function DesignUniqueName(AOwner: TComponent; const AClassName: string): string;
var
  Base: string;
  I: Integer;
begin
  Base := Copy(AClassName, 2, MAXINT);
  I := 0;
  repeat
    Inc(I);
    Result := Base + IntToStr(I);
  until DesignNameIsUnique(AOwner, Result);
end;

procedure DesignPaintRubberbandRect(AContainer: TWinControl; ARect: TRect; APenStyle: TPenStyle);
var
  DesktopWindow: HWND;
  DC: HDC;
  C: TCanvas;
begin
  if AContainer = nil then
    DesktopWindow := GetDesktopWindow
  else
  begin
    DesktopWindow := AContainer.Handle;
    ARect.TopLeft := AContainer.ScreenToClient(ARect.TopLeft);
    ARect.BottomRight := AContainer.ScreenToClient(ARect.BottomRight);
  end;
  DC := GetDCEx(DesktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
  try
    C := TCanvas.Create;
    try
      C.Handle := DC;
      C.Pen.Style := APenStyle;
      C.Pen.Color := clWhite;
      C.Pen.Mode := pmXor;
      C.Brush.Style := bsClear;
      C.Rectangle(ARect);
    finally
      C.Free;
    end;
  finally
    ReleaseDC(DesktopWindow, DC);
  end;
end;

procedure DesignPaintRules(ACanvas: TCanvas; const ARect: TRect;
  ADivPixels: Integer; ASubDivs: Boolean);
var
  d, d2, w, h, I: Integer;
begin
  d := ADivPixels;
  d2 := d div 2;
  w := (ARect.Right - ARect.Left + d - 1) div d;
  h := (ARect.Bottom - ARect.Top + d - 1) div d;
  with ACanvas do
  begin
    Pen.Style := psDot;
    for I := 0 to w do
    begin
      Pen.Color := $DDDDDD;
      MoveTo(I * d, ARect.Top);
      LineTo(I * d, ARect.Bottom);
      if ASubDivs then
      begin
        Pen.Color := $F0F0F0;
        MoveTo(I * d + d2, ARect.Top);
        LineTo(I * d + d2, ARect.Bottom);
      end;
    end;
    for I := 0 to h do
    begin
      Pen.Color := $DDDDDD;
      MoveTo(ARect.Left, I * d);
      LineTo(ARect.Right, I * d);
      if ASubDivs then
      begin
        Pen.Color := $F0F0F0;
        MoveTo(ARect.Left, I * d + d2);
        LineTo(ARect.Right, I * d + d2);
      end;
    end;
  end;
end;

procedure DesignPaintGrid(ACanvas: TCanvas; const ARect: TRect;
  ABackColor, AGridColor: TColor; ADivPixels: Integer);
var
  b: TBitmap;
  I: Integer;
begin
  b := TBitmap.Create;
  try
    b.Height := DesignRectHeight(ARect);
    b.Width := ADivPixels;
    b.Canvas.Brush.Color := ABackColor;
    b.Canvas.FillRect(Rect(0, 0, b.Width, b.Height));

    I := 0;
    repeat
      b.Canvas.Pixels[0, I] := AGridColor;
      Inc(I, ADivPixels);
    until (I >= b.Height);

    I := ARect.Left;
    repeat
      ACanvas.Draw(I, ARect.Top, b);
      Inc(I, ADivPixels);
    until I >= ARect.Right;
  finally
    b.Free;
  end;
end;

procedure DesignSaveComponentToStream(AComp: TComponent; AStream: TStream);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.WriteComponent(AComp);
    MS.Position := 0;
    ObjectBinaryToText(MS, AStream);
  finally
    MS.Free;
  end;
end;

type
  TAccessComponent = class(TComponent);

function DesignLoadComponentFromStream(AComp: TComponent; AStream: TStream;
  AOnError: TReaderError): TComponent;
var
  MemStream: TMemoryStream;
  CompDesigning: Boolean;
begin
  MemStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(AStream, MemStream);
    MemStream.Position := 0;
    with TReader.Create(MemStream, 4096) do
    try
      OnError := AOnError;
      { We have to set the container into design mode so all loaded components
        are in design mode. }
      CompDesigning := csDesigning in AComp.ComponentState;
      TAccessComponent(AComp).SetDesigning(True, False);
      try
        Result := ReadRootComponent(AComp);
      finally
        if not CompDesigning then
          TAccessComponent(AComp).SetDesigning(CompDesigning, False);
      end;
    finally
      Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure DesignSaveComponentToFile(AComp: TComponent; const AFileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmCreate);
  try
    DesignSaveComponentToStream(AComp, FS);
  finally
    FS.Free;
  end;
end;

procedure DesignLoadComponentFromFile(AComp: TComponent;
  const AFileName: string; AOnError: TReaderError);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead);
  try
    DesignLoadComponentFromStream(AComp, FS, AOnError);
  finally
    FS.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
