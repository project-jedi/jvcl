{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWizardCommom.PAS, released on 2001-12-23.

The Initial Developer of the Original Code is William Yu Wei.
Portions created by William Yu Wei are Copyright (C) 2001 William Yu Wei.
All Rights Reserved.

Contributor(s):
Peter Thörnqvist - converted to JVCL naming conventions on 2003-07-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Purpose:
  All common functions and procedures which used by all components

History:
  12/23/2001       First Create, introduce TKSide, TKSides, TJvWizardFrameStyle,
                     beAllSides, TKDeleteItemEvent
                   function KDrawSides, KDrawBevel, KDrawFrame
  12/25/2001       introduced TKMessageLevel
  01/04/2001       Add function KDrawBorderSides

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQWizardCommon;

{$I jvcl.inc}

interface

uses
  QWindows, QControls, QGraphics, 
  Types, 
  Classes, SysUtils;

const
  beAllEdges = [beLeft, beTop, beRight, beBottom];

type
  TJvWizardFrameStyle =
    (fsWindows, fsNone, fsFlat, fsGroove, fsBump, fsLowered, fsRaised);
  TJvWizardImageAlignment = (iaLeft, iaRight, iaCenter, iaStretch);
  TJvWizardImageLeftRight = iaLeft..iaRight;
  TJvWizardImageLayout = (ilTop, ilBottom, ilCenter, ilStretch, ilTile);
  EJvWizardError = class(Exception);

function JvWizardDrawEdges(ACanvas: TCanvas; ABounds: TRect;
  ULColor, LRColor: TColor; AEdges: TBevelEdges): TRect;

function JvWizardDrawBorderEdges(ACanvas: TCanvas; ABounds: TRect;
  AStyle: TJvWizardFrameStyle; AEdges: TBevelEdges): TRect;

procedure JvWizardDrawImage(ACanvas: TCanvas; AGraphic: TGraphic; ARect: TRect;
  Align: TJvWizardImageAlignment; ALayout: TJvWizardImageLayout);

implementation


uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvQResources;




const
  { Frame Style Color constant arrays }
  KULFrameColor: array [TJvWizardFrameStyle] of TColor = (clNone, clWindow,
    clWindowFrame, clBtnShadow, clBtnHighlight, clBtnShadow, clBlack);

  KLRFrameColor: array [TJvWizardFrameStyle] of TColor = (clNone, clBtnFace,
    clWindowFrame, clBtnHighlight, clBtnShadow, clBtnHighlight, clBtnFace);

{-----------------------------------------------------------------------------
  Procedure: JvWizardDrawEdges
  Author:    yuwei
  Date:      December 23, 2001
  Time:      17:22:42
  Purpose:   Draw a frame with specified the borders on the specified bounds
             of the canvas.
  Arguments:
             ACanvas: TCanvas;
               the canvas where it draws the sides.
             ABounds: TRect;
               the bounds of the canvas for drawing.
             ULColor: TColor;
               the left and top side color.
             LRColor: TColor;
               the right and bottom side color.
             ASides: TBevelEdges;
               which sides it can draw on the canvas.
  Result:
             TRect:
               The bounds within the sides after drawing.
  See also:

  History:
  ---------------------------------------------------------------------------
  Date(mm/dd/yy)   Comments
  ---------------------------------------------------------------------------
  12/23/2001       First Release
-----------------------------------------------------------------------------}

function JvWizardDrawEdges(ACanvas: TCanvas; ABounds: TRect;
  ULColor, LRColor: TColor; AEdges: TBevelEdges): TRect;
begin
  with ACanvas, ABounds do
  begin
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    Pen.Color := ULColor;
    if beLeft in AEdges then
    begin
      MoveTo(Left, Top);
      LineTo(Left, Bottom);
    end;
    if beTop in AEdges then
    begin
      MoveTo(Left, Top);
      LineTo(Right, Top);
    end;
    Pen.Color := LRColor;
    if beRight in AEdges then
    begin
      MoveTo(Right - 1, Top);
      LineTo(Right - 1, Bottom);
    end;
    if beBottom in AEdges then
    begin
      MoveTo(Left, Bottom - 1);
      LineTo(Right, Bottom - 1);
    end;
  end;
  if beLeft in AEdges then
    Inc(ABounds.Left);
  if beTop in AEdges then
    Inc(ABounds.Top);
  if beRight in AEdges then
    Dec(ABounds.Right);
  if beBottom in AEdges then
    Dec(ABounds.Bottom);
  Result := ABounds;
end;

function JvWizardDrawBorderEdges(ACanvas: TCanvas; ABounds: TRect;
  AStyle: TJvWizardFrameStyle; AEdges: TBevelEdges): TRect;
var
  ULColor, LRColor: TColor;
  R: TRect;
begin
  { Draw the Frame }
  if not (AStyle in [fsNone, fsWindows]) then
  begin
    ULColor := KULFrameColor[AStyle];
    LRColor := KLRFrameColor[AStyle];
    if AStyle in [fsFlat] then
      ABounds := JvWizardDrawEdges(ACanvas, ABounds, ULColor, LRColor, AEdges)
    else
    begin
      R := ABounds;
      Inc(R.Left);
      Inc(R.Top);
      JvWizardDrawEdges(ACanvas, R, LRColor, LRColor, AEdges);
      OffsetRect(R, -1, -1);
      JvWizardDrawEdges(ACanvas, R, ULColor, ULColor, AEdges);
      if beLeft in AEdges then
        Inc(ABounds.Left, 2);
      if beTop in AEdges then
        Inc(ABounds.Top, 2);
      if beRight in AEdges then
        Dec(ABounds.Right, 2);
      if beBottom in AEdges then
        Dec(ABounds.Bottom, 2);
    end;
  end;
  Result := ABounds;
end;

procedure JvWizardDrawTiled(ACanvas: TCanvas; AGraphic: TGraphic; ARect: TRect);
var
  AWidth, AHeight: Integer;  
  Bmp: QGraphics.TBitmap; 
begin

  if not Assigned(AGraphic) or (AGraphic.Width = 0) or (AGraphic.Height = 0) then
    raise EJvWizardError.CreateRes(@RsETilingError);
  // Create a temporary bitmap to draw into. This is both to speed things up a bit
  // and also to clip the image to the ARect param (using Draw doesn't clip the image,
  // but it does support auto-detecting transparency)
  Bmp := {Graphics.}TBitmap.Create;
  try
    Bmp.Width := ARect.Right - ARect.Left;
    Bmp.Height := ARect.Bottom - ARect.Top;
    Bmp.Canvas.Brush.Color := ACanvas.Brush.Color;
    Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
    AWidth := 0;
    while AWidth <= Bmp.Width do
    begin
      AHeight := 0;
      while AHeight <= Bmp.Height do
      begin
        Bmp.Canvas.Draw(AWidth, AHeight, AGraphic);
        Inc(AHeight, AGraphic.Height);
      end;
      Inc(AWidth, AGraphic.Width);
    end;  
    BitBlt(ACanvas, ARect.Left, ARect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas, 0, 0, SRCCOPY); 
  finally
    Bmp.Free;
  end;
end;

procedure JvWizardDrawImage(ACanvas: TCanvas; AGraphic: TGraphic; ARect: TRect;
  Align: TJvWizardImageAlignment; ALayout: TJvWizardImageLayout);
var
  Offset: TPoint;
  AWidth, AHeight: Integer;
begin
  if Assigned(AGraphic) then
  begin
    if ALayout = ilTile then
    begin
      JvWizardDrawTiled(ACanvas, AGraphic, ARect);
      Exit;
    end;
    Offset := Point(0, 0);
    AWidth := ARect.Right - ARect.Left;
    AHeight := ARect.Bottom - ARect.Top;
    if (Align in [iaCenter, iaRight]) and (AWidth > AGraphic.Width) then
    begin
      Offset.X := AWidth - AGraphic.Width;
      if Align = iaCenter then
      begin
        Offset.X := Offset.X div 2;
        ARect.Right := ARect.Right - Offset.X;
      end;
    end;
    if (ALayout in [ilCenter, ilBottom]) and (AHeight > AGraphic.Height) then
    begin
      Offset.Y := AHeight - AGraphic.Height;
      if ALayout = ilCenter then
      begin
        Offset.Y := Offset.Y div 2;
        ARect.Bottom := ARect.Bottom - Offset.Y;
      end;
    end;
    if (ALayout = ilTop) and (AHeight > AGraphic.Height) then
      ARect.Bottom := ARect.Top + AGraphic.Height;
    if (Align = iaLeft) and (AWidth > AGraphic.Width) then
      ARect.Right := ARect.Left + AGraphic.Width;
    ARect.Left := ARect.Left + Offset.X;
    ARect.Top := ARect.Top + Offset.Y;
    if (Align = iaStretch) or (ALayout = ilStretch) then
      ACanvas.StretchDraw(ARect, AGraphic)
    else
      ACanvas.Draw(ARect.Left, ARect.Top, AGraphic);
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

