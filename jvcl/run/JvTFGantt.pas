{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFGantt.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

  .CDK.REGLINK=JvTFGanttComponentsReg.pas
  Created 10/6/2001 6:14:06 PM
  Eagle Software CDK, Version 5.13 Rev. B

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvTFGantt;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QMenus, QStdCtrls, QExtCtrls, Types,
  {$ENDIF VisualCLX}
  JvTFUtils, JvTFManager;

type
  TJvTFGanttScrollBar = class(TScrollBar)
  private
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    procedure CreateWnd; override;
    function GetLargeChange: Integer; virtual;
    procedure SetLargeChange(Value: Integer); virtual;
    procedure UpdateRange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property LargeChange: Integer read GetLargeChange write SetLargeChange default 1;
  end;

  TJvTFGanttScale = (ugsYear, ugsQuarter, ugsMonth, ugsWeek, ugsDay, ugsHour, ugsHalfHour, ugsQuarterHour, ugsMinute);

  TJvTFGanttScaleFormat = class(TPersistent)
  private
    FScale: TJvTFGanttScale;
    FFont: TFont;
    FFormat: string;
    FWidth: Integer;
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Format: string read FFormat write FFormat;
    property Font: TFont read GetFont write SetFont;
    property Scale: TJvTFGanttScale read FScale write FScale;
    property Width: Integer read FWidth write FWidth;
  end;

  TJvTFGantt = class(TJvTFControl)
  private
    // property fields
    FMajorScale: TJvTFGanttScaleFormat;
    FMinorScale: TJvTFGanttScaleFormat;
    FHScrollBar: TJvTFGanttScrollBar;
    FVScrollBar: TJvTFGanttScrollBar;
    FVisibleScrollBars: TJvTFVisibleScrollBars;
    FCustomGlyphs: TBitmap;
    // Other class variables
    FPaintBuffer: TBitmap;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
  protected
    procedure DrawMajor(ACanvas: TCanvas); virtual;
    procedure DrawMinor(ACanvas: TCanvas); virtual;
    procedure SetVisibleScrollBars(Value: TJvTFVisibleScrollBars); virtual;
    function CalcHeaderHeight: Integer;
    procedure AlignScrollBars; virtual;
    function GetMinorScale: TJvTFGanttScaleFormat; virtual;
    procedure SetMinorScale(const Value: TJvTFGanttScaleFormat); virtual;
    function GetMajorScale: TJvTFGanttScaleFormat; virtual;
    procedure SetMajorScale(const Value: TJvTFGanttScaleFormat); virtual;
    procedure DrawClientArea; virtual;
    procedure DrawHeader(ACanvas: TCanvas); virtual;
    procedure Loaded; override;
    procedure Resize; override;
    procedure DrawCustomGlyph(SomeBitmap: TBitmap;
      TargetLeft, TargetTop, ImageIndex, NumGlyphsPerBitmap: Integer); dynamic;
    function ClientCursorPos: TPoint;
    function ValidMouseAtDesignTime: Boolean;
    procedure AdjustComponentHeightBasedOnFontChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareAllBitmaps;
    procedure PrepareBitmaps(SomeGlyph: TBitmap; ResourceName: PChar); dynamic;
    procedure Paint; override;
  published
    property MajorScale: TJvTFGanttScaleFormat read GetMajorScale write SetMajorScale;
    property MinorScale: TJvTFGanttScaleFormat read GetMinorScale write SetMinorScale;
    property VisibleScrollBars: TJvTFVisibleScrollBars read FVisibleScrollBars write SetVisibleScrollBars
      default [vsbHorz, vsbVert];
    property Align;
    property Anchors;
  end;

implementation

{$IFDEF USEJVCL}
uses
  JvJVCLUtils, JvResources;
{$ENDIF USEJVCL}

{$IFNDEF USEJVCL}
resourcestring
  RsThisIsTheMajorScale = 'This is the Major Scale';
  RsThisIsTheMinorScale = 'This is the Minor Scale';
{$ENDIF USEJVCL}

//=== { TJvTFGantt } =========================================================

constructor TJvTFGantt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPaintBuffer := TBitmap.Create;
  FCustomGlyphs := TBitmap.Create;
  FVisibleScrollBars := [vsbHorz, vsbVert];

  FVScrollBar := TJvTFGanttScrollBar.Create(Self);
  with FVScrollBar do
  begin
    Kind := sbVertical;
    TabStop := False;
    Anchors := [];
    Parent := Self;
    Visible := True;
    // OnScroll := ScrollBarScroll;
  end;

  FHScrollBar := TJvTFGanttScrollBar.Create(Self);
  with FHScrollBar do
  begin
    Kind := sbHorizontal;
    TabStop := False;
    Anchors := [];
    Parent := Self;
    Visible := True;
    // OnScroll := ScrollBarScroll;
  end;

  FMajorScale := TJvTFGanttScaleFormat.Create;
  FMajorScale.Scale := ugsMonth;
  FMajorScale.Format := 'mmmm';
  FMinorScale := TJvTFGanttScaleFormat.Create;
  FMinorScale.Scale := ugsDay;
  FMinorScale.Format := 'dd';

  PrepareAllBitmaps;
end;

destructor TJvTFGantt.Destroy;
begin
  FPaintBuffer.Free;
  FMajorScale.Free;
  FMinorScale.Free;
  FVScrollBar.Free;
  FHScrollBar.Free;
  FCustomGlyphs.Free;
  inherited Destroy;
end;

procedure TJvTFGantt.Loaded;
begin
  inherited Loaded;
  AlignScrollBars;
end;

procedure TJvTFGantt.DrawMajor(ACanvas: TCanvas);
var
  Caption: string;
begin
  ACanvas.Font.Assign(FMajorScale.Font);
  Caption := RsThisIsTheMajorScale;
  ACanvas.TextOut((Width div 2) - (ACanvas.TextWidth(Caption) div 2), 2, Caption);
end;

procedure TJvTFGantt.DrawMinor(ACanvas: TCanvas);
var
  Caption: string;
begin
  ACanvas.Font.Assign(FMinorScale.Font);
  Caption := RsThisIsTheMinorScale;
  ACanvas.TextOut((Width div 2) - (ACanvas.TextWidth(Caption) div 2),
    (CalcHeaderHeight div 2) + 2, Caption);
end;

function TJvTFGantt.CalcHeaderHeight: Integer;
begin
  Result := 0;

  Canvas.Font.Assign(FMajorScale.Font);
  {$IFDEF USEJVCL}
  Result := Result + CanvasMaxTextHeight(Canvas);
  {$ELSE}
  Result := Result + Canvas.TextHeight('Ay');
  {$ENDIF USEJVCL}

  Canvas.Font.Assign(FMinorScale.Font);
  {$IFDEF USEJVCL}
  Result := Result + CanvasMaxTextHeight(Canvas);
  {$ELSE}
  Result := Result + Canvas.TextHeight('Ay');
  {$ENDIF USEJVCL}

  Result := Result + 4;
end;

procedure TJvTFGantt.Resize;
begin
  inherited Resize;
  AlignScrollBars;
end;

procedure TJvTFGantt.SetMajorScale(const Value: TJvTFGanttScaleFormat);
begin
  FMajorScale.Assign(Value);
end;

function TJvTFGantt.GetMajorScale: TJvTFGanttScaleFormat;
begin
  Result := FMajorScale;
end;

procedure TJvTFGantt.SetMinorScale(const Value: TJvTFGanttScaleFormat);
begin
  FMinorScale.Assign(Value);
end;

function TJvTFGantt.GetMinorScale: TJvTFGanttScaleFormat;
begin
  Result := FMinorScale;
end;

procedure TJvTFGantt.SetVisibleScrollBars(Value: TJvTFVisibleScrollBars);
begin
  if Value <> FVisibleScrollBars then
  begin
    FVisibleScrollBars := Value;
    AlignScrollBars;
    FVScrollBar.Visible := vsbVert in FVisibleScrollBars;
    FHScrollBar.Visible := vsbHorz in FVisibleScrollBars;
  end;
end;

procedure TJvTFGantt.AlignScrollBars;
begin
  // DO NOT INVALIDATE GRID IN THIS METHOD
  FVScrollBar.Left := ClientWidth - FVScrollBar.Width;
  FVScrollBar.Top := CalcHeaderHeight;
  FVScrollBar.Height := FHScrollBar.Top - FVScrollBar.Top;

  FHScrollBar.Top := ClientHeight - FHScrollBar.Height;
  FHScrollBar.Left := 0;
  FHScrollBar.Width := FVScrollBar.Left - FHScrollBar.Left;

  with FVScrollBar do
    if vsbHorz in VisibleScrollBars then
      Height := FHScrollBar.Top - Top
    else
      Height := Self.ClientHeight - Top;

  with FHScrollBar do
    if vsbVert in VisibleScrollBars then
      Width := FVScrollBar.Left - Left
    else
      Width := Self.ClientWidth - Left;
end;

procedure TJvTFGantt.DrawClientArea;
begin
   // Draw the client area
end;

procedure TJvTFGantt.DrawHeader(ACanvas: TCanvas);
begin
  DrawMajor(ACanvas);
  DrawMinor(ACanvas);
end;

procedure TJvTFGantt.Paint;
begin
  inherited Paint;
  with FPaintBuffer do
  begin
    Width := ClientWidth;
    Height := ClientHeight;

    with Canvas do
    begin
      Brush.Color := Self.Color;
      FillRect(Rect(0, 0, Width, Height));
    end;

    DrawHeader(Canvas);
    DrawClientArea;
  end;
  if Enabled then
    Windows.BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, FPaintBuffer.Canvas.Handle, 0, 0, SRCCOPY)
  else
    Windows.DrawState(Canvas.Handle, 0, nil, FPaintBuffer.Handle, 0, 0, 0, 0, 0, DST_BITMAP or DSS_UNION or
      DSS_DISABLED);

end;

{ Draws SomeBitmap out to the canvas. Use ImageIndex = 0 and NumGlyphsPerBitmap = 1 to draw the entire image,
   or use other values to specify sub-glyphs within the image (for bitmaps that contain several same-sized
   images aligned side-to-side in a single row).

   TargetLeft and TargetTop are the left and top coordinates in the Canvas where you would like this image to appear.
   Use 0 and 0 to place the image in the top left corner.

   CDK: Call this method from an appropriate point in your code (e.g., a "Paint" or "DrawItem" override).

   Examples:

      // Draws entire image:
      DrawCustomGlyph(FCustomGlyphs, 0, 0, 0, 1);

      // Draws last image within FCustomGlyph (which contains four side-to-side images):
      DrawCustomGlyph(FCustomGlyphs, 0, 0, 3, 4);
}

procedure TJvTFGantt.DrawCustomGlyph(SomeBitmap: TBitmap;
  TargetLeft, TargetTop, ImageIndex, NumGlyphsPerBitmap: Integer);
var
  LocalImageWidth: Integer;
  SourceRect, DestRect: TRect;
begin
  with Canvas do
  begin
    with SourceRect do
    begin
      if NumGlyphsPerBitmap = 0 then
        NumGlyphsPerBitmap := 1;
      LocalImageWidth := SomeBitmap.Width div NumGlyphsPerBitmap;
      Left := ImageIndex * LocalImageWidth;
      Top := 0;
      Right := Left + LocalImageWidth;
      Bottom := Top + SomeBitmap.Height;
    end;
    with DestRect do
    begin
      Left := TargetLeft;
      Top := TargetTop;
      Right := Left + LocalImageWidth;
      Bottom := Top + SomeBitmap.Height;
    end;
    CopyRect(DestRect, SomeBitmap.Canvas, SourceRect);
  end;
end;

{ Prepares glyphs for display.
   The following colors in your glyphs will be replaced:

            Yellow with clBtnHighlight
            Silver with clBtnFace
            Gray with clBtnShadow
            White with clWindow
            Red with clWindowText

   CDK: Modify your glyphs so that they conform to the colors above, or alternatively
   modify the colors referenced in the code below.
}

procedure TJvTFGantt.PrepareBitmaps(SomeGlyph: TBitmap; ResourceName: PChar);
var
  LocalBitmap: TBitmap;

  procedure ReplaceColors(SourceBmp, TargetBmp: TBitmap; SourceColor, TargetColor: TColor);
  begin
    TargetBmp.Canvas.Brush.Color := TargetColor;
    TargetBmp.Canvas.BrushCopy(SourceBmp.Canvas.ClipRect, SourceBmp,
      SourceBmp.Canvas.ClipRect, SourceColor);
  end;

begin
  LocalBitmap := TBitmap.Create;
  try
    LocalBitmap.LoadFromResourceName(HInstance, ResourceName);
    SomeGlyph.Width := LocalBitmap.Width;
    SomeGlyph.Height := LocalBitmap.Height;

      { Replace the following colors after loading bitmap:

            clYellow with clBtnHighlight
            clSilver with clBtnFace
            clGray with clBtnShadow
            clWhite with clWindow
            clRed with clWindowText
      }

      { Must call ReplaceColors an odd number of times, to ensure that final image ends up in SomeGlyph.
         As it turns out, we need to make exactly five replacements. Note that each subsequent call to
         ReplaceColors switches the order of parameters LocalBitmap and SomeGlyph. This is because
         we are copying the image back and forth, replacing individual colors with each copy. }

    ReplaceColors(LocalBitmap, SomeGlyph, clYellow, clBtnHighlight);
    ReplaceColors(SomeGlyph, LocalBitmap, clSilver, clBtnFace);
    ReplaceColors(LocalBitmap, SomeGlyph, clGray, clBtnShadow);
    ReplaceColors(SomeGlyph, LocalBitmap, clWhite, clWindow);
    ReplaceColors(LocalBitmap, SomeGlyph, clRed, clWindowText);
  finally
    LocalBitmap.Free;
  end;
end;

procedure TJvTFGantt.PrepareAllBitmaps;
begin
   { CDK: Replace BITMAP_RESOURCE_NAME with the name of your bitmap resource. }
//   PrepareBitmaps(FCustomGlyphs, 'BITMAP_RESOURCE_NAME');
   { CDK: If you have other Glyphs that need loading/preparing, place additional
      calls to PrepareBitmaps here. }
end;

procedure TJvTFGantt.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  PrepareAllBitmaps;
end;

function TJvTFGantt.ClientCursorPos: TPoint;
begin
  GetCursorPos(Result);
  Result := ScreenToClient(Result);
end;

function TJvTFGantt.ValidMouseAtDesignTime: Boolean;
begin
  Result := False;
end;

procedure TJvTFGantt.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  // True = Allow design-time mouse hits to get through if Alt key is down.
  Msg.Result := Ord(ValidMouseAtDesignTime);
end;

procedure TJvTFGantt.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  AdjustComponentHeightBasedOnFontChange;
end;

procedure TJvTFGantt.AdjustComponentHeightBasedOnFontChange;
begin
{ CDK: Add code to calculate the new height. If this is a composite component
  and you have any edit boxes, the edit box size will have already changed
  based on the new font (providing this method is called from a CM_FontChanged
  message handler).

  For example, your code might look like this:

  LockHeight := False;
  Height := Edit1.Height;
  Button1.Height := Height;
  LockHeight := True;
}
end;

//=== { TJvTFGanttScaleFormat } ==============================================

constructor TJvTFGanttScaleFormat.Create;
begin
  // (rom) added inherited Create
  inherited Create;
  FFont := TFont.Create;
end;

destructor TJvTFGanttScaleFormat.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

function TJvTFGanttScaleFormat.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TJvTFGanttScaleFormat.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//=== { TJvTFGanttScrollBar } ================================================

constructor TJvTFGanttScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // If we set the csNoDesignVisible flag then visibility at design time
  //  is controlled by the Visible property, which is exactly what we want.
  ControlStyle := ControlStyle + [csNoDesignVisible];
  ParentCtl3D := False;
  Ctl3D := False;
end;

procedure TJvTFGanttScrollBar.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  Msg.Result := 1;
end;

procedure TJvTFGanttScrollBar.CreateWnd;
begin
  inherited CreateWnd;
  UpdateRange;
end;

function TJvTFGanttScrollBar.GetLargeChange: Integer;
begin
  Result := inherited LargeChange;
end;

procedure TJvTFGanttScrollBar.SetLargeChange(Value: Integer);
begin
  inherited LargeChange := Value;
  UpdateRange;
end;

procedure TJvTFGanttScrollBar.UpdateRange;
var
  Info: TScrollInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  with Info do
  begin
    cbSize := SizeOf(Info);
    fMask := SIF_PAGE;
    nPage := LargeChange;
  end;
  SetScrollInfo(Handle, SB_CTL, Info, True);
end;

end.

