unit JvTFGantt; { TJvTFGantt component. }
 {.CDK.REGLINK=JvTFGanttComponentsReg.pas}{ Registration file is JvTFGanttComponentsReg.pas. }
{ Created 10/6/2001 6:14:06 PM }
{ Eagle Software CDK, Version 5.13 Rev. B }

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls,
  JvTFUtils, JvTFManager;

type
  TJvTFGanttScrollBar = class(TScrollBar)
  protected
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
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
    FWidth: integer;
  protected
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Format: string read FFormat write FFormat;
    property Font: TFont read GetFont write SetFont;
    property Scale: TJvTFGanttScale read FScale write FScale;
    property Width: integer read FWidth write FWidth;
  end;

  TJvTFGantt = class(TJvTFControl)
  private
      // property fields
    FMajorScale: TJvTFGanttScaleFormat;
    FMinorScale: TJvTFGanttScaleFormat;

    FHScrollBar: TJvTFGanttScrollBar;
    FVScrollBar: TJvTFGanttScrollBar;
    FVisibleScrollBars: TJvTFVisibleScrollBars;

      // Other class variables
    PaintBuffer: TBitmap;

    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DesignHitTest;
  protected
      { Protected declarations }
    FCustomGlyphs: TBitmap;

    procedure DrawMajor(ACanvas: TCanvas); virtual;
    procedure DrawMinor(ACanvas: TCanvas); virtual;
    procedure SeTJvTFVisibleScrollBars(Value: TJvTFVisibleScrollBars); virtual;
    function CalcHeaderHeight: integer;
    procedure AlignScrollBars; virtual;
    function GetMinorScale: TJvTFGanttScaleFormat; virtual;
    procedure SetMinorScale(const Value: TJvTFGanttScaleFormat); virtual;
    function GetMajorScale: TJvTFGanttScaleFormat; virtual;
    procedure SetMajorScale(const Value: TJvTFGanttScaleFormat); virtual;

    procedure DrawClientArea; virtual;
    procedure DrawHeader(ACanvas: TCanvas); virtual;
    procedure Loaded; override;
    procedure Resize; override;
    procedure DrawCustomGlyph(someBitmap: TBitmap; targetLeft, targetTop, imageIndex, numGlyphsPerBitmap: integer); dynamic;
    function ClientCursorPos: TPoint;
    function ValidMouseAtDesignTime: boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FontChanged;
    procedure AdjustComponentHeightBasedOnFontChange; virtual;
  public
      { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareAllBitmaps;
    procedure PrepareBitmaps(someGlyph: TBitmap; resourceName: PChar); dynamic;
    procedure Paint; override;
  published
    property MajorScale: TJvTFGanttScaleFormat read GetMajorScale write SetMajorScale;
    property MinorScale: TJvTFGanttScaleFormat read GetMinorScale write SetMinorScale;
    property VisibleScrollBars: TJvTFVisibleScrollBars read FVisibleScrollBars write SeTJvTFVisibleScrollBars;

      // inherited
    property Align;
    property Anchors;
  end; { TJvTFGantt }


implementation
{$IFDEF USEJVCL}
uses
  JvJVCLUtils, JvResources;
{$ENDIF}  

{$IFNDEF USEJVCL}
resourcestring
  RsThisIsTheMajorScale = 'This is the Major Scale';
  RsThisIsTheMinorScale = 'This is the Minor Scale';
{$ENDIF}

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
  ACanvas.TextOut((Width div 2) - (ACanvas.TextWidth(Caption) div 2), (CalcHeaderHeight div 2) + 2, Caption);
end;

function TJvTFGantt.CalcHeaderHeight: integer;
begin
  result := 0;
  Canvas.Font.Assign(FMajorScale.Font);
  result := result + Canvas.TextHeight('Wq');
  Canvas.Font.Assign(FMinorScale.Font);
  result := result + Canvas.TextHeight('Wq');
  result := result + 4;
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
  result := FMajorScale;
end;

procedure TJvTFGantt.SetMinorScale(const Value: TJvTFGanttScaleFormat);
begin
  FMinorScale.Assign(Value);
end;

function TJvTFGantt.GetMinorScale: TJvTFGanttScaleFormat;
begin
  result := FMinorScale;
end;

procedure TJvTFGantt.SeTJvTFVisibleScrollBars(Value: TJvTFVisibleScrollBars);
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
  begin
    if vsbHorz in VisibleScrollBars then
      Height := FHScrollBar.Top - Top
    else
      Height := Self.ClientHeight - Top;
  end;

  with FHScrollBar do
  begin
    if vsbVert in VisibleScrollBars then
      Width := FVScrollBar.Left - Left
    else
      Width := Self.ClientWidth - Left;
  end;
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
  with PaintBuffer do
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
    Windows.BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, PaintBuffer.Canvas.Handle, 0, 0, SRCCOPY)
  else
    Windows.DrawState(Canvas.Handle, 0, nil, PaintBuffer.Handle, 0, 0, 0, 0, 0, DST_BITMAP or DSS_UNION or DSS_DISABLED);

end;

procedure TJvTFGantt.DrawCustomGlyph(someBitmap: TBitmap; targetLeft, targetTop, imageIndex, numGlyphsPerBitmap: integer);
{ Draws someBitmap out to the canvas. Use imageIndex = 0 and numGlyphsPerBitmap = 1 to draw the entire image,
   or use other values to specify sub-glyphs within the image (for bitmaps that contain several same-sized
   images aligned side-to-side in a single row).

   targetLeft and targetTop are the left and top coordinates in the Canvas where you would like this image to appear.
   Use 0 and 0 to place the image in the top left corner.

   CDK: Call this method from an appropriate point in your code (e.g., a "Paint" or "DrawItem" override).

   Examples:

      // Draws entire image:
      DrawCustomGlyph(FCustomGlyphs, 0, 0, 0, 1);

      // Draws last image within FCustomGlyph (which contains four side-to-side images):
      DrawCustomGlyph(FCustomGlyphs, 0, 0, 3, 4);
}
var
  localImageWidth: integer;
  SourceRect,
    DestRect: TRect;
begin
  with Canvas do
  begin
    with SourceRect do
    begin
      if numGlyphsPerBitmap = 0 then
        numGlyphsPerBitmap := 1;
      localImageWidth := someBitmap.Width div numGlyphsPerBitmap;
      Left := imageIndex * localImageWidth;
      Top := 0;
      Right := Left + localImageWidth;
      Bottom := Top + someBitmap.Height;
    end; { with }
    with DestRect do
    begin
      Left := targetLeft;
      Top := targetTop;
      Right := Left + localImageWidth;
      Bottom := Top + someBitmap.Height;
    end; { with }
    CopyRect(DestRect, someBitmap.Canvas, SourceRect);
  end; { with }
end; { DrawCustomGlyph }

procedure TJvTFGantt.PrepareBitmaps(someGlyph: TBitmap; resourceName: PChar); { public }
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
var
  localBitmap: TBitmap;

  procedure ReplaceColors(SourceBmp, TargetBMP: TBitmap; sourceColor, targetColor: TColor);
  begin
    TargetBMP.Canvas.Brush.Color := targetColor;
    TargetBMP.Canvas.BrushCopy(SourceBmp.Canvas.ClipRect, SourceBmp, SourceBmp.Canvas.ClipRect, sourceColor);
  end; { ReplaceColors }

begin
  localBitmap := TBitmap.Create;
  try
    localBitmap.LoadFromResourceName(HInstance, resourceName);
    someGlyph.Width := localBitmap.Width;
    someGlyph.Height := localBitmap.Height;

      { Replace the following colors after loading bitmap:

            clYellow with clBtnHighlight
            clSilver with clBtnFace
            clGray with clBtnShadow
            clWhite with clWindow
            clRed with clWindowText
      }

      { Must call ReplaceColors an odd number of times, to ensure that final image ends up in someGlyph.
         As it turns out, we need to make exactly five replacements. Note that each subsequent call to
         ReplaceColors switches the order of parameters localBitmap and someGlyph. This is because
         we are copying the image back and forth, replacing individual colors with each copy. }

    ReplaceColors(localBitmap, someGlyph, clYellow, clBtnHighlight);
    ReplaceColors(someGlyph, localBitmap, clSilver, clBtnFace);
    ReplaceColors(localBitmap, someGlyph, clGray, clBtnShadow);
    ReplaceColors(someGlyph, localBitmap, clWhite, clWindow);
    ReplaceColors(localBitmap, someGlyph, clRed, clWindowText);
  finally { wrap up }
    if assigned(localBitmap) then
      localBitmap.Free;
  end; { try/finally }
end; { PrepareBitmaps }

procedure TJvTFGantt.PrepareAllBitmaps;
begin
   { CDK: Replace BITMAP_RESOURCE_NAME with the name of your bitmap resource. }
//   PrepareBitmaps(FCustomGlyphs, 'BITMAP_RESOURCE_NAME');
   { CDK: If you have other Glyphs that need loading/preparing, place additional
      calls to PrepareBitmaps here. }
end; { PrepareAllBitmaps }

procedure TJvTFGantt.CMSysColorChange(var Msg: TMessage); { private }
begin
  inherited;
  PrepareAllBitmaps;
end; { CMSysColorChange }

function TJvTFGantt.ClientCursorPos: TPoint;
begin
  GetCursorPos(result);
  result := ScreenToClient(result);
end; { ClientCursorPos }

function TJvTFGantt.ValidMouseAtDesignTime: boolean;
begin
  result := False;
end; { ValidMouseAtDesignTime }

procedure TJvTFGantt.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  if ValidMouseAtDesignTime then
    Msg.Result := 1 { Allow design-time mouse hits to get through if Alt key is down. }
  else
    Msg.Result := 0;
end; { CMDesignHitTest }

procedure TJvTFGantt.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustComponentHeightBasedOnFontChange;
end; { CMFontChanged }

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
end; { AdjustComponentHeightBasedOnFontChange }

procedure TJvTFGantt.Loaded;
begin
  inherited Loaded;
  AlignScrollBars;
end; { Loaded }

destructor TJvTFGantt.Destroy;
begin
  PaintBuffer.Free;
  FMajorScale.Free;
  FMinorScale.Free;
  FVScrollBar.Free;
  FHScrollBar.Free;

  if assigned(FCustomGlyphs) then
  begin
    FCustomGlyphs.Free;
    FCustomGlyphs := nil;
  end;
  inherited Destroy;
end; { Destroy }

constructor TJvTFGantt.Create(AOwner: TComponent);
{ Creates an object of type TJvTFGantt, and initializes properties. }
begin
  inherited Create(AOwner);
  PaintBuffer := TBitmap.Create;
  FCustomGlyphs := TBitmap.Create;
  FVisibleScrollBars := [vsbHorz, vsbVert];

  FVScrollBar := TJvTFGanttScrollBar.Create(Self);
  with FVScrollBar do
  begin
    Kind := sbVertical;
    TabStop := False;
    Anchors := [];
    Parent := Self;
    Visible := true;
//      OnScroll := ScrollBarScroll;
  end;

  FHScrollBar := TJvTFGanttScrollBar.Create(Self);
  with FHScrollBar do
  begin
    Kind := sbHorizontal;
    TabStop := False;
    Anchors := [];
    Parent := Self;
    Visible := true;
//      OnScroll := ScrollBarScroll;
  end;


  FMajorScale := TJvTFGanttScaleFormat.Create;
  FMajorScale.Scale := ugsMonth;
  FMajorScale.Format := 'mmmm';
  FMinorScale := TJvTFGanttScaleFormat.Create;
  FMinorScale.Scale := ugsDay;
  FMinorScale.Format := 'dd';

  PrepareAllBitmaps;
end; { Create }



{ TJvTFGanttScaleFormat }

destructor TJvTFGanttScaleFormat.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

constructor TJvTFGanttScaleFormat.Create;
begin
  FFont := TFont.Create;
end;

function TJvTFGanttScaleFormat.GetFont: TFont;
begin
  result := FFont;
end;

procedure TJvTFGanttScaleFormat.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;


{ TJvTFGanttScrollBar }

constructor TJvTFGanttScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  // If we set the csNoDesignVisible flag then visibility at design time
  //  is controled by the Visible property, which is exactly what we want.
  ControlStyle := ControlStyle + [csNoDesignVisible];
  ParentCtl3D := False;
  Ctl3D := False;
end;

procedure TJvTFGanttScrollBar.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;

procedure TJvTFGanttScrollBar.CreateWnd;
begin
  inherited;
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
    cbsize := SizeOf(Info);
    fmask := SIF_PAGE;
    nPage := LargeChange;
  end;
  SetScrollInfo(Handle, SB_CTL, Info, True);
end;


initialization
finalization
end.

