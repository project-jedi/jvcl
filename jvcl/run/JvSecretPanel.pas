{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSecretPanel.pas, released on 2003-10-19.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software
  Peter Thornqvist [peter3 at sourceforge dot net]

Changes:
2003-10-19:
  * Moved TJvSecretPanel from JvxCtrls to this unit

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSecretPanel;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, ExtCtrls, Forms,
  JvTimer, JvComponent, JvTypes;

type
  TGlyphLayout = (glGlyphLeft, glGlyphRight, glGlyphTop, glGlyphBottom);
  TScrollDirection = (sdVertical, sdHorizontal);
  TPanelDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect) of object;

  TJvSecretPanel = class(TJvCustomPanel)
  private
    FActive: Boolean;
    FAlignment: TAlignment;
    FLines: TStringList;
    FCycled: Boolean;
    FScrollCnt: Integer;
    FMaxScroll: Integer;
    FTxtDivider: Byte;
    FFirstLine: Integer;
    FTimer: TJvTimer;
    FTxtRect: TRect;
    FPaintRect: TRect;
    FGlyphOrigin: TPoint;
    FMemoryImage: TBitmap;
    FGlyph: TBitmap;
    FHiddenList: TList;
    FTextStyle: TPanelBevel;
    FDirection: TScrollDirection;
    FGlyphLayout: TGlyphLayout;
    FOnPaintClient: TPanelDrawEvent;
    FOnStartPlay: TNotifyEvent;
    FOnStopPlay: TNotifyEvent;
    FAsyncDrawing: Boolean;
    procedure SetAsyncDrawing(Value: Boolean);
    function GetInflateWidth: Integer;
    function GetInterval: Cardinal;
    function GetLines: TStrings;
    procedure SetInterval(Value: Cardinal);
    procedure SetGlyph(Value: TBitmap);
    procedure SetLines(Value: TStrings);
    procedure SetActive(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetGlyphLayout(Value: TGlyphLayout);
    procedure SetTextStyle(Value: TPanelBevel);
    procedure SetDirection(Value: TScrollDirection);
    procedure RecalcDrawRect;
    procedure PaintGlyph;
    procedure PaintText;
    procedure UpdateMemoryImage;
    procedure GlyphChanged(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
  protected
    procedure FontChanged; override;
    procedure ColorChanged; override;
    procedure DoBoundsChanged; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Paint; override;
    procedure PaintClient(Canvas: TCanvas; Rect: TRect); virtual;
    procedure TimerExpired(Sender: TObject); virtual;
    procedure StartPlay; dynamic;
    procedure StopPlay; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play;
    procedure Stop;
    property Canvas;
  published
    property AsyncDrawing: Boolean read FAsyncDrawing write SetAsyncDrawing default True;
    property Active: Boolean read FActive write SetActive default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Cycled: Boolean read FCycled write FCycled default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphLayout: TGlyphLayout read FGlyphLayout write SetGlyphLayout
      default glGlyphLeft;
    property Interval: Cardinal read GetInterval write SetInterval default 30;
    property Lines: TStrings read GetLines write SetLines;
    property ScrollDirection: TScrollDirection read FDirection write SetDirection
      default sdVertical;
    property TextStyle: TPanelBevel read FTextStyle write SetTextStyle default bvNone;
    property Anchors;
    {$IFDEF VCL}
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property DragCursor;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property Constraints;
    property Align;
    property BevelInner;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragMode;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnPaintClient: TPanelDrawEvent read FOnPaintClient write FOnPaintClient;
    property OnStartPlay: TNotifyEvent read FOnStartPlay write FOnStartPlay;
    property OnStopPlay: TNotifyEvent read FOnStopPlay write FOnStopPlay;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnResize;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF VCL}
  CommCtrl,
  {$ENDIF VCL}
  Consts, SysUtils, Math, ActnList,
  JvJCLUtils, JvJVCLUtils, JvThemes, JvConsts;

const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
//  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK); make Delphi 5 compiler happy // andreas

//=== { TJvSecretPanel } =====================================================

constructor TJvSecretPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollCnt := 0;
  FAlignment := taCenter;
  FActive := False;
  FTxtDivider := 1;
  FGlyphLayout := glGlyphLeft;
  ControlStyle := ControlStyle - [csSetCaption];
  BevelOuter := bvLowered;
  FTextStyle := bvNone;
  FLines := TStringList.Create;
  FLines.OnChange := LinesChanged;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  FHiddenList := TList.Create;
  FTimer := TJvTimer.Create(Self);
  with FTimer do
  begin
    Enabled := False;
    OnTimer := TimerExpired;
    Interval := 30;
    SyncEvent := False;
    FAsyncDrawing := True;
  end;
end;

destructor TJvSecretPanel.Destroy;
begin
  SetActive(False);
  FGlyph.OnChange := nil;
  FGlyph.Free;
  FLines.OnChange := nil;
  FLines.Free;
  FHiddenList.Free;
  inherited Destroy;
end;

procedure TJvSecretPanel.GlyphChanged(Sender: TObject);
begin
  if Active then
  begin
    UpdateMemoryImage;
    Invalidate;
  end;
end;

procedure TJvSecretPanel.LinesChanged(Sender: TObject);
begin
  if Active then
  begin
    FScrollCnt := 0;
    UpdateMemoryImage;
    Invalidate;
  end;
end;

procedure TJvSecretPanel.FontChanged;
begin
  inherited FontChanged;
  if Active then
    UpdateMemoryImage;
end;

procedure TJvSecretPanel.ColorChanged;
begin
  inherited ColorChanged;
  if Active then
    UpdateMemoryImage;
end;

procedure TJvSecretPanel.DoBoundsChanged;
begin
  inherited DoBoundsChanged;
  if Active then
  begin
    UpdateMemoryImage;
    Invalidate;
  end;
end;

procedure TJvSecretPanel.SetAsyncDrawing(Value: Boolean);
begin
  if FAsyncDrawing <> Value then
  begin
    FTimer.SyncEvent := not Value;
    FAsyncDrawing := Value;
  end;
end;

procedure TJvSecretPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if (AControl = nil) and Active then
    UpdateMemoryImage;
end;

function TJvSecretPanel.GetInflateWidth: Integer;
begin
  Result := BorderWidth;
  if BevelOuter <> bvNone then
    Inc(Result, BevelWidth);
  if BevelInner <> bvNone then
    Inc(Result, BevelWidth);
end;

procedure TJvSecretPanel.RecalcDrawRect;
const
  MinOffset = 3;
var
  InflateWidth: Integer;
  LastLine: Integer;
begin
  FTxtRect := GetClientRect;
  FPaintRect := FTxtRect;
  InflateWidth := GetInflateWidth;
  InflateRect(FPaintRect, -InflateWidth, -InflateWidth);
  Inc(InflateWidth, MinOffset);
  InflateRect(FTxtRect, -InflateWidth, -InflateWidth);
  with FGlyphOrigin do
  begin
    case FGlyphLayout of
      glGlyphLeft:
        begin
          X := FTxtRect.Left;
          Y := (FTxtRect.Bottom + FTxtRect.Top - Glyph.Height) div 2;
          if Y < FTxtRect.Top then
            Y := FTxtRect.Top;
          if Glyph.Width > 0 then
          begin
            Inc(X, MinOffset);
            FTxtRect.Left := X + Glyph.Width + InflateWidth;
          end;
        end;
      glGlyphRight:
        begin
          Y := (FTxtRect.Bottom + FTxtRect.Top - Glyph.Height) div 2;
          if Y < FTxtRect.Top then
            Y := FTxtRect.Top;
          X := FTxtRect.Right - Glyph.Width;
          if Glyph.Width > 0 then
          begin
            Dec(X, MinOffset);
            if X < FTxtRect.Left then
              X := FTxtRect.Left;
            FTxtRect.Right := X - InflateWidth;
          end;
        end;
      glGlyphTop:
        begin
          Y := FTxtRect.Top;
          X := (FTxtRect.Right + FTxtRect.Left - Glyph.Width) div 2;
          if X < FTxtRect.Left then
            X := FTxtRect.Left;
          if Glyph.Height > 0 then
          begin
            Inc(Y, MinOffset);
            FTxtRect.Top := Y + Glyph.Height + (InflateWidth + MinOffset);
          end;
        end;
      glGlyphBottom:
        begin
          X := (FTxtRect.Right + FTxtRect.Left - Glyph.Width) div 2;
          if X < FTxtRect.Left then
            X := FTxtRect.Left;
          Y := FTxtRect.Bottom - Glyph.Height;
          if Glyph.Height > 0 then
          begin
            Dec(Y, MinOffset);
            if Y < FTxtRect.Top then
              Y := FTxtRect.Top;
            FTxtRect.Bottom := Y - (InflateWidth + MinOffset);
          end;
        end;
    end;
  end;
  if FDirection = sdHorizontal then
  begin
    LastLine := Lines.Count - 1;
    while (LastLine >= 0) and (Trim(Lines[LastLine]) = '') do
      Dec(LastLine);
    InflateWidth := RectHeight(FTxtRect) -
      (LastLine + 1 - FFirstLine) * FTxtDivider;
    if InflateWidth > 0 then
      InflateRect(FTxtRect, 0, -InflateWidth div 2);
  end;
  with FTxtRect do
    if (Left >= Right) or (Top >= Bottom) then
      FTxtRect := Rect(0, 0, 0, 0);
end;

procedure TJvSecretPanel.PaintGlyph;
begin
  if not FGlyph.Empty then
  begin
    RecalcDrawRect;
    DrawBitmapTransparent(Canvas, FGlyphOrigin.X, FGlyphOrigin.Y,
      FGlyph, FGlyph.TransparentColor and not PaletteMask);
  end;
end;

procedure TJvSecretPanel.PaintText;
var
  STmp: array [0..255] of Char;
  R: TRect;
  I: Integer;
  Flags: Longint;
begin
  if (Lines.Count = 0) or IsRectEmpty(FTxtRect) or not HandleAllocated then
    Exit;
  FMemoryImage.Canvas.Lock;
  try
    with FMemoryImage.Canvas do
    begin
      I := SaveDC(Handle);
      try
        with FTxtRect do
          MoveWindowOrg(Handle, -Left, -Top);
        Brush.Color := Self.Color;
        PaintClient(FMemoryImage.Canvas, FPaintRect);
      finally
        RestoreDC(Handle, I);
        SetBkMode(Handle, Transparent);
      end;
    end;
    R := Bounds(0, 0, RectWidth(FTxtRect), RectHeight(FTxtRect));
    if FDirection = sdHorizontal then
    begin
      if IsRightToLeft then
      begin
        R.Right := R.Left + FScrollCnt;
        R.Left := R.Right - (FMaxScroll - RectWidth(FTxtRect));
      end
      else
      begin
        R.Left := R.Right - FScrollCnt;
        R.Right := R.Left + (FMaxScroll - RectWidth(FTxtRect));
      end;
    end
    else
    begin { sdVertical }
      R.Top := R.Bottom - FScrollCnt;
    end;
    R.Bottom := R.Top + FTxtDivider;
    Flags := DT_EXPANDTABS or Alignments[FAlignment] or DT_SINGLELINE or
      DT_NOCLIP or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);
    for I := FFirstLine to Lines.Count do
    begin
      if I = Lines.Count then
        StrCopy(STmp, ' ')
      else
        StrPLCopy(STmp, Lines[I], SizeOf(STmp) - 1);
      if R.Top >= RectHeight(FTxtRect) then
        Break
      else
      if R.Bottom > 0 then
      begin
        if FTextStyle <> bvNone then
        begin
          FMemoryImage.Canvas.Font.Color := clBtnHighlight;
          case FTextStyle of
            bvLowered:
              begin
                OffsetRect(R, 1, 1);
                DrawText(FMemoryImage.Canvas, STmp, -1, R, Flags);
                OffsetRect(R, -1, -1);
              end;
            bvRaised:
              begin
                OffsetRect(R, -1, -1);
                DrawText(FMemoryImage.Canvas, STmp, -1, R, Flags);
                OffsetRect(R, 1, 1);
              end;
          end;
          FMemoryImage.Canvas.Font.Color := Self.Font.Color;
          SetBkMode(FMemoryImage.Canvas.Handle, Transparent);
        end;
        DrawText(FMemoryImage.Canvas, STmp, -1, R, Flags);
      end;
      OffsetRect(R, 0, FTxtDivider);
    end;
    Canvas.Lock;
    try
      BitBlt(Canvas.Handle, FTxtRect.Left, FTxtRect.Top, FMemoryImage.Width,
        FMemoryImage.Height, FMemoryImage.Canvas.Handle, 0, 0, SRCCOPY);
      ValidateRect(Handle, @FTxtRect);
    finally
      Canvas.Unlock;
    end;
  finally
    FMemoryImage.Canvas.Unlock;
  end;
end;

procedure TJvSecretPanel.PaintClient(Canvas: TCanvas; Rect: TRect);
begin
  if Assigned(FOnPaintClient) then
    FOnPaintClient(Self, Canvas, Rect)
  else
    Canvas.FillRect(Rect);
end;

procedure TJvSecretPanel.Paint;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  SaveIndex: Integer;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then
      BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  SaveIndex := SaveDC(Canvas.Handle);
  try
    with Rect do
      IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    Canvas.Brush.Color := Self.Color;
    PaintClient(Canvas, Rect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
  if Active then
  begin
    PaintGlyph;
    {PaintText;}
  end;
end;

procedure TJvSecretPanel.StartPlay;
begin
  if Assigned(FOnStartPlay) then
    FOnStartPlay(Self);
end;

procedure TJvSecretPanel.StopPlay;
begin
  if Assigned(FOnStopPlay) then
    FOnStopPlay(Self);
end;

procedure TJvSecretPanel.TimerExpired(Sender: TObject);
begin
  if FScrollCnt < FMaxScroll then
  begin
    Inc(FScrollCnt);
    if Assigned(FMemoryImage) then
      PaintText;
  end
  else
  if Cycled then
  begin
    FScrollCnt := 0;
    if Assigned(FMemoryImage) then
      PaintText;
  end
  else
  begin
    FTimer.Synchronize(Stop);
  end;
end;

procedure TJvSecretPanel.UpdateMemoryImage;
var
  Metrics: TTextMetric;
  I: Integer;
begin
  if FMemoryImage = nil then
    FMemoryImage := TBitmap.Create;
  FMemoryImage.Canvas.Lock;
  try
    FFirstLine := 0;
    while (FFirstLine < Lines.Count) and (Trim(Lines[FFirstLine]) = '') do
      Inc(FFirstLine);
    Canvas.Font := Self.Font;
    GetTextMetrics(Canvas.Handle, Metrics);
    FTxtDivider := Metrics.tmHeight + Metrics.tmExternalLeading;
    if FTextStyle <> bvNone then
      Inc(FTxtDivider);
    RecalcDrawRect;
    if FDirection = sdHorizontal then
    begin
      FMaxScroll := 0;
      for I := FFirstLine to Lines.Count - 1 do
        FMaxScroll := Max(FMaxScroll, Canvas.TextWidth(Lines[I]));
      Inc(FMaxScroll, RectWidth(FTxtRect));
    end
    else
    begin { sdVertical }
      FMaxScroll := ((Lines.Count - FFirstLine) * FTxtDivider) +
        RectHeight(FTxtRect);
    end;
    FMemoryImage.Width := RectWidth(FTxtRect);
    FMemoryImage.Height := RectHeight(FTxtRect);
    with FMemoryImage.Canvas do
    begin
      Font := Self.Font;
      Brush.Color := Self.Color;
      SetBkMode(Handle, Transparent);
    end;
  finally
    FMemoryImage.Canvas.Unlock;
  end;
end;

function TJvSecretPanel.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TJvSecretPanel.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TJvSecretPanel.Play;
begin
  SetActive(True);
end;

procedure TJvSecretPanel.Stop;
begin
  SetActive(False);
end;

procedure TJvSecretPanel.SetActive(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
    begin
      FScrollCnt := 0;
      UpdateMemoryImage;
      try
        FTimer.Enabled := True;
        StartPlay;
      except
        FActive := False;
        FTimer.Enabled := False;
        raise;
      end;
    end
    else
    begin
      FMemoryImage.Canvas.Lock;
      { ensure that canvas is locked before timer is disabled }
      FTimer.Enabled := False;
      FScrollCnt := 0;
      FMemoryImage.Free;
      FMemoryImage := nil;
      StopPlay;
      if (csDesigning in ComponentState) and
          not (csDestroying in ComponentState) then
        {$IFDEF VCL}
        ValidParentForm(Self).Designer.Modified;
        {$ENDIF VCL}
        {$IFDEF VisualCLX}
        ValidParentForm(Self).DesignerHook.Modified;
        {$ENDIF VisualCLX}
    end;
    if not (csDestroying in ComponentState) then
      for I := 0 to Pred(ControlCount) do
      begin
        if FActive then
        begin
          if Controls[I].Visible then
            FHiddenList.Add(Controls[I]);
          if not (csDesigning in ComponentState) then
            Controls[I].Visible := False;
        end
        else
        if FHiddenList.IndexOf(Controls[I]) >= 0 then
        begin
          Controls[I].Visible := True;
          Controls[I].Invalidate;
          if csDesigning in ComponentState then
            Controls[I].Update;
        end;
      end;
    if not FActive then
      FHiddenList.Clear;
    Invalidate;
  end;
end;

procedure TJvSecretPanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if Active then
      Invalidate;
  end;
end;

procedure TJvSecretPanel.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TJvSecretPanel.SetDirection(Value: TScrollDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    if FActive then
    begin
      FScrollCnt := 0;
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

procedure TJvSecretPanel.SetTextStyle(Value: TPanelBevel);
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    if FActive then
    begin
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

procedure TJvSecretPanel.SetGlyphLayout(Value: TGlyphLayout);
begin
  if FGlyphLayout <> Value then
  begin
    FGlyphLayout := Value;
    if FActive then
    begin
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

function TJvSecretPanel.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TJvSecretPanel.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
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

