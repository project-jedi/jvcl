{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvArrowBtn.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  The TJvArrowButton component implements an arrow button like
  the ones used in Office 97: one button and one arrow with
  separate events.

Known Issues:


-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvArrowButton;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Controls, Forms, Graphics, Buttons, CommCtrl, Menus,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QTypes, QControls, QForms, QGraphics, QButtons, QImgList, QMenus, Types,
  QWindows,
  {$ENDIF VisualCLX}
  JvComponent, JvTypes;

type
  TJvArrowButton = class(TJvGraphicControl)
  private
    FGroupIndex: Integer;
    FGlyph: Pointer;
    FDown: Boolean;
    FArrowClick: Boolean;
    FPressBoth: Boolean;
    FArrowWidth: Integer;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FFillFont: TFont;
    FMargin: Integer;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    FPopup: TPopupMenu;
    FOnDrop: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure SetArrowWidth(Value: Integer);
    procedure SetFillFont(Value: TFont);
    procedure UpdateTracking;
    procedure CMButtonPressed(var Msg: TJvCMButtonPressed); message CM_BUTTONPRESSED;
    {$IFDEF VCL}
    procedure WMLButtonDblClk(var Msg: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    {$ENDIF VCL}
  protected
    FState: TButtonState;
    {$IFDEF VCL}
    function GetPalette: HPALETTE; override;
    {$ENDIF VCL}
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;

    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 13;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property DropDown: TPopupMenu read FPopup write FPopup;
    property Caption;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property FillFont: TFont read FFillFont write SetFillFont;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentFont default True;
    property ParentShowHint;
    property PressBoth: Boolean read FPressBoth write FPressBoth default True;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Visible;
    property OnDrop: TNotifyEvent read FOnDrop write FOnDrop;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  {$IFDEF VCL}
  Consts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QConsts,
  {$ENDIF VisualCLX}
  JvConsts, JvThemes;

type
  TGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
      {$IFDEF VisualCLX} override; {$ENDIF VisualCLX}
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class(TObject)
  private
    FGlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(var List: TGlyphList);
    function Empty: Boolean;
  end;

  TButtonGlyph = class(TObject)
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array [TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{$IFDEF VCL}
function DrawText(Canvas: TCanvas; Caption: TCaption; var R: TRect;
  Flags: Integer): Integer;
begin
  Result := Windows.DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, Flags);
end;
{$ENDIF VCL}

procedure DrawLine(Canvas: TCanvas; X, Y, X2, Y2: Integer);
begin
  Canvas.MoveTo(X, Y);
  Canvas.LineTo(X2, Y2);
end;

// (rom) best move to JCL

procedure GrayedBitmap(Bmp: TBitmap);
var
  I, J, W, H: Integer;
  ColT: TColor;
  Col: TColor;
begin
  if Bmp.Empty then
    Exit;

  W := Bmp.Width;
  H := Bmp.Height;
  ColT := Bmp.Canvas.Pixels[0, 0];

  // (rom) speed up by using Scanline
  for I := 0 to W do
    for J := 0 to H do
    begin
      Col := Bmp.Canvas.Pixels[I, J];
      if (Col <> clWhite) and (Col <> ColT) then
        Col := clBlack
      else
        Col := ColT;
      Bmp.Canvas.Pixels[I, J] := Col;
    end;
end;

//=== TGlyphList =============================================================

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  FUsed := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  FUsed.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := FUsed.OpenBit;
  if Result >= FUsed.Size then
  begin
    Result := inherited Add(nil, nil);
    FUsed.Size := Result + 1;
  end;
  FUsed[Result] := True;
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TGlyphList.Delete(Index: Integer);
begin
  if FUsed[Index] then
  begin
    Dec(FCount);
    FUsed[Index] := False;
  end;
end;

//=== TGlyphCache ============================================================

constructor TGlyphCache.Create;
begin
  inherited Create;
  FGlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  FGlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := FGlyphLists.Count - 1 downto 0 do
  begin
    Result := FGlyphLists[I];
    if (AWidth = Result.Width) and (AHeight = Result.Height) then
      Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  FGlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(var List: TGlyphList);
begin
  if (List <> nil) and (List.Count = 0) then
  begin
    FGlyphLists.Remove(List);
    FreeAndNil(List);
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

var
  GlyphCache: TGlyphCache = nil;
  Pattern: TBitmap = nil;
  ButtonCount: Integer = 0;

//=== TButtonGlyph ===========================================================

procedure CreateBrushPattern;
var
  X, Y: Integer;
begin
  Pattern.Free; // (rom) just to be sure
  Pattern := TBitmap.Create;
  Pattern.Width := 8;
  Pattern.Height := 8;
  with Pattern.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, Pattern.Width, Pattern.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then { toggles between even/odd pixles }
          Pixels[X, Y] := clBtnHighlight; { on even/odd rows }
  end;
end;

constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then
    GlyphCache := TGlyphCache.Create;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
    FreeAndNil(GlyphCache);
  inherited Destroy;
end;

procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(TButtonState) to High(TButtonState) do
  begin
    if FIndexs[I] <> -1 then
      FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then
        Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

function TButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then
    State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then
    Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then
    Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    {$IFDEF VCL}
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    {$ENDIF VCL}
    I := State;
    if Ord(I) >= NumGlyphs then
      I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown, bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            {$IFDEF VCL}
            DDB.HandleType := bmDDB;
            {$ENDIF VCL}
            if NumGlyphs > 1 then
              with TmpImage.Canvas do
              begin { Change white & gray to clBtnHighlight and clBtnShadow }
                CopyRect(IRect, DDB.Canvas, ORect);
                MonoBmp.Monochrome := True;
                MonoBmp.Width := IWidth;
                MonoBmp.Height := IHeight;

                { Convert white to clBtnHighlight }
                DDB.Canvas.Brush.Color := clWhite;
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnHighlight;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

                { Convert gray to clBtnShadow }
                DDB.Canvas.Brush.Color := clGray;
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnShadow;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

                { Convert transparent color to clBtnFace }
                DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnFace;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                GrayedBitmap(MonoBmp);
                {$IFDEF VCL}
                HandleType := bmDDB;
                {$ENDIF VCL}
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;

          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
begin
  if (FOriginal = nil) or (FOriginal.Width = 0) or (FOriginal.Height = 0) then
    Exit;
  Index := CreateButtonGlyph(State);
  with GlyphPos do
    {$IFDEF VCL}
    if Transparent or (State = bsExclusive) then
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        clNone, clNone, ILD_Transparent)
    else
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        ColorToRGB(clBtnFace), clNone, ILD_Normal);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    // (ahuser) transparent not really supported under CLX
    if Transparent or (State = bsExclusive) then
    begin
      FGlyphList.Draw(Canvas, X, Y, Index, itImage, True);
    end
    else
      FGlyphList.Draw(Canvas, X, Y, Index, itImage, True);
    {$ENDIF VisualCLX}
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState);
var
  S: string;
begin
  S := Caption;
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Canvas, S, TextBounds, 0);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Canvas, S, TextBounds, 0);
    end
    else
      DrawText(Canvas, S, TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
  S: string;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
  else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    S := Caption;
    DrawText(Canvas, S, TextBounds, DT_CALCRECT);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.X);
end;

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);
  DrawButtonText(Canvas, Caption, Result, State);
end;

//=== TJvArrowButton =========================================================

constructor TJvArrowButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 42, 25);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  {$IFDEF VCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF VCL}
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  FFillFont := TFont.Create;
  FFillFont.Assign(Font);
  FAllowAllUp := False;
  FArrowWidth := 13;
  FGroupIndex := 0;
  ParentFont := True;
  FDown := False;
  FFlat := False;
  FLayout := blGlyphLeft;
  FMargin := -1;
  FSpacing := 4;
  FPressBoth := True;
  Inc(ButtonCount);
end;

destructor TJvArrowButton.Destroy;
begin
  TButtonGlyph(FGlyph).Free;
  FFillFont.Free;
  Dec(ButtonCount);
  if ButtonCount = 0 then
    FreeAndNil(Pattern);
  inherited Destroy;
end;

procedure TJvArrowButton.Paint;
const
  DownStyles: array [Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array [Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  DivX, DivY: Integer;
  Push: Boolean;
begin
  if not Enabled then
    FState := bsDisabled
  else
  if FState = bsDisabled then
  begin
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  end;
  if FMouseInControl then
    Canvas.Font := FFillFont
  else
    Canvas.Font := Self.Font;
  PaintRect := Rect(0, 0, Width - FArrowWidth, Height);

  if FArrowClick and not FDown then
    FState := bsUp;

  if not FFlat then
  begin
    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if (FState in [bsDown, bsExclusive]) then
      DrawFlags := DrawFlags or DFCS_PUSHED;
    if IsMouseOver(Self) then
      DrawFlags := DrawFlags or DFCS_HOT;
    DrawThemedFrameControl(Self, Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
  end
  else
  begin
    if (FState in [bsDown, bsExclusive]) or
      (FMouseInControl and (FState <> bsDisabled)) or
      (csDesigning in ComponentState) then
      DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
        FillStyles[FFlat] or BF_RECT);
    InflateRect(PaintRect, -1, -1);
  end;

  if FState in [bsDown, bsExclusive] then
  begin
    if (FState = bsExclusive) and (not FFlat or not FMouseInControl) then
    begin
      if Pattern = nil then
        CreateBrushPattern;
      Canvas.Brush.Bitmap := Pattern;
      Canvas.FillRect(PaintRect);
    end;
    Offset.X := 1;
    Offset.Y := 1;
  end
  else
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;
  { draw image: }
  TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
    FSpacing, FState, FFlat);

  { calculate were to put arrow part }
  PaintRect := Rect(Width - FArrowWidth, 0, Width, Height);
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
    Dec(PaintRect.Left);
  {$ENDIF JVCLThemesEnabled}
  Push := FArrowClick or (FPressBoth and (FState in [bsDown, bsExclusive]));
  if Push then
  begin
    Offset.X := 1;
    Offset.Y := 1;
  end
  else
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;

  if not FFlat then
  begin
    DrawFlags := DFCS_BUTTONPUSH; // or DFCS_ADJUSTRECT;
    if Push then
      DrawFlags := DrawFlags or DFCS_PUSHED;
    if IsMouseOver(Self) then
      DrawFlags := DrawFlags or DFCS_HOT;
    DrawThemedFrameControl(Self, Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
  end
  else
  if FMouseInControl and Enabled or (csDesigning in ComponentState) then
    DrawEdge(Canvas.Handle, PaintRect, DownStyles[Push],
      FillStyles[FFlat] or BF_RECT);
  { find middle pixel }
  with PaintRect do
  begin
    DivX := Right - Left;
    DivX := DivX div 2;
    DivY := Bottom - Top;
    DivY := DivY div 2;
    Bottom := Bottom - (DivY + DivX div 2) + 1;
    Top := Top + (DivY + DivX div 2) + 1;
    Left := Left + (DivX div 2);
    Right := (Right - DivX div 2);
  end;

  if not FFlat then
    Dec(Offset.X);
  OffsetRect(PaintRect, Offset.X, Offset.Y);

  if Enabled then
    Canvas.Pen.Color := clBlack
  else
    Canvas.Pen.Color := clBtnShadow;

  { Draw arrow }
  while PaintRect.Left < PaintRect.Right + 1 do
  begin
    DrawLine(Canvas, PaintRect.Left, PaintRect.Bottom, PaintRect.Right, PaintRect.Bottom);
    InflateRect(PaintRect, -1, 1);
  end;
end;

procedure TJvArrowButton.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat then
    if Enabled then
    begin
      GetCursorPos(P);
      FMouseInControl := not (FindDragTarget(P, True) = Self);
      {$IFDEF VCL}
      if FMouseInControl then
        Perform(CM_MOUSELEAVE, 0, 0)
      else
        Perform(CM_MOUSEENTER, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      if FMouseInControl then
        MouseLeave(Self)
      else
        MouseEnter(Self);
      {$ENDIF VisualCLX}
    end;
end;

procedure TJvArrowButton.Loaded;
var
  State: TButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

procedure TJvArrowButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Pnt: TPoint;
  {$IFDEF VCL}
  Msg: TMsg;
  {$ENDIF VCL}
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Enabled then
    Exit;
  FArrowClick := (X >= Width - FArrowWidth) and (X <= Width) and (Y >= 0) and (Y <= Height);

  if Button = mbLeft then
  begin
    if not FDown then
      FState := bsDown
    else
      FState := bsExclusive;
    Repaint; // Invalidate;
  end;

  if Assigned(FPopup) and FArrowClick then
  begin
    Pnt := ClientToScreen(Point(0, Height));
    FPopup.Popup(Pnt.X, Pnt.Y);
    {$IFDEF VCL}
    while PeekMessage(Msg, HWND_DESKTOP, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      {nothing};
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    repeat
      Application.ProcessMessages
    until IsWindowVisible(FPopup.Handle) = False;
    {$ENDIF VisualCLX}
  end;

  if FArrowClick then
    if Assigned(FOnDrop) then
      FOnDrop(Self);
  FArrowClick := False;
  Repaint;
end;

procedure TJvArrowButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if not Enabled then
  begin
    FState := bsUp;
    Repaint;
  end;

  DoClick := (X >= 0) and (X <= Width - FArrowWidth) and (Y >= 0) and (Y <= Height);

  if FGroupIndex = 0 then
  begin
    { Redraw face in case mouse is captured }
    FState := bsUp;
    FMouseInControl := False;
    if DoClick and not (FState in [bsExclusive, bsDown]) then
      Invalidate;
  end
  else
  if DoClick then
  begin
    SetDown(not FDown);
    if FDown then
      Repaint;
  end
  else
  begin
    if FDown then
      FState := bsExclusive;
    Repaint;
  end;
  if DoClick then
    Click;
  UpdateTracking;
  Repaint;
end;

procedure TJvArrowButton.Click;
begin
  inherited Click;
end;

{$IFDEF VCL}
function TJvArrowButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;
{$ENDIF VCL}

function TJvArrowButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

procedure TJvArrowButton.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

function TJvArrowButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TJvArrowButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else
  if Value > 4 then
    Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvArrowButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvArrowButton.UpdateExclusive;
var
  Msg: TJvCMButtonPressed;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.Index := FGroupIndex;
    Msg.Control := Self;
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvArrowButton.SetDown(Value: Boolean);
begin
  if (FGroupIndex = 0) then
    Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then
      Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then
        Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then
      UpdateExclusive;
  end;
end;

procedure TJvArrowButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TJvArrowButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TJvArrowButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJvArrowButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvArrowButton.SetArrowWidth(Value: Integer);
begin
  if FArrowWidth <> Value then
  begin
    FArrowWidth := Value;
    Repaint;
  end;
end;

procedure TJvArrowButton.SetFillFont(Value: TFont);
begin
  FFillFont.Assign(Value);
  Repaint;
end;

procedure TJvArrowButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvArrowButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TJvArrowButton.EnabledChanged;
const
  NewState: array [Boolean] of TButtonState = (bsDisabled, bsUp);
begin
  inherited EnabledChanged;
  TButtonGlyph(FGlyph).CreateButtonGlyph(NewState[Enabled]);
  UpdateTracking;
  Repaint;
end;

procedure TJvArrowButton.CMButtonPressed(var Msg: TJvCMButtonPressed);
var
  Sender: TJvArrowButton;
begin
  if Msg.Index = FGroupIndex then
  begin
    Sender := TJvArrowButton(Msg.Control);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

function TJvArrowButton.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := IsAccel(Key, Caption) and Enabled and (ssAlt in Shift);
  if Result then
    Click
  else
    Result := inherited WantKey(Key, Shift, KeyText);
end;

procedure TJvArrowButton.FontChanged;
begin
  inherited FontChanged;
  Invalidate;
end;

procedure TJvArrowButton.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

{$IFDEF VCL}
procedure TJvArrowButton.WMLButtonDblClk(var Msg: TWMLButtonDown);
begin
  inherited;
  if FDown then
    DblClick;
end;

procedure TJvArrowButton.CMSysColorChange(var Msg: TMessage);
begin
  with TButtonGlyph(FGlyph) do
  begin
    Invalidate;
    CreateButtonGlyph(FState);
  end;
end;
{$ENDIF VCL}

procedure TJvArrowButton.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if FFlat and not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    Repaint;
  end;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled and Enabled and not Flat then
    Repaint;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvArrowButton.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if FFlat and FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled and Enabled and not Flat then
    Invalidate;
  {$ENDIF JVCLThemesEnabled}
end;

end.

