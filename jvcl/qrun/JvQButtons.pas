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

The Original Code is: JvButtons.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

       components  : TJvaCaptionButton,
                     TJvaColorButton,
                     TJvNoFrameButton,
                     TJvHTButton
       description : Buttons

Known Issues:
Maciej Kaczkowski:
  [X] Height of JvHTComboBox - on design time you cannot use mouse for resize
  [X] alignment not work correctly on JvHTButtonGlyph
  [X] not tested on BCB & Kylix

Create label with caption:
<ALIGN CENTER>Item 1 <b>bold</b> <u>underline</u><br><ALIGN RIGHT><FONT COLOR="clRed">red <FONT COLOR="clgreen">green <FONT COLOR="clblue">blue</i><br><ALIGN LEFT><FONT COLOR="clTeal">Item 2 <i>italic ITALIC</i> <s>strikeout STRIKEOUT </s><hr><br><ALIGN CENTER><FONT COLOR="clRed" BGCOLOR="clYellow">red with yellow background</FONT><FONT COLOR="clwhite"> white <FONT COLOR="clnavy"><b><i>navy</i></b>

Some information about coding:
[?] If you want use few times function <ALIGN> you must use before next <ALIGN>
    function <BR>
[?] After <HR> must be <BR>

Changes:
========
Maciej Kaczkowski:
  [+] <BR> - new line
  [+] <HR> - horizontal line
  [+] <S> and </S> - StrikeOut
  [+] Multiline for JvHTListBox, JvHTComboBox
      TJvHTButton
  [+] You can change Height of JvHTComboBox
  [+] Tags: &amp; &quot; &reg; &copy; &trade;
      &nbsp; &lt; &gt;
  [+] <ALIGN [CENTER, LEFT, RIGHT]>
  [*] <C:color> was changed to ex.:
      <FONT COLOR="clRed" BGCOLOR="clWhite">
      </FONT>
  [*] procedure ItemHtDrawEx - rewrited
  [*] function ItemHtPlain - optimized
-----------------------------------------------------------------------------}
// $Id$

unit JvQButtons;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages,
  Classes, QGraphics, QControls, QForms, QButtons, 
  QImgList,  
  JvQJCLUtils, JvQComponent, JvQExButtons;

type
  { VCL Buttons unit does not publish TJvButtonGlyph class,
    so we do it for other programers (Delphi 3 version) }
  TJvButtonGlyph = class(TObject)
  private
    FGlyphList: TImageList;
    FIndexs: array [TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FColor: TColor; 
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetColor(Value: TColor);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState); virtual;
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect);
  protected
    FOriginal: TBitmap;
    procedure CalcTextRect(Canvas: TCanvas; var TextRect: TRect;
      Caption: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean): TRect;
    { DrawExternal draws any glyph (not glyph property) -
      if you don't needed to save previous glyph set IgnoreOld to True -
      this increases performance }
    function DrawExternal(AGlyph: TBitmap; ANumGlyphs: TNumGlyphs; AColor: TColor; IgnoreOld: Boolean;
      Canvas: TCanvas; const Client: TRect; const Offset: TPoint; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean): TRect; 
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property Color: TColor read FColor write SetColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvHTButtonGlyph = class(TJvButtonGlyph)
  private
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState); override;
  protected
    procedure CalcTextRect(Canvas: TCanvas; var TextRect: TRect;
      Caption: string); override;
  end;
 

  TPaintButtonEvent = procedure(Sender: TObject; IsDown, IsDefault: Boolean; State: TButtonState) of object;

  TJvaColorButton = class(TJvExBitBtn)
  private 
    FCanvas: TCanvas; 
    FGlyphDrawer: TJvButtonGlyph;
    FOnPaint: TPaintButtonEvent; 
  protected
    IsFocused: Boolean;  
    procedure Paint; override; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawing(const IsDown, IsDefault: Boolean; const State: TButtonState); 
  published
    property Color;
    property ParentColor;
    property OnPaint: TPaintButtonEvent read FOnPaint write FOnPaint;
  end;

  TJvNoFrameButton = class(TJvExSpeedButton)
  private
    FGlyphDrawer: TJvButtonGlyph;
    FNoBorder: Boolean;
    FOnPaint: TPaintButtonEvent;
    procedure SetNoBorder(Value: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawing(const IsDown: Boolean; const State: TButtonState);
    property Canvas;
  published
    property Color;
    property ParentColor;
    property NoBorder: Boolean read FNoBorder write SetNoBorder;
    property OnPaint: TPaintButtonEvent read FOnPaint write FOnPaint;
  end;

  TJvHTButton = class(TJvaColorButton)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING} 
  SysUtils, Math,
  JvQHtControls, JvQDsgnIntf, JvQConsts, JvQResources, JvQTypes, JvQThemes;

type
  TJvGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;  reintroduce; 
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TJvGlyphCache = class(TObject)
  private
    FGlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TJvGlyphList;
    procedure ReturnList(List: TJvGlyphList);
    function Empty: Boolean;
  end;

//=== { TJvGlyphList } =======================================================

constructor TJvGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  FUsed := TBits.Create;
end;

destructor TJvGlyphList.Destroy;
begin
  FUsed.Free;
  inherited Destroy;
end;

function TJvGlyphList.AllocateIndex: Integer;
begin
  Result := FUsed.OpenBit;
  if Result >= FUsed.Size then
  begin
    Result := inherited Add(nil, nil);
    FUsed.Size := Result + 1;
  end;
  FUsed[Result] := True;
end;

function TJvGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TJvGlyphList.Delete(Index: Integer);
begin
  if FUsed[Index] then
  begin
    Dec(FCount);
    FUsed[Index] := False;
  end;
end;

//=== { TJvGlyphCache } ======================================================

constructor TJvGlyphCache.Create;
begin
  inherited Create;
  FGlyphLists := TList.Create;
end;

destructor TJvGlyphCache.Destroy;
begin
  FGlyphLists.Free;
  inherited Destroy;
end;

function TJvGlyphCache.GetList(AWidth, AHeight: Integer): TJvGlyphList;
var
  I: Integer;
begin
  for I := FGlyphLists.Count - 1 downto 0 do
  begin
    Result := FGlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then
        Exit;
  end;
  Result := TJvGlyphList.CreateSize(AWidth, AHeight);
  FGlyphLists.Add(Result);
end;

procedure TJvGlyphCache.ReturnList(List: TJvGlyphList);
begin
  if List = nil then
    Exit;
  if List.Count = 0 then
  begin
    FGlyphLists.Remove(List);
    List.Free;
  end;
end;

function TJvGlyphCache.Empty: Boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

//=== { TJvButtonGlyph } =====================================================

var
  GlyphCache: TJvGlyphCache = nil;
  Pattern: TBitmap = nil;

procedure CreateBrushPattern(FaceColor, HighLightColor: TColor);
var
  X, Y: Integer;
begin
  Pattern := TBitmap.Create;
  Pattern.Width := 8;
  Pattern.Height := 8;
  with Pattern.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FaceColor {clBtnFace};
    FillRect(Rect(0, 0, Pattern.Width, Pattern.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then { toggles between even/odd pixels }
          Pixels[X, Y] := HighLightColor {clBtnHighlight}; { on even/odd rows }
  end;
end;

constructor TJvButtonGlyph.Create;
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
    GlyphCache := TJvGlyphCache.Create;
end;

destructor TJvButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TJvButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then
      TJvGlyphList(FGlyphList).Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(TJvGlyphList(FGlyphList));
  FGlyphList := nil;
end;

procedure TJvButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;



procedure TJvButtonGlyph.SetGlyph(Value: TBitmap);
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

procedure TJvButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

procedure TJvButtonGlyph.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    GlyphChanged(Glyph);
  end;
end;


function TJvButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
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
  if (FOriginal.Width or FOriginal.Height) = 0 then
    Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TJvGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := Color {clBtnFace}; 
    I := State;
    if Ord(I) >= NumGlyphs then
      I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
        bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal); 
            if NumGlyphs > 1 then
              with TmpImage.Canvas do
              begin { Change white & gray to clBtnHighlight and clBtnShadow } 
                Start; 
                CopyRect(IRect, DDB.Canvas, ORect); 
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
                Brush.Color := Color {clBtnFace};
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax); 
                Stop; 
              end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal); 
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
                Start; 
                Brush.Color := Color {clBtnFace};
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
                Stop; 
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TJvButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
begin
  if FOriginal = nil then
    Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then
    Exit;
  Index := CreateButtonGlyph(State);  
  with GlyphPos do
    if Transparent or (State = bsExclusive) then
    begin
      FGlyphList.Masked := True;
      FGlyphList.BkColor := clNone;
      FGlyphList.Draw(Canvas, X, Y, Index, itImage); // (ahuser) VisualCLX missing Transparent draw method
    end
    else
    begin
      FGlyphList.Masked := False;
      FGlyphList.BkColor := Color;
      FGlyphList.Draw(Canvas, X, Y, Index, itImage);
    end; 
end;

procedure TJvButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState);
var
  Flags: Longint;
begin
  Flags := 0; 
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Canvas, Caption, Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Canvas, Caption, Length(Caption), TextBounds, Flags);
    end
    else
      DrawText(Canvas, Caption, Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE or Flags);
  end;
end;

procedure TJvButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
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
    CalcTextRect(Canvas, TextBounds, Caption);
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
    TextPos.Y + Client.Top + Offset.Y);
end;

function TJvButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
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

function TJvButtonGlyph.DrawExternal(AGlyph: TBitmap; ANumGlyphs: TNumGlyphs; AColor: TColor; IgnoreOld: Boolean;
  Canvas: TCanvas; const Client: TRect; const Offset: TPoint; const Caption: string;
  Layout: TButtonLayout; Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean): TRect;
var
  OldGlyph: TBitmap;
  OldNumGlyphs: TNumGlyphs;
  OldColor: TColor;
begin
  OldGlyph := FOriginal;
  OldNumGlyphs := NumGlyphs;
  OldColor := FColor;
  try
    FOriginal := AGlyph;
    NumGlyphs := ANumGlyphs;
    FColor := AColor;
    GlyphChanged(FOriginal);
    Result := Draw(Canvas, Client, Offset, Caption, Layout, Margin,
      Spacing, State, Transparent);
  finally
    FOriginal := OldGlyph;
    NumGlyphs := OldNumGlyphs;
    FColor := OldColor;
    if not IgnoreOld then
      GlyphChanged(FOriginal);
  end;
end;

procedure TJvButtonGlyph.CalcTextRect(Canvas: TCanvas; var TextRect: TRect;
  Caption: string);
begin
  TextRect := Rect(0, 0, TextRect.Right - TextRect.Left, 0);
  DrawText(Canvas, Caption, Length(Caption), TextRect, DT_CALCRECT);
end;

procedure TJvHTButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState);
var
  Cap: string;
begin
  Cap := '<ALIGN CENTER>' + Caption; // Kaczkowski
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      ItemHtDraw(Canvas, TextBounds, [], Cap);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      ItemHtDraw(Canvas, TextBounds, [], Cap);
    end
    else
      ItemHtDraw(Canvas, TextBounds, [], Cap);
  end;
end;

procedure TJvHTButtonGlyph.CalcTextRect(Canvas: TCanvas; var TextRect: TRect;
  Caption: string);
begin
  TextRect := Rect(0, 0, ItemHtWidth(Canvas, TextRect, [], Caption),
    ItemHtHeight(Canvas, Caption));     // Kaczkowski
end;

//=== { TJvaCaptionButton } ==================================================



//=== { TJvaColorButton } ====================================================

constructor TJvaColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphDrawer := TJvButtonGlyph.Create;  
  FCanvas := Canvas; 
end;

destructor TJvaColorButton.Destroy;
begin
  FreeAndNil(FGlyphDrawer);
  inherited Destroy; 
end;




procedure TJvaColorButton.Paint;
var
  IsDown, IsDefault: Boolean;
  State: TButtonState;
begin
  if csDestroying in ComponentState then
    Exit;
  IsDown := Down;
  IsDefault := Focused;
  if not Enabled then
    State := bsDisabled
  else
  if IsDown then
    State := bsDown
  else
    State := bsUp;
  if Assigned(FOnPaint) then
    FOnPaint(Self, IsDown, IsDefault, State)
  else
    DefaultDrawing(IsDown, IsDefault, State);
end;


procedure TJvaColorButton.DefaultDrawing(const IsDown, IsDefault: Boolean; const State: TButtonState);
var
  R: TRect;
  Flags: Longint;
begin  
  if (csDestroying in ComponentState) or (FCanvas.Handle = nil) then
    Exit; 
  R := ClientRect;
  Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
  if IsDown then
    Flags := Flags or DFCS_PUSHED;
  if State = bsDisabled then
    Flags := Flags or DFCS_INACTIVE;
 
  begin
    { DrawFrameControl doesn't allow for drawing a button as the
        default button, so it must be done here. }
    if IsFocused or IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { DrawFrameControl must draw within this border }
      InflateRect(R, -1, -1);
    end;

    { DrawFrameControl does not draw a pressed button correctly }
    if IsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := Color {clBtnFace};
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
    begin
      DrawFrameControl(FCanvas.Handle, R, DFC_BUTTON, Flags);
      FCanvas.Pen.Style := psSolid;
      FCanvas.Pen.Color := Color {clBtnShadow};
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := Color;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;
  end;

  if IsFocused then
  begin
    R := ClientRect;
    InflateRect(R, -1, -1);
  end;

  FCanvas.Font := Self.Font;
  if IsDown then
    OffsetRect(R, 1, 1);

  FGlyphDrawer.DrawExternal(Glyph, NumGlyphs, Color, True, FCanvas, R, Point(0, 0), Caption, Layout, Margin,
    Spacing, State, False {True});
 
    if IsFocused and IsDefault then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := Color;  {clBtnFace}
      DrawFocusRect(FCanvas.Handle, R);
    end;
end;

//=== { TJvNoFrameButton } ===================================================

constructor TJvNoFrameButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphDrawer := TJvButtonGlyph.Create;
  FNoBorder := True;
end;

destructor TJvNoFrameButton.Destroy;
begin
  FGlyphDrawer.Free;
  FGlyphDrawer := nil;
  inherited Destroy;
end;

procedure TJvNoFrameButton.Paint;
begin
  if not Enabled then
  begin
    FState := bsDisabled;
    // FDragging := False;
  end
  else
  if FState = bsDisabled then
    if Down and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  if Assigned(FOnPaint) then
    FOnPaint(Self, Down, False, FState)
  else
    DefaultDrawing(Down, FState);
end;

procedure TJvNoFrameButton.DefaultDrawing(const IsDown: Boolean; const State: TButtonState);
const
  DownStyles: array [Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array [Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  Offset: TPoint;
begin
  if Flat and not NoBorder then
    inherited Paint
  else
  begin
    Canvas.Font := Self.Font;
    PaintRect := Rect(0, 0, Width, Height);
    if not NoBorder then
    begin 
      QWindows. 
      DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
        FillStyles[Transparent] or BF_RECT);
      InflateRect(PaintRect, -1, -1);
    end;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(PaintRect);
    if NoBorder and (csDesigning in ComponentState) then
      DrawDesignFrame(Canvas, PaintRect);
    InflateRect(PaintRect, -1, -1);

    if FState in [bsDown, bsExclusive] then
    begin
      if (FState = bsExclusive) then
      begin
        if Pattern = nil then
          CreateBrushPattern(clBtnFace, clBtnHighlight);
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
    {O} 
    FGlyphDrawer.DrawExternal(Glyph, NumGlyphs, Color, True, Canvas, PaintRect, Offset, Caption, Layout, Margin,
      Spacing, FState, False {True});
  end;
end;

procedure TJvNoFrameButton.SetNoBorder(Value: Boolean);
begin
  if FNoBorder <> Value then
  begin
    FNoBorder := Value;
    Refresh;
  end;
end;

//=== { TJvHTButton } ========================================================

constructor TJvHTButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphDrawer.Free;
  FGlyphDrawer := TJvHTButtonGlyph.Create;
end;

destructor TJvHTButton.Destroy;
begin
  TJvHTButtonGlyph(FGlyphDrawer).Free;
  FGlyphDrawer := nil;
  inherited Destroy;
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

