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

{$I jvcl.inc}

unit JvButtons;

interface

uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, CommCtrl,
  Buttons, JvWndProcHook,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QGraphics, QControls, QForms, QButtons, QImgList, QWindows,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
  JvComponent, JvExButtons;

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
    {$IFDEF VCL}
    FBiDiMode: TBiDiMode; {o}
    FParentBiDiMode: Boolean;
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentBiDiMode(Value: Boolean);
    {$ENDIF VCL}
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
    {$IFDEF VCL}
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode;
    {$ENDIF VCL}
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

  {$IFDEF VCL}
  TJvaCaptionButton = class(TJvComponent)
  private
    FGlyph: TJvButtonGlyph;
    FCaption: string;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FRect: TRect;
    FMouseLButtonDown: Boolean;
    FPress: Boolean;
    FOnClick: TNotifyEvent;
    FBPos: Integer;
    FWidth: Integer;
    WHook: TJvWindowHook;
    FActive: Boolean;
    FFont: TFont;
    FVisible: Boolean;
    procedure DoBeforeMsg(Sender: TObject; var Msg: TMessage; var Handled: Boolean);
    procedure DoAfterMsg(Sender: TObject; var Msg: TMessage; var Handled: Boolean);
//    procedure HookWndProc(var Msg: TMessage);
    procedure Draw;
    function MouseOnButton(X, Y: Integer): Boolean;
    procedure Resize;
    procedure GlyphChanged(Sender: TObject);

    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetLeft: Integer;

    procedure SetCaption(Value: string);
    function IsCaptionStored: Boolean;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetBPos(const Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetFont(Value: TFont);
    procedure FontChange(Sender: TObject);
    procedure SetDown(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
  protected
    FState: TButtonState;
    function CalcOffset: TPoint;
    procedure Changed; dynamic;
    function BorderStyle: TFormBorderStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; dynamic;
    procedure Update;
  published
    property Position: Integer read FBPos write SetBPos;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Caption: string read FCaption write SetCaption stored IsCaptionStored;
    property Width: Integer read FWidth write SetWidth default -1;
    property Font: TFont read FFont write SetFont;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property Down: Boolean read FPress write SetDown default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;
  {$ENDIF VCL}

  TPaintButtonEvent = procedure(Sender: TObject; IsDown, IsDefault: Boolean; State: TButtonState) of object;

  TJvaColorButton = class(TJvExBitBtn)
  private
    {$IFDEF VisualCLX}
    FCanvas: TCanvas;
    {$ENDIF VisualCLX}
    FGlyphDrawer: TJvButtonGlyph;
    FOnPaint: TPaintButtonEvent;
    {$IFDEF VCL}
    FCanvas: TControlCanvas ;
    function GetCanvas: TCanvas;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    {$ENDIF VCL}
  protected
    IsFocused: Boolean;
    {$IFDEF VCL}
    procedure SetButtonStyle(ADefault: Boolean); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure Paint; override;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawing(const IsDown, IsDefault: Boolean; const State: TButtonState);
    property Canvas {$IFDEF VCL}: TCanvas read GetCanvas {$ENDIF};
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
  Math,
  JvHtControls, JvDsgnIntf, JvConsts, JvResources, JvTypes, JvThemes;

type
  TJvGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
      {$IFDEF VisualCLX} reintroduce; {$ENDIF}
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

{$IFDEF VCL}

procedure TJvButtonGlyph.SetBiDiMode(Value: TBiDiMode);
begin
  if FBiDiMode <> Value then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Invalidate;
  end;
end;

procedure TJvButtonGlyph.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    Invalidate;
  end;
end;

{$ENDIF VCL}

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
    {$IFDEF VCL}
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    {$ENDIF VCL}
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
            {$IFDEF VCL}
            DDB.HandleType := bmDDB;
            {$ENDIF VCL}
            if NumGlyphs > 1 then
              with TmpImage.Canvas do
              begin { Change white & gray to clBtnHighlight and clBtnShadow }
                {$IFDEF VisualCLX}
                Start;
                {$ENDIF VisualCLX}
                CopyRect(IRect, DDB.Canvas, ORect);
                {$IFDEF VCL}
                MonoBmp.Monochrome := True;
                {$ENDIF VCL}
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
                {$IFDEF VisualCLX}
                Stop;
                {$ENDIF VisualCLX}
              end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
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
                {$IFDEF VisualCLX}
                Start;
                {$ENDIF VisualCLX}
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
                {$IFDEF VisualCLX}
                Stop;
                {$ENDIF VisualCLX}
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
  {$IFDEF VCL}
  with GlyphPos do
    if Transparent or (State = bsExclusive) then
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        clNone, clNone, ILD_Transparent)
    else
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        ColorToRGB(Color {clBtnFace}), clNone, ILD_Normal);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
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
  {$ENDIF VisualCLX}
end;

procedure TJvButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState);
var
  Flags: Longint;
begin
  Flags := 0;
  {$IFDEF VCL}
  if FBiDiMode <> bdLeftToRight then
    Flags := DT_RTLREADING;
  {$ENDIF VCL}
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      {$IFDEF VCL}
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      DrawText(Canvas, Caption, Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Canvas, Caption, Length(Caption), TextBounds, Flags);
      {$ENDIF VisualCLX}
    end
    else
      {$IFDEF VCL}
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE or Flags);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      DrawText(Canvas, Caption, Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE or Flags);
      {$ENDIF VisualCLX}
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
  {$IFDEF VCL}
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextRect, DT_CALCRECT);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  DrawText(Canvas, Caption, Length(Caption), TextRect, DT_CALCRECT);
  {$ENDIF VisualCLX}
end;

procedure TJvHTButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState);
var
  cap: String;
begin
  cap := '<ALIGN CENTER>' + Caption; // Kaczkowski
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

{$IFDEF VCL}

constructor TJvaCaptionButton.Create(AOwner: TComponent);

  function FindButtonPos: Integer;
  var
    I: Integer;
    B: TComponent;
  begin
    Result := 4;
    for I := 0 to Owner.ComponentCount - 1 do
    begin
      B := Owner.Components[I];
      if B is TJvaCaptionButton then
        Result := Max(Result, (B as TJvaCaptionButton).FBPos + 1);
    end;
  end;

begin
  if not (AOwner is TForm) then
    raise EJVCLException.CreateResFmt(@RsEOwnerMustBeForm, [ClassName]);
  inherited Create(AOwner);

  FGlyph := TJvButtonGlyph.Create;
  TJvButtonGlyph(FGlyph).OnChange := GlyphChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FBPos := FindButtonPos;
  FMouseLButtonDown := False;
  FPress := False;
  FWidth := -1;
  FMargin := -1;
  FVisible := True;
  WHook := TJvWindowHook.Create(nil);
  WHook.BeforeMessage := DoBeforeMsg;
  WHook.AfterMessage := DoAfterMsg;
  WHook.Control := (Owner as TForm);
  WHook.Active := True;
  Resize;
end;

destructor TJvaCaptionButton.Destroy;
begin
  WHook.Free;
  if Owner <> nil then
    RedrawWindow((Owner as TForm).Handle, PRect(0), 0, RDW_FRAME or RDW_NOINTERNALPAINT or RDW_INVALIDATE);
  TJvButtonGlyph(FGlyph).Free;
  FFont.Free;
  inherited Destroy;
end;

function TJvaCaptionButton.BorderStyle: TFormBorderStyle;
begin
  if csDesigning in ComponentState then
    Result := bsSizeable
  else
    Result := (Owner as TForm).BorderStyle;
end;

function TJvaCaptionButton.GetHeight: Integer;
begin
  if BorderStyle in [bsSizeToolWin, bsToolWindow] then
    Result := GetSystemMetrics(SM_CYSMSIZE)
  else
    Result := GetSystemMetrics(SM_CYSIZE);
end;

function TJvaCaptionButton.GetWidth: Integer;
begin
  if FWidth <> -1 then
    Result := FWidth
  else
  if BorderStyle in [bsSizeToolWin, bsToolWindow] then
    Result := GetSystemMetrics(SM_CXSMSIZE)
  else
    Result := GetSystemMetrics(SM_CXSIZE);
end;

function TJvaCaptionButton.GetLeft: Integer;
var
  F: Integer;

  function FirstButtonPos: Integer;
  var
    i: Integer;
    B: TComponent;
  begin
    Result := FBPos;
    for i := 0 to Owner.ComponentCount - 1 do
    begin
      B := Owner.Components[i];
      if B is TJvaCaptionButton then
        Result := Min(Result, (B as TJvaCaptionButton).FBPos);
    end;
  end;

  function RightButtonWidth: Integer;
  var
    I: Integer;
    B: TComponent;
  begin
    Result := 0;
    for I := 0 to Owner.ComponentCount - 1 do
    begin
      B := Owner.Components[I];
      if (B is TJvaCaptionButton) and
        ((B as TJvaCaptionButton).FBPos <= FBPos) then
        Inc(Result, (B as TJvaCaptionButton).GetWidth);
    end;
  end;

begin
  if BorderStyle in [bsSizeToolWin, bsToolWindow] then
    F := GetSystemMetrics(SM_CXSMSIZE)
  else
    F := GetSystemMetrics(SM_CXSIZE);
  Result := (Owner as TForm).Width - CalcOffset.X * 2 - F * FirstButtonPos;
  Result := Result - RightButtonWidth;
  // Result := 100;
end;

procedure TJvaCaptionButton.Resize;
begin
  FRect := Bounds(GetLeft, 0, GetWidth, GetHeight);
  RedrawWindow((Owner as TForm).Handle, PRect(0), 0, RDW_FRAME or RDW_NOINTERNALPAINT or RDW_INVALIDATE);
end;

function TJvaCaptionButton.CalcOffset: TPoint;
begin
  case BorderStyle of
    bsSingle:
      begin
        { Result.X := GetSystemMetrics(SM_CXBORDER) + 1;
         Result.Y := GetSystemMetrics(SM_CYBORDER) + 1; }
        Result.X := GetSystemMetrics(SM_CXDLGFRAME);
        Result.Y := GetSystemMetrics(SM_CYDLGFRAME);
      end;
    bsDialog:
      begin
        Result.X := GetSystemMetrics(SM_CXDLGFRAME) - 1 {?};
        Result.Y := GetSystemMetrics(SM_CYDLGFRAME);
      end;
    bsSizeable:
      begin
        Result.X := GetSystemMetrics(SM_CXFRAME);
        Result.Y := GetSystemMetrics(SM_CYFRAME);
      end;
    bsNone:
      begin
        Result.X := 0;
        Result.Y := 0;
      end;
    bsToolWindow:
      begin
        Result.X := GetSystemMetrics(SM_CXDLGFRAME);
        Result.Y := GetSystemMetrics(SM_CYDLGFRAME);
      end;
    bsSizeToolWin:
      begin
        Result.X := GetSystemMetrics(SM_CXFRAME);
        Result.Y := GetSystemMetrics(SM_CYFRAME);
      end;
  end;
end;

procedure TJvaCaptionButton.Draw;
var
  DC: HDC;
  R: TRect;
  Canvas: TCanvas;
  Offset: TPoint;
const
  CaptionColor: array [Boolean] of TColor = (clInactiveCaption, clActiveCaption);
begin
  if not FVisible then
    exit;
  Offset := CalcOffset;
  DC := GetWindowDC((Owner as TForm).Handle);
  Canvas := TCanvas.Create;
  Canvas.Font := FFont;
  try
    SetWindowOrgEx(DC, -Offset.X, -Offset.Y, nil);
    R := FRect;
    Canvas.Handle := DC;
    Canvas.Brush.Color := CaptionColor[FActive];
    //Canvas.FillRect(R); { commented for Windows98 gradient caption compatibility }
    Inc(R.Left, 2);
    Inc(R.Top, 2);
    Dec(R.Bottom, 2);
    if FPress then
      DrawThemedFrameControl(WHook.Control, DC, R, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED)
    else
      DrawThemedFrameControl(WHook.Control, DC, R, DFC_BUTTON, DFCS_BUTTONPUSH);

    R := Rect(R.Left + 1, R.Top + 1, R.Right - 2, R.Bottom - 2);
    if FPress then
      OffsetRect(R, 1, 1);

    if FPress then
      TJvButtonGlyph(FGlyph).Draw(Canvas, R, Point(0, 0),
        FCaption, FLayout, FMargin, FSpacing, bsDown, True)
    else
      TJvButtonGlyph(FGlyph).Draw(Canvas, R, Point(0, 0),
        FCaption, FLayout, FMargin, FSpacing, bsUp, True);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    ReleaseDC((Owner as TForm).Handle, DC);
  end;
end;

(*
procedure TJvaCaptionButton.HookWndProc(var Msg: TMessage);
var
  P: TPoint;
  OldPress: Boolean;
begin
  if Owner = nil then
    exit;
  case Msg.Msg of
    WM_NCACTIVATE: // after
      begin
        FActive := Boolean(Msg.wParam);
        WHook.CallOldProc(Msg);
        Draw;
      end;
    WM_SETTEXT, WM_NCPAINT: // after
      begin
        WHook.CallOldProc(Msg);
        Draw;
      end;
    WM_SIZE: // after
      begin
        WHook.CallOldProc(Msg);
        Resize;
      end;
    WM_NCLBUTTONDOWN: // before
      if FVisible and
        MouseOnButton(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor) then
      begin
        SetCapture((Owner as TForm).Handle);
        FMouseLButtonDown := True;
        FPress := True;
        Draw;
      end
      else
        WHook.CallOldProc(Msg);
    WM_NCLBUTTONDBLCLK: // before
      if FVisible and
        MouseOnButton(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor) then
      begin
        { FPress := True;
          Draw;
          FPress := False;
          Draw;}
      end
      else
        WHook.CallOldProc(Msg);
    WM_LBUTTONUP: // before
      if FVisible and FMouseLButtonDown then
      begin
        ReleaseCapture;
        FMouseLButtonDown := False;
        FPress := False;
        Draw;
        P := (Owner as TForm).ClientToScreen(Point(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor));
        if MouseOnButton(P.X, P.Y) then
          Click;
      end
      else
        WHook.CallOldProc(Msg);
    WM_MOUSEMOVE: // before
      if FMouseLButtonDown then
      begin
        P := (Owner as TForm).ClientToScreen(Point(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor));
        OldPress := FPress;
        FPress := MouseOnButton(P.X, P.Y);
        if OldPress <> FPress then
          Draw;
      end
      else
        WHook.CallOldProc(Msg);
    WM_NCHITTEST: // before
      if FVisible and
        MouseOnButton(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor) then
        Msg.Result := HTBORDER
      else
        WHook.CallOldProc(Msg);
    WM_NCRBUTTONDOWN: // before
      { if FVisible and
          MouseOnButton(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor) then
         WHook.CallOldProc(Msg)
       else} WHook.CallOldProc(Msg);
    WM_SETTINGCHANGE: // after
      begin
        WHook.CallOldProc(Msg);
        Changed;
      end;
  else
    WHook.CallOldProc(Msg);
  end;
end;
*)

procedure TJvaCaptionButton.Changed;
var
  I: Integer;
  B: TComponent;
begin
  for I := 0 to Owner.ComponentCount - 1 do
  begin
    B := Owner.Components[I];
    if (B is TJvaCaptionButton) then
    begin
      (B as TJvaCaptionButton).Resize;
      (B as TJvaCaptionButton).Draw;
    end;
  end;
end;

function TJvaCaptionButton.MouseOnButton(X, Y: Integer): Boolean;
begin
  with (Owner as TForm) do
    Result := PtInRect(FRect, Point(X - Left - CalcOffset.X, Y - Top - CalcOffset.Y));
end;

procedure TJvaCaptionButton.Click;
begin
  if csDesigning in ComponentState then
    DesignerSelectComponent(Self);
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TJvaCaptionButton.GlyphChanged(Sender: TObject);
begin
  Changed;
end;

procedure TJvaCaptionButton.SetCaption(Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

function TJvaCaptionButton.IsCaptionStored: Boolean;
begin
  Result := FCaption <> '';
end;

procedure TJvaCaptionButton.SetFont(Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TJvaCaptionButton.FontChange(Sender: TObject);
begin
  Changed;
end;

function TJvaCaptionButton.GetGlyph: TBitmap;
begin
  Result := FGlyph.Glyph;
end;

procedure TJvaCaptionButton.SetGlyph(Value: TBitmap);
begin
  if FGlyph.Glyph <> Value then
  begin
    FGlyph.Glyph := Value;
    Changed;
  end;
end;

function TJvaCaptionButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FGlyph.NumGlyphs;
end;

procedure TJvaCaptionButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else
  if Value > 4 then
    Value := 4;
  if Value <> FGlyph.NumGlyphs then
  begin
    FGlyph.NumGlyphs := Value;
    Changed;
  end;
end;

procedure TJvaCaptionButton.SetBPos(const Value: Integer);
begin
  if FBPos <> Value then
  begin
    FBPos := Value;
    Changed;
  end;
end;

procedure TJvaCaptionButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TJvaCaptionButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TJvaCaptionButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TJvaCaptionButton.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

procedure TJvaCaptionButton.Update;
begin
  Draw;
end;

procedure TJvaCaptionButton.SetDown(const Value: Boolean);
begin
  if FPress <> Value then
  begin
    FPress := Value;
    Changed;
  end;
end;

procedure TJvaCaptionButton.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TJvaCaptionButton.DoAfterMsg(Sender: TObject; var Msg: TMessage;
  var Handled: Boolean);
begin
  if Owner = nil then
    Exit;
  case Msg.Msg of
    WM_NCACTIVATE:
      begin
        FActive := Boolean(Msg.wParam);
        Draw;
      end;
    WM_SETTEXT, WM_NCPAINT:
      Draw;
    WM_SIZE:
      Resize;
    WM_SETTINGCHANGE:
      Changed;
  end;
end;

procedure TJvaCaptionButton.DoBeforeMsg(Sender: TObject; var Msg: TMessage;
  var Handled: Boolean);
var
  P: TPoint;
  OldPress: Boolean;
begin
  if Owner = nil then
    Exit;
  case Msg.Msg of
    WM_NCLBUTTONDOWN:
      if FVisible and
        MouseOnButton(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor) then
      begin
        SetCapture((Owner as TForm).Handle);
        FMouseLButtonDown := True;
        FPress := True;
        Handled := True;
        Draw;
      end;
    WM_NCLBUTTONDBLCLK:
      if FVisible and
        MouseOnButton(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor) then
      begin
        { FPress := True;
          Draw;
          FPress := False;
          Draw;}
        Handled := True;
      end;
    WM_LBUTTONUP:
      if FVisible and FMouseLButtonDown then
      begin
        ReleaseCapture;
        FMouseLButtonDown := False;
        FPress := False;
        Draw;
        P := (Owner as TForm).ClientToScreen(Point(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor));
        if MouseOnButton(P.X, P.Y) then
          Click;
        Handled := True;
      end;
    WM_MOUSEMOVE:
      if FMouseLButtonDown then
      begin
        P := (Owner as TForm).ClientToScreen(Point(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor));
        OldPress := FPress;
        FPress := MouseOnButton(P.X, P.Y);
        if OldPress <> FPress then
          Draw;
        Handled := True;
      end;
    WM_NCHITTEST:
      if FVisible and
        MouseOnButton(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor) then
      begin
        Msg.Result := HTBORDER;
        Handled := True;
      end;
    WM_NCRBUTTONDOWN:
      { if FVisible and
          MouseOnButton(TWMNCHitMessage(Msg).XCursor, TWMNCHitMessage(Msg).YCursor) then
         WHook.CallOldProc(Msg)
       else}
      ;
  end;
end;

{$ENDIF VCL}

//=== { TJvaColorButton } ====================================================

constructor TJvaColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphDrawer := TJvButtonGlyph.Create;
  {$IFDEF VCL}
  FCanvas := TControlCanvas.Create;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Control := Self;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FCanvas := Canvas;
  {$ENDIF VisualCLX}
end;

destructor TJvaColorButton.Destroy;
begin
  FreeAndNil(FGlyphDrawer);
  inherited Destroy;
  {$IFDEF VCL}
  FreeAndNil(FCanvas);
  {$ENDIF VCL}
end;

{$IFDEF VCL}

function TJvaColorButton.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvaColorButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
    IsFocused := ADefault;
  inherited SetButtonStyle(ADefault);
end;

procedure TJvaColorButton.CNDrawItem(var Msg: TWMDrawItem);
var
  DrawItemStruct: TDrawItemStruct;
  IsDown, IsDefault: Boolean;
  State: TButtonState;
begin
  if csDestroying in ComponentState then
    Exit;
  DrawItemStruct := Msg.DrawItemStruct^;
  FCanvas.Handle := DrawItemStruct.hDC;
  with DrawItemStruct do
  begin
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;

    if not Enabled then
      State := bsDisabled
    else
      if IsDown then
      State := bsDown
    else
      State := bsUp;
  end;

  if Assigned(FOnPaint) then
    FOnPaint(Self, IsDown, IsDefault, State)
  else
    DefaultDrawing(IsDown, IsDefault, State);

  FCanvas.Handle := 0;
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}
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
{$ENDIF VisualCLX}

procedure TJvaColorButton.DefaultDrawing(const IsDown, IsDefault: Boolean; const State: TButtonState);
var
  R: TRect;
  Flags: Longint;
begin
  {$IFDEF VCL}
  if (csDestroying in ComponentState) or (FCanvas.Handle = 0) then
    Exit;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if (csDestroying in ComponentState) or (FCanvas.Handle = nil) then
    Exit;
  {$ENDIF VisualCLX}
  R := ClientRect;
  Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
  if IsDown then
    Flags := Flags or DFCS_PUSHED;
  if State = bsDisabled then
    Flags := Flags or DFCS_INACTIVE;

  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    if IsFocused or IsDefault then
      Flags := Flags or DFCS_MONO; // mis-used
    if MouseOver then
      Flags := Flags or DFCS_HOT;
    DrawThemedFrameControl(Self, FCanvas.Handle, R, DFC_BUTTON, Flags);
  end
  else
  {$ENDIF JVCLThemesEnabled}
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

  {$IFDEF JVCLThemesEnabled}
  if not ThemeServices.ThemesEnabled then
  {$ENDIF JVCLThemesEnabled}
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
  FglyphDrawer := nil;
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
    {$IFDEF VCL}
    FGlyphDrawer.BiDiMode := BiDiMode;
    {$ENDIF VCL}
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

end.

