{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvArrowBtn.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 at sourceforge dot net]
Portions created by Peter Th�rnqvist are Copyright (C) 2002 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  The TJvArrowButton component implements an arrow button like
  the ones used in Office 97: one button and one arrow with
  separate events.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvArrowButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Windows, Messages, Controls, Graphics, Buttons, Menus, Types,
  CommCtrl,
  JvComponent, JvTypes;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvArrowButton = class(TJvGraphicControl)
  private
    FGroupIndex: Integer;
    FGlyph: TObject;
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
    FDropDown: TPopupMenu;
    FDropOnButtonClick: Boolean;
    FOnDrop: TNotifyEvent;
    FVerticalAlignment: TVerticalAlignment;
    FAlignment: TAlignment;
    FFlatArrowColor: TColor;
    FFlatArrowDisabledColor: TColor;
    FSplittedButton: Boolean;
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
    procedure CMButtonPressed(var Msg: TCMButtonPressed); message CM_BUTTONPRESSED;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    procedure SetFlatArrowColor(const Value: TColor);
    procedure SetFlatArrowDisabledColor(const Value: TColor);
  protected
    FState: TButtonState;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;

    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Action;
    property Anchors;
    property Constraints;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 13;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property DropDown: TPopupMenu read FDropDown write FDropDown;
    property DropOnButtonClick: Boolean read FDropOnButtonClick write FDropOnButtonClick default False;
    property Caption;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FlatArrowColor: TColor read FFlatArrowColor write SetFlatArrowColor default clBlack;
    property FlatArrowDisabledColor: TColor read FFlatArrowDisabledColor write SetFlatArrowDisabledColor default clBtnShadow;
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
    property SplittedButton: Boolean read FSplittedButton write FSplittedButton default True;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default taVerticalCenter;
    property Visible;
    property OnDrop: TNotifyEvent read FOnDrop write FOnDrop;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

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

uses
  SysUtils, Forms, ActnList, ImgList,
  JvConsts, JvThemes, JvJCLUtils;

type
  TGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: Integer;
    FRefCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    procedure AddReference;
    function RemoveReference: Integer;

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
    FArrowButton: TJvArrowButton;
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
      TextBounds: TRect; State: TButtonState; Alignment: TAlignment;
      VerticalAlignment: TVerticalAlignment);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      Alignment: TAlignment; VerticalAlignment: TVerticalAlignment);
  public
    constructor Create(AArrowButton: TJvArrowButton);
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; TextAlignment: TAlignment; TextVerticalAlignment: TVerticalAlignment): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure DrawLine(Canvas: TCanvas; X, Y, X2, Y2: Integer);
begin
  Canvas.MoveTo(X, Y);
  Canvas.LineTo(X2, Y2);
end;

// (rom) best move to JCL

procedure GrayBitmap(Bmp: TBitmap);
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

//=== { TGlyphList } =========================================================

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

procedure TGlyphList.AddReference;
begin
  Inc(FRefCount);
end;

function TGlyphList.RemoveReference: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
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

//=== { TGlyphCache } ========================================================

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
    Result := TGlyphList(FGlyphLists[I]);
    if (AWidth = Result.Width) and (AHeight = Result.Height) then
    begin
      Result.AddReference;
      Exit;
    end;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  FGlyphLists.Add(Result);
  Result.AddReference;
end;

procedure TGlyphCache.ReturnList(var List: TGlyphList);
begin
  if (List <> nil) and (List.RemoveReference = 0) then
  begin
    FGlyphLists.Remove(List);
    FreeAndNil(List);
  end
  else
    List := nil;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

var
  GlyphCache: TGlyphCache = nil;
  Pattern: TBitmap = nil;
  ButtonCount: Integer = 0;

//=== { TButtonGlyph } =======================================================

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

constructor TButtonGlyph.Create(AArrowButton: TJvArrowButton);
var
  I: TButtonState;
begin
  inherited Create;
  FArrowButton := AArrowButton;
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
    if (FIndexs[I] <> -1) and (FGlyphList <> nil) then
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
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
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
            DDB.HandleType := bmDDB;
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
                GrayBitmap(MonoBmp);
                HandleType := bmDDB;
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
  CustomAction: TCustomAction;
  ActionList: TCustomActionList;
  ImageList: TCustomImageList;
begin
  ImageList := nil;
  Index := -1;

  if (FOriginal <> nil) and not FOriginal.Empty then
  begin
    Index := CreateButtonGlyph(State);
    ImageList := FGlyphList;
  end
  else
  begin
    if (FArrowButton.Action is TCustomAction) then
    begin
      CustomAction := TCustomAction(FArrowButton.Action);
      ActionList := CustomAction.ActionList;
      if (ActionList.Images <> nil) and (CustomAction.ImageIndex >= 0) and
        (CustomAction.ImageIndex < ActionList.Images.Count) then
      begin
        ImageList := ActionList.Images;
        Index := CustomAction.ImageIndex;
      end;
    end;
  end;

  if Assigned(ImageList) then
    if Transparent or (State = bsExclusive) then
      ImageList_DrawEx(ImageList.Handle, Index, Canvas.Handle, GlyphPos.X, GlyphPos.Y, 0, 0,
        clNone, clNone, ILD_Transparent)
    else
      ImageList_DrawEx(ImageList.Handle, Index, Canvas.Handle, GlyphPos.X, GlyphPos.Y, 0, 0,
        ColorToRGB(clBtnFace), clNone, ILD_Normal);
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; Alignment: TAlignment; VerticalAlignment: TVerticalAlignment);
const
  AlignFlags: array[TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
  VerticalAlignFlags: array[TVerticalAlignment] of Integer = (DT_TOP, DT_BOTTOM, DT_VCENTER);
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
      DrawText(Canvas, S, -1, TextBounds, AlignFlags[Alignment] or VerticalAlignFlags[VerticalAlignment] or DT_SINGLELINE);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Canvas, S, -1, TextBounds, AlignFlags[Alignment] or VerticalAlignFlags[VerticalAlignment] or DT_SINGLELINE);
    end
    else
    begin
      DrawText(Canvas, S, -1, TextBounds, AlignFlags[Alignment] or VerticalAlignFlags[VerticalAlignment] or DT_SINGLELINE);
    end;
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect; Alignment: TAlignment;
  VerticalAlignment: TVerticalAlignment);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
  S: string;
  ActionList: TCustomActionList;
  CustomAction: TCustomAction;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if (FOriginal <> nil) and not FOriginal.Empty then
  begin
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
  end
  else
  begin
    GlyphSize := Point(0, 0);
    if (FArrowButton.Action is TCustomAction) then
    begin
      CustomAction := TCustomAction(FArrowButton.Action);
      ActionList := CustomAction.ActionList;
      if (ActionList.Images <> nil) and (CustomAction.ImageIndex >= 0) and
        (CustomAction.ImageIndex < ActionList.Images.Count) then
        GlyphSize := Point(ActionList.Images.Width, ActionList.Images.Height);
    end;
  end;

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    S := Caption;
    DrawText(Canvas, S, -1, TextBounds, DT_CALCRECT);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Spacing = -1 then
    Spacing := 0;
  if Margin = -1 then
    Margin := 0;

  TotalSize := Point(ClientSize.X - Margin, ClientSize.Y - Margin);

  // Calculate glyph and text position. Start with the glyph layout that has
  // an impact on the area available to the text. The glyph is always centered
  // over the text it is attached to.
  TextPos.X := 0;
  TextPos.Y := 0;
  GlyphPos.X := 0;
  GlyphPos.Y := 0;
  case Layout of
    blGlyphLeft:
      begin
        TextPos.X := GlyphSize.X + Spacing;
        GlyphPos.X := - GlyphSize.X - Spacing;
        GlyphPos.Y := (TextSize.Y - GlyphSize.Y) div 2;
      end;
    blGlyphRight:
      begin
        TextPos.X := - GlyphSize.X - Spacing;
        GlyphPos.X := TextSize.X + Spacing;
        GlyphPos.Y := (TextSize.Y - GlyphSize.Y) div 2;
      end;
    blGlyphTop:
      begin
        TextPos.Y := GlyphSize.Y + Spacing;
        GlyphPos.Y := - GlyphSize.Y - Spacing;
        GlyphPos.X := (TextSize.X - GlyphSize.X) div 2;
      end;
    blGlyphBottom:
      begin
        TextPos.Y := - GlyphSize.Y - Spacing;
        GlyphPos.Y := TextSize.Y + Spacing;
        GlyphPos.X := (TextSize.X - GlyphSize.X) div 2;
      end;
  end;

  // Then continue with the horizontal text alignment
  case Alignment of
    taLeftJustify:
      begin
        if TextPos.X < 0 then
          TextPos.X := 0;
        Inc(TextPos.X, Margin);
        Inc(GlyphPos.X, TextPos.X);
      end;
    taCenter:
      begin
        TextPos.X := (TextPos.X + TotalSize.X - TextSize.X) div 2;
        Inc(GlyphPos.X, TextPos.X);
      end;
    taRightJustify:
      begin
        if TextPos.X > 0 then
          TextPos.X := 0;
        TextPos.X := TextPos.X + TotalSize.X - TextSize.X;
        Inc(GlyphPos.X, TextPos.X);
      end;
  end;

  // And finish with the vertical text alignment
  case VerticalAlignment of
    taAlignTop:
      begin
        if TextPos.Y < 0 then
          TextPos.Y := 0;
        Inc(TextPos.Y, Margin);
        Inc(GlyphPos.Y, TextPos.Y);
      end;
    taVerticalCenter:
      begin
        TextPos.Y := (TextPos.Y + TotalSize.Y - TextSize.Y) div 2;
        Inc(GlyphPos.Y, TextPos.Y);
      end;
    taAlignBottom:
      begin
        if TextPos.Y > 0 then
          TextPos.Y := 0;
        TextPos.Y := TextPos.Y + TotalSize.Y - TextSize.Y;
        Inc(GlyphPos.Y, TextPos.Y);
      end;
  end;

  // ensure no glyph goes out of the allowed area
  if GlyphPos.X < 0 then
    GlyphPos.X := 0;
  if GlyphPos.X + GlyphSize.X > TotalSize.X then
    GlyphPos.X := TotalSize.X - GlyphSize.X;
  if GlyphPos.Y < 0 then
    GlyphPos.Y := 0;
  if GlyphPos.Y + GlyphSize.Y > TotalSize.Y then
    GlyphPos.Y := TotalSize.Y - GlyphSize.Y;

  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left + Offset.X);
  Inc(GlyphPos.Y, Client.Top + Offset.Y);
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.X);
end;

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean; TextAlignment: TAlignment;
  TextVerticalAlignment: TVerticalAlignment): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, TextAlignment, TextVerticalAlignment);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);
  DrawButtonText(Canvas, Caption, Result, State, TextAlignment, TextVerticalAlignment);
end;

//=== { TJvArrowButton } =====================================================

constructor TJvArrowButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 42, 25);
  ControlStyle := [csCaptureMouse, {csOpaque, }csDoubleClicks];
  IncludeThemeStyle(Self, [csParentBackground]);
  FGlyph := TButtonGlyph.Create(Self);
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  FFillFont := TFont.Create;
  FFillFont.Assign(Font);
  FFlatArrowColor := clBlack;
  FFlatArrowDisabledColor := clBtnShadow;
  FAllowAllUp := False;
  FArrowWidth := 13;
  FGroupIndex := 0;
  ParentFont := True;
  FDown := False;
  FFlat := False;
  FLayout := blGlyphLeft;
  FAlignment := taCenter;
  FVerticalAlignment := taVerticalCenter;
  FMargin := -1;
  FSpacing := 4;
  FSplittedButton := True;
  FPressBoth := True;
  Inc(ButtonCount);
end;

destructor TJvArrowButton.Destroy;
begin
  FGlyph.Free;
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
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
begin
  FMouseInControl := IsMouseOver(Self);
  if not Enabled then
    FState := bsDisabled
  else
  if FState = bsDisabled then
  begin
    if Down and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  end;
  if FMouseInControl then
    Canvas.Font := FillFont
  else
    Canvas.Font := Self.Font;

  if SplittedButton then
    PaintRect := Rect(0, 0, Width - ArrowWidth, Height)
  else
    PaintRect := Rect(0, 0, Width, Height);

  if FArrowClick and not Down then
    FState := bsUp;

  if not Flat then
  begin
    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if not Enabled and not (csDesigning in ComponentState) then
      DrawFlags := DrawFlags or DFCS_INACTIVE
    else if (FState in [bsDown, bsExclusive]) or (not SplittedButton and FArrowClick) then
      DrawFlags := DrawFlags or DFCS_PUSHED;
    if FMouseInControl and not (csDesigning in ComponentState) then
      DrawFlags := DrawFlags or DFCS_HOT;
    DrawThemedFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
  end
  else
  begin
    if (FState in [bsDown, bsExclusive]) or
      (FMouseInControl and (FState <> bsDisabled)) or
      (csDesigning in ComponentState) then
    begin
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} then
      begin
        Details := ThemeServices.GetElementDetails(ttbButtonNormal);
        if not Enabled and (csDesigning in ComponentState)  then
          Details := ThemeServices.GetElementDetails(ttbButtonDisabled)
        else if (FState in [bsDown, bsExclusive]) or (not SplittedButton and FArrowClick) then
          Details := ThemeServices.GetElementDetails(ttbButtonPressed)
        else if FMouseInControl and (FState <> bsDisabled) or (csDesigning in ComponentState) then
          Details := ThemeServices.GetElementDetails(ttbButtonHot);
        ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
        ThemeServices.GetElementContentRect(Canvas.Handle, Details, PaintRect, PaintRect);
      end
      else
      {$ENDIF JVCLThemesEnabled}
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          FillStyles[Flat] or BF_RECT);
    end;
    InflateRect(PaintRect, -1, -1);
  end;

  if not SplittedButton then
    PaintRect := Rect(0, 0, Width - ArrowWidth, Height);

  if FState in [bsDown, bsExclusive] then
  begin
    if (FState = bsExclusive) and (not Flat or not FMouseInControl) then
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
  TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Offset, Caption, Layout, Margin,
    Spacing, FState, Flat {$IFDEF JVCLThemesEnabled} or ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} {$ENDIF},
    Alignment, VerticalAlignment);

  { calculate were to put arrow part }
  PaintRect := Rect(Width - ArrowWidth, 0, Width, Height);
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} then
    Dec(PaintRect.Left);
  {$ENDIF JVCLThemesEnabled}
  Push := FArrowClick or (PressBoth and (FState in [bsDown, bsExclusive]));
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

  if FSplittedButton then
  begin
    if not Flat then
    begin
      DrawFlags := DFCS_BUTTONPUSH; // or DFCS_ADJUSTRECT;
      if not Enabled and not (csDesigning in ComponentState) then
        DrawFlags := DrawFlags or DFCS_INACTIVE
      else if Push then
        DrawFlags := DrawFlags or DFCS_PUSHED
      else if FMouseInControl and not (csDesigning in ComponentState) then
        DrawFlags := DrawFlags or DFCS_HOT;
      DrawThemedFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
    end
    else
    if FMouseInControl and Enabled or (csDesigning in ComponentState) then
    begin
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} then
      begin
        if not Enabled and (csDesigning in ComponentState)  then
          Details := ThemeServices.GetElementDetails(ttbButtonDisabled)
        else if FState in [bsDown, bsExclusive] then
          Details := ThemeServices.GetElementDetails(ttbButtonPressed)
        else if FMouseInControl and (FState <> bsDisabled) or (csDesigning in ComponentState) then
          Details := ThemeServices.GetElementDetails(ttbButtonHot);
        ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      end
      else
      {$ENDIF JVCLThemesEnabled}
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[Push],
          FillStyles[Flat] or BF_RECT);
    end;
  end;

  { find middle pixel }
  DivX := PaintRect.Right - PaintRect.Left;
  DivX := DivX div 2;
  DivY := PaintRect.Bottom - PaintRect.Top;
  DivY := DivY div 2;
  PaintRect.Bottom := PaintRect.Bottom - (DivY + DivX div 2) + 1;
  PaintRect.Top := PaintRect.Top + (DivY + DivX div 2) + 1;
  PaintRect.Left := PaintRect.Left + (DivX div 2);
  PaintRect.Right := (PaintRect.Right - DivX div 2);

  OffsetRect(PaintRect, Offset.X, Offset.Y);

  if not SplittedButton and (not Flat or (FMouseInControl and Enabled)) then
  begin
    { Draw vertical 'bar' }
    Canvas.Pen.Color := clBtnShadow;
    DrawLine(Canvas, Width - ArrowWidth - 1 + Offset.X, 4, Width - ArrowWidth - 1 + Offset.X, Height - 4);
    Canvas.Pen.Color := clBtnHighlight;
    DrawLine(Canvas, Width - ArrowWidth + Offset.X, 4, Width - ArrowWidth + Offset.X, Height - 4);
    Dec(PaintRect.Right, 1);
  end;

  if Flat and (not FMouseInControl or (csDesigning in ComponentState)) then
  begin
    if Enabled then
      Canvas.Pen.Color := FFlatArrowColor
    else
      Canvas.Pen.Color := FFlatArrowDisabledColor;
  end
  else
  begin
    if Enabled and not (csDesigning in ComponentState) then
      Canvas.Pen.Color := clBlack
    else
      Canvas.Pen.Color := clBtnShadow;
  end;

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
  if Flat then
    if Enabled then
    begin
      GetCursorPos(P);
      FMouseInControl := not (FindDragTarget(P, True) = Self);
      if FMouseInControl then
        Perform(CM_MOUSELEAVE, 0, 0)
      else
        Perform(CM_MOUSEENTER, 0, 0);
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

procedure TJvArrowButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pnt: TPoint;
  Msg: TMsg;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Enabled then
    Exit;
  FArrowClick := (X >= Width - ArrowWidth) and (X <= Width) and (Y >= 0) and (Y <= Height) or DropOnButtonClick;

  if Button = mbLeft then
  begin
    if not Down then
      FState := bsDown
    else
      FState := bsExclusive;
    Repaint; // Invalidate;
  end;

  if Assigned(FDropDown) and FArrowClick then
  begin
    DropDown.PopupComponent := Self;
    Pnt := ClientToScreen(Point(0, Height));
    DropDown.Popup(Pnt.X, Pnt.Y);
    while PeekMessage(Msg, HWND_DESKTOP, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      {nothing};
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  end;

  if FArrowClick then
    if Assigned(FOnDrop) then
      FOnDrop(Self);
  FArrowClick := False;
  Repaint;
end;

procedure TJvArrowButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if not Enabled then
  begin
    FState := bsUp;
    Repaint;
  end;

  DoClick := (X >= 0) and (X <= Width - ArrowWidth) and (Y >= 0) and (Y <= Height) and not DropOnButtonClick;

  if GroupIndex = 0 then
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
    SetDown(not Down);
    if Down then
      Repaint;
  end
  else
  begin
    if Down then
      FState := bsExclusive;
    Repaint;
  end;
  if DoClick then
    Click;
  UpdateTracking;
  Repaint;
end;


function TJvArrowButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;


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
  Msg: TCMButtonPressed;
begin
  if (GroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.Index := GroupIndex;
    Msg.Control := Self;
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvArrowButton.SetDown(Value: Boolean);
begin
  if GroupIndex = 0 then
    Value := False;
  if Value <> FDown then
  begin
    if FDown and (not AllowAllUp) then
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

procedure TJvArrowButton.SetFlatArrowColor(const Value: TColor);
begin
  if Value <> FFlatArrowColor then
  begin
    FFlatArrowColor := Value;
    Invalidate;
  end;
end;

procedure TJvArrowButton.SetFlatArrowDisabledColor(const Value: TColor);
begin
  if Value <> FFlatArrowDisabledColor then
  begin
    FFlatArrowDisabledColor := Value;
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

procedure TJvArrowButton.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvArrowButton.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  if FVerticalAlignment <> Value then
  begin
    FVerticalAlignment := Value;
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

procedure TJvArrowButton.CMButtonPressed(var Msg: TCMButtonPressed);
var
  Sender: TJvArrowButton;
  {$IFDEF JVCLThemesEnabled}
  R: TRect;
  {$ENDIF JVCLThemesEnabled}
begin
  if Msg.Index = GroupIndex then
  begin
    Sender := TJvArrowButton(Msg.Control);
    if Sender <> Self then
    begin
      if Sender.Down and Down then
      begin
        FDown := False;
        FState := bsUp;
        {$IFDEF JVCLThemesEnabled}
        if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} and Enabled and not Flat then
        begin
          R := BoundsRect;
          Windows.InvalidateRect(Parent.Handle, {$IFNDEF COMPILER12_UP}@{$ENDIF ~COMPILER12_UP}R, True);
        end
        else
        {$ENDIF JVCLThemesEnabled}
          Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

function TJvArrowButton.WantKey(Key: Integer; Shift: TShiftState): Boolean;
begin
  Result := IsAccel(Key, Caption) and Enabled and (Shift * KeyboardShiftStates = [ssAlt]);
  if Result then
    Click
  else
    Result := inherited WantKey(Key, Shift);
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

procedure TJvArrowButton.WMLButtonDblClk(var Msg: TWMLButtonDown);
begin
  inherited;
  if Down then
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

procedure TJvArrowButton.MouseEnter(Control: TControl);
{$IFDEF JVCLThemesEnabled}
var
  R: TRect;
{$ENDIF JVCLThemesEnabled}
begin
  inherited MouseEnter(Control);
  if Flat and not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    Repaint;
  end;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} and Enabled and not Flat then
  begin
    R := BoundsRect;
    Windows.InvalidateRect(Parent.Handle, {$IFNDEF COMPILER12_UP}@{$ENDIF ~COMPILER12_UP}R, True);
  end;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvArrowButton.MouseLeave(Control: TControl);
{$IFDEF JVCLThemesEnabled}
var
  R: TRect;
{$ENDIF JVCLThemesEnabled}
begin
  inherited MouseLeave(Control);
  if Flat and FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} and Enabled and not Flat then
  begin
    R := BoundsRect;
    Windows.InvalidateRect(Parent.Handle, {$IFNDEF COMPILER12_UP}@{$ENDIF ~COMPILER12_UP}R, True);
  end;
  {$ENDIF JVCLThemesEnabled}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
