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

The Original Code is: JvSplitter.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
dejoy(dejoy att ynl dott gov dott cn)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQNetscapeSplitter;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  QWindows, QMessages, QGraphics, QForms, QExtCtrls, QControls, 
  Qt, 
  JvQExExtCtrls;

const
  MOVEMENT_TOLERANCE = 5; // See WMLButtonUp message handler.
  JvDefaultButtonHighlightColor = TColor($00FFCFCF); // RGB(207,207,255)

type
  TJvButtonWidthKind = (btwPixels, btwPercentage);
  TJvButtonStyle = (bsNetscape, bsWindows);
  TJvWindowsButton = (wbMin, wbMax, wbClose);
  TJvWindowsButtons = set of TJvWindowsButton;

  TJvCustomNetscapeSplitter = class(TJvExSplitter)
  private 
    FShowButton: Boolean;
    FButtonWidthKind: TJvButtonWidthKind;
    FButtonWidth: Integer;
    FOnMaximize: TNotifyEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FMaximized: Boolean;
    FMinimized: Boolean;
    // Internal use for "restoring" from "maximized" state
    FRestorePos: Integer;
    // For internal use to avoid calling GetButtonRect when not necessary
    FLastKnownButtonRect: TRect;
    // Internal use to avoid unecessary painting
    FIsHighlighted: Boolean;
    // Internal for detecting real clicks
    FGotMouseDown: Boolean;
    FButtonColor: TColor;
    FButtonHighlightColor: TColor;
    FArrowColor: TColor;
    FTextureColor1: TColor;
    FTextureColor2: TColor;
    FAutoHighlightColor: Boolean;
    FAllowDrag: Boolean;
    FButtonStyle: TJvButtonStyle;
    FWindowsButtons: TJvWindowsButtons;
    FOnClose: TNotifyEvent;
    FButtonCursor: TCursor;
    procedure SetShowButton(const Value: Boolean);
    procedure SetButtonWidthKind(const Value: TJvButtonWidthKind);
    procedure SetButtonWidth(const Value: Integer);
    function GetButtonRect: TRect;
    procedure SetMaximized(const Value: Boolean);
    procedure SetMinimized(const Value: Boolean);
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    procedure SetArrowColor(const Value: TColor);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonHighlightColor(const Value: TColor);
    procedure SetButtonStyle(const Value: TJvButtonStyle);
    procedure SetTextureColor1(const Value: TColor);
    procedure SetTextureColor2(const Value: TColor);
    procedure SetAutoHighlightColor(const Value: Boolean);
    procedure SetAllowDrag(const Value: Boolean);
    procedure SetWindowsButtons(const Value: TJvWindowsButtons);
    procedure SetButtonCursor(const Value: TCursor);
  protected
    // Internal use for moving splitter position with FindControl and
    // UpdateControlSize
    FControl: TControl;
    FDownPos: TPoint; 
    procedure LoadOtherProperties(Reader: TReader); dynamic;
    procedure StoreOtherProperties(Writer: TWriter); dynamic;
    procedure DefineProperties(Filer: TFiler); override; 
    function DoCanResize(var NewSize: Integer): Boolean; override; 
    procedure Loaded; override;
    procedure PaintButton(Highlight: Boolean); dynamic;
    function DrawArrow(ACanvas: TCanvas; AvailableRect: TRect; Offset: Integer;
      ArrowSize: Integer; Color: TColor): Integer; dynamic;
    function WindowButtonHitTest(X, Y: Integer): TJvWindowsButton; dynamic;
    function ButtonHitTest(X, Y: Integer): Boolean; dynamic;
    procedure DoMaximize; dynamic;
    procedure DoMinimize; dynamic;
    procedure DoRestore; dynamic;
    procedure DoClose; dynamic;
    procedure FindControl; dynamic;
    procedure UpdateControlSize(NewSize: Integer); dynamic;
    function GrabBarColor: TColor;
    function VisibleWinButtons: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property ButtonRect: TRect read GetButtonRect;
    property RestorePos: Integer read FRestorePos write FRestorePos;
    property Maximized: Boolean read FMaximized write SetMaximized;
    property Minimized: Boolean read FMinimized  write SetMinimized;
    property AllowDrag: Boolean read FAllowDrag write SetAllowDrag default True;
    property ButtonCursor: TCursor read FButtonCursor write SetButtonCursor;
    property ButtonStyle: TJvButtonStyle read FButtonStyle write SetButtonStyle default bsNetscape;
    property WindowsButtons: TJvWindowsButtons read FWindowsButtons write SetWindowsButtons
      default [wbMin, wbMax, wbClose];
    property ButtonWidthKind: TJvButtonWidthKind read FButtonWidthKind write SetButtonWidthKind
      default btwPixels;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 100;
    property ShowButton: Boolean read FShowButton write SetShowButton default True;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnFace;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clNavy;
    property ButtonHighlightColor: TColor read FButtonHighlightColor write SetButtonHighlightColor
      default JvDefaultButtonHighlightColor;
    property AutoHighlightColor: Boolean read FAutoHighlightColor write SetAutoHighlightColor
      default False;
    property TextureColor1: TColor read FTextureColor1 write SetTextureColor1 default clWhite;
    property TextureColor2: TColor read FTextureColor2 write SetTextureColor2 default clNavy;
    property Align: TAlign read GetAlign write SetAlign; // Need to know when it changes to redraw arrows
    property Width default 10; // it looks best with 10
    property Beveled default False; // it looks best without the bevel
    property Enabled;
    property HintColor;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnMaximize: TNotifyEvent read FOnMaximize write FOnMaximize;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnParentColorChange;
  end;

  TJvNetscapeSplitter = class(TJvCustomNetscapeSplitter)
  published
    property Maximized;
    property Minimized;
    property AllowDrag;
    property ButtonCursor;
    property ButtonStyle;
    property WindowsButtons;
    property ButtonWidthKind;
    property ButtonWidth;
    property ShowButton;
    property ButtonColor;
    property ArrowColor;
    property ButtonHighlightColor;
    property AutoHighlightColor;
    property TextureColor1;
    property TextureColor2;
    property Align;
    property Width;
    property Beveled;
    property Enabled;
    property ShowHint;
    property HintColor;
    property OnClose;
    property OnMaximize;
    property OnMinimize;
    property OnRestore;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvQThemes;

procedure SetRectEmpty(var R: TRect);
begin
  FillChar(R, SizeOf(TRect), #0);
end;

constructor TJvCustomNetscapeSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);

  Beveled := False;
  FAllowDrag := True;
  FButtonStyle := bsNetscape;
  FWindowsButtons := [wbMin, wbMax, wbClose];
  FButtonWidthKind := btwPixels;
  FButtonWidth := 100;
  FShowButton := True;
  SetRectEmpty(FLastKnownButtonRect);
  FIsHighlighted := False;
  FGotMouseDown := False;
  FControl := nil;
  FDownPos := Point(0, 0);
  FMaximized := False;
  FMinimized := False;
  FRestorePos := -1;
  Width := 10;
  FButtonColor := clBtnFace;
  FArrowColor := clNavy;
  FButtonHighlightColor := JvDefaultButtonHighlightColor;
  FAutoHighlightColor := False;
  FTextureColor1 := clWhite;
  FTextureColor2 := clNavy;
end;



//dfs

function TJvCustomNetscapeSplitter.ButtonHitTest(X, Y: Integer): Boolean;
begin
  // We use FLastKnownButtonRect here so that we don't have to recalculate the
  // button rect with GetButtonRect every time the mouse moved.  That would be
  // EXTREMELY inefficient.
  Result := PtInRect(FLastKnownButtonRect, Point(X, Y));
  if Align in [alLeft, alRight] then
  begin
    if (not AllowDrag) or ((Y >= FLastKnownButtonRect.Top) and
      (Y <= FLastKnownButtonRect.Bottom)) then  
      QWindows.SetCursor(Screen.Cursors[ButtonCursor])
    else
      QWindows.SetCursor(Screen.Cursors[Cursor]); 
  end
  else
  begin
    if (not AllowDrag) or ((X >= FLastKnownButtonRect.Left) and
      (X <= FLastKnownButtonRect.Right)) then  
      QWindows.SetCursor(Screen.Cursors[ButtonCursor])
    else
      QWindows.SetCursor(Screen.Cursors[Cursor]); 
  end;
end;

procedure TJvCustomNetscapeSplitter.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('RestorePos', LoadOtherProperties, StoreOtherProperties,
    Minimized or Maximized);
end;


function TJvCustomNetscapeSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := inherited DoCanResize(NewSize);
  // D4 version has a bug that causes it to not honor MinSize, which causes a
  // really nasty problem.
  if Result and (NewSize < MinSize) then
    NewSize := MinSize;
end;


procedure TJvCustomNetscapeSplitter.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJvCustomNetscapeSplitter.DoMaximize;
begin
  if Assigned(FOnMaximize) then
    FOnMaximize(Self);
end;

procedure TJvCustomNetscapeSplitter.DoMinimize;
begin
  if Assigned(FOnMinimize) then
    FOnMinimize(Self);
end;

procedure TJvCustomNetscapeSplitter.DoRestore;
begin
  if Assigned(FOnRestore) then
    FOnRestore(Self);
end;

function TJvCustomNetscapeSplitter.DrawArrow(ACanvas: TCanvas; AvailableRect: TRect;
  Offset, ArrowSize: Integer; Color: TColor): Integer;
var
  X, Y, Q, I, J: Integer;
  ArrowAlign: TAlign;
begin
  // STB Nitro drivers have a LineTo bug, so I've opted to use the slower
  // SetPixel method to draw the arrows.

  if not Odd(ArrowSize) then
    Dec(ArrowSize);
  if ArrowSize < 1 then
    ArrowSize := 1;

  if FMaximized then
  begin
    case Align of
      alLeft:
        ArrowAlign := alRight;
      alRight:
        ArrowAlign := alLeft;
      alTop:
        ArrowAlign := alBottom;
    else //alBottom
      ArrowAlign := alTop;
    end;
  end
  else
    ArrowAlign := Align;
  Q := ArrowSize * 2 - 1;
  Result := Q;
  ACanvas.Pen.Color := Color;
  with AvailableRect do
  begin
    case ArrowAlign of
      alLeft:
        begin
          X := Left + ((Right - Left - ArrowSize) div 2) + 1;
          if Offset < 0 then
            Y := Bottom + Offset - Q
          else
            Y := Top + Offset;
          for J := X + ArrowSize - 1 downto X do
          begin
            for I := Y to Y + Q - 1 do
              ACanvas.Pixels[J, I] := Color;
            Inc(Y);
            Dec(Q, 2);
          end;
        end;
      alRight:
        begin
          X := Left + ((Right - Left - ArrowSize) div 2) + 1;
          if Offset < 0 then
            Y := Bottom + Offset - Q
          else
            Y := Top + Offset;
          for J := X to X + ArrowSize - 1 do
          begin
            for I := Y to Y + Q - 1 do
              ACanvas.Pixels[J, I] := Color;
            Inc(Y);
            Dec(Q, 2);
          end;
        end;
      alTop:
        begin
          if Offset < 0 then
            X := Right + Offset - Q
          else
            X := Left + Offset;
          Y := Top + ((Bottom - Top - ArrowSize) div 2) + 1;
          for I := Y + ArrowSize - 1 downto Y do
          begin
            for J := X to X + Q - 1 do
              ACanvas.Pixels[J, I] := Color;
            Inc(X);
            Dec(Q, 2);
          end;
        end;
    else // alBottom
      if Offset < 0 then
        X := Right + Offset - Q
      else
        X := Left + Offset;
      Y := Top + ((Bottom - Top - ArrowSize) div 2) + 1;
      for I := Y to Y + ArrowSize - 1 do
      begin
        for J := X to X + Q - 1 do
          ACanvas.Pixels[J, I] := Color;
        Inc(X);
        Dec(Q, 2);
      end;
    end;
  end;
end;

procedure TJvCustomNetscapeSplitter.FindControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  if Parent = nil then
    Exit;
  FControl := nil;
  P := Point(Left, Top);
  case Align of
    alLeft:
      Dec(P.X);
    alRight:
      Inc(P.X, Width);
    alTop:
      Dec(P.Y);
    alBottom:
      Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    FControl := Parent.Controls[I];
    if FControl.Visible and FControl.Enabled then
    begin
      R := FControl.BoundsRect;
      if (R.Right - R.Left) = 0 then
        Dec(R.Left);
      if (R.Bottom - R.Top) = 0 then
        Dec(R.Top);
      if PtInRect(R, P) then
        Exit;
    end;
  end;
  FControl := nil;
end;

function TJvCustomNetscapeSplitter.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

function TJvCustomNetscapeSplitter.GetButtonRect: TRect;
var
  BW: Integer;
begin
  if ButtonStyle = bsWindows then
  begin
    if Align in [alLeft, alRight] then
      BW := (ClientRect.Right - ClientRect.Left) * VisibleWinButtons
    else
      BW := (ClientRect.Bottom - ClientRect.Top) * VisibleWinButtons;
    if BW < 1 then
      SetRectEmpty(Result)
    else
    begin
      if Align in [alLeft, alRight] then
        Result := Rect(0, 0, ClientRect.Right - ClientRect.Left,
          BW - VisibleWinButtons)
      else
        Result := Rect(ClientRect.Right - BW + VisibleWinButtons, 0,
          ClientRect.Right, ClientRect.Bottom - ClientRect.Top);
      InflateRect(Result, -1, -1);
    end;
  end
  else
  begin
    // Calc the rectangle the button goes in
    if ButtonWidthKind = btwPercentage then
    begin
      if Align in [alLeft, alRight] then
        BW := ClientRect.Bottom - ClientRect.Top
      else
        BW := ClientRect.Right - ClientRect.Left;
      BW := MulDiv(BW, FButtonWidth, 100);
    end
    else
      BW := FButtonWidth;
    if BW < 1 then
      SetRectEmpty(Result)
    else
    begin
      Result := ClientRect;
      if Align in [alLeft, alRight] then
      begin
        Result.Top := (ClientRect.Bottom - ClientRect.Top - BW) div 2;
        Result.Bottom := Result.Top + BW;
        InflateRect(Result, -1, 0);
      end
      else
      begin
        Result.Left := (ClientRect.Right - ClientRect.Left - BW) div 2;
        Result.Right := Result.Left + BW;
        InflateRect(Result, 0, -1);
      end;
    end;
  end;
  if not IsRectEmpty(Result) then
  begin
    if Result.Top < 1 then
      Result.Top := 1;
    if Result.Left < 1 then
      Result.Left := 1;
    if Result.Bottom >= ClientRect.Bottom then
      Result.Bottom := ClientRect.Bottom - 1;
    if Result.Right >= ClientRect.Right then
      Result.Right := ClientRect.Right - 1;
    // Make smaller if it's beveled
    if Beveled then
      if Align in [alLeft, alRight] then
        InflateRect(Result, -3, 0)
      else
        InflateRect(Result, 0, -3);
  end;
  FLastKnownButtonRect := Result;
end;

function TJvCustomNetscapeSplitter.GrabBarColor: TColor;
var
  BeginRGB: array [0..2] of Byte;
  RGBDifference: array [0..2] of Integer;
  R, G, B: Byte;
  BeginColor, EndColor: TColor;
  NumberOfColors: Integer;
begin
  //Need to figure out how many colors available at runtime
  NumberOfColors := 256;

  BeginColor := clActiveCaption;
  EndColor := clBtnFace;

  BeginRGB[0] := GetRValue(ColorToRGB(BeginColor));
  BeginRGB[1] := GetGValue(ColorToRGB(BeginColor));
  BeginRGB[2] := GetBValue(ColorToRGB(BeginColor));

  RGBDifference[0] := GetRValue(ColorToRGB(EndColor)) - BeginRGB[0];
  RGBDifference[1] := GetGValue(ColorToRGB(EndColor)) - BeginRGB[1];
  RGBDifference[2] := GetBValue(ColorToRGB(EndColor)) - BeginRGB[2];

  R := BeginRGB[0] + MulDiv(180, RGBDifference[0], NumberOfColors - 1);
  G := BeginRGB[1] + MulDiv(180, RGBDifference[1], NumberOfColors - 1);
  B := BeginRGB[2] + MulDiv(180, RGBDifference[2], NumberOfColors - 1);

  Result := RGB(R, G, B);
end;

procedure TJvCustomNetscapeSplitter.Loaded;
begin
  inherited Loaded;
  if FRestorePos = -1 then
  begin
    FindControl;
    if FControl <> nil then
      case Align of
        alLeft, alRight:
          FRestorePos := FControl.Width;
        alTop, alBottom:
          FRestorePos := FControl.Height;
      end;
  end;
end;

procedure TJvCustomNetscapeSplitter.LoadOtherProperties(Reader: TReader);
begin
  RestorePos := Reader.ReadInteger;
end;

procedure TJvCustomNetscapeSplitter.PaintButton(Highlight: Boolean);
const
  TEXTURE_SIZE = 3;
var
  BtnRect: TRect;
  CaptionBtnRect: TRect;
  BW: Integer;
  TextureBmp: TBitmap;
  X, Y: Integer;
  RW, RH: Integer;
  OffscreenBmp: TBitmap;
  WinButton: array [0..2] of TJvWindowsButton;
  B: TJvWindowsButton;
  BtnFlag: UINT;
begin
  if (not FShowButton) or (not Enabled) or (GetParentForm(Self) = nil) then
    Exit;

  if FAutoHighlightColor then
    FButtonHighlightColor := GrabBarColor;

  BtnRect := ButtonRect; // So we don't repeatedly call GetButtonRect
  if IsRectEmpty(BtnRect) then
    Exit; // nothing to draw

  OffscreenBmp := TBitmap.Create;
  try
    OffsetRect(BtnRect, -BtnRect.Left, -BtnRect.Top);
    OffscreenBmp.Width := BtnRect.Right;
    OffscreenBmp.Height := BtnRect.Bottom;

    if ButtonStyle = bsWindows then
    begin
      OffscreenBmp.Canvas.Brush.Color := Color;
      OffscreenBmp.Canvas.FillRect(BtnRect);
      if Align in [alLeft, alRight] then
        BW := BtnRect.Right
      else
        BW := BtnRect.Bottom;
      FillChar(WinButton, SizeOf(WinButton), 0);
      X := 0;
      if Align in [alLeft, alRight] then
      begin
        for B := High(TJvWindowsButton) downto Low(TJvWindowsButton) do
          if B in WindowsButtons then
          begin
            WinButton[X] := B;
            Inc(X);
          end;
      end
      else
      begin
        for B := Low(TJvWindowsButton) to High(TJvWindowsButton) do
          if B in WindowsButtons then
          begin
            WinButton[X] := B;
            Inc(X);
          end;
      end;
      for X := 0 to VisibleWinButtons - 1 do
      begin
        if Align in [alLeft, alRight] then
          CaptionBtnRect := Bounds(0, X * BW, BW, BW)
        else
          CaptionBtnRect := Bounds(X * BW, 0, BW, BW);
        BtnFlag := 0;
        case WinButton[X] of
          wbMin:
            if Minimized then
              BtnFlag := DFCS_CAPTIONRESTORE
            else
              BtnFlag := DFCS_CAPTIONMIN;
          wbMax:
            if Maximized then
              BtnFlag := DFCS_CAPTIONRESTORE
            else
              BtnFlag := DFCS_CAPTIONMAX;
          wbClose:
            BtnFlag := DFCS_CAPTIONCLOSE;
        end;
        DrawFrameControl(OffscreenBmp.Canvas.Handle,
          CaptionBtnRect, DFC_CAPTION, BtnFlag);
      end;
    end
    else
    begin
      // Draw basic button
      OffscreenBmp.Canvas.Brush.Color := clGray;  
      FrameRect(OffscreenBmp.Canvas, BtnRect); 
      InflateRect(BtnRect, -1, -1);

      OffscreenBmp.Canvas.Pen.Color := clWhite;
      with BtnRect, OffscreenBmp.Canvas do
      begin
        // This is not going to work with the STB bug.  Have to find workaround.
        MoveTo(Left, Bottom - 1);
        LineTo(Left, Top);
        LineTo(Right, Top);
      end;
      Inc(BtnRect.Left);
      Inc(BtnRect.Top);

      if Highlight then
        OffscreenBmp.Canvas.Brush.Color := ButtonHighlightColor
      else
        OffscreenBmp.Canvas.Brush.Color := ButtonColor;
      OffscreenBmp.Canvas.FillRect(BtnRect);
      FIsHighlighted := Highlight;
      Dec(BtnRect.Right);
      Dec(BtnRect.Bottom);

      // Draw the insides of the button
      with BtnRect do
      begin
        // Draw the arrows
        if Align in [alLeft, alRight] then
        begin
          InflateRect(BtnRect, 0, -4);
          BW := BtnRect.Right - BtnRect.Left;
          DrawArrow(OffscreenBmp.Canvas, BtnRect, 1, BW, ArrowColor);
          BW := DrawArrow(OffscreenBmp.Canvas, BtnRect, -1, BW, ArrowColor);
          InflateRect(BtnRect, 0, -(BW + 4));
        end
        else
        begin
          InflateRect(BtnRect, -4, 0);
          BW := BtnRect.Bottom - BtnRect.Top;
          DrawArrow(OffscreenBmp.Canvas, BtnRect, 1, BW, ArrowColor);
          BW := DrawArrow(OffscreenBmp.Canvas, BtnRect, -1, BW, ArrowColor);
          InflateRect(BtnRect, -(BW + 4), 0);
        end;

        // Draw the texture
        // Note: This is so complex because I'm trying to make as much like the
        //       Netscape splitter as possible.  They use a 3x3 texture pattern, and
        //       that's harder to tile.  If the had used an 8x8 (or smaller
        //       divisibly, i.e. 2x2 or 4x4), I could have used Brush.Bitmap and
        //       FillRect and they whole thing would have been about half the size,
        //       twice as fast, and 1/10th as complex.
        RW := BtnRect.Right - BtnRect.Left;
        RH := BtnRect.Bottom - BtnRect.Top;
        if (RW >= TEXTURE_SIZE) and (RH >= TEXTURE_SIZE) then
        begin
          TextureBmp := TBitmap.Create;
          try
            with TextureBmp do
            begin
              Width := RW;
              Height := RH;
              // Draw first square
              Canvas.Brush.Color := OffscreenBmp.Canvas.Brush.Color;
              Canvas.FillRect(Rect(0, 0, RW + 1, RH + 1));
              Canvas.Pixels[1, 1] := TextureColor1;
              Canvas.Pixels[2, 2] := TextureColor2;

              // Tile first square all the way across
              for X := 1 to ((RW div TEXTURE_SIZE) + ord(RW mod TEXTURE_SIZE > 0)) do
                Canvas.CopyRect(Bounds(X * TEXTURE_SIZE, 0, TEXTURE_SIZE,
                  TEXTURE_SIZE), Canvas, Rect(0, 0, TEXTURE_SIZE, TEXTURE_SIZE));

              // Tile first row all the way down
              for Y := 1 to ((RH div TEXTURE_SIZE) + ord(RH mod TEXTURE_SIZE > 0)) do
                Canvas.CopyRect(Bounds(0, Y * TEXTURE_SIZE, RW, TEXTURE_SIZE),
                  Canvas, Rect(0, 0, RW, TEXTURE_SIZE));

              // Above could be better if it reversed process when splitter was
              // taller than it was wider.  Optimized only for horizontal right now.
            end;
            // Copy texture bitmap to the screen.
            OffscreenBmp.Canvas.CopyRect(BtnRect, TextureBmp.Canvas,
              Rect(0, 0, RW, RH));
          finally
            TextureBmp.Free;
          end;
        end;
      end;
    end;
(**)
    Canvas.CopyRect(ButtonRect, OffscreenBmp.Canvas, Rect(0, 0,
      OffscreenBmp.Width, OffscreenBmp.Height));
  finally
    OffscreenBmp.Free;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetAlign(Value: TAlign);
begin
  if Align <> Value then
  begin
    inherited Align := Value;
    Invalidate; // Direction changing, redraw arrows. 
  end;
end;

procedure TJvCustomNetscapeSplitter.SetAllowDrag(const Value: Boolean);
var
  Pt: TPoint;
begin
  if FAllowDrag <> Value then
  begin
    FAllowDrag := Value;
    // Have to reset cursor in case it's on the splitter at the moment
    GetCursorPos(Pt);
    Pt := ScreenToClient(Pt);
    ButtonHitTest(Pt.X, Pt.Y);
  end;
end;

procedure TJvCustomNetscapeSplitter.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    if (ButtonStyle = bsNetscape) and ShowButton then
      Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetAutoHighlightColor(const Value: Boolean);
begin
  if FAutoHighlightColor <> Value then
  begin
    FAutoHighlightColor := Value;
    if FAutoHighlightColor then
      FButtonHighlightColor := GrabBarColor
    else
      FButtonHighlightColor := JvDefaultButtonHighlightColor;
    if (ButtonStyle = bsNetscape) and ShowButton then
      Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    if (ButtonStyle = bsNetscape) and ShowButton then
      Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetButtonCursor(const Value: TCursor);
begin
  FButtonCursor := Value;
end;

procedure TJvCustomNetscapeSplitter.SetButtonHighlightColor(const Value: TColor);
begin
  if FButtonHighlightColor <> Value then
  begin
    FButtonHighlightColor := Value;
    if (ButtonStyle = bsNetscape) and ShowButton then
      Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetButtonStyle(const Value: TJvButtonStyle);
begin
  FButtonStyle := Value;
  if ShowButton then
    Invalidate;
end;

procedure TJvCustomNetscapeSplitter.SetButtonWidth(const Value: Integer);
begin
  if Value <> FButtonWidth then
  begin
    FButtonWidth := Value;
    if (ButtonWidthKind = btwPercentage) and (FButtonWidth > 100) then
      FButtonWidth := 100;
    if FButtonWidth < 0 then
      FButtonWidth := 0;
    if (ButtonStyle = bsNetscape) and ShowButton then
      Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetButtonWidthKind(const Value: TJvButtonWidthKind);
begin
  if Value <> FButtonWidthKind then
  begin
    FButtonWidthKind := Value;
    if (FButtonWidthKind = btwPercentage) and (ButtonWidth > 100) then
      FButtonWidth := 100;
    if (ButtonStyle = bsNetscape) and ShowButton then
      Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetMaximized(const Value: Boolean);
begin
  if Value <> FMaximized then
  begin
    if csLoading in ComponentState then
    begin
      FMaximized := Value;
      Exit;
    end;

    FindControl;
    if FControl = nil then
      Exit;

    if Value then
    begin
      if FMinimized then
        FMinimized := False
      else
      begin
        case Align of
          alLeft, alRight:
            FRestorePos := FControl.Width;
          alTop, alBottom:
            FRestorePos := FControl.Height;
        else
          Exit;
        end;
      end;
      if ButtonStyle = bsNetscape then
        UpdateControlSize(-3000)
      else
        case Align of
          alLeft, alBottom:
            UpdateControlSize(3000);
          alRight, alTop:
            UpdateControlSize(-3000);
        else
          Exit;
        end;
      FMaximized := Value;
      DoMaximize;
    end
    else
    begin
      UpdateControlSize(FRestorePos);
      FMaximized := Value;
      DoRestore;
    end;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetMinimized(const Value: Boolean);
begin
  if Value <> FMinimized then
  begin
    if csLoading in ComponentState then
    begin
      FMinimized := Value;
      Exit;
    end;

    FindControl;
    if FControl = nil then
      Exit;

    if Value then
    begin
      if FMaximized then
        FMaximized := False
      else
      begin
        case Align of
          alLeft, alRight:
            FRestorePos := FControl.Width;
          alTop, alBottom:
            FRestorePos := FControl.Height;
        else
          Exit;
        end;
      end;
      FMinimized := Value;
      // Just use something insanely large to get it to move to the other extreme
      case Align of
        alLeft, alBottom:
          UpdateControlSize(-3000);
        alRight, alTop:
          UpdateControlSize(3000);
      else
        Exit;
      end;
      DoMinimize;
    end
    else
    begin
      FMinimized := Value;
      UpdateControlSize(FRestorePos);
      DoRestore;
    end;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetShowButton(const Value: Boolean);
begin
  if Value <> FShowButton then
  begin
    FShowButton := Value;
    SetRectEmpty(FLastKnownButtonRect);
    Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetTextureColor1(const Value: TColor);
begin
  if FTextureColor1 <> Value then
  begin
    FTextureColor1 := Value;
    if (ButtonStyle = bsNetscape) and ShowButton then
      Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetTextureColor2(const Value: TColor);
begin
  if FTextureColor2 <> Value then
  begin
    FTextureColor2 := Value;
    if (ButtonStyle = bsNetscape) and ShowButton then
      Invalidate;
  end;
end;

procedure TJvCustomNetscapeSplitter.SetWindowsButtons(const Value: TJvWindowsButtons);
begin
  FWindowsButtons := Value;
  if (ButtonStyle = bsWindows) and ShowButton then
    Invalidate;
end;

procedure TJvCustomNetscapeSplitter.StoreOtherProperties(Writer: TWriter);
begin
  Writer.WriteInteger(RestorePos);
end;

procedure TJvCustomNetscapeSplitter.UpdateControlSize(NewSize: Integer);

  procedure MoveViaMouse(FromPos, ToPos: Integer; Horizontal: Boolean);
  begin
    if Horizontal then
    begin
      MouseDown(mbLeft, [ssLeft], FromPos, 0);
      MouseMove([ssLeft], ToPos, 0);
      MouseUp(mbLeft, [ssLeft], ToPos, 0);
    end
    else
    begin
      MouseDown(mbLeft, [ssLeft], 0, FromPos);
      MouseMove([ssLeft], 0, ToPos);
      MouseUp(mbLeft, [ssLeft], 0, ToPos);
    end;
  end;

begin
  if FControl <> nil then
  begin
    { You'd think that using FControl directly would be the way to change it's
      position (and thus the splitter's position), wouldn't you?  But, TSplitter
      has this nutty idea that the only way a control's size will change is if
      the mouse moves the splitter.  If you size the control manually, the
      splitter has an internal variable (FOldSize) that will not get updated.
      Because of this, if you try to then move the newly positioned splitter
      back to the old position, it won't go there (NewSize <> OldSize must be
      True).  Now, what are the odds that the user will move the splitter back
      to the exact same pixel it used to be on?  Normally, extremely low.  But,
      if the splitter has been restored from it's minimized position, it then
      becomes quite likely:  i.e. they drag it back all the way to the min
      position.  What a pain. }
    case Align of
      alLeft:
        MoveViaMouse(Left, FControl.Left + NewSize, True);
              // alLeft: FControl.Width := NewSize;
      alTop:
        MoveViaMouse(Top, FControl.Top + NewSize, False);
             // FControl.Height := NewSize;
      alRight:
        MoveViaMouse(Left, (FControl.Left + FControl.Width - Width) - NewSize, True);
        {begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - NewSize);
            FControl.Width := NewSize;
          finally
            Parent.EnableAlign;
          end;
        end;}
      alBottom:
        MoveViaMouse(Top, (FControl.Top + FControl.Height - Height) - NewSize, False);
        {begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - NewSize);
            FControl.Height := NewSize;
          finally
            Parent.EnableAlign;
          end;
        end;}
    end;
    Update;
  end;
end;

function TJvCustomNetscapeSplitter.VisibleWinButtons: Integer;
var
  X: TJvWindowsButton;
begin
  Result := 0;
  for X := Low(TJvWindowsButton) to High(TJvWindowsButton) do
    if X in WindowsButtons then
      Inc(Result);
end;

function TJvCustomNetscapeSplitter.WindowButtonHitTest(X, Y: Integer): TJvWindowsButton;
var
  BtnRect: TRect;
  I: Integer;
  B: TJvWindowsButton;
  WinButton: array [0..2] of TJvWindowsButton;
  BW: Integer;
  BRs: array [0..2] of TRect;
begin
  Result := wbMin;
  // Figure out which one was hit.  This function assumes ButtonHitTest has
  // been called and returned True.
  BtnRect := ButtonRect; // So we don't repeatedly call GetButtonRect
  I := 0;
  if Align in [alLeft, alRight] then
  begin
    for B := High(TJvWindowsButton) downto Low(TJvWindowsButton) do
      if B in WindowsButtons then
      begin
        WinButton[I] := B;
        Inc(I);
      end;
  end
  else
    for B := Low(TJvWindowsButton) to High(TJvWindowsButton) do
      if B in WindowsButtons then
      begin
        WinButton[I] := B;
        Inc(I);
      end;

  if Align in [alLeft, alRight] then
    BW := BtnRect.Right - BtnRect.Left
  else
    BW := BtnRect.Bottom - BtnRect.Top;
  FillChar(BRs, SizeOf(BRs), 0);
  for I := 0 to VisibleWinButtons - 1 do
    if ((Align in [alLeft, alRight]) and PtInRect(Bounds(BtnRect.Left,
      BtnRect.Top + (BW * I), BW, BW), Point(X, Y))) or ((Align in [alTop,
      alBottom]) and PtInRect(Bounds(BtnRect.Left + (BW * I), BtnRect.Top, BW,
        BW), Point(X, Y))) then
    begin
      Result := WinButton[I];
      break;
    end;
end;

procedure TJvCustomNetscapeSplitter.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if FRestorePos < 0 then
  begin
    FindControl;
    if FControl <> nil then
      case Align of
        alLeft, alRight:
          FRestorePos := FControl.Width;
        alTop, alBottom:
          FRestorePos := FControl.Height;
      end;
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

