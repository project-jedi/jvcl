{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaptionButton.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ @abstract(TJvCaptionButton lets you put your own custom button in the caption area of a form.) }

unit JvCaptionButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, CommCtrl,
  JvComponent, JvWndProcHook;

type
  TJvStandardButton = (tsbNone, tsbClose, tsbHelp, tsbMax, tsbMin, tsbRestore);

  TJvCaptionButton = class(TJvComponent)
  private
    FGlyph: TBitmap;
    FIl: TImageList;
    FCaption: string;
    FOnClick: TNotifyEvent;
    FButtonRect: TRect;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FDrawFlags: Integer;
    FAlignment: TAlignment;
    FToggle: Boolean;
    FDown: Boolean;
    FStandard: TJvStandardButton;
    FFont: TFont;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    procedure SetFont(Value: TFont);
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(Value: string);
    procedure SetStandard(Value: TJvStandardButton);
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetGlyph(Value: TBitmap);
    function WndProc(var Msg: TMessage): Boolean;
    procedure GetWndProc;
    procedure ResetWndProc;
    procedure DrawButton;
    procedure EraseButton;
    {Paint-related messages}
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCActivate(var Msg: TWMNCActivate); message WM_NCACTIVATE;
    {Mouse down-related messages}
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Msg: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Msg: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    function GetParentForm: TCustomForm;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    property ParentForm: TCustomForm read GetParentForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetButton;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Font: TFont read FFont write SetFont;
    property ButtonLeft: Integer read FLeft write SetLeft default 1;
    property ButtonTop: Integer read FTop write SetTop default 1;
    property ButtonWidth: Integer read FWidth write SetWidth;
    property ButtonHeight: Integer read FHeight write SetHeight;
    property Toggle: Boolean read FToggle write FToggle default False;
    property Down: Boolean read FDown;
    property Standard: TJvStandardButton read FStandard write SetStandard default tsbNone;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
  end;

const
  htCaptionButton = htSizeLast + 1;

implementation

constructor TJvCaptionButton.Create(AOwner: TComponent);
var
  SysInfoPara: TNonClientMetrics;
begin
  inherited Create(AOwner);
  SysInfoPara.cbSize := sizeof(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @SysInfoPara, 0);
  with SysInfoPara do
  begin
    FLeft := iCaptionWidth * 4 + 4;
    FTop := GetSystemMetrics(SM_CXFRAME) + 2;
    FWidth := iCaptionWidth;
    FHeight := iCaptionHeight - 4;
  end;
  FGlyph := TBitmap.Create;
  FFont := TFont.Create;
  FIl := TImageList.CreateSize(FWidth, FHeight);
  FDown := False;
  FToggle := False;
  FStandard := tsbNone;
  FDrawFlags := 0;
  FCaption := '';
  FAlignment := taLeftJustify;
  ResetButton;
end;

destructor TJvCaptionButton.Destroy;
begin
  ResetWndProc;
  FGlyph.Free;
  FIl.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TJvCaptionButton.ResetButton;
begin
  ResetWndProc;
  GetWndProc;
  EraseButton;
end;

procedure TJvCaptionButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TJvCaptionButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TJvCaptionButton.SetFont(Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    EraseButton;
  end;
end;

procedure TJvCaptionButton.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    EraseButton;
  end;
end;

procedure TJvCaptionButton.SetCaption(Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    EraseButton;
  end;
end;

procedure TJvCaptionButton.SetStandard(Value: TJvStandardButton);
begin
  if FStandard <> Value then
  begin
    FStandard := Value;
    case Value of
      tsbNone:
        FDrawFlags := 0;
      tsbClose:
        FDrawFlags := DFCS_CAPTIONCLOSE;
      tsbHelp:
        FDrawFlags := DFCS_CAPTIONHELP;
      tsbMax:
        FDrawFlags := DFCS_CAPTIONMAX;
      tsbMin:
        FDrawFlags := DFCS_CAPTIONMIN;
      tsbRestore:
        FDrawFlags := DFCS_CAPTIONRESTORE;
    end;
    EraseButton;
  end;
end;

procedure TJvCaptionButton.SetLeft(Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    EraseButton;
  end;
//  DrawButton;
end;

procedure TJvCaptionButton.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    EraseButton;
  end;
//  DrawButton;
end;

procedure TJvCaptionButton.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    EraseButton;
  end;
//  DrawButton;
end;

procedure TJvCaptionButton.SetTop(Value: Integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    EraseButton;
  end;
//  DrawButton;
end;

procedure TJvCaptionButton.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  if not FGlyph.Empty then
  begin
    FIl.Clear;
    FIl.Width := FGlyph.Width;
    FIl.Height := FGlyph.Height;
    FIl.AddMasked(FGlyph, FGlyph.TransparentColor);
  end;
  EraseButton;
end;

procedure TJvCaptionButton.DrawButton;
var
  X, Y, tmpFlags: Integer;
  Canvas: TCanvas;
  R: TRect;
  P: TCustomForm;
begin
  P := ParentForm;
  if P = nil then
    Exit;
  Canvas := TControlCanvas.Create;
  Canvas.Handle := GetWindowDC(P.Handle);

  FButtonRect := Bounds(P.Width - FLeft, FTop, FWidth, FHeight);

  if FDown then
    tmpFlags := FDrawFlags or DFCS_PUSHED
  else
    tmpFlags := FDrawFlags;

  DrawButtonFace(Canvas, FButtonRect, 1, bsAutoDetect, False, FDown, False);
  if FStandard <> tsbNone then
    DrawFrameControl(Canvas.Handle, FButtonRect, DFC_CAPTION, tmpFlags)
  else
  begin
    R := FButtonRect;
    X := 0;
    if Assigned(FGlyph) then
    begin
      Y := ((R.Bottom - R.Top) - FGlyph.Height) div 2;
      case FAlignment of
        taLeftJustify:
          X := FWidth - (FGlyph.Width) - 4;
        taRightJustify:
          X := 4;
        taCenter:
          X := ((FWidth - FGlyph.Width) div 2);
      end;
{      ImageList_DrawEx(FIl.Handle,0,Canvas.Handle,
         FButtonRect.Left + X + Ord(FDown),FButtonRect.Top + Y + Ord(FDown),0,0,
           clNone,clNone,ILD_TRANSPARENT);}
      Canvas.Draw(FButtonRect.Left + X + Ord(FDown), FButtonRect.Top + Y + Ord(FDown), FGlyph);
    end;

    if Length(FCaption) > 0 then
    begin
      Canvas.Font.Height := FFont.Height;
      case FAlignment of
        taLeftJustify:
          Inc(R.Left, 4);
        taRightJustify:
          R.Left := R.Right - (Canvas.TextWidth(FCaption) + 4);
        taCenter:
          R.Left := R.Left + (FWidth - Canvas.TextWidth(FCaption)) div 2;
      end;
      SetBkMode(Canvas.Handle, Windows.Transparent);
      OffsetRect(R, Ord(FDown), Ord(FDown));
      DrawText(Canvas.Handle, PChar(FCaption), -1, R, DT_NOPREFIX);
    end;

  end;

  ReleaseDC(P.Handle, Canvas.Handle);
  Canvas.Handle := 0;
  Canvas.Free;
end;

{Paint triggering events}

procedure TJvCaptionButton.WMNCActivate(var Msg: TWMNCActivate);
begin
  inherited;
  EraseButton;
//  DrawButton;
end;

{ Painting events }

procedure TJvCaptionButton.WMNCPaint(var Msg: TWMNCPaint);
begin
  inherited;
  DrawButton;
end;

procedure TJvCaptionButton.WMSize(var Msg: TWMSize);
begin
  inherited;
  ParentForm.Perform(WM_NCACTIVATE, Word(ParentForm.Active), 0);
  EraseButton;
end;

procedure TJvCaptionButton.WMSetText(var Msg: TWMSetText);
begin
  inherited;
  EraseButton;
end;

{Mouse-related procedures}

procedure TJvCaptionButton.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TCustomForm;
begin
  inherited;
  P := ParentForm;
  {Check to see if the mouse was clicked in the area of the button}
  if PtInRect(FButtonRect, Point(Msg.XPos - P.Left, Msg.YPos - P.Top)) then
    Msg.Result := htCaptionButton
  else
    FDown := False;
//   DrawButton;
end;

procedure TJvCaptionButton.WMNCLButtonDown(var Msg: TWMNCLButtonDown);
var
  P: TPoint;
  PF: TCustomForm;
begin
  inherited;
  PF := ParentForm;
  P := SmallPointToPoint(SmallPoint(Msg.XCursor, Msg.YCursor));
  P := Point(P.X - PF.Left + 2, P.Y - PF.Top + 2);
  if (Msg.HitTest = htCaptionButton) and PtInRect(FButtonRect, P) then
  begin
    if FToggle then
      FDown := not FDown
    else
      FDown := True;
    with TWMMouse(Msg) do
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
    SetCapture(PF.Handle);
    DrawButton;
  end
  else
  if FDown then
  begin
    FDown := False;
    DrawButton;
  end;
end;

procedure TJvCaptionButton.WMNCLButtonUp(var Msg: TWMNCLButtonUp);
begin
  inherited;
  if FDown {and (Msg.HitTest = htCaptionButton)} then
  begin
    with TWMMouse(Msg) do
      MouseUp(mbLeft, KeysToShiftState(Keys), XPos, YPos);
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
  if not FToggle then
    FDown := False;
  ReleaseCapture;
  DrawButton;
end;

procedure TJvCaptionButton.GetWndProc;
var
  P: TCustomForm;
begin
  P := ParentForm;
  if P <> nil then
    RegisterWndProcHook(P, WndProc, hoAfterMsg);
end;

procedure TJvCaptionButton.ResetWndProc;
var
  P: TCustomForm;
begin
  P := ParentForm;
  if P <> nil then
    UnregisterWndProcHook(P, WndProc, hoAfterMsg);
  EraseButton;
end;

function TJvCaptionButton.WndProc(var Msg: TMessage): Boolean;
begin
  { let others listen in too }
  Result := False;
  case Msg.Msg of
    WM_SETTEXT:
      WMSetText(TWmSetText(Msg));
    WM_NCPAINT:
      WMNCPaint(TWMNCPaint(Msg));
    WM_NCACTIVATE:
      WMNCActivate(TWMNCActivate(Msg));
    WM_NCHITTEST:
      WMNCHitTest(TWMNcHitTest(Msg));
    WM_NCLBUTTONDOWN, WM_LBUTTONDOWN:
      WMNCLButtondown(TWMNCLButtondown(Msg));
    WM_NCLBUTTONUP, WM_LBUTTONUP:
      WMNCLButtonUp(TWMNCLButtonUp(Msg));
    WM_SIZE, WM_WINDOWPOSCHANGED:
      WMSize(TWMSize(Msg));
  end;
end;

procedure TJvCaptionButton.EraseButton;
var
  P: TCustomForm;
begin
  P := ParentForm;
  if (P <> nil) and P.HandleAllocated then
    RedrawWindow(P.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE);
end;

function TJvCaptionButton.GetParentForm: TCustomForm;
begin
  if Owner is TControl then
    Result := Forms.GetParentForm(TControl(Owner))
  else
    Result := nil;
end;

end.

