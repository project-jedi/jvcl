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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ @abstract(TJvCaptionButton lets you put your own custom button in the caption area of a form.) }

unit JvCaptionButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,Buttons,
  CommCtrl, JvComponent;

type
  TJvStandardButton=(tsbNone,tsbClose,tsbHelp,tsbMax,tsbMin,tsbRestore);
  TJvCaptionButton = class(TJvComponent)
  private
    { Private declarations }
    FOwner:TForm;
    FGlyph:TBitmap;
    IL:TImageList;
    FCaption:string;
    FOnClick:TNotifyEvent;
    FButtonRect:TRect;
    FLeft,FTop,FWidth,FHeight:integer;
    FDrawFlags:integer;
    FAlignment:TAlignment;
    FToggle,FDown:boolean;
    FStandard:TJvStandardButton;
    FFont:TFont;
    FOnMouseDown:TMouseEvent;
    FOnMouseUp:TMouseEvent;
    FHandle    : THandle;
    FDefProc   : Pointer;
    FWndProc   : Pointer;
    procedure SetFont(Value:TFont);
    procedure SetAlignment(Value:TAlignment);
    procedure SetCaption(Value:string);
    procedure SetStandard(Value:TJvStandardButton);
    procedure SetLeft(Value:integer);
    procedure SetTop(Value:integer);
    procedure SetWidth(Value:integer);
    procedure SetHeight(Value:integer);
    procedure SetGlyph(Value:TBitmap);
    procedure WndProc(var Msg:TMessage);
    procedure GetWndProc;
    procedure ResetWndProc;
    procedure DrawButton;
    {Paint-related messages}
    procedure WMSize(var Msg:TWMSize); message WM_SIZE;
    procedure WMSetText(var Msg : TWMSetText); message WM_SETTEXT;
    procedure WMNCPaint(var Msg : TWMNCPaint); message WM_NCPAINT;
    procedure WMNCActivate(var Msg : TWMNCActivate); message WM_NCACTIVATE;
    {Mouse down-related messages}
    procedure WMNCHitTest(var Msg : TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Msg : TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Msg : TWMNCLButtonUp); message WM_NCLBUTTONUP;
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure ResetButton;
  published
    { Published declarations }
    property Alignment:TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption:string read FCaption write SetCaption;
    property Glyph:TBitmap read FGlyph write SetGlyph;
    property Font:TFont read FFont write SetFont;
    property ButtonLeft:integer read FLeft write SetLeft default 1;
    property ButtonTop:integer read FTop write SetTop default 1;
    property ButtonWidth:integer read FWidth write SetWidth;
    property ButtonHeight:integer read FHeight write SetHeight;
    property Toggle:boolean read FToggle write FToggle default False;
    property Down: boolean read FDown;
    property Standard:TJvStandardButton read FStandard write SetStandard default tsbNone;
    property OnClick:TNotifyEvent read FOnClick write FOnClick;
    property OnMouseUp:TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseDown:TMouseEvent read FOnMouseDown write FOnMouseDown;
  end;


const
  htCaptionButton = htSizeLast + 1;

implementation

constructor TJvCaptionButton.Create(AOwner:TComponent);
var SysInfoPara:TNonClientMetrics;
begin
  inherited Create(AOwner);
  SysInfoPara.cbSize := sizeof(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS,0,@SysInfoPara,0);
  with SysInfoPara do
  begin
    FLeft   := iCaptionWidth * 4 + 4;
    FTop    := GetSystemMetrics(SM_CXFRAME) + 2;
    FWidth  := iCaptionWidth;
    FHeight := iCaptionHeight - 4;
  end;
  FGlyph  := TBitmap.Create;
  FFont := TFont.Create;
  FOwner  := TForm(AOwner);
  FFont.Assign(FOwner.Font);
  IL := TImageList.CreateSize(FWidth,FHeight);
  FDown   := False;
  FToggle := False;
  FStandard  := tsbNone;
  FDrawFlags := 0;
  FCaption := '';
  FAlignment := taLeftJustify;
  ResetButton;
end;

procedure TJvCaptionButton.ResetButton;
begin
  if FHandle <> 0 then
    ResetWndProc;
  if Owner <> nil then
    FOwner := TForm(Owner);
  FHandle := 0;
  GetWndProc;
  DrawButton;
end;

destructor TJvCaptionButton.Destroy;
begin
  FGlyph.Free;
  IL.Free;
  ResetWndProc;
  FFont.Free;
  inherited Destroy;
end;

procedure TJvCaptionButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(self,Button,Shift,X,Y);
end;

procedure TJvCaptionButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(self,Button,Shift,X,Y);
end;


procedure TJvCaptionButton.SetFont(Value:TFont);
begin
  FFont.Assign(Value);
  DrawButton;
end;
  
procedure TJvCaptionButton.SetAlignment(Value:TAlignment);
begin
  if FAlignment <> Value then
    FAlignment := Value;
  DrawButton;
end;

procedure TJvCaptionButton.SetCaption(Value:string);
begin
  if FCaption <> Value then
    FCaption := Value;
  DrawButton;
end;

procedure TJvCaptionButton.SetStandard(Value:TJvStandardButton);
begin
  if FStandard <> Value then
  begin
    FStandard := Value;
    case Value of
      tsbNone: FDrawFlags := 0;
      tsbClose: FDrawFlags := DFCS_CAPTIONCLOSE;
      tsbHelp: FDrawFlags := DFCS_CAPTIONHELP;
      tsbMax: FDrawFlags := DFCS_CAPTIONMAX;
      tsbMin: FDrawFlags := DFCS_CAPTIONMIN;
      tsbRestore: FDrawFlags := DFCS_CAPTIONRESTORE;
    end;
  end;
  DrawButton;
end;

procedure TJvCaptionButton.SetLeft(Value:integer);
begin
  if FLeft <> Value then
    FLeft := Value;
  DrawButton;
end;

procedure TJvCaptionButton.SetWidth(Value:integer);
begin
  if FWidth <> Value then
    FWidth := Value;
  DrawButton;
end;

procedure TJvCaptionButton.SetHeight(Value:integer);
begin
  if FHeight <> Value then
    FHeight := Value;
  DrawButton;
end;


procedure TJvCaptionButton.SetTop(Value:integer);
begin
  if FTop <> Value then
    FTop := Value;
  DrawButton;
end;

procedure TJvCaptionButton.SetGlyph(Value:TBitmap);
begin
  FGlyph.Assign(Value);
  if not FGlyph.Empty then
  begin
    IL.Clear;
    IL.Width := FGlyph.Width;
    IL.Height := FGlyph.Height;
    Il.AddMasked(FGlyph,FGlyph.TransparentColor);
  end;
  DrawButton;
end;


procedure TJvCaptionButton.DrawButton;
var X,Y,tmpFlags:integer; Canvas:TCanvas;R:TRect;
begin

  Canvas := TControlCanvas.Create;
  Canvas.Handle := GetWindowDC(FOwner.Handle);

  FButtonRect := Bounds(FOwner.Width - FLeft, FTop,FWidth,FHeight);

  if FDown then
    tmpFlags := FDrawFlags or DFCS_PUSHED
  else
    tmpFlags := FDrawFlags;

  DrawButtonFace(Canvas, FButtonRect, 1, bsAutoDetect, False, FDown, False);
  if (FStandard <> tsbNone) then
    DrawFrameControl(Canvas.Handle,FButtonRect,DFC_CAPTION,tmpFlags)
  else
  begin
    R := FButtonRect;
    X := 0;
    if Assigned(FGlyph) then
    begin
      Y := ((R.Bottom - R.Top) - FGlyph.Height) div 2;
      case FAlignment of
        taLeftJustify:  X := FWidth - (FGlyph.Width) - 4;
        taRightJustify: X := 4;
        taCenter:       X := ((FWidth - FGlyph.Width) div 2);
      end;
{      ImageList_DrawEx(IL.Handle,0,Canvas.Handle,
         FButtonRect.Left + X + Ord(FDown),FButtonRect.Top + Y + Ord(FDown),0,0,
           clNone,clNone,ILD_TRANSPARENT);}
      Canvas.Draw(FButtonRect.Left + X + Ord(FDown),FButtonRect.Top + Y + Ord(FDown),FGlyph);
    end;

    if Length(FCaption) > 0 then
    begin
      Canvas.Font.Height := FFont.Height;
      case FAlignment of
        taLeftJustify:  Inc(R.Left,4);
        taRightJustify: R.Left := R.Right - (Canvas.TextWidth(FCaption) + 4);
        taCenter:       R.Left := R.Left + (FWidth - Canvas.TextWidth(FCaption)) div 2;
      end;
      SetBkMode(Canvas.Handle,Windows.Transparent);
      OffsetRect(R,Ord(FDown),Ord(FDown));
      DrawText(Canvas.Handle,PChar(FCaption),-1,R,DT_NOPREFIX);
    end;

  end;

  ReleaseDC(FOwner.Handle, Canvas.Handle);
  Canvas.Handle := 0;
  Canvas.Free;
end;

{Paint triggering events}
procedure TJvCaptionButton.WMNCActivate(var Msg : TWMNCActivate);
begin
  inherited;
  DrawButton;
end;

{ Painting events }

procedure TJvCaptionButton.WMNCPaint(var Msg : TWMNCPaint);
begin
  inherited;
  DrawButton;
end;

procedure TJvCaptionButton.WMSize(var Msg:TWMSize);
begin
  inherited;
  FOwner.Perform(WM_NCACTIVATE, Word(FOwner.Active), 0);
  DrawButton;
end;

procedure TJvCaptionButton.WMSetText(var Msg : TWMSetText);
begin
  inherited;
  DrawButton;
end;

{Mouse-related procedures}
procedure TJvCaptionButton.WMNCHitTest(var Msg : TWMNCHitTest);
begin
  Inherited;
     {Check to see if the mouse was clicked in the area of the button}
   if PtInRect(FButtonRect, Point(Msg.XPos - FOwner.Left, Msg.YPos - FOwner.Top)) then
      Msg.Result := htCaptionButton;
//   DrawButton;
end;

procedure TJvCaptionButton.WMNCLButtonDown(var Msg : TWMNCLButtonDown);
begin
  inherited;
  if (Msg.HitTest = htCaptionButton) then
  begin
    if FToggle then
      FDown := not FDown
    else
      FDown := True;
    with TWMMouse(Msg) do
      MouseDown(mbLeft,KeysToShiftState(Keys),XPos,YPos);
  end
  else
   FDown := False;
  DrawButton;
end;

procedure TJvCaptionButton.WMNCLButtonUp(var Msg : TWMNCLButtonUp);
begin
  inherited;
  if (Msg.HitTest = htCaptionButton) then
  begin
    with TWMMouse(Msg) do
      MouseUp(mbLeft,KeysToShiftState(Keys),XPos,YPos);
    if FDown then if Assigned(FOnClick) then FOnClick(self);
  end;
  if not FToggle then FDown := False;
  DrawButton;
end;


procedure TJvCaptionButton.GetWndProc;
begin
  if Owner is TForm then
  begin
    FHandle := TForm(Owner).Handle;
    FWndProc :=  {$IFDEF DELPHI6_UP}Classes.{$ENDIF}MakeObjectInstance(WndProc);
    FDefProc := Pointer(GetWindowLong(FHandle,GWL_WNDPROC ));
    SetWindowLong(FHandle,GWL_WNDPROC,longint(FWndProc));
  end;
end;

procedure TJvCaptionButton.ResetWndProc;
begin
  if FHandle <> 0 then
  begin
    SetWindowLong(FHandle,GWL_WNDPROC,longint(FDefProc));
     {$IFDEF DELPHI6_UP}Classes.{$ENDIF}FreeObjectInstance(FWndProc);
  end
end;

procedure TJvCaptionButton.WndProc(var Msg:TMessage);
begin
  { allways let Windows do it's thing }
  with Msg do
    Result := CallWindowProc( FDefProc, FHandle, Msg, WParam, LParam);
  case Msg.Msg of
    WM_DESTROY:
    begin
      ResetWndProc;
      FHandle := 0;
    end;
    WM_SETTEXT:                      WMSetText(TWmSetText(Msg));
    WM_NCPAINT:                      WMNCPaint(TWMNCPaint(Msg));
    WM_NCACTIVATE:                   WMNCActivate(TWMNCActivate(Msg));
    WM_NCHITTEST:                    WMNCHitTest(TWMNcHitTest(Msg));
    WM_NCLBUTTONDOWN,WM_LBUTTONDOWN: WMNCLButtondown(TWMNCLButtondown(Msg));
    WM_NCLBUTTONUP,WM_LBUTTONUP:     WMNCLButtonUp(TWMNCLButtonUp(Msg));
    WM_SIZE,WM_WINDOWPOSCHANGED:     WMSize(TWMSize(Msg));
  end;
end;


end.
