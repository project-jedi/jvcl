{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCaption.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgCaption;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  JvComponent,
  JvgTypes,
  JvgUtils,
  JvgCommClasses,
  StdCtrls,
  ExtCtrls;

type
  TJvgCaption = class(TJvComponent)
  private
    FExcludeButtons: boolean;
    FExcludeIcon: boolean;
    FCaptBox: TJvgBevel;
    FTextBox: TJvgBevel;
    FIconBox: TJvgBevel;
    FPrevWndProc: Pointer;
    FNewWndProc: Pointer;
    //    FParent		  : TForm;
    FCaptionColor: TColor;
    FTextStyle: TglTextStyle;
    FFont: TFont;
    FTexture, bmp: TBitmap;
    FImage: TImage;
    FTextureTransparent: boolean;
    FAutoTrColor: TglAutoTransparentColor;
    FTransparentColor: TColor;

    FGlyphClose: TBitmap;
    OwnerWidth: integer;
    BtnCount: integer;
    CloseRect: TRect;
    _CYCAPTION: integer;
    _CXFRAME: integer;
    _CYFRAME: integer;
    _CXSMICON: integer;
    _CYSMICON: integer;
    _CXICON: integer;
    _CYICON: integer;

    procedure SetExcludeIcon(Value: boolean);
    procedure SetExcludeButtons(Value: boolean);
    procedure SetCaptionColor(Value: TColor);
    procedure SetTextStyle(Value: TglTextStyle);
    procedure SetFont(Value: TFont);
    procedure SetTexture(Value: TBitmap);
    procedure SetImage(Value: TImage);
    function GetTexture: TBitmap;
    procedure SetTextureTransparent(Value: boolean);
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetTransparentColor(Value: TColor);

    procedure Repaint;
    procedure DrawIcon(DC: HDC; R: TRect);
    function DrawCaption(DrawAll: boolean): TRect;
    procedure ParentWindowHookProc(var Msg_: TMessage);
    procedure SetParentWindowHook;
    procedure FreeParentWindowHook;
    function CountCaptionButtons: integer;
    procedure SmthChanged(Sender: TObject);
  protected
    //    procedure WndProc(var Message: TMessage);override;
    procedure Loaded; override;
    procedure Notification(Component: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //    property Parent: TForm read Fparent write SetParent;
    property ExcludeButtons: boolean
      read FExcludeButtons write SetExcludeButtons default true;
    property ExcludeIcon: boolean
      read FExcludeIcon write SetExcludeIcon default false;
    property CaptionColor: TColor
      read FCaptionColor write SetCaptionColor default clBtnFace;
    property TextStyle: TglTextStyle
      read FTextStyle write SetTextStyle default fstRaised;
    property Font: TFont read FFont write SetFont;
    property CaptBox: TJvgBevel read FCaptBox write FCaptBox;
    property TextBox: TJvgBevel read FTextBox write FTextBox;
    property IconBox: TJvgBevel read FIconBox write FIconBox;
    property Texture: TBitmap read GetTexture write SetTexture;
    property Image: TImage read FImage write SetImage;
    property TextureTransparent: boolean read FTextureTransparent write
      SetTextureTransparent
      default false;
    property AutoTransparentColor: TglAutoTransparentColor
      read FAutoTrColor write SetAutoTrColor default ftcLeftBottomPixel;
    property TransparentColor: TColor read FTransparentColor write
      SetTransparentColor
      default clBlack;
  end;

  {$DEFINE GL_CAPT_BUTTONS}
implementation
{$IFDEF GL_CAPT_BUTTONS}
{$R ..\Resources\JvgCaption.res}
{$ENDIF}

//==============================================================

constructor TJvgCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptBox := TJvgBevel.Create;
  FTextBox := TJvgBevel.Create;
  FIconBox := TJvgBevel.Create;
  FFont := TFont.Create;
  FExcludeButtons := true;
  FExcludeIcon := false;
  FCaptionColor := clBtnFace;
  FTextStyle := fstRaised;
  FTextBox.Inner := bvRaised;
  FIconBox.Inner := bvNone;
  FIconBox.Outer := bvNone;
  FTextureTransparent := false;
  FAutoTrColor := ftcLeftBottomPixel;
  FCaptBox.OnChanged := SmthChanged;
  FTextBox.OnChanged := SmthChanged;
  FIconBox.OnChanged := SmthChanged;

  //  FParent:=nil;
  if not (AOwner is TForm) then
    exit; //FParent:=TForm(AOwner) else exit;

  {$IFDEF GL_CAPT_BUTTONS}
  //if (csDesigning in ComponentState)and not(csLoading in ComponentState)then
  begin
    FGlyphClose := TBitmap.Create;
    FGlyphClose.LoadFromResourceName(hInstance, 'CLOSE');
  end;
  {$ENDIF}

  _CYCAPTION := GetSystemMetrics(SM_CYCAPTION);
  _CYFRAME := GetSystemMetrics(SM_CYFRAME);
  _CXFRAME := GetSystemMetrics(SM_CXFRAME);
  _CXSMICON := GetSystemMetrics(SM_CXSMICON);
  _CYSMICON := GetSystemMetrics(SM_CYSMICON);
  _CXICON := GetSystemMetrics(SM_CXICON);
  _CYICON := GetSystemMetrics(SM_CYICON);

  SetParentWindowHook;
end;
//-----

destructor TJvgCaption.Destroy;
begin
  FFont.Free;
  FCaptBox.Free;
  FTextBox.Free;
  FIconBox.Free;
  if Assigned(FTexture) then
    FTexture.Free;
  if Assigned(FGlyphClose) then
    FGlyphClose.Free;
  FreeParentWindowHook;
  inherited Destroy;
end;

procedure TJvgCaption.Loaded;
begin
  inherited;
  if Assigned(FTexture) and (not FTexture.Empty) then
    Bmp := FTexture;
end;

procedure TJvgCaption.Notification(Component: TComponent; Operation:
  TOperation);
begin
  if (Component <> Self) and (Operation = opInsert) and (Component is
    TJvgCaption) then
    raise
      Exception.Create('Cannot create more than one instance of TJvgCaption component');
end;
//=========================================================.special procs.

procedure TJvgCaption.SetParentWindowHook;
var
  P: Pointer;
begin
  P := Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC));
  if (P <> FNewWndProc) then
  begin
    FPrevWndProc := P;
    FNewWndProc := MakeObjectInstance(ParentWindowHookProc);
    SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(FNewWndProc));
  end;
end;
//==============================================================

procedure TJvgCaption.FreeParentWindowHook;
begin
  if (FNewWndProc <> nil) and (FPrevWndProc <> nil)
    and (Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC)) =
    FNewWndProc) then
  begin
    //Repaint;
    SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
    FNewWndProc := nil;
  end;
end;
//==============================================================

procedure TJvgCaption.ParentWindowHookProc(var Msg_: TMessage);
var
  pt: TPoint;

  procedure DefaultProc; //___________________________________
  begin
    with Msg_ do
      Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg,
        WParam, LParam);
  end;

begin //_______________________________________________________
  OwnerWidth := TForm(Owner).Width;
  with Msg_ do
    case Msg of
      //	WM_CREATE: if TForm(Owner)<>nil then FreeParentWindowHook;
      WM_NCPAINT,
        //	WM_MOUSEMOVE,
//WM_MOUSEACTIVATE,
      WM_MOUSEACTIVATE,
        WM_NCACTIVATE,
        WM_SYSCOLORCHANGE,
        //	WM_NCLBUTTONUP,
      WM_NCLBUTTONDBLCLK,
        WM_SIZE:
        begin
          DefaultProc;
          DrawCaption(true);
        end;
      WM_NCLBUTTONDOWN:
        begin
          DefaultProc;
          DrawCaption(false);
        end;
      WM_LBUTTONUP:
        begin
          DefaultProc;
          {$IFDEF GL_CAPT_BUTTONS}
          GetCursorPos(pt);
          dec(pt.x, TForm(Owner).left);
          dec(pt.y, TForm(Owner).top);
          if PtInRect(CloseRect, pt) then
            SendMessage(TForm(Owner).Handle, WM_CLOSE, 0, 0);
          {$ENDIF}
        end;
      WM_NCHITTEST:
        begin
          {$IFDEF GL_CAPT_BUTTONS}
          pt := {TForm(Owner).ScreenToClient}(Point(Loword(lParam) -
            TForm(Owner).left, Hiword(lParam) - TForm(Owner).Top));
          if PtInRect(CloseRect, pt) then
          begin
            result := HTCLIENT;
            exit;
          end;
          {$ENDIF}
          DefaultProc;
          if (Result = HTLEFT) or (Result = HTRIGHT) or (Result = HTTOP)
            or (Result = HTBOTTOM) or (Result = HTBOTTOMLEFT)
            or (Result = HTBOTTOMRIGHT) or (Result = HTTOPLEFT)
            or (Result = HTTOPRIGHT) then
          begin
            DrawCaption(false);
          end;
        end;
      //	WM_SETTEXT: DrawCaption( false );
      //	WM_ACTIVATE: DrawCaption;
      WM_DESTROY:
        begin
          FreeParentWindowHook;
          DefaultProc;
        end;
    else
      DefaultProc;
    end;
end;
//==============================================================

procedure TJvgCaption.DrawIcon(DC: HDC; R: TRect);
var
  IconHandle: HIcon;
  IconDC: HDC;
  OldIconBMP, IconBMP: HBitmap;
  Brush, OldBrush: HBrush;
begin

  with TForm(Owner) do
    if Icon.Handle <> 0 then
      IconHandle := Icon.Handle
    else if Application.Icon.Handle <> 0 then
      IconHandle := Application.Icon.Handle
    else
      IconHandle := LoadIcon(0, IDI_APPLICATION);

  IconDC := CreateCompatibleDC(DC);
  IconBMP := CreateCompatibleBitmap(DC, _CXICON, _CYICON);
  OldIconBMP := SelectObject(IconDC, IconBMP);
  Brush := CreateSolidBrush(ColorToRGB(CaptionColor));
  OldBrush := SelectObject(IconDC, Brush);
  //  FillRect( IconDC, R, Brush );
  PatBlt(IconDC, 0, 0, _CXICON, _CYICON, PATCOPY);

  Windows.DrawIcon(IconDC, 0, 0, IconHandle);
  StretchBlt(DC, R.Left, R.Top, R.Bottom - R.Top, R.Bottom - R.Top, IconDC,
    0, 0, _CXICON, _CYICON, SRCCOPY);

  DeleteObject(SelectObject(IconDC, OldIconBMP));
  DeleteObject(SelectObject(IconDC, OldBrush));
  DeleteDC(IconDC);
end;

//==============================================================

function TJvgCaption.DrawCaption(DrawAll: boolean): TRect;
var
  DC: HDC;
  R, IconR: TRect;
  x, y, x_, y_, IWidth, IHeight: integer;
begin
  DC := GetWindowDC(TForm(Owner).Handle);
  try
    GetWindowRect(TForm(Owner).Handle, R);
    OwnerWidth := R.Right - R.left;

    R.Left := _CXFRAME - 1;
    R.Top := _CYFRAME - 1;
    R.Right := OwnerWidth - _CXFRAME;
    R.Bottom := R.Top + _CYCAPTION - 1;

    BtnCount := CountCaptionButtons;
    if (BtnCount = 0) and (not DrawAll) then
      exit;
    R := DrawBoxEx(DC, R, FCaptBox.Sides, FCaptBox.Inner, FCaptBox.Outer,
      FCaptBox.Bold, CaptionColor, true);
    if not DrawAll then
      exit;

    if (not FExcludeIcon) and (biSystemMenu in TForm(Owner).BorderIcons) then
    begin
      IconR := Rect(R.Left, R.Top, R.Left + _CXSMICON + 3, R.Top +
        _CYSMICON);
      IconR := DrawBoxEx(DC, IconR, FIconBox.Sides, FIconBox.Inner,
        FIconBox.Outer, FIconBox.Bold, CaptionColor, false);
      DrawIcon(DC, IconR);
      inc(R.Left, _CXSMICON + 4);
    end;

    dec(R.Right, BtnCount * (_CXSMICON + 1));
    if BtnCount <> 0 then
      dec(R.Right, 4);
    R := DrawBoxEx(DC, R, FTextBox.Sides, FTextBox.Inner, FTextBox.Outer,
      FTextBox.Bold, CaptionColor, true);

    with TForm(Owner).Canvas do
    begin
      inc(R.right);
      inc(R.bottom);
      Brush.Color := CaptionColor {clActiveCaption};
      Brush.Style := bsSolid;
      Windows.FillRect(DC, R, Brush.Handle);
    end;
    inc(R.Left, 2);

    if IsItAFilledBitmap(bmp) then
    begin
      x := r.Left - 2;
      y := r.top;
      IHeight := r.bottom - r.top;
      IWidth := r.Right - r.Left;
      x_ := x;
      y_ := y;
      {      while x < IWidth do
            begin
       while y < IHeight do begin
         BitBlt(DC, x, y, min( IWidth, bmp.Width ), min( IHeight, bmp.Height ), bmp.Canvas.Handle, 0,0, SRCCOPY );
         Inc(y, min( IHeight, bmp.Height ));
       end;
       Inc(x, min( IWidth, bmp.Width ));
       y:=0;
            end;}
      while x_ < r.right do
      begin
        //IWidth:=SavedIWidth; SavedIWidth:=IWidth;
        if x_ + IWidth > r.right then
          IWidth := r.right - x_;
        while y_ < r.bottom do
        begin
          //	    IHeight:=SavedIHeight; SavedIHeight:=IHeight;
          if y_ + IHeight > r.bottom then
            IHeight := r.bottom - y_;
          BitBlt(DC, x_, y_, min(IWidth, bmp.Width), min(IHeight,
            bmp.Height), bmp.Canvas.Handle, 0, 0, SRCCOPY);
          Inc(y_, min(IHeight, bmp.Height));
        end;
        Inc(x_, min(IWidth, bmp.Width));
        y_ := y;
      end;
    end;
    //...draw close button
    {$IFDEF GL_CAPT_BUTTONS}
    if (BtnCount = 0) and (tag = 1) then
    begin
      CloseRect := Bounds(r.right - FGlyphClose.Width - 2, r.top,
        FGlyphClose.Width, FGlyphClose.Height);
      //      BitBlt( DC, r.right-FGlyphClose.Width-2, r.top, FGlyphClose.Width, FGlyphClose.Height, FGlyphClose.Canvas.Handle, 0,0, SRCCOPY );
      CreateBitmapExt(DC, FGlyphClose, R, r.right - FGlyphClose.Width - 8,
        r.top - 3,
        fwoNone, fdsDefault, true,
        GetPixel(FGlyphClose.Canvas.Handle, 0, FGlyphClose.Height - 1)
        {TransparentColor},
        0);
    end
    else
      CloseRect := Rect(0, 0, 0, 0);
    {$ENDIF}

    DrawTextInRect(DC, R, TForm(Owner).Caption, FTextStyle, FFont,
      DT_SINGLELINE or DT_VCENTER or DT_LEFT);
  finally
    ReleaseDC(TForm(Owner).Handle, DC);
  end;
  Result := R;
end;

function TJvgCaption.CountCaptionButtons: integer;
begin
  if not (biSystemMenu in TForm(Owner).BorderIcons) then
  begin
    Result := 0;
    exit;
  end;
  result := 1;
  if not (TForm(Owner).BorderStyle in [bsToolWindow, bsSizeToolWin, bsDialog]) then
  begin
    if (biMinimize in TForm(Owner).BorderIcons)
      or (biMaximize in TForm(Owner).BorderIcons) then
      inc(Result, 2)
    else if biHelp in TForm(Owner).BorderIcons then
      inc(Result);
  end;
end;

procedure TJvgCaption.SmthChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TJvgCaption.Repaint;
var
  RGN: HRGN;
begin
  RGN := CreateRectRgn(0, 0, TForm(Owner).Width, _CYCAPTION);
  SendMessage(THandle(TForm(Owner).Handle), WM_NCPAINT, HRGN(RGN), 0);
  DeleteObject(RGN);
end;
//============================================================PROPERTIES

procedure TJvgCaption.SetExcludeIcon(Value: boolean);
begin
  FExcludeIcon := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;

procedure TJvgCaption.SetExcludeButtons(Value: boolean);
begin
  FExcludeButtons := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;

procedure TJvgCaption.SetCaptionColor(Value: TColor);
begin
  FCaptionColor := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;

procedure TJvgCaption.SetTextStyle(Value: TglTextStyle);
begin
  FTextStyle := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;

procedure TJvgCaption.SetFont(Value: TFont);
begin
  if not Assigned(Value) then
    exit;
  FFont.Assign(Value);
  Repaint;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;

procedure TJvgCaption.SetAutoTrColor(Value: TglAutoTransparentColor);
begin
  FAutoTrColor := Value;
  FTransparentColor := GetTransparentColor(FTexture, Value);
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;

procedure TJvgCaption.SetTextureTransparent(Value: boolean);
begin
  if FTextureTransparent = Value then
    exit;
  FTextureTransparent := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;
//________________________________________________________

procedure TJvgCaption.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor = Value then
    exit;
  FTransparentColor := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;
//________________________________________________________
{procedure TJvgCaption.SetTexture( Value: TBitmap );
begin
  if Assigned(FTexture) then FTexture.Free;
  FTexture := TBitmap.Create;
  FTexture.Assign(Value);
end;}

function TJvgCaption.GetTexture: TBitmap;
begin
  if not Assigned(FTexture) then
    FTexture := TBitmap.Create;
  Result := FTexture;
end;

procedure TJvgCaption.SetTexture(Value: TBitmap);
begin
  if Assigned(FTexture) then
    FTexture.Free;
  FTexture := TBitmap.Create;
  FTexture.Assign(Value);
  if Assigned(Value) then
    Bmp := FTexture
  else if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap
  else
    Bmp := nil;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;

procedure TJvgCaption.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap
  else if Assigned(FTexture) then
    Bmp := FTexture
  else
    Bmp := nil;
  if not (csLoading in ComponentState) then
    DrawCaption(true);
end;

end.
