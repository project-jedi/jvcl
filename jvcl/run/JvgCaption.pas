{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCaption.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgCaption;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgTypes, JvgUtils, JvgCommClasses;

type
  {$IFDEF USEJVCL}
  TJvgCaption = class(TJvComponent)
  {$ELSE}
  TJvgCaption = class(TComponent)
  {$ENDIF USEJVCL}
  private
    FExcludeButtons: Boolean;
    FExcludeIcon: Boolean;
    FCaptBox: TJvgBevelOptions;
    FTextBox: TJvgBevelOptions;
    FIconBox: TJvgBevelOptions;
    FPrevWndProc: Pointer;
    FNewWndProc: Pointer;
    // FParent: TForm;
    FCaptionColor: TColor;
    FTextStyle: TglTextStyle;
    FFont: TFont;
    FTexture: TBitmap;
    FBmp: TBitmap;
    FImage: TImage;
    FTextureTransparent: Boolean;
    FAutoTransparentColor: TglAutoTransparentColor;
    FTransparentColor: TColor;

    FGlyphClose: TBitmap;
    FOwnerWidth: Integer;
    FBtnCount: Integer;
    FCloseRect: TRect;
    FCYCaption: Integer;
    FCXFrame: Integer;
    FCYFrame: Integer;
    FCXSMIcon: Integer;
    FCYSMIcon: Integer;
    FCXIcon: Integer;
    FCYIcon: Integer;
    procedure SetExcludeIcon(Value: Boolean);
    procedure SetExcludeButtons(Value: Boolean);
    procedure SetCaptionColor(Value: TColor);
    procedure SetTextStyle(Value: TglTextStyle);
    procedure SetFont(Value: TFont);
    procedure SetTexture(Value: TBitmap);
    procedure SetImage(Value: TImage);
    function GetTexture: TBitmap;
    procedure SetTextureTransparent(Value: Boolean);
    procedure SetAutoTransparentColor(Value: TglAutoTransparentColor);
    procedure SetTransparentColor(Value: TColor);
    procedure Repaint;
    procedure DrawIcon(DC: HDC; R: TRect);
    function DrawCaption(DrawAll: Boolean): TRect;
    procedure ParentWindowHookProc(var Msg: TMessage);
    procedure SetParentWindowHook;
    procedure FreeParentWindowHook;
    function CountCaptionButtons: Integer;
    procedure SmthChanged(Sender: TObject);
  protected
    //    procedure WndProc(var Message: TMessage);override;
    procedure Loaded; override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //    property Parent: TForm read FParent write SetParent;
    property ExcludeButtons: Boolean
      read FExcludeButtons write SetExcludeButtons default True;
    property ExcludeIcon: Boolean
      read FExcludeIcon write SetExcludeIcon default False;
    property CaptionColor: TColor
      read FCaptionColor write SetCaptionColor default clBtnFace;
    property TextStyle: TglTextStyle
      read FTextStyle write SetTextStyle default fstRaised;
    property Font: TFont read FFont write SetFont;
    property CaptBox: TJvgBevelOptions read FCaptBox write FCaptBox;
    property TextBox: TJvgBevelOptions read FTextBox write FTextBox;
    property IconBox: TJvgBevelOptions read FIconBox write FIconBox;
    property Texture: TBitmap read GetTexture write SetTexture;
    property Image: TImage read FImage write SetImage;
    property TextureTransparent: Boolean
      read FTextureTransparent write SetTextureTransparent default False;
    property AutoTransparentColor: TglAutoTransparentColor
      read FAutoTransparentColor write SetAutoTransparentColor default ftcLeftBottomPixel;
    property TransparentColor: TColor
      read FTransparentColor write SetTransparentColor default clBlack;
  end;

{$DEFINE GL_CAPT_BUTTONS}

implementation

{$IFDEF USEJVCL}
uses
  Math,
  JvResources, JvJVCLUtils;
{$ELSE}
uses
  Math;
{$ENDIF USEJVCL}

{$IFDEF GL_CAPT_BUTTONS}
{$IFDEF MSWINDOWS}
{$R ..\Resources\JvgCaption.res}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvgCaption.res}
{$ENDIF UNIX}
{$ENDIF GL_CAPT_BUTTONS}

{$IFNDEF USEJVCL}

resourcestring
  RsEOnlyOneInstanceOfTJvgCaption = 'Cannot create more than one instance of TJvgCaption component';

function JvMakeObjectInstance(Method: TWndMethod): Pointer;
begin
  {$IFDEF COMPILER6_UP}
  Result := Classes.MakeObjectInstance(Method);
  {$ELSE}
  Result := MakeObjectInstance(Method);
  {$ENDIF COMPILER6_UP}
end;

procedure JvFreeObjectInstance(ObjectInstance: Pointer);
begin
  if ObjectInstance <> nil then
    {$IFDEF COMPILER6_UP}
    Classes.FreeObjectInstance(ObjectInstance);
    {$ELSE}
    FreeObjectInstance(ObjectInstance);
    {$ENDIF COMPILER6_UP}
end;

{$ENDIF USEJVCL}

constructor TJvgCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptBox := TJvgBevelOptions.Create;
  FTextBox := TJvgBevelOptions.Create;
  FIconBox := TJvgBevelOptions.Create;
  FFont := TFont.Create;
  FExcludeButtons := True;
  FExcludeIcon := False;
  FCaptionColor := clBtnFace;
  FTextStyle := fstRaised;
  FTextBox.Inner := bvRaised;
  FIconBox.Inner := bvNone;
  FIconBox.Outer := bvNone;
  FTextureTransparent := False;
  FAutoTransparentColor := ftcLeftBottomPixel;
  FCaptBox.OnChanged := SmthChanged;
  FTextBox.OnChanged := SmthChanged;
  FIconBox.OnChanged := SmthChanged;

  //  FParent := nil;
  if not (AOwner is TForm) then
    Exit; //FParent:=TForm(AOwner) else Exit;

  {$IFDEF GL_CAPT_BUTTONS}
  //if (csDesigning in ComponentState)and not (csLoading in ComponentState) then
  begin
    FGlyphClose := TBitmap.Create;
    FGlyphClose.LoadFromResourceName(hInstance, 'CLOSE');
  end;
  {$ENDIF GL_CAPT_BUTTONS}

  FCYCaption := GetSystemMetrics(SM_CYCAPTION);
  FCYFrame := GetSystemMetrics(SM_CYFRAME);
  FCXFrame := GetSystemMetrics(SM_CXFRAME);
  FCXSMIcon := GetSystemMetrics(SM_CXSMICON);
  FCYSMIcon := GetSystemMetrics(SM_CYSMICON);
  FCXIcon := GetSystemMetrics(SM_CXICON);
  FCYIcon := GetSystemMetrics(SM_CYICON);

  SetParentWindowHook;
end;

destructor TJvgCaption.Destroy;
begin
  FFont.Free;
  FCaptBox.Free;
  FTextBox.Free;
  FIconBox.Free;
  FTexture.Free;
  FGlyphClose.Free;
  FreeParentWindowHook;
  inherited Destroy;
end;

procedure TJvgCaption.Loaded;
begin
  inherited Loaded;
  if Assigned(FTexture) and not FTexture.Empty then
    FBmp := FTexture;
end;

procedure TJvgCaption.Notification(Component: TComponent;
  Operation: TOperation);
begin
  if (Component <> Self) and (Operation = opInsert) and (Component is TJvgCaption) then
    raise Exception.CreateRes(@RsEOnlyOneInstanceOfTJvgCaption);
end;

procedure TJvgCaption.SetParentWindowHook;
var
  P: Pointer;
begin
  P := Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC));
  if P <> FNewWndProc then
  begin
    FPrevWndProc := P;
    FNewWndProc := JvMakeObjectInstance(ParentWindowHookProc);
    SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, Longint(FNewWndProc));
  end;
end;

procedure TJvgCaption.FreeParentWindowHook;
begin
  if (FNewWndProc <> nil) and (FPrevWndProc <> nil) and
    (Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC)) = FNewWndProc) then
  begin
    //Repaint;
    SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, Longint(FPrevWndProc));
    // (rom) JvFreeObjectInstance call added
    JvFreeObjectInstance(FNewWndProc);
    FNewWndProc := nil;
  end;
end;

procedure TJvgCaption.ParentWindowHookProc(var Msg: TMessage);
var
  Pt: TPoint;

  procedure DefaultProc;
  begin
    Msg.Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg.Msg,
      Msg.WParam, Msg.LParam);
  end;

begin
  FOwnerWidth := TForm(Owner).Width;
  case Msg.Msg of
    // WM_CREATE: if TForm(Owner)<>nil then FreeParentWindowHook;
    WM_NCPAINT, // WM_MOUSEMOVE,
    WM_MOUSEACTIVATE, WM_NCACTIVATE, WM_SYSCOLORCHANGE, // WM_NCLBUTTONUP,
    WM_NCLBUTTONDBLCLK, WM_SIZE:
      begin
        DefaultProc;
        DrawCaption(True);
      end;
    WM_NCLBUTTONDOWN:
      begin
        DefaultProc;
        DrawCaption(False);
      end;
    WM_LBUTTONUP:
      begin
        DefaultProc;
        {$IFDEF GL_CAPT_BUTTONS}
        GetCursorPos(Pt);
        Dec(Pt.X, TForm(Owner).Left);
        Dec(Pt.Y, TForm(Owner).Top);
        if PtInRect(FCloseRect, Pt) then
          SendMessage(TForm(Owner).Handle, WM_CLOSE, 0, 0);
        {$ENDIF GL_CAPT_BUTTONS}
      end;
    WM_NCHITTEST:
      begin
        {$IFDEF GL_CAPT_BUTTONS}
        Pt := {TForm(Owner).ScreenToClient}(Point(LoWord(Msg.LParam) -
          TForm(Owner).Left, HiWord(Msg.LParam) - TForm(Owner).Top));
        if PtInRect(FCloseRect, Pt) then
        begin
          Msg.Result := HTCLIENT;
          Exit;
        end;
        {$ENDIF GL_CAPT_BUTTONS}
        DefaultProc;
        if (Msg.Result = HTLEFT) or (Msg.Result = HTRIGHT) or (Msg.Result = HTTOP) or
          (Msg.Result = HTBOTTOM) or (Msg.Result = HTBOTTOMLEFT) or
          (Msg.Result = HTBOTTOMRIGHT) or (Msg.Result = HTTOPLEFT) or
          (Msg.Result = HTTOPRIGHT) then
          DrawCaption(False);
      end;
    // WM_SETTEXT: DrawCaption( False );
    // WM_ACTIVATE: DrawCaption;
    WM_DESTROY:
      begin
        FreeParentWindowHook;
        DefaultProc;
      end;
  else
    DefaultProc;
  end;
end;

procedure TJvgCaption.DrawIcon(DC: HDC; R: TRect);
var
  IconHandle: HICON;
  IconDC: HDC;
  OldIconBMP, IconBMP: HBITMAP;
  Brush, OldBrush: HBRUSH;
begin

  with TForm(Owner) do
    if Icon.Handle <> 0 then
      IconHandle := Icon.Handle
    else
    if Application.Icon.Handle <> 0 then
      IconHandle := Application.Icon.Handle
    else
      IconHandle := LoadIcon(0, IDI_APPLICATION);

  IconDC := CreateCompatibleDC(DC);
  IconBMP := CreateCompatibleBitmap(DC, FCXIcon, FCYIcon);
  OldIconBMP := SelectObject(IconDC, IconBMP);
  Brush := CreateSolidBrush(ColorToRGB(CaptionColor));
  OldBrush := SelectObject(IconDC, Brush);
  //  FillRect( IconDC, R, Brush );
  PatBlt(IconDC, 0, 0, FCXIcon, FCYIcon, PATCOPY);

  Windows.DrawIcon(IconDC, 0, 0, IconHandle);
  StretchBlt(DC, R.Left, R.Top, R.Bottom - R.Top, R.Bottom - R.Top, IconDC,
    0, 0, FCXIcon, FCYIcon, SRCCOPY);

  DeleteObject(SelectObject(IconDC, OldIconBMP));
  DeleteObject(SelectObject(IconDC, OldBrush));
  DeleteDC(IconDC);
end;

function TJvgCaption.DrawCaption(DrawAll: Boolean): TRect;
var
  DC: HDC;
  R, IconR: TRect;
  X, Y, X1, Y1, IWidth, IHeight: Integer;
begin
  DC := GetWindowDC(TForm(Owner).Handle);
  try
    GetWindowRect(TForm(Owner).Handle, R);
    FOwnerWidth := R.Right - R.Left;

    R.Left := FCXFrame - 1;
    R.Top := FCYFrame - 1;
    R.Right := FOwnerWidth - FCXFrame;
    R.Bottom := R.Top + FCYCaption - 1;

    FBtnCount := CountCaptionButtons;
    if (FBtnCount = 0) and (not DrawAll) then
      Exit;
    R := DrawBoxEx(DC, R, FCaptBox.Sides, FCaptBox.Inner, FCaptBox.Outer,
      FCaptBox.Bold, CaptionColor, True);
    if not DrawAll then
      Exit;

    if (not FExcludeIcon) and (biSystemMenu in TForm(Owner).BorderIcons) then
    begin
      IconR := Rect(R.Left, R.Top, R.Left + FCXSMIcon + 3, R.Top + FCYSMIcon);
      IconR := DrawBoxEx(DC, IconR, FIconBox.Sides, FIconBox.Inner,
        FIconBox.Outer, FIconBox.Bold, CaptionColor, False);
      DrawIcon(DC, IconR);
      Inc(R.Left, FCXSMIcon + 4);
    end;

    Dec(R.Right, FBtnCount * (FCXSMIcon + 1));
    if FBtnCount <> 0 then
      Dec(R.Right, 4);
    R := DrawBoxEx(DC, R, FTextBox.Sides, FTextBox.Inner, FTextBox.Outer,
      FTextBox.Bold, CaptionColor, True);

    with TForm(Owner).Canvas do
    begin
      Inc(R.Right);
      Inc(R.Bottom);
      Brush.Color := CaptionColor {clActiveCaption};
      Brush.Style := bsSolid;
      Windows.FillRect(DC, R, Brush.Handle);
    end;
    Inc(R.Left, 2);

    if IsItAFilledBitmap(FBmp) then
    begin
      X := R.Left - 2;
      Y := R.Top;
      IHeight := R.Bottom - R.Top;
      IWidth := R.Right - R.Left;
      X1 := X;
      Y1 := Y;
      {      while X < IWidth do
            begin
       while Y < IHeight do
       begin
         BitBlt(DC, X, Y, Min( IWidth, FBmp.Width ), Min( IHeight, FBmp.Height ), FBmp.Canvas.Handle, 0,0, SRCCOPY );
         Inc(Y, Min( IHeight, FBmp.Height ));
       end;
       Inc(X, Min( IWidth, FBmp.Width ));
       Y:=0;
            end;}
      while X1 < R.Right do
      begin
        //IWidth:=SavedIWidth; SavedIWidth:=IWidth;
        if X1 + IWidth > R.Right then
          IWidth := R.Right - X1;
        while Y1 < R.Bottom do
        begin
          // IHeight := SavedIHeight; SavedIHeight:=IHeight;
          if Y1 + IHeight > R.Bottom then
            IHeight := R.Bottom - Y1;
          BitBlt(DC, X1, Y1, Min(IWidth, FBmp.Width), Min(IHeight,
            FBmp.Height), FBmp.Canvas.Handle, 0, 0, SRCCOPY);
          Inc(Y1, Min(IHeight, FBmp.Height));
        end;
        Inc(X1, Min(IWidth, FBmp.Width));
        Y1 := Y;
      end;
    end;
    //...draw close button
    {$IFDEF GL_CAPT_BUTTONS}
    if (FBtnCount = 0) and (Tag = 1) then
    begin
      FCloseRect := Bounds(R.Right - FGlyphClose.Width - 2, R.Top,
        FGlyphClose.Width, FGlyphClose.Height);
      //      BitBlt( DC, R.Right-FGlyphClose.Width-2, R.Top, FGlyphClose.Width, FGlyphClose.Height, FGlyphClose.Canvas.Handle, 0,0, SRCCOPY );
      CreateBitmapExt(DC, FGlyphClose, R, R.Right - FGlyphClose.Width - 8,
        R.Top - 3,
        fwoNone, fdsDefault, True,
        GetPixel(FGlyphClose.Canvas.Handle, 0, FGlyphClose.Height - 1)
        {TransparentColor},
        0);
    end
    else
      FCloseRect := Rect(0, 0, 0, 0);
    {$ENDIF GL_CAPT_BUTTONS}

    DrawTextInRect(DC, R, TForm(Owner).Caption, FTextStyle, FFont,
      DT_SINGLELINE or DT_VCENTER or DT_LEFT);
  finally
    ReleaseDC(TForm(Owner).Handle, DC);
  end;
  Result := R;
end;

function TJvgCaption.CountCaptionButtons: Integer;
begin
  if not (biSystemMenu in TForm(Owner).BorderIcons) then
  begin
    Result := 0;
    Exit;
  end;
  Result := 1;
  if not (TForm(Owner).BorderStyle in [bsToolWindow, bsSizeToolWin, bsDialog]) then
  begin
    if (biMinimize in TForm(Owner).BorderIcons) or
      (biMaximize in TForm(Owner).BorderIcons) then
      Inc(Result, 2)
    else
    if biHelp in TForm(Owner).BorderIcons then
      Inc(Result);
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
  RGN := CreateRectRgn(0, 0, TForm(Owner).Width, FCYCaption);
  SendMessage(THandle(TForm(Owner).Handle), WM_NCPAINT, HRGN(RGN), 0);
  DeleteObject(RGN);
end;

procedure TJvgCaption.SetExcludeIcon(Value: Boolean);
begin
  FExcludeIcon := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

procedure TJvgCaption.SetExcludeButtons(Value: Boolean);
begin
  FExcludeButtons := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

procedure TJvgCaption.SetCaptionColor(Value: TColor);
begin
  FCaptionColor := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

procedure TJvgCaption.SetTextStyle(Value: TglTextStyle);
begin
  FTextStyle := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

procedure TJvgCaption.SetFont(Value: TFont);
begin
  if not Assigned(Value) then
    Exit;
  FFont.Assign(Value);
  Repaint;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

procedure TJvgCaption.SetAutoTransparentColor(Value: TglAutoTransparentColor);
begin
  FAutoTransparentColor := Value;
  FTransparentColor := GetTransparentColor(FTexture, Value);
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

procedure TJvgCaption.SetTextureTransparent(Value: Boolean);
begin
  if FTextureTransparent = Value then
    Exit;
  FTextureTransparent := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

procedure TJvgCaption.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor = Value then
    Exit;
  FTransparentColor := Value;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

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
  FTexture.Free;
  FTexture := TBitmap.Create;
  FTexture.Assign(Value);
  if Assigned(Value) then
    FBmp := FTexture
  else
  if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
    FBmp := nil;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

procedure TJvgCaption.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
  if Assigned(FTexture) then
    FBmp := FTexture
  else
    FBmp := nil;
  if not (csLoading in ComponentState) then
    DrawCaption(True);
end;

end.

