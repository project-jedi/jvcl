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

UNIT JvgCaption;

INTERFACE

USES
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

TYPE
   TJvgCaption = CLASS(TJvComponent)
   PRIVATE
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

      PROCEDURE SetExcludeIcon(Value: boolean);
      PROCEDURE SetExcludeButtons(Value: boolean);
      PROCEDURE SetCaptionColor(Value: TColor);
      PROCEDURE SetTextStyle(Value: TglTextStyle);
      PROCEDURE SetFont(Value: TFont);
      PROCEDURE SetTexture(Value: TBitmap);
      PROCEDURE SetImage(Value: TImage);
      FUNCTION GetTexture: TBitmap;
      PROCEDURE SetTextureTransparent(Value: boolean);
      PROCEDURE SetAutoTrColor(Value: TglAutoTransparentColor);
      PROCEDURE SetTransparentColor(Value: TColor);

      PROCEDURE Repaint;
      PROCEDURE DrawIcon(DC: HDC; R: TRect);
      FUNCTION DrawCaption(DrawAll: boolean): TRect;
      PROCEDURE ParentWindowHookProc(VAR Msg_: TMessage);
      PROCEDURE SetParentWindowHook;
      PROCEDURE FreeParentWindowHook;
      FUNCTION CountCaptionButtons: integer;
      PROCEDURE SmthChanged(Sender: TObject);
   PROTECTED
      //    procedure WndProc(var Message: TMessage);override;
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE Notification(Component: TComponent; Operation: TOperation);
         OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      //    property Parent: TForm read Fparent write SetParent;
      PROPERTY ExcludeButtons: boolean
         READ FExcludeButtons WRITE SetExcludeButtons DEFAULT true;
      PROPERTY ExcludeIcon: boolean
         READ FExcludeIcon WRITE SetExcludeIcon DEFAULT false;
      PROPERTY CaptionColor: TColor
         READ FCaptionColor WRITE SetCaptionColor DEFAULT clBtnFace;
      PROPERTY TextStyle: TglTextStyle
         READ FTextStyle WRITE SetTextStyle DEFAULT fstRaised;
      PROPERTY Font: TFont READ FFont WRITE SetFont;
      PROPERTY CaptBox: TJvgBevel READ FCaptBox WRITE FCaptBox;
      PROPERTY TextBox: TJvgBevel READ FTextBox WRITE FTextBox;
      PROPERTY IconBox: TJvgBevel READ FIconBox WRITE FIconBox;
      PROPERTY Texture: TBitmap READ GetTexture WRITE SetTexture;
      PROPERTY Image: TImage READ FImage WRITE SetImage;
      PROPERTY TextureTransparent: boolean READ FTextureTransparent WRITE
         SetTextureTransparent
         DEFAULT false;
      PROPERTY AutoTransparentColor: TglAutoTransparentColor
         READ FAutoTrColor WRITE SetAutoTrColor DEFAULT ftcLeftBottomPixel;
      PROPERTY TransparentColor: TColor READ FTransparentColor WRITE
         SetTransparentColor
         DEFAULT clBlack;
   END;

   {$DEFINE GL_CAPT_BUTTONS}
IMPLEMENTATION
{$IFDEF GL_CAPT_BUTTONS}
{$R JvgCaption.res}
{$ENDIF}

//==============================================================

CONSTRUCTOR TJvgCaption.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
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
   IF NOT (AOwner IS TForm) THEN
      exit;                             //FParent:=TForm(AOwner) else exit;

   {$IFDEF GL_CAPT_BUTTONS}
   //if (csDesigning in ComponentState)and not(csLoading in ComponentState)then
   BEGIN
      FGlyphClose := TBitmap.Create;
      FGlyphClose.LoadFromResourceName(hInstance, 'CLOSE');
   END;
   {$ENDIF}

   _CYCAPTION := GetSystemMetrics(SM_CYCAPTION);
   _CYFRAME := GetSystemMetrics(SM_CYFRAME);
   _CXFRAME := GetSystemMetrics(SM_CXFRAME);
   _CXSMICON := GetSystemMetrics(SM_CXSMICON);
   _CYSMICON := GetSystemMetrics(SM_CYSMICON);
   _CXICON := GetSystemMetrics(SM_CXICON);
   _CYICON := GetSystemMetrics(SM_CYICON);

   SetParentWindowHook;
END;
//-----

DESTRUCTOR TJvgCaption.Destroy;
BEGIN
   FFont.Free;
   FCaptBox.Free;
   FTextBox.Free;
   FIconBox.Free;
   IF Assigned(FTexture) THEN
      FTexture.Free;
   IF Assigned(FGlyphClose) THEN
      FGlyphClose.Free;
   FreeParentWindowHook;
   INHERITED Destroy;
END;

PROCEDURE TJvgCaption.Loaded;
BEGIN
   INHERITED;
   IF Assigned(FTexture) AND (NOT FTexture.Empty) THEN
      Bmp := FTexture;
END;

PROCEDURE TJvgCaption.Notification(Component: TComponent; Operation:
   TOperation);
BEGIN
   IF (Component <> Self) AND (Operation = opInsert) AND (Component IS
      TJvgCaption) THEN
      RAISE
         Exception.Create('Cannot create more than one instance of TJvgCaption component');
END;
//=========================================================.special procs.

PROCEDURE TJvgCaption.SetParentWindowHook;
VAR
   P                          : Pointer;
BEGIN
   P := Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC));
   IF (P <> FNewWndProc) THEN
   BEGIN
      FPrevWndProc := P;
      FNewWndProc := MakeObjectInstance(ParentWindowHookProc);
      SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(FNewWndProc));
   END;
END;
//==============================================================

PROCEDURE TJvgCaption.FreeParentWindowHook;
BEGIN
   IF (FNewWndProc <> NIL) AND (FPrevWndProc <> NIL)
      AND (Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC)) =
         FNewWndProc) THEN
   BEGIN
      //Repaint;
      SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
      FNewWndProc := NIL;
   END;
END;
//==============================================================

PROCEDURE TJvgCaption.ParentWindowHookProc(VAR Msg_: TMessage);
VAR
   pt                         : TPoint;

   PROCEDURE DefaultProc;               //___________________________________
   BEGIN
      WITH Msg_ DO
         Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg,
            WParam, LParam);
   END;

BEGIN //_______________________________________________________
   OwnerWidth := TForm(Owner).Width;
   WITH Msg_ DO
      CASE Msg OF
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
            BEGIN
               DefaultProc;
               DrawCaption(true);
            END;
         WM_NCLBUTTONDOWN:
            BEGIN
               DefaultProc;
               DrawCaption(false);
            END;
         WM_LBUTTONUP:
            BEGIN
               DefaultProc;
               {$IFDEF GL_CAPT_BUTTONS}
               GetCursorPos(pt);
               dec(pt.x, TForm(Owner).left);
               dec(pt.y, TForm(Owner).top);
               IF PtInRect(CloseRect, pt) THEN
                  SendMessage(TForm(Owner).Handle, WM_CLOSE, 0, 0);
               {$ENDIF}
            END;
         WM_NCHITTEST:
            BEGIN
               {$IFDEF GL_CAPT_BUTTONS}
               pt := {TForm(Owner).ScreenToClient}(Point(Loword(lParam) -
                  TForm(Owner).left, Hiword(lParam) - TForm(Owner).Top));
               IF PtInRect(CloseRect, pt) THEN
               BEGIN
                  result := HTCLIENT;
                  exit;
               END;
               {$ENDIF}
               DefaultProc;
               IF (Result = HTLEFT) OR (Result = HTRIGHT) OR (Result = HTTOP)
                  OR (Result = HTBOTTOM) OR (Result = HTBOTTOMLEFT)
                  OR (Result = HTBOTTOMRIGHT) OR (Result = HTTOPLEFT)
                  OR (Result = HTTOPRIGHT) THEN
               BEGIN
                  DrawCaption(false);
               END;
            END;
         //	WM_SETTEXT: DrawCaption( false );
         //	WM_ACTIVATE: DrawCaption;
         WM_DESTROY:
            BEGIN
               FreeParentWindowHook;
               DefaultProc;
            END;
      ELSE
         DefaultProc;
      END;
END;
//==============================================================

PROCEDURE TJvgCaption.DrawIcon(DC: HDC; R: TRect);
VAR
   IconHandle                 : HIcon;
   IconDC                     : HDC;
   OldIconBMP, IconBMP        : HBitmap;
   Brush, OldBrush            : HBrush;
BEGIN

   WITH TForm(Owner) DO
      IF Icon.Handle <> 0 THEN
         IconHandle := Icon.Handle
      ELSE IF Application.Icon.Handle <> 0 THEN
         IconHandle := Application.Icon.Handle
      ELSE
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
END;

//==============================================================

FUNCTION TJvgCaption.DrawCaption(DrawAll: boolean): TRect;
VAR
   DC                         : HDC;
   R, IconR                   : TRect;
   x, y, x_, y_, IWidth, IHeight: integer;
BEGIN
   DC := GetWindowDC(TForm(Owner).Handle);
   TRY
      GetWindowRect(TForm(Owner).Handle, R);
      OwnerWidth := R.Right - R.left;

      R.Left := _CXFRAME - 1;
      R.Top := _CYFRAME - 1;
      R.Right := OwnerWidth - _CXFRAME;
      R.Bottom := R.Top + _CYCAPTION - 1;

      BtnCount := CountCaptionButtons;
      IF (BtnCount = 0) AND (NOT DrawAll) THEN
         exit;
      R := DrawBoxEx(DC, R, FCaptBox.Sides, FCaptBox.Inner, FCaptBox.Outer,
         FCaptBox.Bold, CaptionColor, true);
      IF NOT DrawAll THEN
         exit;

      IF (NOT FExcludeIcon) AND (biSystemMenu IN TForm(Owner).BorderIcons) THEN
      BEGIN
         IconR := Rect(R.Left, R.Top, R.Left + _CXSMICON + 3, R.Top +
            _CYSMICON);
         IconR := DrawBoxEx(DC, IconR, FIconBox.Sides, FIconBox.Inner,
            FIconBox.Outer, FIconBox.Bold, CaptionColor, false);
         DrawIcon(DC, IconR);
         inc(R.Left, _CXSMICON + 4);
      END;

      dec(R.Right, BtnCount * (_CXSMICON + 1));
      IF BtnCount <> 0 THEN
         dec(R.Right, 4);
      R := DrawBoxEx(DC, R, FTextBox.Sides, FTextBox.Inner, FTextBox.Outer,
         FTextBox.Bold, CaptionColor, true);

      WITH TForm(Owner).Canvas DO
      BEGIN
         inc(R.right);
         inc(R.bottom);
         Brush.Color := CaptionColor {clActiveCaption};
         Brush.Style := bsSolid;
         Windows.FillRect(DC, R, Brush.Handle);
      END;
      inc(R.Left, 2);

      IF IsItAFilledBitmap(bmp) THEN
      BEGIN
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
         WHILE x_ < r.right DO
         BEGIN
            //IWidth:=SavedIWidth; SavedIWidth:=IWidth;
            IF x_ + IWidth > r.right THEN
               IWidth := r.right - x_;
            WHILE y_ < r.bottom DO
            BEGIN
               //	    IHeight:=SavedIHeight; SavedIHeight:=IHeight;
               IF y_ + IHeight > r.bottom THEN
                  IHeight := r.bottom - y_;
               BitBlt(DC, x_, y_, min(IWidth, bmp.Width), min(IHeight,
                  bmp.Height), bmp.Canvas.Handle, 0, 0, SRCCOPY);
               Inc(y_, min(IHeight, bmp.Height));
            END;
            Inc(x_, min(IWidth, bmp.Width));
            y_ := y;
         END;
      END;
      //...draw close button
      {$IFDEF GL_CAPT_BUTTONS}
      IF (BtnCount = 0) AND (tag = 1) THEN
      BEGIN
         CloseRect := Bounds(r.right - FGlyphClose.Width - 2, r.top,
            FGlyphClose.Width, FGlyphClose.Height);
         //      BitBlt( DC, r.right-FGlyphClose.Width-2, r.top, FGlyphClose.Width, FGlyphClose.Height, FGlyphClose.Canvas.Handle, 0,0, SRCCOPY );
         CreateBitmapExt(DC, FGlyphClose, R, r.right - FGlyphClose.Width - 8,
            r.top - 3,
            fwoNone, fdsDefault, true,
            GetPixel(FGlyphClose.Canvas.Handle, 0, FGlyphClose.Height - 1)
               {TransparentColor},
            0);
      END
      ELSE
         CloseRect := Rect(0, 0, 0, 0);
      {$ENDIF}

      DrawTextInRect(DC, R, TForm(Owner).Caption, FTextStyle, FFont,
         DT_SINGLELINE OR DT_VCENTER OR DT_LEFT);
   FINALLY
      ReleaseDC(TForm(Owner).Handle, DC);
   END;
   Result := R;
END;

FUNCTION TJvgCaption.CountCaptionButtons: integer;
BEGIN
   IF NOT (biSystemMenu IN TForm(Owner).BorderIcons) THEN
   BEGIN
      Result := 0;
      exit;
   END;
   result := 1;
   IF NOT (TForm(Owner).BorderStyle IN [bsToolWindow, bsSizeToolWin, bsDialog])
      THEN
   BEGIN
      IF (biMinimize IN TForm(Owner).BorderIcons)
         OR (biMaximize IN TForm(Owner).BorderIcons) THEN
         inc(Result, 2)
      ELSE IF biHelp IN TForm(Owner).BorderIcons THEN
         inc(Result);
   END;
END;

PROCEDURE TJvgCaption.SmthChanged(Sender: TObject);
BEGIN
   Repaint;
END;

PROCEDURE TJvgCaption.Repaint;
VAR
   RGN                        : HRGN;
BEGIN
   RGN := CreateRectRgn(0, 0, TForm(Owner).Width, _CYCAPTION);
   SendMessage(THandle(TForm(Owner).Handle), WM_NCPAINT, HRGN(RGN), 0);
   DeleteObject(RGN);
END;
//============================================================PROPERTIES

PROCEDURE TJvgCaption.SetExcludeIcon(Value: boolean);
BEGIN
   FExcludeIcon := Value;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;

PROCEDURE TJvgCaption.SetExcludeButtons(Value: boolean);
BEGIN
   FExcludeButtons := Value;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;

PROCEDURE TJvgCaption.SetCaptionColor(Value: TColor);
BEGIN
   FCaptionColor := Value;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;

PROCEDURE TJvgCaption.SetTextStyle(Value: TglTextStyle);
BEGIN
   FTextStyle := Value;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;

PROCEDURE TJvgCaption.SetFont(Value: TFont);
BEGIN
   IF NOT Assigned(Value) THEN
      exit;
   FFont.Assign(Value);
   Repaint;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;

PROCEDURE TJvgCaption.SetAutoTrColor(Value: TglAutoTransparentColor);
BEGIN
   FAutoTrColor := Value;
   FTransparentColor := GetTransparentColor(FTexture, Value);
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;

PROCEDURE TJvgCaption.SetTextureTransparent(Value: boolean);
BEGIN
   IF FTextureTransparent = Value THEN
      exit;
   FTextureTransparent := Value;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;
//________________________________________________________

PROCEDURE TJvgCaption.SetTransparentColor(Value: TColor);
BEGIN
   IF FTransparentColor = Value THEN
      exit;
   FTransparentColor := Value;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;
//________________________________________________________
{procedure TJvgCaption.SetTexture( Value: TBitmap );
begin
  if Assigned(FTexture) then FTexture.Free;
  FTexture := TBitmap.Create;
  FTexture.Assign(Value);
end;}

FUNCTION TJvgCaption.GetTexture: TBitmap;
BEGIN
   IF NOT Assigned(FTexture) THEN
      FTexture := TBitmap.Create;
   Result := FTexture;
END;

PROCEDURE TJvgCaption.SetTexture(Value: TBitmap);
BEGIN
   IF Assigned(FTexture) THEN
      FTexture.Free;
   FTexture := TBitmap.Create;
   FTexture.Assign(Value);
   IF Assigned(Value) THEN
      Bmp := FTexture
   ELSE IF Assigned(FImage) AND Assigned(FImage.Picture) AND
      Assigned(FImage.Picture.Bitmap) THEN
      Bmp := FImage.Picture.Bitmap
   ELSE
      Bmp := NIL;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;

PROCEDURE TJvgCaption.SetImage(Value: TImage);
BEGIN
   FImage := Value;
   IF Assigned(FImage) AND Assigned(FImage.Picture) AND
      Assigned(FImage.Picture.Bitmap) THEN
      Bmp := FImage.Picture.Bitmap
   ELSE IF Assigned(FTexture) THEN
      Bmp := FTexture
   ELSE
      Bmp := NIL;
   IF NOT (csLoading IN ComponentState) THEN
      DrawCaption(true);
END;

END.

