{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgShadow.PAS, released on 2003-01-15.

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

UNIT JvgShadow;

INTERFACE
USES Dialogs,
   Windows,
   Messages,
   Classes,
   Controls,
   Graphics,
   forms,
   JvgTypes,
   JvgCommClasses,
   JVComponent,
   JvgUtils,
   StdCtrls,
   ExtCtrls,
   SysUtils,
   Mask,
   Jvg3DColors;
TYPE

   TJvgShadow = CLASS(TJvGraphicControl)
   PRIVATE
      FControl: TControl;
      FStyle: TJvgTextBoxStyle;
      FStyleActive: TJvgTextBoxStyle;
      FShadowed: boolean;
      FShadowDepth: word;
      FShadowImage: TBitmap;
      FShadowImageBuff: TBitmap;
      FAutoTrColor: TglAutoTransparentColor;
      FTransparentShadow: boolean;
      FMaskedShadow: boolean;
      FTransparentColor: TColor;
      FMaskedFromColor: TColor;
      FMaskedToColor: TColor;

      FAfterPaint: TNotifyEvent;
      FOnEnter: TNotifyEvent;
      FOnExit: TNotifyEvent;
      ThreeDColors: TJvg3DLocalColors;
      fDontUseDefaultImage: boolean;
      PROCEDURE CreateShadowImageBuff(R: TRect);
      PROCEDURE CreateDefaultShadowImage;

      PROCEDURE SetControl(Value: TControl);
      PROCEDURE SetShadowed(Value: boolean);
      PROCEDURE SetShadowDepth(Value: word);
      PROCEDURE SetShadowImage(Value: TBitmap);
      FUNCTION GetShadowImage: TBitmap;
      PROCEDURE SetAutoTrColor(Value: TglAutoTransparentColor);
      PROCEDURE SetTransparentShadow(Value: boolean);
      PROCEDURE SetMaskedShadow(Value: boolean);
      PROCEDURE SetTransparentColor(Value: TColor);
      PROCEDURE SetMaskedFromColor(Value: TColor);
      PROCEDURE SetMaskedToColor(Value: TColor);

      PROCEDURE CMFontChanged(VAR Message: TMessage); MESSAGE CM_FONTCHANGED;
      PROCEDURE OnEnter_(Sender: TObject);
      PROCEDURE OnExit_(Sender: TObject);
      PROCEDURE SmthChanged(Sender: TObject);
      PROCEDURE SetDigitsOnly(Value: boolean);
   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE SetParent(Value: TWinControl); OVERRIDE;
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
   PUBLIC
      fNeedRecreateShadowImageBuff: boolean;
      fDestroying: boolean;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      {$IFDEF COMPILER5_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY Align;
      PROPERTY Control: TControl READ FControl WRITE SetControl;
      PROPERTY Visible;
      PROPERTY Style: TJvgTextBoxStyle READ FStyle WRITE FStyle;
      PROPERTY StyleActive: TJvgTextBoxStyle READ FStyleActive WRITE
         FStyleActive;
      //    property DigitsOnly: boolean read FDigitsOnly write SetDigitsOnly  default false;
      PROPERTY Shadowed: boolean READ FShadowed WRITE SetShadowed DEFAULT true;
      PROPERTY ShadowDepth: word READ FShadowDepth WRITE SetShadowDepth DEFAULT
         6;
      PROPERTY ShadowImage: TBitmap READ GetShadowImage WRITE SetShadowImage
         STORED fDontUseDefaultImage;
      PROPERTY AutoTransparentColor: TglAutoTransparentColor
         READ FAutoTrColor WRITE SetAutoTrColor DEFAULT ftcRightTopPixel;
      PROPERTY TransparentShadow: boolean READ FTransparentShadow WRITE
         SetTransparentShadow
         DEFAULT true;
      PROPERTY MaskedShadow: boolean READ FMaskedShadow WRITE SetMaskedShadow
         DEFAULT false;
      PROPERTY TransparentColor: TColor READ FTransparentColor
         WRITE SetTransparentColor DEFAULT clOlive;
      PROPERTY MaskedFromColor: TColor READ FMaskedFromColor
         WRITE SetMaskedFromColor DEFAULT clOlive;
      PROPERTY MaskedToColor: TColor READ FMaskedToColor
         WRITE SetMaskedToColor DEFAULT clBtnFace;
      PROPERTY AfterPaint: TNotifyEvent READ FAfterPaint WRITE FAfterPaint;
      PROPERTY OnControlEnter: TNotifyEvent READ FOnEnter WRITE FOnEnter;
      PROPERTY OnControlExit: TNotifyEvent READ FOnExit WRITE FOnExit;
   END;

PROCEDURE Register;

IMPLEMENTATION
//{$R JvgShadow.res}

{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//___________________________________________________ TJvgShadow Methods _

CONSTRUCTOR TJvgShadow.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   ThreeDColors := TJvg3DLocalColors.Create(self);
   FStyle := TJvgTextBoxStyle.Create;
   FStyleActive := TJvgTextBoxStyle.Create;
   FTransparentColor := clOlive;
   IF (csDesigning IN ComponentState) AND NOT (csLoading IN ComponentState) THEN
   BEGIN
      CreateDefaultShadowImage;
   END;
   //  FStyle.Inner := bvRaised;
   //  FStyleActive.Inner := bvRaised;
   //  FStyleActive.Bold := true;
   //  FStyleActive.HighlightColor := clWhite;

   Height := 23;
   Width := 120;
   FShadowed := true;
   FShadowDepth := 6;
   FAutoTrColor := ftcRightTopPixel;
   FTransparentShadow := true;
   FTransparentColor := clOlive;
   FMaskedFromColor := clOlive;
   FMaskedToColor := clBtnFace;
   FStyle.OnChanged := SmthChanged;
   FStyleActive.OnChanged := SmthChanged;

   fNeedRecreateShadowImageBuff := true;
END;

DESTRUCTOR TJvgShadow.Destroy;
BEGIN
   FStyle.Free;
   FStyleActive.Free;
   ThreeDColors.Free;
   IF Assigned(FShadowImage) THEN
      FShadowImage.Free;
   IF Assigned(FShadowImageBuff) THEN
      FShadowImageBuff.Free;
   INHERITED;
END;

PROCEDURE TJvgShadow.Loaded;
BEGIN
   INHERITED;
   IF FShadowed THEN
   BEGIN
      FShadowImageBuff := TBitmap.Create;
      IF FShadowImage = NIL THEN
         CreateDefaultShadowImage;
   END;
END;

PROCEDURE TJvgShadow.Paint;
VAR
   r                          : TRect;
   CurrStyle                  : TJvgTextBoxStyle;
   OldPointer                 : Pointer;
BEGIN
   r := ClientRect;
   IF Shadowed THEN
   BEGIN
      inc(r.left, FShadowDepth);
      inc(r.top, FShadowDepth);
      IF (csDesigning IN ComponentState) OR fNeedRecreateShadowImageBuff THEN
      BEGIN
         CreateShadowImageBuff(R);
         fNeedRecreateShadowImageBuff := false;
      END;
      BitBlt(Canvas.Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
         FShadowImageBuff.Canvas.Handle, 0, 0, SRCCOPY);
      OffsetRect(r, -FShadowDepth, -FShadowDepth);
   END
   ELSE
   BEGIN
      dec(r.right);
      dec(r.bottom);
   END;

   IF Assigned(Control) AND (Control IS TWinControl) AND
      TWinControl(Control).Focused THEN
      CurrStyle := FStyleActive
   ELSE
      CurrStyle := FStyle;

   WITH CurrStyle DO
   BEGIN
      ThreeDColors.Highlight := HighlightColor;
      ThreeDColors.Shadow := ShadowColor;
      OldPointer := glGlobalData.lp3DColors;
      glGlobalData.lp3DColors := ThreeDColors;
      r := DrawBoxEx(Canvas.Handle, r, Sides, Inner, Outer,
         Bold, Style.BackgroundColor, false);
      glGlobalData.lp3DColors := OldPointer;
   END;

   IF Assigned(Control) THEN
   BEGIN
      OffsetRect(r, left, top);
      IF Control.left <> r.left THEN
         Control.left := r.left;
      IF Control.top <> r.top THEN
         Control.top := r.top;
      IF NOT EqualRect(Control.ClientRect, Bounds(0, 0, r.right - r.left + 1,
         r.bottom - r.top + 1)) THEN
         Control.SetBounds(r.left, r.top, r.right - r.left + 1, r.bottom - r.top
            + 1);
   END;
   IF Assigned(FAfterPaint) THEN
      FAfterPaint(self);
END;

PROCEDURE TJvgShadow.SetParent(Value: TWinControl);
BEGIN
   INHERITED;
   IF Assigned(Control) THEN
      IF NOT (csDestroying IN ComponentState) THEN
         Control.Parent := Value;
END;

PROCEDURE TJvgShadow.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF (AComponent = Control) AND (Operation = opRemove) THEN
      Control := NIL;
END;

PROCEDURE TJvgShadow.CMFontChanged(VAR Message: TMessage);
BEGIN
   IF Assigned(Control) AND (Control IS TControl) THEN
      TJvgPublicWinControl(Control).Font := Font;
END;

{procedure TJvgShadow.OnKeyPress_(Sender: TObject; var Key: Char);
begin
  if FDigitsOnly then begin
    if Key = #8 then exit
  //  if Length(ACodeEdit.Text)>=CodeDigitsCount then Key := #0
    else if (Key<'0')or(Key>'9') then Key := #0;
  end;
  if Assigned(FOnKeyPress) then FOnKeyPress(self, Key);
end;
}

PROCEDURE TJvgShadow.OnEnter_(Sender: TObject);
BEGIN
   IF Assigned(FOnEnter) THEN
      FOnEnter(self);
   IF Assigned(Control) THEN
   BEGIN
      TJvgPublicWinControl(Control).Font.Color := StyleActive.TextColor;
      TJvgPublicWinControl(Control).Color := StyleActive.BackgroundColor;
      IF csDesigning IN ComponentState THEN
         Repaint
      ELSE
         Paint;
   END;
END;

PROCEDURE TJvgShadow.OnExit_(Sender: TObject);
BEGIN
   IF Assigned(FOnExit) THEN
      FOnExit(self);
   IF Assigned(Control) THEN
   BEGIN
      TJvgPublicWinControl(Control).Font.Color := Style.TextColor;
      TJvgPublicWinControl(Control).Color := Style.BackgroundColor;
      IF csDesigning IN ComponentState THEN
         Repaint
      ELSE
         Paint;
   END;
END;

PROCEDURE TJvgShadow.SmthChanged(Sender: TObject);
BEGIN
   Invalidate;
END;

PROCEDURE TJvgShadow.CreateShadowImageBuff(R: TRect);
BEGIN
   CreateDefaultShadowImage;
   WITH FShadowImageBuff DO
   BEGIN
      Width := R.Right - R.Left;
      Height := R.Bottom - R.Top;
      Canvas.Brush.Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Bounds(0, 0, Width, Height));
   END;
   IF FTransparentShadow THEN
      GetParentImageRect(self, Bounds(Left + R.Left, Top + R.Top,
         FShadowImageBuff.Width, FShadowImageBuff.Height),
         FShadowImageBuff.Canvas.Handle);

   CreateBitmapExt(FShadowImageBuff.Canvas.Handle, FShadowImage,
      Rect(0, 0, FShadowImageBuff.Width, FShadowImageBuff.Height), 0, 0,
      fwoTile, fdsDefault, FTransparentShadow, FTransparentColor, 0);
   IF FMaskedShadow THEN
      ChangeBitmapColor(FShadowImageBuff, FMaskedFromColor, FMaskedToColor);
END;

PROCEDURE TJvgShadow.CreateDefaultShadowImage;
CONST
   SIZE                       = 8;
VAR
   i, j                       : byte;
BEGIN
   IF Assigned(FShadowImage) THEN
      FShadowImage.Free;
   IF Assigned(FShadowImageBuff) THEN
      FShadowImageBuff.Free;
   FShadowImage := TBitmap.Create;
   FShadowImageBuff := TBitmap.Create;
   FShadowImage.Width := SIZE;
   FShadowImage.Height := SIZE;
   i := 0;
   j := 0;
   FShadowImage.Canvas.FillRect(Rect(0, 0, SIZE, SIZE));
   WHILE j < SIZE DO
   BEGIN
      WHILE i < SIZE DO
      BEGIN
         FShadowImage.Canvas.Pixels[i, j] := 0;
         inc(i, 2);
      END;
      inc(j);
      IF i = 8 THEN
         i := 1
      ELSE
         i := 0;
   END;
   FTransparentColor := clWhite;
   fDontUseDefaultImage := false;
END;
//___________________________________________________ TJvgShadow Methods _

PROCEDURE TJvgShadow.SetControl(Value: TControl);
BEGIN
   IF Value <> self THEN
      FControl := Value;
   IF FControl IS TWinControl THEN
   BEGIN
      TJvgPublicWinControl(FControl).OnEnter := OnEnter_;
      TJvgPublicWinControl(FControl).OnExit := OnExit_;
   END;
   Invalidate;
END;

{
procedure TJvgShadow.SetText( Value: string );
var
  i: integer;
  fIsDigit: boolean;
begin
  if DigitsOnly then
  begin
    Value := trim( Value );
    fIsDigit := true;
    try
      i := StrToInt( Value );
    except
      fIsDigit := false;
    end;
    if fIsDigit then Control.Text := Value;
  end
 else Control.Text := Value;

end;
}

PROCEDURE TJvgShadow.SetDigitsOnly(Value: boolean);
//var
//  i: integer;
BEGIN                                   //{$O-}
   {  if DigitsOnly = Value then exit;
     FDigitsOnly := Value;
     if DigitsOnly then
     begin
       Control.Text := trim( Control.Text );
        try
         i := StrToInt( Control.Text );
       except
         Control.Text := '';
       end;
     end;}
    // {$O+}
END;

PROCEDURE TJvgShadow.SetShadowed(Value: boolean);
BEGIN
   IF FShadowed = Value THEN
      exit;
   FShadowed := Value;
   IF FShadowed AND (FShadowImage = NIL) THEN
      CreateDefaultShadowImage;
   Invalidate;
END;

PROCEDURE TJvgShadow.SetShadowDepth(Value: word);
BEGIN
   IF FShadowDepth = Value THEN
      exit;
   FShadowDepth := Value;
   invalidate;
END;

PROCEDURE TJvgShadow.SetShadowImage(Value: TBitmap);
BEGIN
   IF Assigned(FShadowImage) THEN
      FShadowImage.Free;
   FShadowImage := TBitmap.Create;
   FShadowImage.Assign(Value);
   fDontUseDefaultImage := true;
   Repaint;
END;

FUNCTION TJvgShadow.GetShadowImage: TBitmap;
BEGIN
   IF NOT Assigned(FShadowImage) THEN
      FShadowImage := TBitmap.Create;
   Result := FShadowImage;
END;

PROCEDURE TJvgShadow.SetAutoTrColor(Value: TglAutoTransparentColor);
BEGIN
   FAutoTrColor := Value;
   FTransparentColor := GetTransparentColor(FShadowImage, Value);
   fNeedRecreateShadowImageBuff := true;
   Invalidate;
END;

PROCEDURE TJvgShadow.SetTransparentShadow(Value: boolean);
BEGIN
   FTransparentShadow := Value;
   fNeedRecreateShadowImageBuff := true;
   Invalidate;
END;

PROCEDURE TJvgShadow.SetMaskedShadow(Value: boolean);
BEGIN
   FMaskedShadow := Value;
   fNeedRecreateShadowImageBuff := true;
   Invalidate;
END;

PROCEDURE TJvgShadow.SetTransparentColor(Value: TColor);
BEGIN
   FTransparentColor := Value;
   fNeedRecreateShadowImageBuff := FTransparentShadow;
   Invalidate;
END;

PROCEDURE TJvgShadow.SetMaskedFromColor(Value: TColor);
BEGIN
   FMaskedFromColor := Value;
   fNeedRecreateShadowImageBuff := FMaskedShadow;
   Invalidate;
END;

PROCEDURE TJvgShadow.SetMaskedToColor(Value: TColor);
BEGIN
   FMaskedToColor := Value;
   fNeedRecreateShadowImageBuff := FMaskedShadow;
   Invalidate;
END;

END.

