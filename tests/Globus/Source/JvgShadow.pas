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

unit JvgShadow;

interface
uses Dialogs,
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
type

  TJvgShadow = class(TJvGraphicControl)
  private
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
    procedure CreateShadowImageBuff(R: TRect);
    procedure CreateDefaultShadowImage;

    procedure SetControl(Value: TControl);
    procedure SetShadowed(Value: boolean);
    procedure SetShadowDepth(Value: word);
    procedure SetShadowImage(Value: TBitmap);
    function GetShadowImage: TBitmap;
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetTransparentShadow(Value: boolean);
    procedure SetMaskedShadow(Value: boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetMaskedFromColor(Value: TColor);
    procedure SetMaskedToColor(Value: TColor);

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure OnEnter_(Sender: TObject);
    procedure OnExit_(Sender: TObject);
    procedure SmthChanged(Sender: TObject);
    procedure SetDigitsOnly(Value: boolean);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure SetParent(Value: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    fNeedRecreateShadowImageBuff: boolean;
    fDestroying: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$IFDEF COMPILER5_UP}
    property Anchors;
    {$ENDIF}
    property Align;
    property Control: TControl read FControl write SetControl;
    property Visible;
    property Style: TJvgTextBoxStyle read FStyle write FStyle;
    property StyleActive: TJvgTextBoxStyle read FStyleActive write
      FStyleActive;
    //    property DigitsOnly: boolean read FDigitsOnly write SetDigitsOnly  default false;
    property Shadowed: boolean read FShadowed write SetShadowed default true;
    property ShadowDepth: word read FShadowDepth write SetShadowDepth default
      6;
    property ShadowImage: TBitmap read GetShadowImage write SetShadowImage
      stored fDontUseDefaultImage;
    property AutoTransparentColor: TglAutoTransparentColor
      read FAutoTrColor write SetAutoTrColor default ftcRightTopPixel;
    property TransparentShadow: boolean read FTransparentShadow write
      SetTransparentShadow
      default true;
    property MaskedShadow: boolean read FMaskedShadow write SetMaskedShadow
      default false;
    property TransparentColor: TColor read FTransparentColor
      write SetTransparentColor default clOlive;
    property MaskedFromColor: TColor read FMaskedFromColor
      write SetMaskedFromColor default clOlive;
    property MaskedToColor: TColor read FMaskedToColor
      write SetMaskedToColor default clBtnFace;
    property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
    property OnControlEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnControlExit: TNotifyEvent read FOnExit write FOnExit;
  end;

procedure Register;

implementation
//{$R JvgShadow.res}

{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//___________________________________________________ TJvgShadow Methods _

constructor TJvgShadow.Create(AOwner: TComponent);
begin
  inherited;
  ThreeDColors := TJvg3DLocalColors.Create(self);
  FStyle := TJvgTextBoxStyle.Create;
  FStyleActive := TJvgTextBoxStyle.Create;
  FTransparentColor := clOlive;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    CreateDefaultShadowImage;
  end;
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
end;

destructor TJvgShadow.Destroy;
begin
  FStyle.Free;
  FStyleActive.Free;
  ThreeDColors.Free;
  if Assigned(FShadowImage) then
    FShadowImage.Free;
  if Assigned(FShadowImageBuff) then
    FShadowImageBuff.Free;
  inherited;
end;

procedure TJvgShadow.Loaded;
begin
  inherited;
  if FShadowed then
  begin
    FShadowImageBuff := TBitmap.Create;
    if FShadowImage = nil then
      CreateDefaultShadowImage;
  end;
end;

procedure TJvgShadow.Paint;
var
  r: TRect;
  CurrStyle: TJvgTextBoxStyle;
  OldPointer: Pointer;
begin
  r := ClientRect;
  if Shadowed then
  begin
    inc(r.left, FShadowDepth);
    inc(r.top, FShadowDepth);
    if (csDesigning in ComponentState) or fNeedRecreateShadowImageBuff then
    begin
      CreateShadowImageBuff(R);
      fNeedRecreateShadowImageBuff := false;
    end;
    BitBlt(Canvas.Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
      FShadowImageBuff.Canvas.Handle, 0, 0, SRCCOPY);
    OffsetRect(r, -FShadowDepth, -FShadowDepth);
  end
  else
  begin
    dec(r.right);
    dec(r.bottom);
  end;

  if Assigned(Control) and (Control is TWinControl) and
    TWinControl(Control).Focused then
    CurrStyle := FStyleActive
  else
    CurrStyle := FStyle;

  with CurrStyle do
  begin
    ThreeDColors.Highlight := HighlightColor;
    ThreeDColors.Shadow := ShadowColor;
    OldPointer := glGlobalData.lp3DColors;
    glGlobalData.lp3DColors := ThreeDColors;
    r := DrawBoxEx(Canvas.Handle, r, Sides, Inner, Outer,
      Bold, Style.BackgroundColor, false);
    glGlobalData.lp3DColors := OldPointer;
  end;

  if Assigned(Control) then
  begin
    OffsetRect(r, left, top);
    if Control.left <> r.left then
      Control.left := r.left;
    if Control.top <> r.top then
      Control.top := r.top;
    if not EqualRect(Control.ClientRect, Bounds(0, 0, r.right - r.left + 1,
      r.bottom - r.top + 1)) then
      Control.SetBounds(r.left, r.top, r.right - r.left + 1, r.bottom - r.top
        + 1);
  end;
  if Assigned(FAfterPaint) then
    FAfterPaint(self);
end;

procedure TJvgShadow.SetParent(Value: TWinControl);
begin
  inherited;
  if Assigned(Control) then
    if not (csDestroying in ComponentState) then
      Control.Parent := Value;
end;

procedure TJvgShadow.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Control) and (Operation = opRemove) then
    Control := nil;
end;

procedure TJvgShadow.CMFontChanged(var Message: TMessage);
begin
  if Assigned(Control) and (Control is TControl) then
    TJvgPublicWinControl(Control).Font := Font;
end;

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

procedure TJvgShadow.OnEnter_(Sender: TObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(self);
  if Assigned(Control) then
  begin
    TJvgPublicWinControl(Control).Font.Color := StyleActive.TextColor;
    TJvgPublicWinControl(Control).Color := StyleActive.BackgroundColor;
    if csDesigning in ComponentState then
      Repaint
    else
      Paint;
  end;
end;

procedure TJvgShadow.OnExit_(Sender: TObject);
begin
  if Assigned(FOnExit) then
    FOnExit(self);
  if Assigned(Control) then
  begin
    TJvgPublicWinControl(Control).Font.Color := Style.TextColor;
    TJvgPublicWinControl(Control).Color := Style.BackgroundColor;
    if csDesigning in ComponentState then
      Repaint
    else
      Paint;
  end;
end;

procedure TJvgShadow.SmthChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvgShadow.CreateShadowImageBuff(R: TRect);
begin
  CreateDefaultShadowImage;
  with FShadowImageBuff do
  begin
    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
    Canvas.Brush.Color := clBtnFace;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Bounds(0, 0, Width, Height));
  end;
  if FTransparentShadow then
    GetParentImageRect(self, Bounds(Left + R.Left, Top + R.Top,
      FShadowImageBuff.Width, FShadowImageBuff.Height),
      FShadowImageBuff.Canvas.Handle);

  CreateBitmapExt(FShadowImageBuff.Canvas.Handle, FShadowImage,
    Rect(0, 0, FShadowImageBuff.Width, FShadowImageBuff.Height), 0, 0,
    fwoTile, fdsDefault, FTransparentShadow, FTransparentColor, 0);
  if FMaskedShadow then
    ChangeBitmapColor(FShadowImageBuff, FMaskedFromColor, FMaskedToColor);
end;

procedure TJvgShadow.CreateDefaultShadowImage;
const
  SIZE = 8;
var
  i, j: byte;
begin
  if Assigned(FShadowImage) then
    FShadowImage.Free;
  if Assigned(FShadowImageBuff) then
    FShadowImageBuff.Free;
  FShadowImage := TBitmap.Create;
  FShadowImageBuff := TBitmap.Create;
  FShadowImage.Width := SIZE;
  FShadowImage.Height := SIZE;
  i := 0;
  j := 0;
  FShadowImage.Canvas.FillRect(Rect(0, 0, SIZE, SIZE));
  while j < SIZE do
  begin
    while i < SIZE do
    begin
      FShadowImage.Canvas.Pixels[i, j] := 0;
      inc(i, 2);
    end;
    inc(j);
    if i = 8 then
      i := 1
    else
      i := 0;
  end;
  FTransparentColor := clWhite;
  fDontUseDefaultImage := false;
end;
//___________________________________________________ TJvgShadow Methods _

procedure TJvgShadow.SetControl(Value: TControl);
begin
  if Value <> self then
    FControl := Value;
  if FControl is TWinControl then
  begin
    TJvgPublicWinControl(FControl).OnEnter := OnEnter_;
    TJvgPublicWinControl(FControl).OnExit := OnExit_;
  end;
  Invalidate;
end;

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

procedure TJvgShadow.SetDigitsOnly(Value: boolean);
//var
//  i: integer;
begin //{$O-}
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
end;

procedure TJvgShadow.SetShadowed(Value: boolean);
begin
  if FShadowed = Value then
    exit;
  FShadowed := Value;
  if FShadowed and (FShadowImage = nil) then
    CreateDefaultShadowImage;
  Invalidate;
end;

procedure TJvgShadow.SetShadowDepth(Value: word);
begin
  if FShadowDepth = Value then
    exit;
  FShadowDepth := Value;
  invalidate;
end;

procedure TJvgShadow.SetShadowImage(Value: TBitmap);
begin
  if Assigned(FShadowImage) then
    FShadowImage.Free;
  FShadowImage := TBitmap.Create;
  FShadowImage.Assign(Value);
  fDontUseDefaultImage := true;
  Repaint;
end;

function TJvgShadow.GetShadowImage: TBitmap;
begin
  if not Assigned(FShadowImage) then
    FShadowImage := TBitmap.Create;
  Result := FShadowImage;
end;

procedure TJvgShadow.SetAutoTrColor(Value: TglAutoTransparentColor);
begin
  FAutoTrColor := Value;
  FTransparentColor := GetTransparentColor(FShadowImage, Value);
  fNeedRecreateShadowImageBuff := true;
  Invalidate;
end;

procedure TJvgShadow.SetTransparentShadow(Value: boolean);
begin
  FTransparentShadow := Value;
  fNeedRecreateShadowImageBuff := true;
  Invalidate;
end;

procedure TJvgShadow.SetMaskedShadow(Value: boolean);
begin
  FMaskedShadow := Value;
  fNeedRecreateShadowImageBuff := true;
  Invalidate;
end;

procedure TJvgShadow.SetTransparentColor(Value: TColor);
begin
  FTransparentColor := Value;
  fNeedRecreateShadowImageBuff := FTransparentShadow;
  Invalidate;
end;

procedure TJvgShadow.SetMaskedFromColor(Value: TColor);
begin
  FMaskedFromColor := Value;
  fNeedRecreateShadowImageBuff := FMaskedShadow;
  Invalidate;
end;

procedure TJvgShadow.SetMaskedToColor(Value: TColor);
begin
  FMaskedToColor := Value;
  fNeedRecreateShadowImageBuff := FMaskedShadow;
  Invalidate;
end;

end.
