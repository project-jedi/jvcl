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
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgShadow;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, Forms, Dialogs,
  StdCtrls, ExtCtrls, SysUtils, Mask,
  JvgCommClasses, JvComponent, JvgTypes, Jvg3DColors;

type
  TJvgShadow = class(TJvGraphicControl)
  private
    FControl: TControl;
    FStyle: TJvgTextBoxStyle;
    FStyleActive: TJvgTextBoxStyle;
    FShadowed: Boolean;
    FShadowDepth: Word;
    FShadowImage: TBitmap;
    FShadowImageBuff: TBitmap;
    FAutoTransparentColor: TglAutoTransparentColor;
    FTransparentShadow: Boolean;
    FMaskedShadow: Boolean;
    FTransparentColor: TColor;
    FMaskedFromColor: TColor;
    FMaskedToColor: TColor;
    FAfterPaint: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FThreeDColors: TJvg3DLocalColors;
    FDontUseDefaultImage: Boolean;
    FNeedRecreateShadowImageBuff: Boolean;
    procedure CreateShadowImageBuff(R: TRect);
    procedure CreateDefaultShadowImage;
    procedure SetControl(Value: TControl);
    procedure SetShadowed(Value: Boolean);
    procedure SetShadowDepth(Value: Word);
    procedure SetShadowImage(Value: TBitmap);
    function GetShadowImage: TBitmap;
    procedure SetAutoTransparentColor(Value: TglAutoTransparentColor);
    procedure SetTransparentShadow(Value: Boolean);
    procedure SetMaskedShadow(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetMaskedFromColor(Value: TColor);
    procedure SetMaskedToColor(Value: TColor);
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure ControlEnter(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure SmthChanged(Sender: TObject);
    //    procedure SetDigitsOnly(Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure SetParent(Value: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Anchors;
    property Align;
    property Control: TControl read FControl write SetControl;
    property Visible;
    property Style: TJvgTextBoxStyle read FStyle write FStyle;
    property StyleActive: TJvgTextBoxStyle read FStyleActive write FStyleActive;
    //    property DigitsOnly: Boolean read FDigitsOnly write SetDigitsOnly  default False;
    property Shadowed: Boolean read FShadowed write SetShadowed default True;
    property ShadowDepth: Word read FShadowDepth write SetShadowDepth default 6;
    property ShadowImage: TBitmap read GetShadowImage write SetShadowImage stored FDontUseDefaultImage;
    property AutoTransparentColor: TglAutoTransparentColor
      read FAutoTransparentColor write SetAutoTransparentColor default ftcRightTopPixel;
    property TransparentShadow: Boolean read FTransparentShadow
      write SetTransparentShadow default True;
    property MaskedShadow: Boolean read FMaskedShadow write SetMaskedShadow
      default False;
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

implementation

uses
  JvgUtils;

constructor TJvgShadow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreeDColors := TJvg3DLocalColors.Create(Self);
  FStyle := TJvgTextBoxStyle.Create;
  FStyleActive := TJvgTextBoxStyle.Create;
  FTransparentColor := clOlive;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    CreateDefaultShadowImage;
  //  FStyle.Inner := bvRaised;
  //  FStyleActive.Inner := bvRaised;
  //  FStyleActive.Bold := True;
  //  FStyleActive.HighlightColor := clWhite;

  Height := 23;
  Width := 120;
  FShadowed := True;
  FShadowDepth := 6;
  FAutoTransparentColor := ftcRightTopPixel;
  FTransparentShadow := True;
  FTransparentColor := clOlive;
  FMaskedFromColor := clOlive;
  FMaskedToColor := clBtnFace;
  FStyle.OnChanged := SmthChanged;
  FStyleActive.OnChanged := SmthChanged;

  FNeedRecreateShadowImageBuff := True;
end;

destructor TJvgShadow.Destroy;
begin
  FStyle.Free;
  FStyleActive.Free;
  FThreeDColors.Free;
  FShadowImage.Free;
  FShadowImageBuff.Free;
  inherited Destroy;
end;

procedure TJvgShadow.Loaded;
begin
  inherited Loaded;
  if FShadowed then
  begin
    FShadowImageBuff := TBitmap.Create;
    if FShadowImage = nil then
      CreateDefaultShadowImage;
  end;
end;

procedure TJvgShadow.Paint;
var
  R: TRect;
  CurrStyle: TJvgTextBoxStyle;
  OldPointer: Pointer;
begin
  R := ClientRect;
  if Shadowed then
  begin
    Inc(R.Left, FShadowDepth);
    Inc(R.Top, FShadowDepth);
    if (csDesigning in ComponentState) or FNeedRecreateShadowImageBuff then
    begin
      CreateShadowImageBuff(R);
      FNeedRecreateShadowImageBuff := False;
    end;
    BitBlt(Canvas.Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
      FShadowImageBuff.Canvas.Handle, 0, 0, SRCCOPY);
    OffsetRect(R, -FShadowDepth, -FShadowDepth);
  end
  else
  begin
    Dec(R.Right);
    Dec(R.Bottom);
  end;

  if Assigned(Control) and (Control is TWinControl) and
    TWinControl(Control).Focused then
    CurrStyle := FStyleActive
  else
    CurrStyle := FStyle;

  with CurrStyle do
  begin
    FThreeDColors.Highlight := HighlightColor;
    FThreeDColors.Shadow := ShadowColor;
    OldPointer := glGlobalData.lp3DColors;
    glGlobalData.lp3DColors := FThreeDColors;
    R := DrawBoxEx(Canvas.Handle, R, Sides, Inner, Outer,
      Bold, Style.BackgroundColor, False);
    glGlobalData.lp3DColors := OldPointer;
  end;

  if Assigned(Control) then
  begin
    OffsetRect(R, Left, Top);
    if Control.Left <> R.Left then
      Control.Left := R.Left;
    if Control.Top <> R.Top then
      Control.Top := R.Top;
    if not EqualRect(Control.ClientRect, Bounds(0, 0, R.Right - R.Left + 1,
      R.Bottom - R.Top + 1)) then
      Control.SetBounds(R.Left, R.Top, R.Right - R.Left + 1, R.Bottom - R.Top + 1);
  end;
  if Assigned(FAfterPaint) then
    FAfterPaint(Self);
end;

procedure TJvgShadow.SetParent(Value: TWinControl);
begin
  inherited SetParent(Value);
  if Assigned(Control) then
    if not (csDestroying in ComponentState) then
      Control.Parent := Value;
end;

procedure TJvgShadow.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Control) and (Operation = opRemove) then
    Control := nil;
end;

procedure TJvgShadow.CMFontChanged(var Msg: TMessage);
begin
  if Assigned(Control) and (Control is TControl) then
    TJvgPublicWinControl(Control).Font := Font;
end;

{procedure TJvgShadow.OnKeyPress_(Sender: TObject; var Key: Char);
begin
  if FDigitsOnly then
  begin
    if Key = #8 then exit
  //  if Length(ACodeEdit.Text)>=CodeDigitsCount then Key := #0
    else if (Key<'0')or(Key>'9') then Key := #0;
  end;
  if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
end;
}

procedure TJvgShadow.ControlEnter(Sender: TObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
  if Assigned(Control) then
  begin
    TJvgPublicWinControl(Control).Font.Color := StyleActive.TextColor;
    TJvgPublicWinControl(Control).Color := StyleActive.BackgroundColor;
    Repaint;
  end;
end;

procedure TJvgShadow.ControlExit(Sender: TObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
  if Assigned(Control) then
  begin
    TJvgPublicWinControl(Control).Font.Color := Style.TextColor;
    TJvgPublicWinControl(Control).Color := Style.BackgroundColor;
    Repaint;
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
    GetParentImageRect(Self, Bounds(Left + R.Left, Top + R.Top,
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
  cSize = 8;
var
  I, J: Byte;
begin
  if Assigned(FShadowImage) then
    FShadowImage.Free;
  if Assigned(FShadowImageBuff) then
    FShadowImageBuff.Free;
  FShadowImage := TBitmap.Create;
  FShadowImageBuff := TBitmap.Create;
  FShadowImage.Width := cSize;
  FShadowImage.Height := cSize;
  I := 0;
  J := 0;
  FShadowImage.Canvas.FillRect(Rect(0, 0, cSize, cSize));
  while J < cSize do
  begin
    while I < cSize do
    begin
      FShadowImage.Canvas.Pixels[I, J] := 0;
      Inc(I, 2);
    end;
    Inc(J);
    if I = 8 then
      I := 1
    else
      I := 0;
  end;
  FTransparentColor := clWhite;
  FDontUseDefaultImage := False;
end;

procedure TJvgShadow.SetControl(Value: TControl);
begin
  if Value <> Self then
    FControl := Value;
  if FControl is TWinControl then
  begin
    TJvgPublicWinControl(FControl).OnEnter := ControlEnter;
    TJvgPublicWinControl(FControl).OnExit := ControlExit;
  end;
  Invalidate;
end;

{
procedure TJvgShadow.SetText( Value: string );
var
  I: Integer;
  fIsDigit: Boolean;
begin
  if DigitsOnly then
  begin
    Value := trim( Value );
    fIsDigit := True;
    try
      I := StrToInt( Value );
    except
      fIsDigit := False;
    end;
    if fIsDigit then Control.Text := Value;
  end
 else Control.Text := Value;

end;
}

(*
procedure TJvgShadow.SetDigitsOnly(Value: Boolean);
//var
//  I: Integer;
begin //{$O-}
  {  if DigitsOnly = Value then exit;
    FDigitsOnly := Value;
    if DigitsOnly then
    begin
      Control.Text := trim( Control.Text );
       try
        I := StrToInt( Control.Text );
      except
        Control.Text := '';
      end;
    end;}
   // {$O+}
end;
*)

procedure TJvgShadow.SetShadowed(Value: Boolean);
begin
  if FShadowed <> Value then
  begin
    FShadowed := Value;
    if FShadowed and (FShadowImage = nil) then
      CreateDefaultShadowImage;
    Invalidate;
  end;
end;

procedure TJvgShadow.SetShadowDepth(Value: Word);
begin
  if FShadowDepth <> Value then
  begin
    FShadowDepth := Value;
    Invalidate;
  end;
end;

procedure TJvgShadow.SetShadowImage(Value: TBitmap);
begin
  if not Assigned(FShadowImage) then
    FShadowImage := TBitmap.Create;
  FShadowImage.Assign(Value);
  FDontUseDefaultImage := True;
  Repaint;
end;

function TJvgShadow.GetShadowImage: TBitmap;
begin
  if not Assigned(FShadowImage) then
    FShadowImage := TBitmap.Create;
  Result := FShadowImage;
end;

procedure TJvgShadow.SetAutoTransparentColor(Value: TglAutoTransparentColor);
begin
  if FAutoTransparentColor <> Value then
  begin
    FAutoTransparentColor := Value;
    FTransparentColor := GetTransparentColor(FShadowImage, Value);
    FNeedRecreateShadowImageBuff := True;
    Invalidate;
  end;
end;

procedure TJvgShadow.SetTransparentShadow(Value: Boolean);
begin
  if FTransparentShadow <> Value then
  begin
    FTransparentShadow := Value;
    FNeedRecreateShadowImageBuff := True;
    Invalidate;
  end;
end;

procedure TJvgShadow.SetMaskedShadow(Value: Boolean);
begin
  if FMaskedShadow <> Value then
  begin
    FMaskedShadow := Value;
    FNeedRecreateShadowImageBuff := True;
    Invalidate;
  end;
end;

procedure TJvgShadow.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    FNeedRecreateShadowImageBuff := FTransparentShadow;
    Invalidate;
  end;
end;

procedure TJvgShadow.SetMaskedFromColor(Value: TColor);
begin
  if FMaskedFromColor <> Value then
  begin
    FMaskedFromColor := Value;
    FNeedRecreateShadowImageBuff := FMaskedShadow;
    Invalidate;
  end;
end;

procedure TJvgShadow.SetMaskedToColor(Value: TColor);
begin
  if FMaskedToColor <> Value then
  begin
    FMaskedToColor := Value;
    FNeedRecreateShadowImageBuff := FMaskedShadow;
    Invalidate;
  end;
end;

end.

