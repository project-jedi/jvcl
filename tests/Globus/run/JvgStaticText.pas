{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgStaticText.PAS, released on 2003-01-15.

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

unit JvgStaticText;

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
  StdCtrls,
  ExtCtrls,
  JvgTypes,
  JVComponent,
  JvgCommClasses,
  JvgUtils;

type

  TJvgStaticText = class(TJvGraphicControl)
  private
    FActiveColor: TColor;
    FAlignment: TglAlignment;
    FAutoSize: boolean;
    FTransparent: boolean;
    FWordWrap: boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseleave: TNotifyEvent;

    fActive: boolean;
    Image: TBitmap;
    procedure CreateImage;
    procedure DrawTextBroadwise;
    procedure AdjustBounds;

    procedure SetAlignment(Value: TglAlignment);
    procedure SetAutoSize(Value: boolean);
    procedure SetTransparent(Value: boolean);
    procedure SetWordWrap(Value: boolean);

  protected
    procedure CMFontChanged(var Message: TMessage);
    procedure Loaded; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property ActiveColor: TColor read FActiveColor write FActiveColor
      default clWhite;
    property Alignment: TglAlignment read FAlignment write SetAlignment
      default ftaBroadwise;
    property AutoSize: boolean read FAutoSize write SetAutoSize
      default true;
    property Transparent: boolean read FTransparent write SetTransparent
      default true;
    property WordWrap: boolean read FWordWrap write SetWordWrap
      default true;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write
      FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write
      FOnMouseLeave;

  end;

procedure Register;

implementation

{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

constructor TJvgStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  //  Image := TBitmap.Create;
  FTransparent := true;
  FActiveColor := clWhite;
  FAutoSize := true;
  FAlignment := ftaBroadwise;
  FWordWrap := true;
  if not (csLoading in ComponentState) then
    CreateImage;
end;
//______

destructor TJvgStaticText.Destroy;
begin
  //  Image.Free;
  inherited Destroy;
end;
//______

procedure TJvgStaticText.CMFontChanged(var Message: TMessage);
begin
  inherited;
end;
//______

procedure TJvgStaticText.Loaded;
begin
  inherited;
  CreateImage;
end;
//______

procedure TJvgStaticText.CMMouseEnter(var Message: TMessage);
begin
  fActive := true;
  paint;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TJvgStaticText.CMMouseLeave(var Message: TMessage);
begin
  fActive := false;
  paint;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;
//______

procedure TJvgStaticText.Paint;
const
  Alignments: array[TglAlignment] of Word = (DT_LEFT,
    DT_RIGHT, DT_CENTER, 0);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Alignment_: TglAlignment;
  Rect: TRect;
begin
  if length(Caption) = 0 then
    exit;
  Alignment_ := FAlignment;
  SetBkMode(Canvas.Handle, integer(FTransparent));
  if fActive then
    SetTextColor(Canvas.Handle, ColorToRGB(ActiveColor))
  else
    SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));
  //  TextOut( Canvas.Handle, 0, 0, 'lpszString', 10);
  //  BitBlt( Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, Width, Height, SRCCOPY );
  if (Alignment = ftaBroadwise) then
  begin
    if FWordWrap then
    begin
      DrawTextBroadwise;
      exit;
    end
    else
      Alignment_ := ftaLeftJustify;
  end;
  Rect := ClientRect;
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect,
    DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[Alignment_]);

end;
//______

procedure TJvgStaticText.CreateImage;
begin
  //  DrawTextBroadwise;
end;
//______

procedure TJvgStaticText.DrawTextBroadwise;
var
  i, DrawPos, Pos1, Pos2, LineWidth,
    LineNo, LexemCount, LexemCount_, TextHeight: cardinal;
  Lexem: string;
  Size: TSIZE;

  function GetNextLexem(var Pos1, Pos2: cardinal; fTrimleft: boolean): string;
  var
    Pos: cardinal;
  begin
    pos := pos1;
    if Caption[Pos] = ' ' then
      repeat inc(Pos);
      until (Pos > length(Caption)) or (Caption[Pos] <> ' ');
    Pos2 := Pos;
    if fTrimleft and (LineNo > 0) then
      Pos1 := Pos;
    repeat inc(Pos2);
    until (Pos2 > length(Caption)) or (Caption[Pos2] = ' ');

    Result := copy(Caption, Pos1, Pos2 - Pos1);
  end;

  procedure DrawLine(AdditSpace: cardinal);
  var
    i, DrawPos1, DrawPos2: cardinal;
    Lexem: string;
    Size: TSIZE;
    X, X_: single;
  begin
    DrawPos1 := DrawPos;
    DrawPos2 := DrawPos;
    X := 0;
    X_ := 0;
    LineWidth := 0;
    for i := 1 to LexemCount do
    begin
      Lexem := GetNextLexem(DrawPos1, DrawPos2, i = 1);
      //      if LexemCount=1 then Lexem:=Lexem+' ';
      GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
      inc(LineWidth, trunc(X));
      X := X + Size.cx;
      if (X > Width) and (LexemCount > 1) then
        exit;
      if LexemCount > 1 then
        X := X + AdditSpace / (LexemCount - 1);
      TextOut(Canvas.Handle, trunc(X_), LineNo * TextHeight, PChar(Lexem),
        length(Lexem));
      X_ := X;
      DrawPos1 := DrawPos2;
    end;
  end;
begin
  LineWidth := 0;
  LineNo := 0;
  DrawPos := 1;
  Pos1 := 1;
  Pos2 := 1;
  LexemCount := 0;
  TextHeight := 0;
  repeat
    Lexem := GetNextLexem(Pos1, Pos2, LexemCount = 0);
    //    if LexemCount=0 then Lexem:=Lexem+' ';
    GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
    inc(LineWidth, Size.cx);
    inc(LexemCount);
    if TextHeight < Size.cy then
      TextHeight := Size.cy;
    if (LineWidth > Width) or (Pos2 > length(Caption)) then
    begin
      if LexemCount = 1 then
        Pos1 := Pos2;
      if (Pos2 <= length(Caption)) then
      begin
        if LexemCount > 1 then
          dec(LexemCount);
        DrawLine(Width - (LineWidth - Size.cx));
      end
      else
        DrawLine(Width - (LineWidth));

      DrawPos := Pos1;
      inc(LineNo);
      LexemCount := 0;
      LineWidth := 0; //TextHeight := 0;
    end
    else
      Pos1 := Pos2;
  until Pos2 > length(Caption);
  if FAutoSize then
    Height := max(12, LineNo * TextHeight);
end;

procedure TJvgStaticText.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect,
      DT_EXPANDTABS or DT_CALCRECT or WordWraps[FWordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    if FAlignment = ftaRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TJvgStaticText.SetAlignment(Value: TglAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TJvgStaticText.SetAutoSize(Value: boolean);
begin
  FAutoSize := Value;
  AdjustBounds;
  Repaint;
end;

procedure TJvgStaticText.SetTransparent(Value: boolean);
begin
  FTransparent := Value;
  Repaint;
end;

procedure TJvgStaticText.SetWordWrap(Value: boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;

end.
