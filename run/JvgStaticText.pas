{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgStaticText.PAS, released on 2003-01-15.

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

unit JvgStaticText;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses, JvgUtils;

type
  {$IFDEF USEJVCL}
  TJvgStaticText = class(TJvGraphicControl)
  {$ELSE}
  TJvgStaticText = class(TGraphicControl)
  {$ENDIF USEJVCL}
  private
    FActiveColor: TColor;
    FAlignment: TglAlignment;
    FAutoSize: Boolean;
    FTransparent: Boolean;
    FWordWrap: Boolean;
    FActive: Boolean;
    procedure DrawTextBroadwise;
    procedure AdjustBounds;
    procedure SetAlignment(Value: TglAlignment);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure SetAutoSize(Value: Boolean); override; 
    procedure Paint; override;
    {$IFDEF USEJVCL}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    {$ENDIF USEJVCL}
  public
    constructor Create(AOwner: TComponent); override;
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
    property ActiveColor: TColor read FActiveColor write FActiveColor default clWhite;
    property Alignment: TglAlignment read FAlignment write SetAlignment default ftaBroadwise;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
    {$IFDEF USEJVCL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF USEJVCL}
  end;

implementation

uses
  Math;

constructor TJvgStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  FTransparent := True;
  FActiveColor := clWhite;
  FAutoSize := True;
  FAlignment := ftaBroadwise;
  FWordWrap := True;
end;

{$IFDEF USEJVCL}

procedure TJvgStaticText.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  FActive := True;
  Repaint;
  inherited MouseEnter(Control);
end;

procedure TJvgStaticText.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  FActive := False;
  Repaint;
  inherited MouseLeave(Control);
end;

{$ENDIF USEJVCL}

procedure TJvgStaticText.Paint;
const
  Alignments: array [TglAlignment] of Word =
    (DT_LEFT, DT_RIGHT, DT_CENTER, 0);
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);
var
  Alignment_: TglAlignment;
  Rect: TRect;
begin
  if Length(Caption) = 0 then
    Exit;
  Alignment_ := Alignment;
  SetBkMode(Canvas.Handle, Ord(Transparent));
  if FActive then
    SetTextColor(Canvas.Handle, ColorToRGB(ActiveColor))
  else
    SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));
  //  TextOut( Canvas.Handle, 0, 0, 'lpszString', 10);
  //  BitBlt( Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, Width, Height, SRCCOPY );
  if Alignment = ftaBroadwise then
  begin
    if WordWrap then
    begin
      DrawTextBroadwise;
      Exit;
    end
    else
      Alignment_ := ftaLeftJustify;
  end;
  Rect := ClientRect;
  Windows.DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect,
    DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment_]);
end;

procedure TJvgStaticText.DrawTextBroadwise;
var
  DrawPos, Pos1, Pos2, LineWidth, LineNo, LexemCount, TextHeight: Integer;
  Lexem: string;
  Size: TSize;

  function GetNextLexem(var Pos1, Pos2: Integer; TrimLeft: Boolean): string;
  var
    Pos: Integer;
  begin
    Pos := Pos1;
    if Caption[Pos] = ' ' then
      repeat
        Inc(Pos);
      until (Pos > Length(Caption)) or (Caption[Pos] <> ' ');
    Pos2 := Pos;
    if TrimLeft and (LineNo > 0) then
      Pos1 := Pos;
    repeat
      Inc(Pos2);
    until (Pos2 > Length(Caption)) or (Caption[Pos2] = ' ');
    Result := Copy(Caption, Pos1, Pos2 - Pos1);
  end;

  procedure DrawLine(AdditSpace: Integer);
  var
    i, DrawPos1, DrawPos2: Integer;
    Lexem: string;
    Size: TSize;
    X, X_: Single;
  begin
    DrawPos1 := DrawPos;
    DrawPos2 := DrawPos;
    X := 0.0;
    X_ := 0.0;
    LineWidth := 0;
    for i := 1 to LexemCount do
    begin
      Lexem := GetNextLexem(DrawPos1, DrawPos2, i = 1);
      //      if LexemCount=1 then Lexem:=Lexem+' ';
      GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), Length(Lexem), Size);
      Inc(LineWidth, Trunc(X));
      X := X + Size.cx;
      if (X > Width) and (LexemCount > 1) then
        Exit;
      if LexemCount > 1 then
        X := X + AdditSpace / (LexemCount - 1);
      TextOut(Canvas.Handle, Trunc(X_), LineNo * TextHeight, PChar(Lexem),
        Length(Lexem));
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
    GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), Length(Lexem), Size);
    Inc(LineWidth, Size.cx);
    Inc(LexemCount);
    if TextHeight < Size.cy then
      TextHeight := Size.cy;
    if (LineWidth > Width) or (Pos2 > Length(Caption)) then
    begin
      if LexemCount = 1 then
        Pos1 := Pos2;
      if (Pos2 <= Length(Caption)) then
      begin
        if LexemCount > 1 then
          Dec(LexemCount);
        DrawLine(Width - (LineWidth - Size.cx));
      end
      else
        DrawLine(Width - LineWidth);

      DrawPos := Pos1;
      Inc(LineNo);
      LexemCount := 0;
      LineWidth := 0; //TextHeight := 0;
    end
    else
      Pos1 := Pos2;
  until Pos2 > Length(Caption);
  if AutoSize then
    Height := Max(12, LineNo * TextHeight);
end;

procedure TJvgStaticText.AdjustBounds;
const
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    Windows.DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect,
      DT_EXPANDTABS or DT_CALCRECT or WordWraps[WordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    if Alignment = ftaRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TJvgStaticText.SetAlignment(Value: TglAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvgStaticText.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
    Repaint;
  end;
end;

procedure TJvgStaticText.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Repaint;
  end;
end;

procedure TJvgStaticText.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

end.

