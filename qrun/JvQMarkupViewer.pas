{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMarkupViewer.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQMarkupViewer;

interface

uses
  SysUtils, Classes,
  Types, QWindows, QMessages, QGraphics, QForms, QControls, QStdCtrls, 
  QTypes, 
  JvQComponent, JvQMarkupCommon;

type
  TJvMarkupViewer = class(TJvCustomControl)
  private
    FScrollBar: TScrollBar;
    FBmp: TBitmap;
    FrameTop: Integer;
    FrameBottom: Integer;
    PageBottom: Integer;
    FElementStack: TJvHTMLElementStack;
    FTagStack: TJvHTMLElementStack;
    FBackColor: TColor;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer; 
    procedure ParseHTML(s: string);
    procedure RenderHTML;
    procedure HTMLClearBreaks;
    procedure HTMLElementDimensions;
    procedure SetBackColor(const Value: TColor);
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginRight(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
    procedure ScrollViewer(Sender: TObject);
  protected
    procedure CreateWnd; override; 
    procedure SetText(const Value: TCaption); override; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Align;  
    property Text: TCaption read GetText write SetText; 
    property BackColor: TColor read FBackColor write SetBackColor;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft default 5;
    property MarginRight: Integer read FMarginRight write SetMarginRight default 5;
    property MarginTop: Integer read FMarginTop write SetMarginTop default 5;
  end;

implementation

uses
  JvQConsts, JvQThemes;

constructor TJvMarkupViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 
  FElementStack := TJvHTMLElementStack.Create;
  FTagStack := TJvHTMLElementStack.Create;
  Width := 300;
  Height := 275;
  FMarginLeft := 5;
  FMarginRight := 5;
  FMarginTop := 5;
  FBackColor := clWhite;
end;

destructor TJvMarkupViewer.Destroy;
begin
  FElementStack.Free;
  FTagStack.Free;
  FBmp.Free;
  FScrollBar.Free;
  inherited Destroy;
end;

procedure TJvMarkupViewer.HTMLClearBreaks;
var
  I, C: Integer;
  Element: TJvHTMLElement;
begin
  C := FElementStack.Count;
  if C = 0 then
    Exit;
  for I := 0 to C - 1 do
  begin
    Element := TJvHTMLElement(FElementStack.Items[I]);
    Element.SolText := '';
    Element.EolText := '';
  end;
end;

procedure TJvMarkupViewer.HTMLElementDimensions;
var
  I, C: Integer;
  Element: TJvHTMLElement;
  h, a, w: Integer;
  tm: TEXTMETRIC;
  s: string;
begin
  C := FElementStack.Count;
  if C = 0 then
    Exit;
  for I := 0 to C - 1 do
  begin
    Element := TJvHTMLElement(FElementStack.Items[I]);
    s := Element.Text;
    Canvas.Font.Name := Element.FontName;
    Canvas.Font.Size := Element.FontSize;
    Canvas.Font.Style := Element.FontStyle;
    Canvas.Font.Color := Element.FontColor;
    GetTextMetrics(Canvas.Handle, tm);
    h := tm.tmHeight;
    a := tm.tmAscent;
    w := Canvas.TextWidth(s);
    Element.Height := h;
    Element.Ascent := a;
    Element.Width := w;
  end;
end;

procedure TJvMarkupViewer.CreateWnd;
begin
  inherited CreateWnd;
  FScrollBar := TScrollBar.Create(Self);
  FScrollBar.Kind := sbVertical;
  FScrollBar.Parent := Self;
  FScrollBar.Align := alRight;
  FScrollBar.Min := 0;
  FScrollBar.Max := 0;
  FScrollBar.OnChange := ScrollViewer;
  FrameTop := 0;
  FrameBottom := ClientHeight;
  FBmp := TBitmap.Create;
  FBmp.Width := ClientWidth - FScrollBar.Width;
  FBmp.Height := ClientHeight;
end;

procedure TJvMarkupViewer.Paint;
var
  sm: Integer;
  w, h: Integer;
begin
  w := ClientWidth - FScrollBar.Width;
  h := ClientHeight;
  FBmp.Width := w;
  FBmp.Height := h;
  RenderHTML;
  Canvas.Draw(0, 0, FBmp);
  FScrollBar.Min := 0;
  sm := PageBottom - ClientHeight;
  if sm > 0 then
    FScrollBar.Max := sm
  else
    FScrollBar.Max := 0;
  FScrollBar.Position := 0;
  FScrollBar.LargeChange := Trunc(0.8 * ClientHeight);
end;

procedure TJvMarkupViewer.ParseHTML(s: string);
var
  p: Integer;
  se, st: string;
  LText: string;
  FStyle: TFontStyles;
  FName: string;
  FSize: Integer;
  LBreakLine: Boolean;
  AColor, FColor: TColor;
  Element: TJvHTMLElement;

  function HTMLStringToColor(v: string; var col: TColor): Boolean;
  var
    vv: string;
  begin
    if Copy(v, 1, 1) <> '#' then
    begin
      vv := 'cl' + v;
      try
        col := StringToColor(vv);
        Result := True;
      except
        Result := False;
      end;
    end
    else
    begin
      try
        vv := '$' + Copy(v, 6, 2) + Copy(v, 4, 2) + Copy(v, 2, 2);
        col := StringToColor(vv);
        Result := True;
      except
        Result := False;
      end
    end
  end;

  procedure PushTag;
  begin
    Element := TJvHTMLElement.Create;
    Element.FontName := FName;
    Element.FontSize := FSize;
    Element.FontStyle := FStyle;
    Element.FontColor := FColor;
    FTagStack.Push(Element);
  end;

  procedure PopTag;
  begin
    Element := FTagStack.pop;
    if Element <> nil then
    begin
      FName := Element.FontName;
      FSize := Element.FontSize;
      FStyle := Element.FontStyle;
      FColor := Element.FontColor;
      Element.Free;
    end;
  end;

  procedure PushElement;
  begin
    Element := TJvHTMLElement.Create;
    Element.Text := LText;
    Element.FontName := FName;
    Element.FontSize := FSize;
    Element.FontStyle := FStyle;
    Element.FontColor := FColor;
    Element.BreakLine := LBreakLine;
    LBreakLine := False;
    FElementStack.Push(Element);
  end;

  procedure ParseTag(SS: string);
  var
    PP: Integer;
    LTag, LPar, LVal: string;
    HavePar: Boolean;
  begin
    SS := Trim(SS);
    HavePar := False;
    PP := Pos(' ', SS);
    if PP = 0 then
      LTag := SS // tag only
    else
    begin // tag + attributes
      LTag := Copy(SS, 1, PP - 1);
      SS := Trim(Copy(SS, PP + 1, Length(SS)));
      HavePar := True;
    end;
    // handle LTag
    LTag := LowerCase(LTag);
    if LTag = 'br' then
      LBreakLine := True
    else
    if LTag = 'b' then
    begin // bold
      PushTag;
      FStyle := FStyle + [fsBold];
    end
    else
    if LTag = '/b' then
    begin // cancel bold
      FStyle := FStyle - [fsBold];
      PopTag;
    end
    else
    if LTag = 'I' then
    begin // italic
      PushTag;
      FStyle := FStyle + [fsItalic];
    end
    else
    if LTag = '/I' then
    begin // cancel italic
      FStyle := FStyle - [fsItalic];
      PopTag;
    end
    else
    if LTag = 'u' then
    begin // underline
      PushTag;
      FStyle := FStyle + [fsUnderline];
    end
    else
    if LTag = '/u' then
    begin // cancel underline
      FStyle := FStyle - [fsUnderline];
      PopTag;
    end
    else
    if LTag = 'font' then
      PushTag
    else
    if LTag = '/font' then
      PopTag;
    if HavePar then
    begin
      repeat
        PP := Pos('="', SS);
        if PP > 0 then
        begin
          LPar := LowerCase(Trim(Copy(SS, 1, PP - 1)));
          Delete(SS, 1, PP + 1);
          PP := Pos('"', SS);
          if PP > 0 then
          begin
            LVal := Copy(SS, 1, PP - 1);
            Delete(SS, 1, PP);
            if LPar = 'face' then
              FName := LVal
            else
            if LPar = 'size' then
              try
                FSize := StrToInt(LVal);
              except
              end
            else
            if LPar = 'color' then
              try
                if HTMLStringToColor(LVal, AColor) then
                  FColor := AColor;
              except
              end
          end;
        end;
      until PP = 0;
    end;
  end;

begin
  FElementStack.Clear;
  FTagStack.Clear;
  FStyle := [];
  FName := 'Arial';
  FSize := 12;
  FColor := clBlack;
  LBreakLine := False;
  repeat
    p := Pos('<', s);
    if p = 0 then
    begin
      LText := s;
      PushElement;
    end
    else
    begin
      if p > 1 then
      begin
        se := Copy(s, 1, p - 1);
        LText := se;
        PushElement;
        Delete(s, 1, p - 1);
      end;
      p := Pos('>', s);
      if p > 0 then
      begin
        st := Copy(s, 2, p - 2);
        Delete(s, 1, p);
        ParseTag(st);
      end;
    end;
  until p = 0;
end;

procedure TJvMarkupViewer.RenderHTML;
var
  R: trect;
  X, Y, xav, clw: Integer;
  BaseLine: Integer;
  I, C: Integer;
  el: TJvHTMLElement;
  eol: Boolean;
  ml: Integer; // margin left
  isol, ieol: Integer;
  MaxHeight, MaxAscent: Integer;
  PendingBreak: Boolean;

  procedure SetFont(ee: TJvHTMLElement);
  begin
    with FBmp.Canvas do
    begin
      Font.Name := ee.FontName;
      Font.Size := ee.FontSize;
      Font.Style := ee.FontStyle;
      Font.Color := ee.FontColor;
    end;
  end;

  procedure RenderString(ee: TJvHTMLElement);
  var
    SS: string;
    w: Integer;
  begin
    SetFont(ee);
    if ee.SolText <> '' then
    begin
      SS := ee.SolText;
      w := FBmp.Canvas.TextWidth(SS);
      FBmp.Canvas.TextOut(X, Y + BaseLine - ee.Ascent - FrameTop, SS);
      X := X + w;
    end;
  end;

begin
  ieol := 0; // Not needed but removed Warning
  R := Rect(0, 0, FBmp.Width, FBmp.Height);
  FBmp.Canvas.Brush.Color := BackColor;
  FBmp.Canvas.FillRect(R);
  FBmp.TransparentColor := BackColor;
  FBmp.Transparent := True;
  C := FElementStack.Count;
  if C = 0 then
    Exit;
  HTMLClearBreaks;
  clw := FBmp.Width - FMarginRight;
  ml := MarginLeft;
  FBmp.Canvas.Brush.Style := bsClear;
  Y := FMarginTop;
  isol := 0;
  PendingBreak := False;
  repeat
    I := isol;
    xav := clw;
    MaxHeight := 0;
    MaxAscent := 0;
    eol := False;
    repeat // scan line
      el := TJvHTMLElement(FElementStack.Items[I]);
      if el.BreakLine then
      begin
        if not PendingBreak then
        begin
          eol := True;
          ieol := I - 1;
          //  break;
        end;
        PendingBreak := not PendingBreak;
      end;
      if not PendingBreak then
      begin
        if el.Height > MaxHeight then
          MaxHeight := el.Height;
        if el.Ascent > MaxAscent then
          MaxAscent := el.Ascent;
        el.Breakup(FBmp.Canvas, xav);
        if el.SolText <> '' then
        begin
          xav := xav - FBmp.Canvas.TextWidth(el.SolText);
          if el.EolText = '' then
          begin
            if I >= C - 1 then
            begin
              eol := True;
              ieol := I;
            end
            else
              Inc(I);
          end
          else
          begin
            eol := True;
            ieol := I;
          end;
        end
        else
        begin
          eol := True;
          ieol := I;
        end;
      end;
    until eol;

    // render line, only when in visible frame
    X := ml;
    BaseLine := MaxAscent;
    if (Y + MaxHeight >= FrameTop) and (Y <= FrameBottom) then
      for I := isol to ieol do
      begin
        el := TJvHTMLElement(FElementStack.Items[I]);
        RenderString(el);
      end;
    Y := Y + MaxHeight;
    if not PendingBreak then
      isol := ieol
    else
      isol := ieol + 1;
  until (ieol >= C - 1) and (el.EolText = '');
  PageBottom := Y;
end;

procedure TJvMarkupViewer.ScrollViewer(Sender: TObject);
begin
  FrameTop := FScrollBar.Position;
  FrameBottom := FrameTop + ClientHeight - 1;
  RenderHTML;
  Canvas.Draw(0, 0, FBmp);
end;

procedure TJvMarkupViewer.SetBackColor(const Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupViewer.SetMarginLeft(const Value: Integer);
begin
  if Value <> FMarginLeft then
  begin
    FMarginLeft := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupViewer.SetMarginRight(const Value: Integer);
begin
  if Value <> FMarginRight then
  begin
    FMarginRight := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupViewer.SetMarginTop(const Value: Integer);
begin
  if Value <> FMarginTop then
  begin
    FMarginTop := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupViewer.SetText(const Value: TCaption);
var
  s: string;
begin


  if Value <> GetText then
    Exit;

  s := Value;
  s := StringReplace(s, sLineBreak, ' ', [rfReplaceAll]);
  s := TrimRight(s);
  ParseHTML(s);
  HTMLElementDimensions;  
  inherited SetText(s); 
end;

end.

