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
    Element := TJvHTMLElement(FElementStack.items[I]);
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
  ftext: string;
  fstyle: TFontStyles;
  fname: string;
  fsize: Integer;
  fbreakLine: Boolean;
  aColor, fColor: TColor;
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
        col := stringtocolor(vv);
        Result := True;
      except
        Result := False;
      end
    end
  end;

  procedure PushTag;
  begin
    Element := TJvHTMLElement.Create;
    Element.FontName := fname;
    Element.FontSize := fsize;
    Element.FontStyle := fstyle;
    Element.FontColor := fColor;
    FTagStack.Push(Element);
  end;

  procedure PopTag;
  begin
    Element := FTagStack.pop;
    if Element <> nil then
    begin
      fname := Element.FontName;
      fsize := Element.FontSize;
      fstyle := Element.FontStyle;
      fcolor := Element.FontColor;
      Element.Free;
    end;
  end;

  procedure PushElement;
  begin
    Element := TJvHTMLElement.Create;
    Element.Text := ftext;
    Element.FontName := fname;
    Element.FontSize := fsize;
    Element.FontStyle := fstyle;
    Element.FontColor := fColor;
    Element.BreakLine := fBreakLine;
    fBreakLine := False;
    FElementStack.Push(Element);
  end;

  procedure ParseTag(ss: string);
  var
    pp: Integer;
    atag, apar, aval: string;
    havepar: Boolean;
  begin
    ss := Trim(ss);
    havepar := False;
    pp := Pos(' ', ss);
    if pp = 0 then
      atag := ss // tag only
    else
    begin // tag + attributes
      atag := Copy(ss, 1, pp - 1);
      ss := Trim(Copy(ss, pp + 1, Length(ss)));
      havepar := True;
    end;
    // handle atag
    atag := LowerCase(atag);
    if atag = 'br' then
      fBreakLine := True
    else
    if atag = 'b' then
    begin // bold
      PushTag;
      fstyle := fstyle + [fsBold];
    end
    else
    if atag = '/b' then
    begin // cancel bold
      fstyle := fstyle - [fsBold];
      PopTag;
    end
    else
    if atag = 'I' then
    begin // italic
      PushTag;
      fstyle := fstyle + [fsItalic];
    end
    else
    if atag = '/I' then
    begin // cancel italic
      fstyle := fstyle - [fsItalic];
      PopTag;
    end
    else
    if atag = 'u' then
    begin // underline
      PushTag;
      fstyle := fstyle + [fsUnderline];
    end
    else
    if atag = '/u' then
    begin // cancel underline
      fstyle := fstyle - [fsUnderline];
      PopTag;
    end
    else
    if atag = 'font' then
      PushTag
    else
    if atag = '/font' then
      PopTag;
    if havepar then
    begin
      repeat
        pp := Pos('="', ss);
        if pp > 0 then
        begin
          aPar := LowerCase(Trim(Copy(ss, 1, pp - 1)));
          Delete(ss, 1, pp + 1);
          pp := Pos('"', ss);
          if pp > 0 then
          begin
            aVal := Copy(ss, 1, pp - 1);
            Delete(ss, 1, pp);
            if aPar = 'face' then
              fname := aVal
            else
            if aPar = 'size' then
              try
                fsize := StrToInt(aval);
              except
              end
            else
            if aPar = 'color' then
              try
                if HTMLStringToColor(aval, aColor) then
                  fcolor := aColor;
              except
              end
          end;
        end;
      until pp = 0;
    end;
  end;

begin
  FElementStack.Clear;
  FTagStack.Clear;
  fstyle := [];
  fname := 'arial';
  fsize := 12;
  fColor := clBlack;
  fBreakLine := False;
  repeat
    p := Pos('<', s);
    if p = 0 then
    begin
      fText := s;
      PushElement;
    end
    else
    begin
      if p > 1 then
      begin
        se := Copy(s, 1, p - 1);
        ftext := se;
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
  x, y, xav, clw: Integer;
  baseline: Integer;
  I, C: Integer;
  el: TJvHTMLElement;
  eol: Boolean;
  ml: Integer; // margin left
  isol, ieol: Integer;
  maxheight, maxascent: Integer;
  pendingBreak: Boolean;

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
    ss: string;
    ww: Integer;
  begin
    SetFont(ee);
    if ee.SolText <> '' then
    begin
      ss := ee.SolText;
      ww := FBmp.Canvas.TextWidth(ss);
      FBmp.Canvas.TextOut(x, y + baseline - ee.Ascent - FrameTop, ss);
      x := x + ww;
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
  y := FMarginTop;
  isol := 0;
  pendingBreak := False;
  repeat
    I := isol;
    xav := clw;
    maxHeight := 0;
    maxAscent := 0;
    eol := False;
    repeat // scan line
      el := TJvHTMLElement(FElementStack.Items[I]);
      if el.BreakLine then
      begin
        if not pendingBreak then
        begin
          eol := True;
          ieol := I - 1;
          //  break;
        end;
        pendingBreak := not pendingBreak;
      end;
      if not pendingBreak then
      begin
        if el.Height > maxheight then
          maxheight := el.Height;
        if el.Ascent > maxascent then
          maxAscent := el.Ascent;
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
    x := ml;
    baseline := maxAscent;
    if (y + MaxHeight >= FrameTop) and (y <= FrameBottom) then
      for I := isol to ieol do
      begin
        el := TJvHTMLElement(FElementStack.Items[I]);
        RenderString(el);
      end;
    y := y + maxHeight;
    if not pendingBreak then
      isol := ieol
    else
      isol := ieol + 1;
  until (ieol >= C - 1) and (el.EolText = '');
  PageBottom := y;
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

