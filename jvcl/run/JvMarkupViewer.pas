{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMarkupViewer.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvMarkupViewer;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms,
  Controls, JvComponent, JvMarkupCommon, StdCtrls;

type
  TJvMarkupViewer = class(TJvCustomControl)
  private
    { Private declarations }
    FScrollBar: TScrollBar;
    bm: TBitmap;
    FrameTop: Integer;
    FrameBottom: Integer;
    PageBottom: Integer;
    ElementStack: TJvHTMLElementStack;
    TagStack: TJvHTMLElementStack;
    FText: TCaption;
    FBackColor: TColor;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    procedure ParseHTML(s: string);
    procedure RenderHTML;
    procedure HTMLClearBreaks;
    procedure HTMLElementDimensions;
    procedure SetBackColor(const Value: TColor);
    procedure SetText(const Value: TCaption);
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginRight(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
    procedure ScrollViewer(Sender: TObject);
  protected
    { Protected declarations }
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    { Published declarations }
    property Align;
    property Text: TCaption read FText write SetText;
    property BackColor: TColor read FBackColor write SetBackColor;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft;
    property MarginRight: Integer read FMarginRight write SetMarginRight;
    property MarginTop: Integer read FMarginTop write SetMarginTop;
  end;

implementation

uses
  JvTypes, JvThemes;

{ TJvMarkupViewer }

constructor TJvMarkupViewer.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);
  Elementstack := TJvHTMLElementStack.Create;
  TagStack := TJvHTMLElementStack.Create;
  Width := 300;
  Height := 275;
  FMarginLeft := 5;
  FMarginRight := 5;
  FMargintop := 5;
  FBackcolor := clwhite;
end;

destructor TJvMarkupViewer.destroy;
begin
  ElementStack.free;
  TagStack.free;
  bm.free;
  FScrollBar.free;
  inherited;
end;

procedure TJvMarkupViewer.HTMLClearBreaks;
var
  i, c: integer;
  El: TJvHTMLElement;
begin
  c := ElementStack.Count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
  begin
    el := TJvHTMLElement(ElementStack.items[i]);
    el.SolText := '';
    el.EolText := '';
  end;
end;

procedure TJvMarkupViewer.HTMLElementDimensions;
var
  i, c: integer;
  El: TJvHTMLElement;
  h, a, w: integer;
  tm: Textmetric;
  s: string;
begin
  c := ElementStack.Count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
  begin
    el := TJvHTMLElement(ElementStack.items[i]);
    s := el.Text;
    canvas.font.name := el.FontName;
    canvas.font.size := el.FontSize;
    canvas.font.style := el.FontStyle;
    canvas.font.Color := el.FontColor;
    gettextmetrics(canvas.handle, tm);
    h := tm.tmHeight;
    a := tm.tmAscent;
    w := canvas.TextWidth(s);
    el.Height := h;
    el.Ascent := a;
    el.Width := w;
  end;
end;

procedure TJvMarkupViewer.CreateWnd;
begin
  inherited;
  FScrollBar := TScrollBar.create(self);
  FScrollBar.kind := sbVertical;
  FScrollBar.parent := self;
  FScrollBar.align := alright;
  FScrollBar.min := 0;
  FScrollBar.max := 0;
  FScrollBar.OnChange := ScrollViewer;
  FrameTop := 0;
  FrameBottom := clientHeight;
  bm := Tbitmap.create;
  bm.width := clientWidth - FScrollbar.width;
  bm.height := clientHeight;
end;

procedure TJvMarkupViewer.Paint;
var
  sm: integer;
  w, h: integer;
begin
  w := ClientWidth - FScrollbar.Width;
  h := ClientHeight;
  if bm.width <> w then bm.width := w;
  if bm.height <> h then bm.height := h;
  RenderHTML;
  Canvas.Draw(0, 0, bm);
  FScrollbar.Min := 0;
  sm := PageBottom - clientheight;
  if sm > 0 then
    FScrollBar.Max := sm
  else
    FScrollBar.max := 0;
  FScrollbar.Position := 0;
  FScrollbar.Largechange := trunc(0.8 * ClientHeight);
end;

procedure TJvMarkupViewer.ParseHTML(s: string);
var
  p: integer;
  se, st: string;
  ftext: string;
  fstyle: TfontStyles;
  fname: string;
  fsize: integer;
  fbreakLine: boolean;
  aColor, fColor: Tcolor;
  Element: TJvHTMLElement;

  function HTMLStringToColor(v: string; var col: Tcolor): boolean;
  var
    vv: string;
  begin
    if copy(v, 1, 1) <> '#' then
    begin
      vv := 'cl' + v;
      try
        col := stringtoColor(vv);
        result := true;
      except
        result := false;
      end;
    end
    else
    begin
      try
        vv := '$' + copy(v, 6, 2) + copy(v, 4, 2) + copy(v, 2, 2);
        col := stringtocolor(vv);
        result := true;
      except
        result := false;
      end
    end
  end;

  procedure pushTag;
  begin
    Element := TJvHTMLElement.Create;
    element.FontName := fname;
    element.FontSize := fsize;
    element.FontStyle := fstyle;
    element.FontColor := fColor;
    TagStack.push(Element);
  end;

  procedure popTag;
  begin
    Element := TagStack.pop;
    if element <> nil then
    begin
      fname := element.FontName;
      fsize := element.FontSize;
      fstyle := element.FontStyle;
      fcolor := element.FontColor;
      Element.Free;
    end;
  end;

  procedure pushElement;
  begin
    Element := TJvHTMLElement.Create;
    Element.Text := ftext;
    element.FontName := fname;
    element.FontSize := fsize;
    element.FontStyle := fstyle;
    element.FontColor := fColor;
    element.BreakLine := fBreakLine;
    fBreakLine := false;
    ElementStack.push(Element);
  end;

  procedure parseTag(ss: string);
  var
    pp: integer;
    atag, apar, aval: string;
    havepar: boolean;
  begin
    ss := trim(ss);
    havepar := false;
    pp := pos(' ', ss);
    if pp = 0 then
    begin // tag only
      atag := ss;
    end
    else
    begin // tag + atrributes
      atag := copy(ss, 1, pp - 1);
      ss := trim(copy(ss, pp + 1, length(ss)));
      havepar := true;
    end;
    // handle atag
    atag := lowercase(atag);
    if atag = 'br' then
      fBreakLine := true
    else if atag = 'b' then
    begin // bold
      pushtag;
      fstyle := fstyle + [fsbold];
    end
    else if atag = '/b' then
    begin // cancel bold
      fstyle := fstyle - [fsbold];
      poptag;
    end
    else if atag = 'i' then
    begin // italic
      pushtag;
      fstyle := fstyle + [fsitalic];
    end
    else if atag = '/i' then
    begin // cancel italic
      fstyle := fstyle - [fsitalic];
      poptag;
    end
    else if atag = 'u' then
    begin // underline
      pushtag;
      fstyle := fstyle + [fsunderline];
    end
    else if atag = '/u' then
    begin // cancel underline
      fstyle := fstyle - [fsunderline];
      poptag;
    end
    else if atag = 'font' then
    begin
      pushtag;
    end
    else if atag = '/font' then
    begin
      poptag
    end;
    if havepar then
    begin
      repeat
        pp := pos('="', ss);
        if pp > 0 then
        begin
          aPar := lowercase(trim(copy(ss, 1, pp - 1)));
          delete(ss, 1, pp + 1);
          pp := pos('"', ss);
          if pp > 0 then
          begin
            aVal := copy(ss, 1, pp - 1);
            delete(ss, 1, pp);
            if aPar = 'face' then
            begin
              fname := aVal;
            end
            else if aPar = 'size' then
            try
              fsize := strtoint(aval);
            except
            end
            else if aPar = 'color' then
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
  ElementStack.Clear;
  TagStack.Clear;
  fstyle := [];
  fname := 'arial';
  fsize := 12;
  fColor := clblack;
  fBreakLine := false;
  repeat
    p := pos('<', s);
    if p = 0 then
    begin
      fText := s;
      PushElement;
    end
    else
    begin
      if p > 1 then
      begin
        se := copy(s, 1, p - 1);
        ftext := se;
        pushElement;
        delete(s, 1, p - 1);
      end;
      p := pos('>', s);
      if p > 0 then
      begin
        st := copy(s, 2, p - 2);
        delete(s, 1, p);
        parseTag(st);
      end;
    end;
  until p = 0;
end;

procedure TJvMarkupViewer.RenderHTML;
var
  R: trect;
  x, y, xav, clw: integer;
  baseline: integer;
  i, c: integer;
  el: TJvHTMLElement;
  eol: boolean;
  ml: integer; // margin left
  isol, ieol: integer;
  maxheight, maxascent: integer;
  pendingBreak: boolean;

  procedure SetFont(ee: TJvHTMLElement);
  begin
    with bm.canvas do
    begin
      font.name := ee.FontName;
      font.Size := ee.FontSize;
      font.Style := ee.FontStyle;
      font.Color := ee.FontColor;
    end;
  end;

  procedure RenderString(ee: TJvHTMLElement);
  var
    ss: string;
    ww: integer;
  begin
    SetFont(ee);
    if ee.soltext <> '' then
    begin
      ss := ee.SolText;
      ww := bm.canvas.TextWidth(ss);
      bm.canvas.TextOut(x, y + baseline - ee.Ascent - frameTop, ss);
      x := x + ww;
    end;
  end;

begin
  ieol := 0; // Not needed but removed Warning
  R := Rect(0, 0, bm.width, bm.height);
  bm.Canvas.Brush.color := BackColor;
  bm.Canvas.FillRect(R);
  bm.TransparentColor := BackColor;
  bm.Transparent := True;
  c := ElementStack.Count;
  if c = 0 then exit;
  HTMLClearBreaks;
  clw := bm.width - FMarginRight;
  ml := MarginLeft;
  bm.canvas.Brush.style := bsclear;
  y := FMarginTop;
  isol := 0;
  pendingBreak := false;
  repeat
    i := isol;
    xav := clw;
    maxHeight := 0;
    maxAscent := 0;
    eol := false;
    repeat // scan line
      el := TJvHTMLElement(ElementStack.items[i]);
      if el.BreakLine then
      begin
        if not pendingBreak then
        begin
          pendingBreak := true;
          eol := true;
          ieol := i - 1;
          //  break;
        end
        else
          pendingBreak := false;
      end;
      if not pendingBreak then
      begin
        if el.Height > maxheight then maxheight := el.Height;
        if el.Ascent > maxAscent then maxAscent := el.Ascent;
        el.Breakup(bm.canvas, xav);
        if el.soltext <> '' then
        begin
          xav := xav - bm.canvas.TextWidth(el.Soltext);
          if el.EolText = '' then
          begin
            if i >= c - 1 then
            begin
              eol := true;
              ieol := i;
            end
            else
            begin
              inc(i);
            end
          end
          else
          begin
            eol := true;
            ieol := i;
          end;
        end
        else
        begin // eol
          eol := true;
          ieol := i;
        end;
      end;
    until eol;
    // render line, only when in visible frame

    x := ml;
    baseline := maxAscent;
    if (y + MaxHeight >= FrameTop) and (y <= FrameBottom) then
      for i := isol to ieol do
      begin
        el := TJvHTMLElement(ElementStack.items[i]);
        RenderString(el);
      end;
    y := y + maxHeight;
    if not pendingbreak then
      isol := ieol
    else
      isol := ieol + 1;
  until (ieol >= c - 1) and (el.EolText = '');
  PageBottom := y;
end;

procedure TJvMarkupViewer.ScrollViewer(Sender: TObject);
begin
  FrameTop := FScrollBar.Position;
  FrameBottom := FrameTop + ClientHeight - 1;
  RenderHTML;
  Canvas.Draw(0, 0, bm);
end;

procedure TJvMarkupViewer.SetBackColor(const Value: TColor);
begin
  if value <> FBackColor then
  begin
    FBackcolor := Value;
    invalidate;
  end;
end;

procedure TJvMarkupViewer.SetMarginLeft(const Value: integer);
begin
  FMarginLeft := Value;
  invalidate;
end;

procedure TJvMarkupViewer.SetMarginRight(const Value: integer);
begin
  FMarginRight := Value;
  invalidate;
end;

procedure TJvMarkupViewer.SetMarginTop(const Value: integer);
begin
  FMarginTop := Value;
  invalidate;
end;

procedure TJvMarkupViewer.SetText(const Value: TCaption);
var
  s: string;
begin
  if value = FText then exit;
  s := value;
  s := stringreplace(s, sLineBreak, ' ', [rfreplaceall]);
  s := Trimright(s);
  parseHTML(s);
  HTMLElementDimensions;
  FText := s;
  invalidate;
end;

end.
