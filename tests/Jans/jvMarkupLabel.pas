{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMarkupLabel.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}

unit JvMarkupLabel;

{$OBJEXPORTALL On}

interface

uses
  Windows, SysUtils, Classes, Graphics, JvComponent;

type
  TJvHTMLElement = class(TObject)
  private
    FFontSize: integer;
    FText: string;
    FFontName: string;
    FFontStyle: TFontStyles;
    FFontColor: TColor;
    FAscent: integer;
    FHeight: integer;
    FWidth: integer;
    FSolText: string;
    FEolText: string;
    FBreakLine: boolean;
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetText(const Value: string);
    procedure SetFontColor(const Value: TColor);
    procedure SetAscent(const Value: integer);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetEolText(const Value: string);
    procedure SetSolText(const Value: string);
    procedure SetBreakLine(const Value: boolean);
  protected
  public
    procedure Break(ACanvas: TCanvas; available: integer);
    property Text: string read FText write SetText;
    property SolText: string read FSolText write SetSolText;
    property EolText: string read FEolText write SetEolText;
    property FontName: string read FFontName write SetFontName;
    property FontSize: integer read FFontSize write SetFontSize;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property FontColor: TColor read FFontColor write SetFontColor;
    property Height: integer read FHeight write SetHeight;
    property Width: integer read FWidth write SetWidth;
    property Ascent: integer read FAscent write SetAscent;
    property BreakLine: boolean read FBreakLine write SetBreakLine;
  end;

  TJvHTMLElementStack = class(TList)
  private
  protected
  public
    destructor Destroy; override;
    procedure Clear; override;
    // will free ALL elements in the stack
    procedure push(Element: TJvHTMLElement);
    function pop: TJvHTMLElement;
    // calling routine is responsible for freeing the element.
    function peek: TJvHTMLElement;
    // calling routine must NOT free the element
  end;

  TJvMarkupLabel = class(TJvGraphicControl)
  private
    { Private declarations }
    ElementStack: TJvHTMLElementStack;
    TagStack: TJvHTMLElementStack;
    FText: string;
    FBackColor: TColor;
    FMarginLeft: integer;
    FMarginRight: integer;
    FMarginTop: integer;
    procedure ParseHTML(s: string);
    procedure RenderHTML;
    procedure HTMLClearBreaks;
    procedure HTMLElementDimensions;
    procedure SetBackColor(const Value: TColor);
    procedure SetText(const Value: string);
    procedure SetMarginLeft(const Value: integer);
    procedure SetMarginRight(const Value: integer);
    procedure SetMarginTop(const Value: integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure paint; override;
  published
    { Published declarations }
    property Text: string read FText write SetText;
    property BackColor: TColor read FBackColor write SetBackColor;
    property MarginLeft: integer read FMarginLeft write SetMarginLeft;
    property MarginRight: integer read FMarginRight write SetMarginRight;
    property MarginTop: integer read FMarginTop write SetMarginTop;
  end;

implementation

{ TJvHTMLElement }

procedure TJvHTMLElement.Break(ACanvas: TCanvas; available: integer);
var
  s: string;
  i, w: integer;
begin
  Acanvas.font.name := fontname;
  Acanvas.font.size := fontsize;
  Acanvas.font.style := fontstyle;
  Acanvas.font.color := fontcolor;
  if solText = '' then
    s := Text
  else
    s := Eoltext;
  if acanvas.TextWidth(s) <= available then
  begin
    soltext := s;
    eoltext := '';
    exit;
  end;
  for i := length(s) downto 1 do
  begin
    if s[i] = ' ' then
    begin
      w := acanvas.TextWidth(copy(s, 1, i));
      if w <= available then
      begin
        soltext := copy(s, 1, i);
        eoltext := copy(s, i + 1, length(s));
        exit;
      end;
    end;
  end;
end;

procedure TJvHTMLElement.SetAscent(const Value: integer);
begin
  FAscent := Value;
end;

procedure TJvHTMLElement.SetBreakLine(const Value: boolean);
begin
  FBreakLine := Value;
end;

procedure TJvHTMLElement.SetEolText(const Value: string);
begin
  FEolText := Value;
end;

procedure TJvHTMLElement.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
end;

procedure TJvHTMLElement.SetFontName(const Value: string);
begin
  FFontName := Value;
end;

procedure TJvHTMLElement.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
end;

procedure TJvHTMLElement.SetFontStyle(const Value: TFontStyles);
begin
  FFontStyle := Value;
end;

procedure TJvHTMLElement.SetHeight(const Value: integer);
begin
  FHeight := Value;
end;

procedure TJvHTMLElement.SetSolText(const Value: string);
begin
  FSolText := Value;
end;

procedure TJvHTMLElement.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TJvHTMLElement.SetWidth(const Value: integer);
begin
  FWidth := Value;
end;

{ TJvHTMLElementStack }

procedure TJvHTMLElementStack.Clear;
var
  i, c: integer;
begin
  c := count;
  if c > 0 then
    for i := 0 to c - 1 do
      TJvHTMLElement(items[i]).free;
  inherited;
end;

destructor TJvHTMLElementStack.Destroy;
begin
  clear;
  inherited;
end;

function TJvHTMLElementStack.peek: TJvHTMLElement;
var
  c: integer;
begin
  c := count;
  if c = 0 then
    result := nil
  else
  begin
    result := TJvHTMLElement(items[c - 1]);
  end;
end;

function TJvHTMLElementStack.pop: TJvHTMLElement;
var
  c: integer;
begin
  c := count;
  if c = 0 then
    result := nil
  else
  begin
    result := TJvHTMLElement(items[c - 1]);
    delete(c - 1);
  end;
end;

procedure TJvHTMLElementStack.push(Element: TJvHTMLElement);
begin
  add(Element);
end;

{ TJvMarkupLabel }

constructor TJvMarkupLabel.create(AOwner: TComponent);
begin
  inherited;
  Elementstack := TJvHTMLElementStack.Create;
  TagStack := TJvHTMLElementStack.Create;
  FBackcolor := clwhite;
  Width := 200;
  Height := 100;
  FMarginLeft := 5;
  FMarginRight := 5;
  FMargintop := 5;
end;

destructor TJvMarkupLabel.destroy;
begin
  ElementStack.free;
  TagStack.free;
  inherited;
end;

procedure TJvMarkupLabel.HTMLClearBreaks;
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

procedure TJvMarkupLabel.HTMLElementDimensions;
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

procedure TJvMarkupLabel.paint;
begin
  RenderHTML;

end;

procedure TJvMarkupLabel.ParseHTML(s: string);
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
      poptag;
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

procedure TJvMarkupLabel.RenderHTML;
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
    with canvas do
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
      ww := canvas.TextWidth(ss);
      canvas.TextOut(x, y + baseline - ee.Ascent, ss);
      x := x + ww;
    end;
  end;

begin
  ieol := 0; // Not Needed but removes warning.
  R := clientrect;
  canvas.Brush.color := BackColor;
  canvas.FillRect(R);
  c := ElementStack.Count;
  if c = 0 then exit;
  HTMLClearBreaks;
  clw := ClientWidth - FMarginRight;
  ml := MarginLeft;
  canvas.Brush.style := bsclear;
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
          ieol := i;
          break;
        end
        else
          pendingBreak := false;
      end;
      if el.Height > maxheight then maxheight := el.Height;
      if el.Ascent > maxAscent then maxAscent := el.Ascent;
      el.Break(canvas, xav);
      if el.soltext <> '' then
      begin
        xav := xav - canvas.TextWidth(el.Soltext);
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
    until eol;
    // render line
    x := ml;
    baseline := maxAscent;
    for i := isol to ieol do
    begin
      el := TJvHTMLElement(ElementStack.items[i]);
      RenderString(el);
    end;
    y := y + maxHeight;
    isol := ieol;
  until (ieol >= c - 1) and (el.EolText = '');
end;

procedure TJvMarkupLabel.SetBackColor(const Value: TColor);
begin
  if value <> FBackColor then
  begin
    FBackcolor := Value;
    invalidate;
  end;
end;

procedure TJvMarkupLabel.SetMarginLeft(const Value: integer);
begin
  FMarginLeft := Value;
  invalidate;
end;

procedure TJvMarkupLabel.SetMarginRight(const Value: integer);
begin
  FMarginRight := Value;
  invalidate;
end;

procedure TJvMarkupLabel.SetMarginTop(const Value: integer);
begin
  FMarginTop := Value;
  invalidate;
end;

procedure TJvMarkupLabel.SetText(const Value: string);
const
  cr = chr(13) + chr(10);
  tab = chr(9);
var
  s: string;
begin
  if value = FText then exit;
  s := value;
  s := stringreplace(s, cr, ' ', [rfreplaceall]);
  s := Trimright(s);
  parseHTML(s);
  HTMLElementDimensions;
  FText := s;
  invalidate;
end;

end.
