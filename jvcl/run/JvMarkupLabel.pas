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

{$I JVCL.INC}

unit JvMarkupLabel;

{$OBJEXPORTALL On}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  JvComponent, JvMarkupCommon;

type
  TJvMarkupLabel = class(TJvGraphicControl)
  private
    FElementStack: TJvHTMLElementStack;
    FTagStack: TJvHTMLElementStack;
    FText: string;
    FBackColor: TColor;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    procedure ParseHTML(S: string);
    procedure RenderHTML;
    procedure HTMLClearBreaks;
    procedure HTMLElementDimensions;
    procedure SetBackColor(const Value: TColor);
    procedure SetText(const Value: string);
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginRight(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property Height default 100;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft default 5;
    property MarginRight: Integer read FMarginRight write SetMarginRight default 5;
    property MarginTop: Integer read FMarginTop write SetMarginTop default 5;
    property Text: string read FText write SetText;
    property Width default 200;
  end;

implementation

uses
  JvConsts, JvThemes;

constructor TJvMarkupLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);
  FElementStack := TJvHTMLElementStack.Create;
  FTagStack := TJvHTMLElementStack.Create;
  FBackColor := clWhite;
  Width := 200;
  Height := 100;
  FMarginLeft := 5;
  FMarginRight := 5;
  FMarginTop := 5;
end;

destructor TJvMarkupLabel.Destroy;
begin
  FElementStack.Free;
  FTagStack.Free;
  inherited Destroy;
end;

procedure TJvMarkupLabel.HTMLClearBreaks;
var
  I: Integer;
  El: TJvHTMLElement;
begin
  for I := 0 to FElementStack.Count - 1 do
  begin
    El := TJvHTMLElement(FElementStack.Items[I]);
    El.SolText := '';
    El.EolText := '';
  end;
end;

procedure TJvMarkupLabel.HTMLElementDimensions;
var
  I: Integer;
  El: TJvHTMLElement;
  Tm: TEXTMETRIC;
begin
  for I := 0 to FElementStack.Count - 1 do
  begin
    El := TJvHTMLElement(FElementStack.Items[I]);
    Canvas.Font.Name := El.FontName;
    Canvas.Font.Size := El.FontSize;
    Canvas.Font.Style := El.FontStyle;
    Canvas.Font.Color := El.FontColor;
    GetTextMetrics(Canvas.Handle, Tm);
    El.Height := Tm.tmHeight;
    El.Ascent := Tm.tmAscent;
    El.Width := Canvas.TextWidth(El.Text);
  end;
end;

procedure TJvMarkupLabel.Paint;
begin
  RenderHTML;
end;

procedure TJvMarkupLabel.ParseHTML(S: string);
var
  P: Integer;
  se, st: string;
  ftext: string;
  fstyle: TFontStyles;
  fname: string;
  fsize: Integer;
  BreakLine: Boolean;
  aColor, fColor: TColor;
  Element: TJvHTMLElement;

  function HTMLStringToColor(V: string; var Col: TColor): Boolean;
  var
    vv: string;
  begin
    if Copy(V, 1, 1) <> '#' then
    begin
      vv := 'cl' + V;
      try
        Col := StringToColor(vv);
        Result := True;
      except
        Result := False;
      end;
    end
    else
    begin
      try
        vv := '$' + Copy(V, 6, 2) + Copy(V, 4, 2) + Copy(V, 2, 2);
        Col := StringToColor(vv);
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
    Element := FTagStack.Pop;
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
    Element.BreakLine := BreakLine;
    BreakLine := False;
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
    if pp = 0 then // tag only
      atag := ss
    else
    begin // tag + attributes
      atag := Copy(ss, 1, pp - 1);
      ss := Trim(Copy(ss, pp + 1, Length(ss)));
      havepar := True;
    end;
    // handle atag
    atag := LowerCase(atag);
    if atag = 'br' then
      BreakLine := True
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
              end;
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
  BreakLine := False;
  repeat
    P := Pos('<', S);
    if P = 0 then
    begin
      fText := S;
      PushElement;
    end
    else
    begin
      if P > 1 then
      begin
        se := Copy(S, 1, P - 1);
        ftext := se;
        PushElement;
        Delete(S, 1, P - 1);
      end;
      P := Pos('>', S);
      if P > 0 then
      begin
        st := Copy(S, 2, P - 2);
        Delete(S, 1, P);
        ParseTag(st);
      end;
    end;
  until P = 0;
end;

procedure TJvMarkupLabel.RenderHTML;
var
  R: TRect;
  x, y, xav, clw: Integer;
  baseline: Integer;
  I, C: Integer;
  El: TJvHTMLElement;
  eol: Boolean;
  ml: Integer; // margin left
  isol, ieol: Integer;
  maxheight, maxascent: Integer;
  pendingBreak: Boolean;

  procedure SetFont(ee: TJvHTMLElement);
  begin
    with Canvas do
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
      ww := Canvas.TextWidth(ss);
      Canvas.TextOut(x, y + baseline - ee.Ascent, ss);
      x := x + ww;
    end;
  end;

begin
  ieol := 0;
  R := ClientRect;
  Canvas.Brush.Color := BackColor;
  DrawThemedBackground(Self, Canvas, R);
  C := FElementStack.Count;
  if C = 0 then
    Exit;
  HTMLClearBreaks;
  clw := ClientWidth - FMarginRight;
  ml := MarginLeft;
  Canvas.Brush.Style := bsClear;
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
      El := TJvHTMLElement(FElementStack.Items[I]);
      if El.BreakLine then
      begin
        if not pendingBreak then
        begin
          pendingBreak := True;
          ieol := I;
          Break;
        end
        else
          pendingBreak := False;
      end;
      if El.Height > maxheight then
        maxheight := El.Height;
      if El.Ascent > maxAscent then
        maxAscent := El.Ascent;
      El.Breakup(Canvas, xav);
      if El.SolText <> '' then
      begin
        xav := xav - Canvas.TextWidth(El.SolText);
        if El.EolText = '' then
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
      begin // eol
        eol := True;
        ieol := I;
      end;
    until eol;
    // render line
    x := ml;
    baseline := maxAscent;
    for I := isol to ieol do
    begin
      El := TJvHTMLElement(FElementStack.Items[I]);
      RenderString(El);
    end;
    y := y + maxHeight;
    isol := ieol;
  until (ieol >= C - 1) and (El.EolText = '');
end;

procedure TJvMarkupLabel.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupLabel.SetMarginLeft(const Value: Integer);
begin
  if FMarginLeft <> Value then
  begin
    FMarginLeft := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupLabel.SetMarginRight(const Value: Integer);
begin
  if FMarginRight <> Value then
  begin
    FMarginRight := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupLabel.SetMarginTop(const Value: Integer);
begin
  if FMarginTop <> Value then
  begin
    FMarginTop := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupLabel.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    FText := StringReplace(FText, sLineBreak, ' ', [rfReplaceAll]);
    FText := TrimRight(FText);
    ParseHTML(FText);
    HTMLElementDimensions;
    Invalidate;
  end;
end;

end.

