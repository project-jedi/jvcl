{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMarkupCommon.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2003-07-17

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  * Classes extracted from JvMarkupLabel and JvMarkupViewer (duplicates)
  
-----------------------------------------------------------------------------}
unit JvMarkupCommon;

interface
uses
  Windows, SysUtils, Classes, Controls, Graphics;
  
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

end.
