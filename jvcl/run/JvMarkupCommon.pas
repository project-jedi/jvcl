{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMarkupCommon.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  * Classes extracted from JvMarkupLabel and JvMarkupViewer (duplicates)
  
-----------------------------------------------------------------------------}
// $Id$

unit JvMarkupCommon;

{$I jvcl.inc}

interface

uses
  Windows, Controls, Graphics, SysUtils, Classes;
  
type
  TJvHTMLElement = class(TObject)
  private
    FFontSize: Integer;
    FText: string;
    FFontName: string;
    FFontStyle: TFontStyles;
    FFontColor: TColor;
    FAscent: Integer;
    FHeight: Integer;
    FWidth: Integer;
    FSolText: string;
    FEolText: string;
    FBreakLine: Boolean;
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetText(const Value: string);
    procedure SetFontColor(const Value: TColor);
    procedure SetAscent(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetEolText(const Value: string);
    procedure SetSolText(const Value: string);
    procedure SetBreakLine(const Value: Boolean);
  public
    procedure Breakup(ACanvas: TCanvas; Available: Integer);
    property Text: string read FText write SetText;
    property SolText: string read FSolText write SetSolText;
    property EolText: string read FEolText write SetEolText;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Integer read FFontSize write SetFontSize;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property FontColor: TColor read FFontColor write SetFontColor;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property Ascent: Integer read FAscent write SetAscent;
    property BreakLine: Boolean read FBreakLine write SetBreakLine;
  end;

  TJvHTMLElementStack = class(TList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    // will free ALL elements in the stack
    procedure Push(Element: TJvHTMLElement);
    function Pop: TJvHTMLElement;
    // calling routine is responsible for freeing the element.
    function Peek: TJvHTMLElement;
    // calling routine must NOT free the element
  end;


implementation

//=== { TJvHTMLElement } =====================================================

procedure TJvHTMLElement.Breakup(ACanvas: TCanvas; Available: Integer);
var
  S: string;
  I, W: Integer;
begin
  ACanvas.Font.Name := FontName;
  ACanvas.Font.Size := FontSize;
  ACanvas.Font.Style := FontStyle;
  ACanvas.Font.Color := FontColor;
  if SolText = '' then
    S := Text
  else
    S := EolText;
  if ACanvas.TextWidth(S) <= Available then
  begin
    SolText := S;
    EolText := '';
    Exit;
  end;
  for I := Length(S) downto 1 do
  begin
    if S[I] = ' ' then
    begin
      W := ACanvas.TextWidth(Copy(S, 1, I));
      if W <= Available then
      begin
        SolText := Copy(S, 1, I);
        EolText := Copy(S, I + 1, Length(S));
        Break;
      end;
    end;
  end;
end;

procedure TJvHTMLElement.SetAscent(const Value: Integer);
begin
  FAscent := Value;
end;

procedure TJvHTMLElement.SetBreakLine(const Value: Boolean);
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

procedure TJvHTMLElement.SetFontSize(const Value: Integer);
begin
  FFontSize := Value;
end;

procedure TJvHTMLElement.SetFontStyle(const Value: TFontStyles);
begin
  FFontStyle := Value;
end;

procedure TJvHTMLElement.SetHeight(const Value: Integer);
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

procedure TJvHTMLElement.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

//=== { TJvHTMLElementStack } ================================================

destructor TJvHTMLElementStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJvHTMLElementStack.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    TJvHTMLElement(Items[I]).Free;
  inherited Clear;
end;

function TJvHTMLElementStack.Peek: TJvHTMLElement;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TJvHTMLElement(Items[Count - 1]);
end;

function TJvHTMLElementStack.Pop: TJvHTMLElement;
begin
  if Count = 0 then
    Result := nil
  else
  begin
    Result := TJvHTMLElement(Items[Count - 1]);
    Delete(Count - 1);
  end;
end;

procedure TJvHTMLElementStack.Push(Element: TJvHTMLElement);
begin
  Add(Element);
end;

end.
