{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRichEditToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvRichEditToHtml;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  SysUtils, Classes, Graphics, Forms, ComCtrls,
  JvRgbToHtml, JvStrToHtml, JvRichEdit, JvComponent;

type
  TJvParaAttributesRec = record
    Alignment: TAlignment;
    Numbering: TNumberingStyle;
  end;

  TJvRichEditParaAttributesRec = record
    Alignment: TParaAlignment;
    Numbering: TJvNumbering;
  end;

  TJvRichEditToHtml = class(TJvComponent)
  private
    FCToH: TJvRgbToHtml;
    FCharToH: TJvStrToHtml;
    FEndSection: string;
    FEndPara: string;
    FTitle: string;
    FFooter: TStringList;
    FHeader: TStringList;
    function AttToHtml(Value: TFont): string;
    function ParaToHtml(Value: TJvParaAttributesRec): string;overload;
    function ParaToHtml(Value: TJvRichEditParaAttributesRec): string;overload;
    function GetFooter: TStrings;
    function GetHeader: TStrings;
    procedure SetFooter(const Value: TStrings);
    procedure SetHeader(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConvertToHtml(Value: TRichEdit; const FileName: string);overload;
    procedure ConvertToHtml(Value: TJvRichEdit; const FileName: string);overload;
    procedure ConvertToHtmlStrings(Value: TRichEdit; Strings: TStrings);overload;
    procedure ConvertToHtmlStrings(Value: TJvRichEdit; Strings: TStrings);overload;
  published
    property Title: string read FTitle write FTitle;
    property Header: TStrings read GetHeader write SetHeader;
    property Footer: TStrings read GetFooter write SetFooter;
  end;

implementation

const
  // (rom) needs renaming?
//  cHTMLHeadBegin = '<HTML>';
//  cHTMLBodyBegin = '<BODY>';
//  cHTMLBodyEnd = '</BODY>';
//  cHTMLEnd = '</HTML>';
//  cHTMLTitleFmt = '<TITLE>%s</TITLE>';

  cHTMLBR = '<BR>';
//  cHTMLFontColorBegin = '<FONT COLOR=#';
//  cHTMLSize = ' SIZE=';
//  cHTMLFace = ' FACE="';
  cHTMLFontEnd = '</FONT>';

  cHTMLBoldBegin = '<B>';
  cHTMLBoldEnd = '</B>';

  cHTMLItalicBegin = '<I>';
  cHTMLItalicEnd = '</I>';

  cHTMLStrikeoutBegin = '<STRIKE>';
  cHTMLStrikeoutEnd = '</STRIKE>';

  cHTMLUnderlineBegin = '<U>';
  cHTMLUnderlineEnd = '</U>';

  cHTMLParaEnd = '</P>';
  cHTMLParaLeft = '<P ALIGN="LEFT">';
  cHTMLParaRight = '<P ALIGN="RIGHT">';
  cHTMLParaCenter = '<P ALIGN="CENTER">';

  cHTMLListBegin = '<LI>';
  cHTMLListEnd = '</LI>';

constructor TJvRichEditToHtml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCToH := TJvRgbToHtml.Create(Self);
  FCharToH := TJvStrToHtml.Create(Self);
  FHeader := TStringList.Create;
  FHeader.Add('<HTML>');
  FHeader.Add('  <HEAD>');
  FHeader.Add('    <TITLE><#TITLE></TITLE>');
  FHeader.Add('  </HEAD>');
  FHeader.Add('  <BODY>');

  FFooter := TStringList.Create;
  FFooter.Add('  </BODY>');
  FFooter.Add('</HTML>');
end;

destructor TJvRichEditToHtml.Destroy;
begin
  FCToH.Free;
  FCharToH.Free;
  FHeader.Free;
  FFooter.Free;
  inherited Destroy;
end;

procedure TJvRichEditToHtml.ConvertToHtml(Value: TRichEdit; const FileName: string);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    ConvertToHtmlStrings(Value, S);
    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;

function TJvRichEditToHtml.AttToHtml(Value: TFont): string;
begin
  FEndSection := cHTMLFontEnd;
  FCToH.RgbColor := Value.Color;
  Result := Format('<FONT COLOR="#%s" SIZE="%d" FACE="%s">',
                    [FCToH.HtmlColor,(Value.Size mod 8) + 2,Value.Name]);
  if fsBold in Value.Style then
  begin
    FEndSection := cHTMLBoldEnd + FEndSection;
    Result := Result + cHTMLBoldBegin;
  end;
  if fsItalic in Value.Style then
  begin
    FEndSection := cHTMLItalicEnd + FEndSection;
    Result := Result + cHTMLItalicBegin;
  end;
  if fsStrikeout in Value.Style then
  begin
    FEndSection := cHTMLStrikeoutEnd + FEndSection;
    Result := Result + cHTMLStrikeoutBegin;
  end;
  if fsUnderline in Value.Style then
  begin
    FEndSection := cHTMLUnderlineEnd + FEndSection;
    Result := Result + cHTMLUnderlineBegin;
  end;
end;

function TJvRichEditToHtml.ParaToHtml(Value: TJvParaAttributesRec): string;
begin
  FEndPara := cHTMLParaEnd;
  case Value.Alignment of
    Classes.taLeftJustify:
      Result := cHTMLParaLeft;
    Classes.taRightJustify:
      Result := cHTMLParaRight;
    Classes.taCenter:
      Result := cHTMLParaCenter;
  end;
  if Value.Numbering = ComCtrls.nsBullet then
  begin
    Result := cHTMLListBegin + Result;
    FEndPara := FEndPara + cHTMLListEnd;
  end;
end;

function Diff(One, Two: TFont): Boolean;
begin
  Result := (One.Color <> Two.Color) or (One.Style <> Two.Style) or
    (One.Name <> Two.Name) or (One.Size <> Two.Size);
end;

function DiffPara(One, Two: TJvParaAttributesRec): Boolean;overload;
begin
  Result := (One.Alignment <> Two.Alignment) or (One.Numbering <> Two.Numbering);
end;

function DiffPara(One, Two: TJvRichEditParaAttributesRec): Boolean;overload;
begin
  Result := (One.Alignment <> Two.Alignment) or (One.Numbering <> Two.Numbering);
end;

procedure TJvRichEditToHtml.ConvertToHtmlStrings(Value: TRichEdit; Strings: TStrings);
var
  I, J, K: Integer;
  Datt, Att, CurrAt: TFont;
  DPara, Para, CurrPara: TJvParaAttributesRec;
  St: string;
  FEnd: string;
begin
  Strings.BeginUpdate;
  Value.Lines.BeginUpdate;
  try
    Strings.Clear;
    Strings.Add(StringReplace(Header.Text, '<#TITLE>',Title,[rfReplaceAll]));
//    Strings.Add(cHTMLHeadBegin);
//    Strings.Add(Format(cHTMLTitleFmt, [Title]));
//    Strings.Add(cHTMLBodyBegin);
    Datt := TFont.Create;
    Att := TFont.Create;
    CurrAt := TFont.Create;

    DPara.Alignment := taLeftJustify;
    DPara.Numbering := ComCtrls.nsNone;
    CurrPara.Alignment := DPara.Alignment;
    CurrPara.Numbering := DPara.Numbering;
    FEndPara := '';

    Datt.Assign(Value.DefAttributes);
    Strings.Add(AttToHtml(Datt));
    FEnd := FEndSection;

    K := 0;
    CurrAt.Assign(Datt);
    FEndSection := '';
    for I := 0 to Value.Lines.Count - 1 do
    begin
      St := '';
      CurrPara.Numbering := ComCtrls.nsNone;
      if Length(Value.Lines[I]) > 0 then
      begin
        for J := 1 to Length(Value.Lines[I]) do
        begin
          Value.SelStart := K + J - 1;
          Value.SelLength := 1;
          Att.Assign(Value.SelAttributes);
          Para.Alignment := Value.Paragraph.Alignment;
          Para.Numbering := Value.Paragraph.Numbering;
          if Diff(Att, CurrAt) then
          begin
            St := St + FEndSection;
            CurrAt.Assign(Att);
            St := St + AttToHtml(Att);
          end;
          if DiffPara(Para, CurrPara) then
          begin
            St := St + FEndPara;
            CurrPara.Alignment := Para.Alignment;
            CurrPara.Numbering := Para.Numbering;
            St := St + ParaToHtml(Para);
          end;
          St := St + CharToHtml(Value.Lines[I][J]);
        end;
      end;
      K := K + Length(Value.Lines[I]) + 2;
      Strings.Add(cHTMLBR + St);
      Application.ProcessMessages;
    end;
    Strings.Add(FEndSection);
    Strings.Add(FEndPara);

    Datt.Free;
    Att.Free;
    CurrAt.Free;

    Strings.Add(FEnd);
    Strings.AddStrings(FFooter);
//    Strings.Add(cHTMLBodyEnd);
//    Strings.Add(cHTMLEnd);
  finally
    Strings.EndUpdate;
    Value.Lines.EndUpdate;
  end;
end;

procedure TJvRichEditToHtml.ConvertToHtml(Value: TJvRichEdit;
  const FileName: string);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    ConvertToHtmlStrings(Value, S);
    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;

procedure TJvRichEditToHtml.ConvertToHtmlStrings(Value: TJvRichEdit;
  Strings: TStrings);
var
  I, J, K: Integer;
  Datt, Att, CurrAt: TFont;
  DPara, Para, CurrPara: TJvRichEditParaAttributesRec;
  St: string;
  FEnd: string;
begin
  Strings.BeginUpdate;
  Value.Lines.BeginUpdate;
  try
    Strings.Clear;
//    Strings.Add(cHTMLHeadBegin);
//    Strings.Add(Format(cHTMLTitleFmt, [Title]));
//    Strings.Add(cHTMLBodyBegin);
    Strings.Add(StringReplace(Header.Text, '<#TITLE>',Title,[rfReplaceAll]));
    Datt := TFont.Create;
    Att := TFont.Create;
    CurrAt := TFont.Create;

    DPara.Alignment := paLeftJustify;
    DPara.Numbering := nsNone;
    CurrPara.Alignment := DPara.Alignment;
    CurrPara.Numbering := DPara.Numbering;
    FEndPara := '';

    Datt.Assign(Value.DefAttributes);
    Strings.Add(AttToHtml(Datt));
    FEnd := FEndSection;

    K := 0;
    CurrAt.Assign(Datt);
    FEndSection := '';
    for I := 0 to Value.Lines.Count - 1 do
    begin
      St := '';
      CurrPara.Numbering := nsNone;
      if Length(Value.Lines[I]) > 0 then
      begin
        for J := 1 to Length(Value.Lines[I]) do
        begin
          Value.SelStart := K + J - 1;
          Value.SelLength := 1;
          Att.Assign(Value.SelAttributes);
          Para.Alignment := Value.Paragraph.Alignment;
          Para.Numbering := Value.Paragraph.Numbering;
          if Diff(Att, CurrAt) then
          begin
            St := St + FEndSection;
            CurrAt.Assign(Att);
            St := St + AttToHtml(Att);
          end;
          if DiffPara(Para, CurrPara) then
          begin
            St := St + FEndPara;
            CurrPara.Alignment := Para.Alignment;
            CurrPara.Numbering := Para.Numbering;
            St := St + ParaToHtml(Para);
          end;
          St := St + CharToHtml(Value.Lines[I][J]);
        end;
      end;
      K := K + Length(Value.Lines[I]) + 2;
      Strings.Add(cHTMLBR + St);
      Application.ProcessMessages;
    end;
    Strings.Add(FEndSection);
    Strings.Add(FEndPara);

    Datt.Free;
    Att.Free;
    CurrAt.Free;

    Strings.Add(FEnd);
    Strings.AddStrings(Footer);
//    Strings.Add(cHTMLBodyEnd);
//    Strings.Add(cHTMLEnd);
  finally
    Strings.EndUpdate;
    Value.Lines.EndUpdate;
  end;
end;

function TJvRichEditToHtml.ParaToHtml(Value: TJvRichEditParaAttributesRec): string;
begin
  FEndPara := cHTMLParaEnd;
  case Value.Alignment of
    paLeftJustify:
      Result := cHTMLParaLeft;
    paRightJustify:
      Result := cHTMLParaRight;
    paCenter:
      Result := cHTMLParaCenter;
  end;
  if Value.Numbering = nsBullet then
  begin
    Result := cHTMLListBegin + Result;
    FEndPara := FEndPara + cHTMLListEnd;
  end;
end;

function TJvRichEditToHtml.GetFooter: TStrings;
begin
  Result := FFooter;
end;

function TJvRichEditToHtml.GetHeader: TStrings;
begin
  Result := FHeader;
end;

procedure TJvRichEditToHtml.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

procedure TJvRichEditToHtml.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

end.

