{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRichEditToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvRichEditToHtml;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, JvRgbToHtml, JvStrToHtml, JvComponent;

type
  TJvParaAttributes = record
    Alignment: TAlignment;
    Numbering: TNumberingStyle;
  end;
{$EXTERNALSYM TJvParaAttributes}

  TJvRichEditToHtml = class(TJvComponent)
  private
    FCToH: TJvRgbToHtml;
    FCharToH: TJvStrToHtml;
    FEndSection: string;
    FEndPara: string;
    FTitle: string;
    function AttToHtml(Value: Tfont): string;
    function ParaToHtml(Value: TJvParaAttributes): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConvertToHtml(Value: TRichEdit; const Filename: string);
    procedure ConvertToHtmlStringList(Value: TRichEdit; Strings: TStringList);
  published
    property Title: string read FTitle write FTitle;
  end;

implementation

resourcestring
  RC_Html1 = '<HTML>';
  RC_Html3 = '<BODY>';
  RC_Html4 = '<BR>';
  RC_Html5 = '</BODY>';
  RC_Html6 = '</HTML>';
  // (rom) needs renaming?
  RC_Html7 = '<TITLE>%s</TITLE>';

  RC_EndFont = '</FONT>';
  RC_Font1 = '<FONT COLOR=#';
  RC_Font2 = ' SIZE=';
  RC_Font3 = ' FACE="';
  RC_Font4 = '">';

  RC_EndBold = '</B>';
  RC_Bold = '<B>';

  RC_EndItalic = '</I>';
  RC_Italic = '<I>';

  RC_EndStrikeOut = '</STRIKE>';
  RC_StrikeOut = '<STRIKE>';

  RC_EndUnderline = '</U>';
  RC_Underline = '<U>';

  RC_EndPar = '</P>';
  RC_LeftPara = '<P ALIGN="LEFT">';
  RC_RightPara = '<P ALIGN="RIGHT">';
  RC_CenterPara = '<P ALIGN="CENTER">';

  RC_LeftIndent = '<LI>';
  RC_EndLeftIndent = '</LI>';

  {**************************************************}

procedure TJvRichEditToHtml.ConvertToHtml(Value: TRichEdit; const Filename: string);
var S: Tstringlist;
begin
  S := TStringlist.Create;
  try
    ConvertToHtmlStringList(Value, S);
    S.SaveToFile(Filename);
  finally
    S.Free;
  end;
end;

{**************************************************}

function TJvRichEditToHtml.AttToHtml(Value: tfont): string;
begin
  FEndSection := RC_EndFont;
  FCToH.RgbColor := Value.Color;
  Result := RC_Font1 + FCtoH.HtmlColor + RC_Font2 + IntToStr((Value.Size mod 8) + 2) + RC_Font3;
  Result := Result + Value.Name + RC_Font4;
  if fsBold in Value.Style then
  begin
    FEndSection := RC_EndBold + FEndSection;
    Result := Result + RC_Bold;
  end;
  if fsItalic in Value.Style then
  begin
    FEndSection := RC_EndItalic + FEndSection;
    Result := Result + RC_Italic;
  end;
  if fsStrikeout in Value.Style then
  begin
    FEndSection := RC_EndStrikeOut + FEndSection;
    Result := Result + RC_StrikeOut;
  end;
  if fsUnderline in Value.Style then
  begin
    FEndSection := RC_EndUnderline + FEndSection;
    Result := Result + RC_Underline;
  end;
end;

{**************************************************}

function TJvRichEditToHtml.ParaToHtml(Value: TJvParaAttributes): string;
begin
  FEndPara := RC_EndPar;
  case Value.Alignment of
    taLeftJustify:
      Result := RC_LeftPara;
    taRightJustify:
      Result := RC_RightPara;
    taCenter:
      Result := RC_CenterPara;
  end;
  if Value.Numbering = nsBullet then
  begin
    Result := RC_LeftIndent + Result;
    FEndPara := FEndPara + RC_EndLeftIndent;
  end;
end;

{**************************************************}

function Diff(One, Two: TFont): Boolean;
begin
  Result := (One.Color <> Two.Color) or (One.Style <> Two.Style) or
    (One.Name <> Two.Name) or (One.Size <> Two.Size);
end;

{**************************************************}

function DiffPara(One, Two: TJvParaAttributes): Boolean;
begin
  Result := (One.Alignment <> Two.Alignment) or (One.Numbering <> Two.Numbering);
end;

{**************************************************}

procedure TJvRichEditToHtml.ConvertToHtmlStringList(Value: TRichEdit; Strings: TStringList);
var
  i, j, k: Integer;
  datt, att, currat: TFont;
  dpara, para, currpara: TJvParaAttributes;
  st: string;
  FEnd: string;
begin
  Strings.BeginUpdate;
  Value.Lines.BeginUpdate;
  try
    Strings.Clear;
    Strings.Add(RC_Html1);
    Strings.Add(Format(RC_Html7, [Title]));
    Strings.Add(RC_Html3);
    datt := TFont.Create;
    att := TFont.Create;
    currat := TFont.Create;

    dpara.Alignment := taLeftJustify;
    dpara.Numbering := nsNone;
    currpara.Alignment := dpara.Alignment;
    currpara.Numbering := dpara.Numbering;
    FendPara := '';

    datt.Assign(Value.DefAttributes);
    Strings.Add(AttToHtml(datt));
    FEnd := FEndSection;

    k := 0;
    Currat.Assign(datt);
    FEndSection := '';
    for i := 0 to Value.Lines.Count - 1 do
    begin
      st := '';
      currpara.Numbering := nsNone;
      if Length(Value.lines[i]) > 0 then
      begin
        for j := 1 to Length(Value.Lines[i]) do
        begin
          Value.SelStart := k + j - 1;
          Value.SelLength := 1;
          att.Assign(Value.SelAttributes);
          para.Alignment := Value.Paragraph.Alignment;
          para.Numbering := Value.Paragraph.Numbering;
          if Diff(att, currat) then
          begin
            st := st + FEndSection;
            currat.Assign(att);
            st := st + AttToHtml(att);
          end;
          if DiffPara(para, currpara) then
          begin
            st := st + FEndPara;
            currpara.Alignment := para.Alignment;
            currpara.Numbering := para.Numbering;
            st := st + ParaToHtml(para);
          end;
          st := st + FCharToH.CharToHtml(Value.Lines[i][j]);
        end;
      end;
      k := k + Length(Value.Lines[i]) + 2;
      Strings.Add(RC_Html4 + st);
      Application.ProcessMessages;
    end;
    Strings.Add(FEndSection);
    Strings.Add(FEndPara);

    datt.Free;
    att.Free;
    currat.Free;

    Strings.Add(FEnd);
    Strings.Add(RC_Html5);
    Strings.Add(RC_Html6);
  finally
    Strings.EndUpdate;
    Value.Lines.EndUpdate;
  end;
end;

{**************************************************}

constructor TJvRichEditToHtml.Create(AOwner: TComponent);
begin
  inherited;
  FCToH := TJvRgbToHtml.Create(Self);
  FCharToH := TJvStrToHtml.Create(Self);
end;

{**************************************************}

destructor TJvRichEditToHtml.Destroy;
begin
  FCToH.Free;
  FCharToH.Free;
  inherited;
end;

end.

