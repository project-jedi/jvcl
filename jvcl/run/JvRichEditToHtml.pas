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

Contributor(s): Michael Beck [mbeck att bigfoot dott com],
                Andreas Hausladen [Andreas dott Hausladen att gmx dott de].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvRichEditToHtml;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
 Windows,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_CHARACTER}
  Character, // inline
  {$ENDIF HAS_UNIT_CHARACTER}
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  SysUtils, Classes, Graphics, Forms, ComCtrls,
  JvRgbToHtml, JvStrToHtml, JvRichEdit, JvComponentBase, JclStrings;

type
  TJvParaAttributesRec = record
    Alignment: TAlignment;
    Numbering: TNumberingStyle;
  end;

  TJvRichEditParaAttributesRec = record
    Alignment: TParaAlignment;
    Numbering: TJvNumbering;
  end;

  TFontInfo = class(TPersistent)
  private
    FFontData: TFontData;
    FColor: TColor;
    FPixelsPerInch: Integer;
    FLink: Boolean;
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
  public
    constructor Create(APixelsPerInch: Integer);
    procedure Assign(Source: TPersistent); override;
    property Color: TColor read FColor write FColor;
    property Link: Boolean read FLink write FLink;

    property Size: Integer read GetSize write SetSize;
    property Height: Integer read FFontData.Height write FFontData.Height;
    property Pitch: TFontPitch read FFontData.Pitch write FFontData.Pitch;
    property Style: TFontStylesBase read FFontData.Style write FFontData.Style;
    property Charset: TFontCharset read FFontData.Charset write FFontData.Charset;
    property Name: TFontDataName read FFontData.Name write FFontData.Name;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvRichEditToHtml = class(TJvComponent)
  private
    FCToH: TJvRgbToHtml;
    FCharToH: TJvStrToHtml;
    FEndSection: string;
    FEndPara: string;
    FTitle: string;
    FFooter: TStrings;
    FHeader: TStrings;
    function AttToHtml(Value: TFontInfo): string;
    function ParaToHtml(Value: TJvParaAttributesRec): string; overload;
    function ParaToHtml(Value: TJvRichEditParaAttributesRec): string; overload;
    procedure SetFooter(const Value: TStrings);
    procedure SetHeader(const Value: TStrings);
    function IsFooterStored: Boolean;
    function IsHeaderStored: Boolean;
    procedure WriteEmptyStrings(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConvertToHtml(Value: TRichEdit; const FileName: string); overload;
    procedure ConvertToHtml(Value: TJvRichEdit; const FileName: string); overload;
    procedure ConvertToHtmlStrings(Value: TRichEdit; Strings: TStrings); overload;
    procedure ConvertToHtmlStrings(Value: TJvRichEdit; Strings: TStrings); overload;
  published
    property Title: string read FTitle write FTitle;
    property Header: TStrings read FHeader write SetHeader stored IsHeaderStored;
    property Footer: TStrings read FFooter write SetFooter stored IsFooterStored;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

{$IFNDEF COMPILER12_UP}
uses
  JvJCLUtils;
{$ENDIF ~COMPILER12_UP}

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
  cHTMLFontEnd = '</SPAN>';

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

  cDefaultHeader: array[0..4] of string = (
    '<HTML>',
    '  <HEAD>',
    '    <TITLE><#TITLE></TITLE>',
    '  </HEAD>',
    '  <BODY>'
  );
  cDefaultFooter: array[0..1] of string = (
    '  </BODY>',
    '</HTML>'
  );

//=== { TFontInfo } ==========================================================

constructor TFontInfo.Create(APixelsPerInch: Integer);
begin
  inherited Create;
  FPixelsPerInch := APixelsPerInch;
end;

procedure TFontInfo.Assign(Source: TPersistent);
begin
  if Source is TTextAttributes then
  begin
    FFontData.Name := TFontDataName(TTextAttributes(Source).Name);
    FFontData.Height := TTextAttributes(Source).Height;
    FFontData.Pitch := TTextAttributes(Source).Pitch;
    FFontData.Style := TTextAttributes(Source).Style;
    FFontData.Charset := TTextAttributes(Source).Charset;
    FColor := TTextAttributes(Source).Color;
    FLink := False;
  end
  else
  if Source is TJvTextAttributes then
  begin
    FFontData.Name := TFontDataName(TJvTextAttributes(Source).Name);
    FFontData.Height := TJvTextAttributes(Source).Height;
    FFontData.Pitch := TJvTextAttributes(Source).Pitch;
    FFontData.Style := TJvTextAttributes(Source).Style;
    FFontData.Charset := TJvTextAttributes(Source).Charset;
    FColor := TJvTextAttributes(Source).Color;
    FLink := TJvTextAttributes(Source).Link;
  end
  else
  if Source is TFontInfo then
  begin
    FFontData := TFontInfo(Source).FFontData;
    FColor := TFontInfo(Source).FColor;
    FLink := TFontInfo(Source).FLink;
  end
  else
    inherited Assign(Source);
end;

function TFontInfo.GetSize: Integer;
begin
  Result := -MulDiv(Height, 72, FPixelsPerInch);
end;

procedure TFontInfo.SetSize(const Value: Integer);
begin
  FFontData.Height := -MulDiv(Value, FPixelsPerInch, 72);
end;

//=== { TJvRichEditToHtml } ==================================================

constructor TJvRichEditToHtml.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FCToH := TJvRgbToHtml.Create(Self);
  FCharToH := TJvStrToHtml.Create(Self);
  FHeader := TStringList.Create;
  for I := 0 to High(cDefaultHeader) do
    FHeader.Add(cDefaultHeader[I]);
  FFooter := TStringList.Create;
  for I := 0 to High(cDefaultFooter) do
    FFooter.Add(cDefaultFooter[I]);
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

function TJvRichEditToHtml.AttToHtml(Value: TFontInfo): string;
var
  Size: Integer;
begin
  FEndSection := cHTMLFontEnd;
  FCToH.RgbColor := Value.Color;

  Size := Abs(Value.Size);
  if Size = 0 then
    Size := 8;
  Result := Format('<SPAN style="color: #%s; font-size: %dpt; font-family: %s;">',
    [FCToH.HtmlColor, Size, Value.Name]);
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

//  if Value.Link then
//  begin
//    FEndSection := '</a>' + FEndSection;
//    Result := Result + '<a href="#">';
//  end;
end;

function Diff(One, Two: TFontInfo): Boolean;
begin
  Result := (One.Color <> Two.Color) or (One.Style <> Two.Style) or
    (One.Name <> Two.Name) or (One.Size <> Two.Size) or
    (One.Link <> Two.Link);
end;

function DiffPara(One, Two: TJvParaAttributesRec): Boolean;overload;
begin
  Result := (One.Alignment <> Two.Alignment) or (One.Numbering <> Two.Numbering);
end;

function DiffPara(One, Two: TJvRichEditParaAttributesRec): Boolean;overload;
begin
  Result := (One.Alignment <> Two.Alignment) or (One.Numbering <> Two.Numbering);
end;

procedure TJvRichEditToHtml.ConvertToHtml(Value: TJvRichEdit; const FileName: string);
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

procedure TJvRichEditToHtml.ConvertToHtmlStrings(Value: TRichEdit; Strings: TStrings);
var
  I, J: Integer;
  Datt, Att, CurrAt: TFontInfo;
  DPara, Para, CurrPara: TJvParaAttributesRec;
  St: TStringBuilder;
  FEnd: string;
  LOnChange: TNotifyEvent;
  LOnSelectionChange: TNotifyEvent;
  Text: string;
  Len: Integer;
  PreviousChar: Char;
begin
  LOnChange := Value.OnChange;
  LOnSelectionChange := Value.OnSelectionChange;
  Strings.BeginUpdate;
  Value.Lines.BeginUpdate;
  try
    Value.OnChange := nil;
    Value.OnSelectionChange := nil;

    Strings.Clear;
    if Header.Count > 0 then
      Strings.Add(StringReplace(Header.Text, '<#TITLE>', Title, [rfReplaceAll]));
    Datt := TFontInfo.Create(Value.Font.PixelsPerInch);
    Att := TFontInfo.Create(Value.Font.PixelsPerInch);
    CurrAt := TFontInfo.Create(Value.Font.PixelsPerInch);

    DPara.Alignment := taLeftJustify;
    DPara.Numbering := ComCtrls.nsNone;
    CurrPara.Alignment := DPara.Alignment;
    CurrPara.Numbering := DPara.Numbering;
    Strings.Add(ParaToHtml(Para));

    Datt.Assign(Value.DefAttributes);
    Strings.Add(AttToHtml(Datt));

    CurrAt.Assign(Datt);
    Value.SelStart := 0;
    Value.SelectAll;
    Text := Value.SelText;
    Len := Length(Text);
    St := TStringBuilder.Create;
    try
      I := 1;
      Value.SelLength := 1;
      while I <= Len do
      begin
        // new line
        Value.SelStart := I - 1;
        Att.Assign(Value.SelAttributes);
        Para.Alignment := Value.Paragraph.Alignment;
        Para.Numbering := Value.Paragraph.Numbering;

        St.Length := 0;
        if DiffPara(Para, CurrPara) or (Para.Numbering = ComCtrls.nsBullet) then
        begin
          St.Append(FEndSection).Append(FEndPara);
          CurrPara.Alignment := Para.Alignment;
          CurrPara.Numbering := Para.Numbering;
          CurrAt.Assign(Att);
          St.Append(ParaToHtml(Para)).Append(AttToHtml(Att));
        end;

        J := I;
        PreviousChar := #0;
        while (J <= Len) and not CharInSet(Text[J], [#$A, #$B, #$D]) do { RICHEDIT uses #$B also for line breaking }
        begin
          Att.Assign(Value.SelAttributes);
          if Diff(Att, CurrAt) then
          begin
            St.Append(FEndSection);
            CurrAt.Assign(Att);
            St.Append(AttToHtml(Att));
            Value.SelStart := J;
          end
          else
          begin
            if (Text[J] = ' ') and (PreviousChar = ' ') then
              St.Append('&nbsp;')
            else
              St.Append(CharToHtml(Text[J]));
            PreviousChar := Text[J];
            Inc(J);
            Value.SelStart := J;
          end;
        end;
        if I = 1 then
          Strings.Add(St.ToString())
        else
          Strings.Add(cHTMLBR + St.ToString());
        I := J + 1;
      end;
    finally
      St.Free;
    end;
    Strings.Add(FEndSection);
    Strings.Add(FEndPara);

    Datt.Free;
    Att.Free;
    CurrAt.Free;

    Strings.Add(FEnd);
    Strings.AddStrings(Footer);
  finally
    Value.OnChange := LOnChange;
    Value.OnSelectionChange := LOnSelectionChange;
    Strings.EndUpdate;
    Value.Lines.EndUpdate;
  end;
end;

procedure TJvRichEditToHtml.ConvertToHtmlStrings(Value: TJvRichEdit; Strings: TStrings);
var
  I, J: Integer;
  Datt, Att, CurrAt: TFontInfo;
  DPara, Para, CurrPara: TJvRichEditParaAttributesRec;
  St: TStringBuilder;
  FEnd: string;
  LOnChange: TNotifyEvent;
  LOnSelectionChange: TNotifyEvent;
  Text: string;
  Len: Integer;
  PreviousChar: Char;
begin
  LOnChange := Value.OnChange;
  LOnSelectionChange := Value.OnSelectionChange;
  Strings.BeginUpdate;
  Value.Lines.BeginUpdate;
  try
    Value.OnChange := nil;
    Value.OnSelectionChange := nil;

    Strings.Clear;
    if Header.Count > 0 then
      Strings.Add(StringReplace(Header.Text, '<#TITLE>', Title, [rfReplaceAll]));
    Datt := TFontInfo.Create(Value.Font.PixelsPerInch);
    Att := TFontInfo.Create(Value.Font.PixelsPerInch);
    CurrAt := TFontInfo.Create(Value.Font.PixelsPerInch);

    DPara.Alignment := paLeftJustify;
    DPara.Numbering := nsNone;
    CurrPara.Alignment := DPara.Alignment;
    CurrPara.Numbering := DPara.Numbering;
    Strings.Add(ParaToHtml(Para));

    Datt.Assign(Value.DefAttributes);
    Strings.Add(AttToHtml(Datt));

    CurrAt.Assign(Datt);
    Value.SelStart := 0;
    Value.SelectAll;
    Text := Value.SelText;
    Len := Length(Text);
    St := TStringBuilder.Create;
    try
      I := 1;
      Value.SelLength := 1;
      while I <= Len do
      begin
        // new line
        Value.SelStart := I - 1;
        Att.Assign(Value.SelAttributes);
        Para.Alignment := Value.Paragraph.Alignment;
        Para.Numbering := Value.Paragraph.Numbering;

        St.Length := 0;
        if DiffPara(Para, CurrPara) or (Para.Numbering = nsBullet) then
        begin
          St.Append(FEndSection).Append(FEndPara);
          CurrPara.Alignment := Para.Alignment;
          CurrPara.Numbering := Para.Numbering;
          CurrAt.Assign(Att);
          St.Append(ParaToHtml(Para)).Append(AttToHtml(Att));
        end;

        J := I;
        PreviousChar := #0;
        while (J <= Len) and not CharInSet(Text[J], [#$A, #$B, #$D]) do { RICHEDIT uses #$B also for line breaking }
        begin
          Att.Assign(Value.SelAttributes);
          if Diff(Att, CurrAt) then
          begin
            St.Append(FEndSection);
            CurrAt.Assign(Att);
            St.Append(AttToHtml(Att));
            Value.SelStart := J;
          end
          else
          begin
            if (Text[J] = ' ') and (PreviousChar = ' ') then
              St.Append('&nbsp;')
            else
              St.Append(CharToHtml(Text[J]));
            PreviousChar := Text[J];
            Inc(J);
            Value.SelStart := J;
          end;
        end;
        if I = 1 then
          Strings.Add(St.ToString())
        else
          Strings.Add(cHTMLBR + St.ToString());
        I := J + 1;
      end;
    finally
      St.Free;
    end;
    Strings.Add(FEndSection);
    Strings.Add(FEndPara);

    Datt.Free;
    Att.Free;
    CurrAt.Free;

    Strings.Add(FEnd);
    Strings.AddStrings(Footer);
  finally
    Value.OnChange := LOnChange;
    Value.OnSelectionChange := LOnSelectionChange;
    Strings.EndUpdate;
    Value.Lines.EndUpdate;
  end;
end;

function TJvRichEditToHtml.ParaToHtml(Value: TJvRichEditParaAttributesRec): string;
begin
  case Value.Alignment of
    paLeftJustify:
      Result := 'STYLE="text-align: left;"';
    paRightJustify:
      Result := 'STYLE="text-align: right;"';
    paCenter:
      Result := 'STYLE="text-align: center;"';
  end;
  if Value.Numbering = nsBullet then
  begin
    Result := '<LI ' + Result + '>';
    FEndPara := '</LI>';
  end
  else
  begin
    Result := '<P ' + Result + '>';
    FEndPara := '</P>';
  end
end;

function TJvRichEditToHtml.ParaToHtml(Value: TJvParaAttributesRec): string;
begin
  case Value.Alignment of
    Classes.taLeftJustify:
      Result := 'STYLE="text-align: left;"';
    Classes.taRightJustify:
      Result := 'STYLE="text-align: right;"';
    Classes.taCenter:
      Result := 'STYLE="text-align: center;"';
  end;
  if Value.Numbering = ComCtrls.nsBullet then
  begin
    Result := '<LI ' + Result + '>';
    FEndPara := '</LI>';
  end
  else
  begin
    Result := '<P ' + Result + '>';
    FEndPara := '</P>';
  end
end;

procedure TJvRichEditToHtml.SetFooter(const Value: TStrings);
begin
  if Value <> FFooter then
    FFooter.Assign(Value);
end;

procedure TJvRichEditToHtml.SetHeader(const Value: TStrings);
begin
  if Value <> FHeader then
    FHeader.Assign(Value);
end;

function TJvRichEditToHtml.IsFooterStored: Boolean;
var
  I: Integer;
begin
  Result := Footer.Count <> Length(cDefaultFooter);
  if not Result then
  begin
    Result := True;
    for I := 0 to High(cDefaultFooter) do
      if Footer[I] <> cDefaultFooter[I] then
        Exit;
    Result := False;
  end;
end;

function TJvRichEditToHtml.IsHeaderStored: Boolean;
var
  I: Integer;
begin
  Result := Header.Count <> Length(cDefaultHeader);
  if not Result then
  begin
    Result := True;
    for I := 0 to High(cDefaultHeader) do
      if Header[I] <> cDefaultHeader[I] then
        Exit;
    Result := False;
  end;
end;

procedure TJvRichEditToHtml.WriteEmptyStrings(Writer: TWriter);
begin
  Writer.WriteListBegin;
  Writer.WriteListEnd;
end;

procedure TJvRichEditToHtml.DefineProperties(Filer: TFiler);

  function DoWriteHeader: Boolean;
  begin
    Result := Header.Count = 0;
    if Result and (Filer.Ancestor <> nil) then
    begin
      Result := True;
      if Filer.Ancestor is TJvRichEditToHtml then
        Result := not Header.Equals(TJvRichEditToHtml(Filer.Ancestor).Header)
    end;
  end;

  function DoWriteFooter: Boolean;
  begin
    Result := Footer.Count = 0;
    if Result and (Filer.Ancestor <> nil) then
    begin
      Result := True;
      if Filer.Ancestor is TJvRichEditToHtml then
        Result := not Footer.Equals(TJvRichEditToHtml(Filer.Ancestor).Footer)
    end;
  end;

begin
  inherited DefineProperties(Filer);
  { Write empty Header/Footer to DFM because the default value differs from '' }
  if Filer is TWriter then
  begin
    Filer.DefineProperty('Header.Strings', nil, WriteEmptyStrings, DoWriteHeader);
    Filer.DefineProperty('Footer.Strings', nil, WriteEmptyStrings, DoWriteFooter);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
