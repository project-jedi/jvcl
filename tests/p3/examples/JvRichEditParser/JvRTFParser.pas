{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvErrProviJvRTFParserder.pas, released on 2003-04-08.

The Initial Developer of the Original Code is Peter Thörnqvist <peter3@peter3.com>.
Portions created by Joe Doe are Copyright (C) 2002 Peter Thörnqvist . All Rights Reserved.

Contributor(s):

Last Modified: 2003-04-08

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* Numbered bullets (not supported by TRichEdit) are not handled correctly:
  the "attribute break" happens one character too late. You can see this if you
  load the D6 readme.rtf file and click Recreate: the numbered bullets in the section
  "Working with the sample imported automation server wrappers for MS Office" 
  bolds the first character after the bullet although it shouldn't
* RTF files converted from doc files (by Word f.ex.) doesn't always render correctly
* Reading the richedit control is very slow. This might be possible to speed up a bit
  with direct calls to the underlying Win API...

-----------------------------------------------------------------------------}
{$I JVCL.INC}
unit JvRTFParser;

interface
uses
  Windows, SysUtils, Classes, Graphics, ComCtrls;

type
  TJvTextAttributeChange = procedure(Sender: TObject;
    Attributes: TTextAttributes; ParaAttributes: TParaAttributes; const AText: string) of object;
  TParserParaAttributes = class(TPersistent)
  private
    FFirstIndent: Longint;
    FLeftIndent: Longint;
    FRightIndent: Longint;
    FAlignment: TAlignment;
    FNumbering: TNumberingStyle;
  public
    procedure Assign(Source: TPersistent); override;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property FirstIndent: Longint read FFirstIndent write FFirstIndent;
    property LeftIndent: Longint read FLeftIndent write FLeftIndent;
    property Numbering: TNumberingStyle read FNumbering write FNumbering;
    property RightIndent: Longint read FRightIndent write FRightIndent;
//    property Tab[Index: Byte]: Longint read GetTab write SetTab;
//    property TabCount: Integer read GetTabCount write SetTabCount;
  end;

  TJvRichEditParser = class(TObject)
  private
    FAttributes: TFont;
    FParaAttributes: TParserParaAttributes;
    FRichEdit: TCustomRichEdit;
    FOnAttributeChange: TJvTextAttributeChange;
  protected
    function NextChange(StartPos, TextLength: integer): integer;
    function SameAttributes(Attr1: TFont; Attr2: TTextAttributes): boolean;
    function SameParaAttributes(Attr1: TParserParaAttributes; Attr2: TParaAttributes): boolean;
  public
    constructor Create(ARichEdit: TCustomRichEdit);
    destructor Destroy; override;

    procedure ExtractContent;
    procedure AttributeChange(Attributes: TTextAttributes; ParaAttributes: TParaAttributes; const AText: string);
    property OnAttributeChange: TJvTextAttributeChange read FOnAttributeChange write FOnAttributeChange;
  end;

implementation
uses
  JclStrings;

{ TJvRichEditParser }

procedure TJvRichEditParser.AttributeChange(Attributes: TTextAttributes;
  ParaAttributes: TParaAttributes; const AText: string);
begin
  if Assigned(FOnAttributeChange) then
    FOnAttributeChange(self, Attributes, ParaAttributes, AText);
end;

constructor TJvRichEditParser.Create(ARichEdit: TCustomRichEdit);
begin
  inherited Create;
  FRichEdit := ARichEdit;
  FAttributes := TFont.Create;
  FParaAttributes := TParserParaAttributes.Create;
end;

destructor TJvRichEditParser.Destroy;
begin
  FAttributes.Free;
  FParaAttributes.Free;
  inherited;
end;

procedure TJvRichEditParser.ExtractContent;
var
  FStartPos, FEndPos, FLength: integer;
//  FNumbering: TNumberingStyle;
  S: string;
begin
  Assert(FRichEdit <> nil, '');
  FRichEdit.Lines.BeginUpdate;
  try
    FLength := Length(FRichEdit.Lines.Text);
    FStartPos := 0;
    FRichEdit.SelStart := 0;
    FRichEdit.SelLength := 0;
    FAttributes.Assign(FRichEdit.SelAttributes);
    FParaAttributes.Assign(FRichEdit.Paragraph);
    FEndPos := NextChange(FStartPos, FLength);
    while (FEndPos < FLength) do
    begin
//      FNumbering := FRichEdit.Paragraph.Numbering;
      FRichEdit.SelStart := FStartPos;
      // have to read text here because SelLength must be shortened later...
      FRichEdit.SelLength := FEndPos - FStartPos;
      S := FRichEdit.SelText;
      FRichEdit.SelStart := FEndPos - 1;
      // ...otherwise we get the values after the selection!
      AttributeChange(FRichEdit.SelAttributes, FRichEdit.Paragraph, S);

      FRichEdit.SelStart := FEndPos;
      FAttributes.Assign(FRichEdit.SelAttributes);
      FParaAttributes.Assign(FRichEdit.Paragraph);
      FStartPos := FEndPos;
      FEndPos := NextChange(FStartPos, FLength);
      if FEndPos <= FStartPos then Exit;
    end;
  finally
    FRichEdit.Lines.EndUpdate;
  end;
end;

function TJvRichEditParser.NextChange(StartPos, TextLength: integer): integer;
begin
  FRichEdit.SelStart := StartPos;
  Result := FRichEdit.SelStart;
  repeat
    if (Result >= TextLength - 1) then
      Break;
    Result := FRichEdit.SelStart;
    FRichEdit.SelStart := Result + 1;
    while FRichEdit.SelStart <= Result do
    begin
      Inc(Result);
      FRichEdit.SelStart := Result;
      if (FRichEdit.SelStart = Result) or (Result >= TextLength - 1) then
        Break;
    end;
    if not SameAttributes(FAttributes, FRichEdit.SelAttributes) or not SameParaAttributes(FParaAttributes,
      FRichEdit.Paragraph) then
      Break;
  until false;
  Result := FRichEdit.SelStart;
end;

function TJvRichEditParser.SameAttributes(Attr1: TFont;
  Attr2: TTextAttributes): boolean;
begin
  // ordered by "most likely to happen"
  Result :=
    (Attr1.Size = Attr2.Size) and
    (Attr1.Color = Attr2.Color) and
    (Attr1.Style = Attr2.Style) and
    (Attr1.Charset = Attr2.CharSet) and
//  (Attr1.Pitch = Attr2.Pitch) and
  AnsiSameText(Attr1.Name, Attr2.Name);
end;

function TJvRichEditParser.SameParaAttributes(Attr1: TParserParaAttributes;
  Attr2: TParaAttributes): boolean;
begin
  Result := (Attr1.FirstIndent = Attr2.FirstIndent) and
    (Attr1.LeftIndent = Attr2.LeftIndent) and
    (Attr1.RightIndent = Attr2.RightIndent) and
    (Attr1.Alignment = Attr2.Alignment) and
    (Attr1.Numbering = Attr2.Numbering);
end;

{ TParserParaAttributes }

procedure TParserParaAttributes.Assign(Source: TPersistent);
begin
  if Source is TParaAttributes then
  begin
    Alignment := TParaAttributes(Source).Alignment;
    FirstIndent := TParaAttributes(Source).FirstIndent;
    LeftIndent := TParaAttributes(Source).LeftIndent;
    Numbering := TParaAttributes(Source).Numbering;
    RightIndent := TParaAttributes(Source).RightIndent;
  end
  else if Source is TParserParaAttributes then
  begin
    Alignment := TParserParaAttributes(Source).Alignment;
    FirstIndent := TParserParaAttributes(Source).FirstIndent;
    LeftIndent := TParserParaAttributes(Source).LeftIndent;
    Numbering := TParserParaAttributes(Source).Numbering;
    RightIndent := TParserParaAttributes(Source).RightIndent;
  end
  else
    inherited;
end;

end.

