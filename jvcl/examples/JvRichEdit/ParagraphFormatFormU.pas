{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit ParagraphFormatFormU;

interface

uses
  Forms, ExtCtrls, StdCtrls, Mask, JvMaskEdit, JvSpin, Controls, Classes,
  JvRichEdit, JvExMask;

type
  TParagraphFormatForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    IndentBox: TGroupBox;
    LeftIndent: TJvSpinEdit;
    Label1: TLabel;
    RightIndent: TJvSpinEdit;
    Label2: TLabel;
    FirstIndent: TJvSpinEdit;
    Label3: TLabel;
    Alignment: TRadioGroup;
    SpacingBox: TGroupBox;
    Label4: TLabel;
    SpaceBefore: TJvSpinEdit;
    Label5: TLabel;
    SpaceAfter: TJvSpinEdit;
    Label6: TLabel;
    LineSpacing: TJvSpinEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure SetAttr(Paragraph: TJvParaAttributes);
    procedure GetAttr(Paragraph: TJvParaAttributes);
  public
    { Public declarations }
  end;

function FormatParagraph(Paragraph: TJvParaAttributes): Boolean;

implementation

{$R *.DFM}

function FormatParagraph(Paragraph: TJvParaAttributes): Boolean;
begin
  with TParagraphFormatForm.Create(Application) do
  try
    SetAttr(Paragraph);
    Result := ShowModal = mrOk;
    if Result then
      GetAttr(Paragraph);
  finally
    Free;
  end;
end;

procedure TParagraphFormatForm.FormCreate(Sender: TObject);
begin
  SpacingBox.Enabled := (RichEditVersion >= 2);
end;

procedure TParagraphFormatForm.GetAttr(Paragraph: TJvParaAttributes);
begin
  Paragraph.LeftIndent := LeftIndent.AsInteger;
  Paragraph.RightIndent := RightIndent.AsInteger;
  Paragraph.FirstIndent := FirstIndent.AsInteger;
  Paragraph.Alignment := TParaAlignment(Alignment.ItemIndex);
  Paragraph.SpaceBefore := SpaceBefore.AsInteger;
  Paragraph.SpaceAfter := SpaceAfter.AsInteger;
  if LineSpacing.AsInteger > 0 then
    Paragraph.LineSpacingRule := lsSpecifiedOrMore
  else
    Paragraph.LineSpacingRule := lsSingle;
  Paragraph.LineSpacing := LineSpacing.AsInteger;
end;

procedure TParagraphFormatForm.SetAttr(Paragraph: TJvParaAttributes);
begin
  LeftIndent.AsInteger := Paragraph.LeftIndent;
  RightIndent.AsInteger := Paragraph.RightIndent;
  FirstIndent.AsInteger := Paragraph.FirstIndent;
  Alignment.ItemIndex := Ord(Paragraph.Alignment);
  SpaceBefore.AsInteger := Paragraph.SpaceBefore;
  SpaceAfter.AsInteger := Paragraph.SpaceAfter;
  LineSpacing.AsInteger := Paragraph.LineSpacing;
end;

end.
