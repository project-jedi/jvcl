{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvParserForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQParserForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, QControls, QForms, QStdCtrls,
  JvQTypes, JvQComponent;

type
  TJvHTMLParserForm = class(TJvForm)
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    edKeyword: TEdit;
    Label1: TLabel;
    edStartTag: TEdit;
    Label2: TLabel;
    edEndTag: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbTakeText: TComboBox;
    edMustBe: TEdit;
    btnAdd: TButton;
    btnRemove: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure edKeywordChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edStartTagChange(Sender: TObject);
    procedure edEndTagChange(Sender: TObject);
    procedure cbTakeTextChange(Sender: TObject);
    procedure edMustBeChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  public
    procedure LoadFromStr(Value: TStrings);
    procedure SaveToStr(Value: TStrings);
  end;

implementation

uses
  QDialogs, JvQHTMLParser, JvQResources;



{$R *.xfm}


procedure TJvHTMLParserForm.edKeywordChange(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    ListBox1.Items[ListBox1.ItemIndex] := (Sender as TEdit).Text;
end;

procedure TJvHTMLParserForm.Button1Click(Sender: TObject);
var
  Ob: TJvParserInfo;
begin
  Ob := TJvParserInfo.Create;
  Ob.StartTag := '';
  Ob.EndTag := '';
  Ob.MustBe := -1;
  Ob.TakeText := 0;
  ListBox1.ItemIndex := ListBox1.Items.AddObject(RsNewObject, TObject(Ob));
  ListBox1Click(Sender);
  edKeyword.SetFocus;
  edKeyword.SelectAll;
end;

procedure TJvHTMLParserForm.ListBox1Click(Sender: TObject);
var
  Ob: TJvParserInfo;
begin
  GroupBox1.Enabled := True;
  Ob := TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]);
  edKeyword.Text := ListBox1.Items[ListBox1.ItemIndex];
  edStartTag.Text := Ob.StartTag;
  edEndTag.Text := Ob.EndTag;
  edMustBe.Text := IntToStr(Ob.MustBe);
  cbTakeText.ItemIndex := Ob.TakeText;
end;

procedure TJvHTMLParserForm.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    I := ListBox1.ItemIndex;
    ListBox1.Items.Delete(I);
    if ListBox1.Items.Count >= I then
      Dec(I);
    if I >= 0 then
    begin
      ListBox1.ItemIndex := I;
      ListBox1Click(Sender);
      edKeyword.SetFocus;
      edKeyword.SelectAll;
    end
    else
      GroupBox1.Enabled := False;
  end;
end;

procedure TJvHTMLParserForm.edStartTagChange(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]).StartTag := (Sender as TEdit).Text;
end;

procedure TJvHTMLParserForm.edEndTagChange(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]).EndTag := (Sender as TEdit).Text;
end;

procedure TJvHTMLParserForm.LoadFromStr(Value: TStrings);
var
  I: Integer;
  Ob: TJvParserInfo;
  Cap: string;
begin
  I := 0;
  while I < Value.Count do
  begin
    Ob := TJvParserInfo.Create;
    try
      Cap := Value[I];
      Inc(I);
      Ob.StartTag := Value[I];
      Inc(I);
      Ob.EndTag := Value[I];
      Inc(I);
      Ob.MustBe := StrToInt(Value[I]);
      Inc(I);
      Ob.TakeText := StrToInt(Value[I]);
      Inc(I);
    finally
      ListBox1.Items.AddObject(Cap, TObject(Ob));
    end;
  end;
end;

procedure TJvHTMLParserForm.cbTakeTextChange(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]).TakeText := (Sender as TComboBox).ItemIndex;
end;

procedure TJvHTMLParserForm.edMustBeChange(Sender: TObject);
var
  I: Integer;
begin
  I := 0;
  try
    I := StrToInt((Sender as TEdit).Text);
  except
    Beep;
  end;
  if ListBox1.ItemIndex <> -1 then
    TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]).MustBe := I;
end;

procedure TJvHTMLParserForm.OkBtnClick(Sender: TObject);
begin
  Tag := 0;
  ModalResult := mrOK;
  Close;
end;

procedure TJvHTMLParserForm.CancelBtnClick(Sender: TObject);
begin
  Tag := 1;
  ModalResult := mrCancel;
  Close;
end;

procedure TJvHTMLParserForm.SaveToStr(Value: TStrings);
var
  I: Integer;
begin
//  ShowMessage('TFormParsers.SaveToStr');
  Value.Clear;
  for I := 0 to ListBox1.Items.Count - 1 do
  begin
    Value.Add(ListBox1.Items[I]);
    Value.Add(TJvParserInfo(ListBox1.Items.Objects[I]).StartTag);
    Value.Add(TJvParserInfo(ListBox1.Items.Objects[I]).EndTag);
    Value.Add(IntToStr(TJvParserInfo(ListBox1.Items.Objects[I]).MustBe));
    Value.Add(IntToStr(TJvParserInfo(ListBox1.Items.Objects[I]).TakeText));
  end;
end;

end.
