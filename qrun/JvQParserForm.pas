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

{$I jvcl.inc}

unit JvQParserForm;

interface

uses
  SysUtils, Classes,  
  QControls, QForms, QStdCtrls, 
  JvQTypes, JvQComponent;

type
  TFormParsers = class(TJvForm)
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox1: TComboBox;
    Edit4: TEdit;
    AddBtn: TButton;
    RemoveBtn: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  public
    procedure LoadFromStr(Value: TstringList);
    function SetFromStr: TstringList;
  end;

implementation



{$R *.xfm}


procedure TFormParsers.Edit1Change(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    ListBox1.Items[ListBox1.ItemIndex] := (Sender as TEdit).Text;
end;

procedure TFormParsers.Button1Click(Sender: TObject);
var
  Ob: TJvParserInfo;
begin
  Ob := TJvParserInfo.Create;
  Ob.StartTag := '';
  Ob.EndTag := '';
  Ob.MustBe := -1;
  Ob.TakeText := 0;
  ListBox1.ItemIndex := ListBox1.Items.AddObject('New', TObject(Ob));
  ListBox1Click(Sender);
end;

procedure TFormParsers.ListBox1Click(Sender: TObject);
var
  Ob: TJvParserInfo;
begin
  GroupBox1.Enabled := True;
  Ob := TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]);
  Edit1.Text := ListBox1.Items[ListBox1.ItemIndex];
  Edit2.Text := Ob.StartTag;
  Edit3.Text := Ob.EndTag;
  Edit4.Text := IntToStr(Ob.MustBe);
  ComboBox1.ItemIndex := Ob.TakeText;
end;

procedure TFormParsers.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    I := ListBox1.ItemIndex;
    ListBox1.Items.Delete(I);
    Dec(I);
    if I >= 0 then
    begin
      ListBox1.ItemIndex := I;
      ListBox1Click(Sender);
    end
    else
      GroupBox1.Enabled := False;
  end;
end;

procedure TFormParsers.Edit2Change(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]).StartTag := (Sender as TEdit).Text;
end;

procedure TFormParsers.Edit3Change(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]).EndTag := (Sender as TEdit).Text;
end;

procedure TFormParsers.LoadFromStr(Value: TStringList);
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

function TFormParsers.SetFromStr: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to ListBox1.Items.Count - 1 do
  begin
    Result.Add(ListBox1.Items[I]);
    Result.Add(TJvParserInfo(ListBox1.Items.Objects[I]).StartTag);
    Result.Add(TJvParserInfo(ListBox1.Items.Objects[I]).EndTag);
    Result.Add(IntToStr(TJvParserInfo(ListBox1.Items.Objects[I]).MustBe));
    Result.Add(IntToStr(TJvParserInfo(ListBox1.Items.Objects[I]).TakeText));
  end;
end;

procedure TFormParsers.ComboBox1Change(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TJvParserInfo(ListBox1.Items.Objects[ListBox1.ItemIndex]).TakeText := (Sender as TComboBox).ItemIndex;
end;

procedure TFormParsers.Edit4Change(Sender: TObject);
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

procedure TFormParsers.OkBtnClick(Sender: TObject);
begin
  Tag := 0;
  Close;
end;

procedure TFormParsers.CancelBtnClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;

end.
