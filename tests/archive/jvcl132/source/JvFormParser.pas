{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormParser.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvFormParser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  JvButton, JvTypes;

type
  TFormParsers = class(TForm)
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
    BUButton1: TJvButton;
    BUButton2: TJvButton;
    BUButton3: TJvButton;
    BUButton4: TJvButton;
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure BUButton3Click(Sender: TObject);
    procedure BUButton4Click(Sender: TObject);
  private
  public
    procedure LoadFromStr(Value: TstringList);
    function SetFromStr: TstringList;
  end;

implementation

{$R *.DFM}

{type
  TParserInf = class
    StartTag: string;
    EndTag: string;
    MustBe: Integer;
    Take: Integer;
  end;}

  {*********************************************************************}

procedure TFormParsers.Edit1Change(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    ListBox1.Items[ListBox1.ItemIndex] := (Sender as TEdit).Text;
end;

{*********************************************************************}

procedure TFormParsers.Button1Click(Sender: TObject);
var
  ob: TParserInf;
begin
  ob := TParserInf.Create;
  ob.StartTag := '';
  ob.EndTag := '';
  ob.MustBe := -1;
  ob.TakeText := 0;
  ListBox1.ItemIndex := ListBox1.Items.AddObject('New', TObject(ob));
  ListBox1Click(Sender);
end;

{*********************************************************************}

procedure TFormParsers.ListBox1Click(Sender: TObject);
var
  ob: TParserInf;
begin
  GroupBox1.Enabled := True;
  ob := TParserInf(ListBox1.Items.Objects[ListBox1.ItemIndex]);
  Edit1.text := ListBox1.Items[ListBox1.ItemIndex];
  Edit2.text := ob.StartTag;
  Edit3.text := ob.EndTag;
  Edit4.text := IntToStr(ob.MustBe);
  ComboBox1.ItemIndex := ob.TakeText;
end;

{*********************************************************************}

procedure TFormParsers.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    i := ListBox1.ItemIndex;
    ListBox1.Items.Delete(i);
    Dec(i);
    if i >= 0 then
    begin
      ListBox1.ItemIndex := i;
      ListBox1Click(Sender);
    end
    else
      GroupBox1.Enabled := False;
  end;
end;

{*********************************************************************}

procedure TFormParsers.Edit2Change(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TParserInf(ListBox1.Items.Objects[ListBox1.ItemIndex]).StartTag := (Sender as TEdit).Text;
end;

{*********************************************************************}

procedure TFormParsers.Edit3Change(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TParserInf(ListBox1.Items.Objects[ListBox1.ItemIndex]).EndTag := (Sender as TEdit).Text;
end;

{*********************************************************************}

procedure TFormParsers.LoadFromStr(Value: TStringList);
var
  i: Integer;
  ob: TParserInf;
  cap: string;
begin
  i := 0;
  while i < Value.Count do
  begin
    ob := TParserInf.Create;
    try
      cap := Value[i];
      Inc(i);
      ob.StartTag := Value[i];
      Inc(i);
      ob.EndTag := Value[i];
      Inc(i);
      ob.MustBe := StrToInt(Value[i]);
      Inc(i);
      ob.TakeText := StrToInt(Value[i]);
      Inc(i);
    finally
      ListBox1.Items.AddObject(cap, TObject(ob));
    end;
  end;
end;

{*********************************************************************}

function TFormParsers.SetFromStr: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    Result.Add(ListBox1.Items[i]);
    Result.Add(TParserInf(ListBox1.Items.Objects[i]).StartTag);
    Result.Add(TParserInf(ListBox1.Items.Objects[i]).EndTag);
    Result.Add(IntToStr(TParserInf(ListBox1.Items.Objects[i]).MustBe));
    Result.Add(IntToStr(TParserInf(ListBox1.Items.Objects[i]).TakeText));
  end;
end;

{*********************************************************************}

procedure TFormParsers.ComboBox1Change(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    TParserInf(ListBox1.Items.Objects[ListBox1.ItemIndex]).TakeText := (Sender as TComboBox).ItemIndex;
end;

{*********************************************************************}

procedure TFormParsers.Edit4Change(Sender: TObject);
var
  i: Integer;
begin
  i := 0;
  try
    i := StrToInt((Sender as TEdit).Text);
  except
    Beep;
  end;
  if ListBox1.ItemIndex <> -1 then
    TParserInf(ListBox1.Items.Objects[ListBox1.ItemIndex]).MustBe := i;
end;

{*********************************************************************}

procedure TFormParsers.BUButton3Click(Sender: TObject);
begin
  Tag := 0;
  Close;
end;

{*********************************************************************}

procedure TFormParsers.BUButton4Click(Sender: TObject);
begin
  Tag := 1;
  Close;
end;

end.
