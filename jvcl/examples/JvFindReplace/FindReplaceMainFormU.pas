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

unit FindReplaceMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, JvFindReplace, JvComponent, ExtCtrls;

type
  TFindReplaceMainForm = class(TForm)
    FindReplace1: TJvFindReplace;
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    Search1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    FindAgain1: TMenuItem;
    Serachown1: TMenuItem;
    Find2: TMenuItem;
    Replace2: TMenuItem;
    FindAgain2: TMenuItem;
    Options1: TMenuItem;
    Rememberlastsearch1: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    procedure Find1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure FindAgain1Click(Sender: TObject);
    procedure FindReplace1NotFound(Sender: TObject);
    procedure FindReplace1Find(Sender: TObject);
    procedure FindReplace1Replace(Sender: TObject);
    procedure Find2Click(Sender: TObject);
    procedure Replace2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FindAgain2Click(Sender: TObject);
    procedure FindReplace1Show(Sender: TObject);
    procedure FindReplace1Close(Sender: TObject);
    procedure Rememberlastsearch1Click(Sender: TObject);
  private
    { Private declarations }
    FOptions:TFindOptions;
    FCount:integer;
  end;

var
  FindReplaceMainForm: TFindReplaceMainForm;

implementation
uses
  JvJVCLUtils;

{$R *.DFM}
function DualInputQuery(const ACaption, Prompt1, Prompt2:string; var AValue1, AValue2:string):boolean;
var
  AForm:TForm;
  ALabel1, ALabel2:TLabel;
  AEdit1, AEdit2:TEdit;
  ASize, i:integer;
begin
  Result := false;
  AForm := CreateMessageDialog(Prompt1,mtCustom,[mbOK	,mbCancel]);
  ASize := 0;
  if AForm <> nil then
  try
    AForm.Caption := ACaption;
    ALabel1 := AForm.FindComponent('Message') as TLabel;
    for i := 0 to AForm.ControlCount - 1 do
      if AForm.Controls[i] is TButton then
        TButton(AForm.Controls[i]).Anchors := [akRight, akBottom];
    if ALabel1 <> nil then
    begin
      AEdit1 := TEdit.Create(AForm);
      AEdit1.Left := ALabel1.Left;
      AEdit1.Width := AForm.ClientWidth - AEdit1.Left * 2;
      AEdit1.Top := ALabel1.Top + ALabel1.Height + 2;
      AEdit1.Parent := AForm;
      AEdit1.Anchors := [akLeft, akTop, akRight];
      AEdit1.Text := AValue1;
      ALabel1.Caption := Prompt1;
      ALabel1.FocusControl := AEdit1;
      Inc(ASize, AEdit1.Height + 2);

      ALabel2 := TLabel.Create(AForm);
      ALabel2.Left := ALabel1.Left;
      ALabel2.Top := AEdit1.Top + AEdit1.Height + 7;
      ALabel2.Caption := Prompt2;
      ALabel2.Parent := AForm;
      Inc(ASize, ALabel2.Height + 7);

      AEdit2 := TEdit.Create(AForm);
      AEdit2.Left := ALabel1.Left;
      AEdit2.Width := AForm.ClientWidth - AEdit2.Left * 2;
      AEdit2.Top := ALabel2.Top + ALabel2.Height + 2;
      AEdit2.Parent := AForm;
      AEdit2.Anchors := [akLeft, akTop, akRight];
      AEdit2.Text := AValue1;
      ALabel2.FocusControl := AEdit2;

      Inc(ASize, AEdit2.Height + 8);
      AForm.ClientHeight := AForm.ClientHeight + ASize;
      AForm.ClientWidth := 320;
      AForm.ActiveControl := AEdit1;
      Result := AForm.ShowModal = mrOK;
      if Result then
      begin
        AValue1 := AEdit1.Text;
        AValue2 := AEdit2.Text;
      end;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TFindReplaceMainForm.Find1Click(Sender: TObject);
begin
  FindReplace1.ShowDialogs := True;
  FindReplace1.Find;
end;

procedure TFindReplaceMainForm.Replace1Click(Sender: TObject);
begin
  FindReplace1.ShowDialogs := True;
  FindReplace1.Replace;
end;

procedure TFindReplaceMainForm.FindAgain1Click(Sender: TObject);
begin
  { reset to saved options }
  FindReplace1.Options := FOptions;
  FindReplace1.FindAgain;
end;

procedure TFindReplaceMainForm.FindReplace1NotFound(Sender: TObject);
begin
  if not FindReplace1.ShowDialogs then
    ShowMessage('Text not found!');
  Caption := 'Not found! ' + IntToStr(FCount);
  Inc(FCount);
end;

procedure TFindReplaceMainForm.FindReplace1Find(Sender: TObject);
begin
  FOptions := FindReplace1.Options;
  Caption := 'Find next clicked! ' + IntToStr(FCount);
  Inc(FCount);
end;

procedure TFindReplaceMainForm.FindReplace1Replace(Sender: TObject);
begin
  Caption := 'Replace clicked! '  + IntToStr(FCount);
  Inc(FCount);
end;

procedure TFindReplaceMainForm.Find2Click(Sender: TObject);
var S:string;
begin
  S := FindReplace1.FindText;
  if InputQuery('Find','Search for:',S) then
  begin
    FindReplace1.ShowDialogs := False;
    FindReplace1.FindText := S;
    FindReplace1.Find;
  end;
end;

procedure TFindReplaceMainForm.Replace2Click(Sender: TObject);
var S,R:string;
begin
   S := FindReplace1.FindText;
   R := FindReplace1.ReplaceText;
   if DualInputQuery('Find and Replace','Find:','Replace:',S, R) and (S <> '') then
     begin
       FindReplace1.ShowDialogs := False;
       FindReplace1.FindText := S;
       FindReplace1.ReplaceText := R;
       FindReplace1.Replace;
     end;
end;

procedure TFindReplaceMainForm.FormCreate(Sender: TObject);
begin
  Fcount := 0;
end;

procedure TFindReplaceMainForm.FindAgain2Click(Sender: TObject);
begin
  FindReplace1.FindAgain;
end;

procedure TFindReplaceMainForm.FindReplace1Show(Sender: TObject);
begin
  Caption := 'Showing';
end;

procedure TFindReplaceMainForm.FindReplace1Close(Sender: TObject);
begin
  Caption := 'Closing';
end;

procedure TFindReplaceMainForm.Rememberlastsearch1Click(Sender: TObject);
begin
   Rememberlastsearch1.Checked := not Rememberlastsearch1.Checked;
   FindReplace1.Keeptext := Rememberlastsearch1.Checked;
end;

end.
