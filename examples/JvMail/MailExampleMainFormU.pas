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

unit MailExampleMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JclMapi, JvMail, ExtCtrls, ComCtrls, StdCtrls, JvComponent, JvDialogs;

type
  TMailExampleMainForm = class(TForm)
    JvMail1: TJvMail;
    ClientTypeGroupBox: TGroupBox;
    AutomaticRadioBtn: TRadioButton;
    MapiRadioBtn: TRadioButton;
    DirectRadioBtn: TRadioButton;
    ClientsListView: TListView;
    ClientLabel: TLabel;
    ToEdit: TEdit;
    Label1: TLabel;
    SubjectEdit: TEdit;
    Label2: TLabel;
    BodyEdit: TRichEdit;
    SendBtn: TButton;
    Label3: TLabel;
    AttachmentMemo: TMemo;
    Label4: TLabel;
    AttachBtn: TButton;
    JvOpenDialog1: TJvOpenDialog;
    CcEdit: TEdit;
    Label5: TLabel;
    DownloadBtn: TButton;
    DownloadsListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure ClientsListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure AutomaticRadioBtnClick(Sender: TObject);
    procedure ClientsListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure SendBtnClick(Sender: TObject);
    procedure AttachBtnClick(Sender: TObject);
    procedure DownloadBtnClick(Sender: TObject);
  private
    procedure BuildClientList;
    procedure UpdateClientName;
  end;

var
  MailExampleMainForm: TMailExampleMainForm;

implementation
{$R *.DFM}

procedure TMailExampleMainForm.BuildClientList;
var
  I: Integer;
begin
  ClientsListView.Items.BeginUpdate;
  ClientsListView.Items.Clear;
  ClientsListView.BringToFront;
  with JvMail1.SimpleMAPI do
  begin
    for I := 0 to ClientCount - 1 do
      with ClientsListView.Items.Add do
      begin
        Caption := Clients[I].RegKeyName;
        Data := Pointer(Clients[I].Valid);
        SubItems.Add(Clients[I].ClientName);
        SubItems.Add(Clients[I].ClientPath);
      end;
    ClientsListView.Items[SelectedClientIndex].Selected := True;
    AutomaticRadioBtn.Enabled := AnyClientInstalled;
    MapiRadioBtn.Enabled := SimpleMapiInstalled;
    DirectRadioBtn.Enabled := ClientCount > 0;
  end;
  ClientsListView.Items.EndUpdate;
end;

procedure TMailExampleMainForm.FormCreate(Sender: TObject);
begin
  BuildClientList;
  UpdateClientName;
end;

procedure TMailExampleMainForm.ClientsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    JvMail1.SimpleMAPI.SelectedClientIndex := Item.Index;
    UpdateClientName;
  end;
end;

procedure TMailExampleMainForm.UpdateClientName;
begin
  ClientLabel.Caption := JvMail1.SimpleMAPI.CurrentClientName;
  ClientsListView.BringToFront;
end;

procedure TMailExampleMainForm.AutomaticRadioBtnClick(Sender: TObject);
begin
  with JvMail1.SimpleMAPI do
  begin
    if AutomaticRadioBtn.Checked then ClientConnectKind := ctAutomatic;
    if MapiRadioBtn.Checked then ClientConnectKind := ctMapi;
    if DirectRadioBtn.Checked then ClientConnectKind := ctDirect;
  end;
  UpdateClientName;
end;

procedure TMailExampleMainForm.ClientsListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not Boolean(Item.Data) then
    Sender.Canvas.Font.Color := clInactiveCaption;
end;

procedure TMailExampleMainForm.SendBtnClick(Sender: TObject);
begin
  JvMail1.Clear;
  JvMail1.Recipient.AddRecipient(ToEdit.Text);
  if CcEdit.Text <> '' then JvMail1.CarbonCopy.AddRecipient(CcEdit.Text);
  JvMail1.Subject := SubjectEdit.Text;
  JvMail1.Body.Text := BodyEdit.Text;
  JvMail1.Attachment.Assign(AttachmentMemo.Lines);
  JvMail1.SendMail;
end;

procedure TMailExampleMainForm.AttachBtnClick(Sender: TObject);
begin
  JvOpenDialog1.FileName := '';
  if JvOpenDialog1.Execute then
    AttachmentMemo.Lines.AddStrings(JvOpenDialog1.Files);
end;

procedure TMailExampleMainForm.DownloadBtnClick(Sender: TObject);
var b:boolean;
begin
  DownloadsListView.Items.Clear;
  JvMail1.LogonOptions := [JvMail.loNewSession, JvMail.loDownloadMail];
  JvMail1.LogOn;
  try
    b := JvMail1.FindFirstMail;
    while b do
    begin
      JvMail1.ReadOptions := [roFifo, roHeaderOnly, roPeek];
      JvMail1.ReadMail;
      with DownloadsListView.Items.Add do
      begin
        Caption := JvMail1.Subject;
        SubItems.Add(JvMail1.ReadedMail.RecipientName);
        SubItems.Add(DateTimeToStr(JvMail1.ReadedMail.DateReceived));
      end;
      b := JvMail1.FindNextMail;
    end;
  finally
    JvMail1.LogOff;
    JvMail1.Clear;
    DownloadsListView.BringToFront;
  end;
end;

end.

