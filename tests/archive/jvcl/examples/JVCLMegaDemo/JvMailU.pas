unit JvMailU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvDialogs, JvComponent, JvMail, StdCtrls, ComCtrls;

type
  TJvMailForm = class(TForm)
    ClientLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ClientTypeGroupBox: TGroupBox;
    AutomaticRadioBtn: TRadioButton;
    MapiRadioBtn: TRadioButton;
    DirectRadioBtn: TRadioButton;
    ClientsListView: TListView;
    ToEdit: TEdit;
    SubjectEdit: TEdit;
    BodyEdit: TRichEdit;
    SendBtn: TButton;
    AttachmentMemo: TMemo;
    AttachBtn: TButton;
    CcEdit: TEdit;
    JvMail1: TJvMail;
    JvOpenDialog1: TJvOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure AttachBtnClick(Sender: TObject);
    procedure ClientsListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure AutomaticRadioBtnClick(Sender: TObject);
    procedure ClientsListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    procedure BuildClientList;
    procedure UpdateClientName;
  end;

implementation

{$R *.dfm}

uses
  JclMapi;
  
procedure TJvMailForm.BuildClientList;
var
  I: Integer;
begin
  ClientsListView.Items.BeginUpdate;
  ClientsListView.Items.Clear;
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

procedure TJvMailForm.UpdateClientName;
begin
  ClientLabel.Caption := JvMail1.SimpleMAPI.CurrentClientName;
end;

procedure TJvMailForm.FormCreate(Sender: TObject);
begin
  BuildClientList;
  UpdateClientName;
end;

procedure TJvMailForm.SendBtnClick(Sender: TObject);
begin
  JvMail1.Clear;
  JvMail1.Recipient.AddRecipient(ToEdit.Text);
  if CcEdit.Text <> '' then JvMail1.CarbonCopy.AddRecipient(CcEdit.Text);
  JvMail1.Subject := SubjectEdit.Text;
  JvMail1.Body.Text := BodyEdit.Text;
  JvMail1.Attachment.Assign(AttachmentMemo.Lines);
  JvMail1.SendMail;
end;

procedure TJvMailForm.AttachBtnClick(Sender: TObject);
begin
  JvOpenDialog1.FileName := '';
  if JvOpenDialog1.Execute then
    AttachmentMemo.Lines.AddStrings(JvOpenDialog1.Files);
end;

procedure TJvMailForm.ClientsListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not Boolean(Item.Data) then
    Sender.Canvas.Font.Color := clInactiveCaption;
end;

procedure TJvMailForm.AutomaticRadioBtnClick(Sender: TObject);
begin
  with JvMail1.SimpleMAPI do
  begin
    if AutomaticRadioBtn.Checked then ClientConnectKind := ctAutomatic;
    if MapiRadioBtn.Checked then ClientConnectKind := ctMapi;
    if DirectRadioBtn.Checked then ClientConnectKind := ctDirect;
  end;
  UpdateClientName;
end;

procedure TJvMailForm.ClientsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    JvMail1.SimpleMAPI.SelectedClientIndex := Item.Index;
    UpdateClientName;
  end;
end;

end.
