unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvComponent, JvClipboardViewer, ExtCtrls, ExtDlgs,
  ComCtrls, Menus, JvComboListBox, JvListBox;

type

  TForm1 = class(TForm)
    JvClipboardViewer1: TJvClipboardViewer;
    Label1: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    Label2: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    cbDrawStyle: TComboBox;
    PopupMenu1: TPopupMenu;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Original1: TMenuItem;
    Stretch1: TMenuItem;
    Proportional1: TMenuItem;
    procedure JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
    procedure JvClipboardViewer1Text(Sender: TObject; Text: string);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure FormCreate(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure cbDrawStyleChange(Sender: TObject);
    procedure Proportional1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { Private declarations }
    LB: TJvComboListBox;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  Math, Clipbrd;


{$R *.dfm}

function Max(Values: array of integer): integer;
var
  i: integer;
begin
  Result := Values[Low(Values)];
  for i := Low(Values) + 1 to High(Values) do
    if Values[i] > Result then
      Result := Values[i];
end;

procedure TForm1.JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
var
  P: TPicture;
begin
  P := TPicture.Create;
  P.Bitmap.Assign(Image);
  LB.AddImage(P);
//  LB.ItemHeight := Max(LB.ItemHeight, B.Height + 8);
  UpDown1.Position := LB.ItemHeight;
  Caption := Format('Count: %d', [LB.Items.Count]);
end;

procedure TForm1.JvClipboardViewer1Text(Sender: TObject; Text: string);
begin
  LB.AddText(StringReplace(Text, #13#10, ' ', [rfReplaceAll]));
  Caption := Format('Clipboard count: %d', [LB.Items.Count]);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Memo1.SelLength = 0 then
    Memo1.SelectAll;
  Memo1.CopyToClipboard;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AFormat: Word;
  AData: Cardinal;
  APalette: HPalette;
begin
  Image1.Picture.SaveToClipboardFormat(AFormat, AData, APalette);
  Clipboard.SetAsHandle(AFormat, AData);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.Filename);
end;

procedure TForm1.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  LB.ItemHeight := UpDown1.Position;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LB := TJvComboListBox.Create(Self);
  LB.Align := alRight;
  LB.Width := 200;
  LB.Parent := self;
  LB.DropDownMenu := PopupMenu1;
  cbDrawStyle.ItemIndex := Ord(LB.DrawStyle);
  UpDown1Click(UpDown1, btNext);
end;


procedure TForm1.Paste1Click(Sender: TObject);
begin
  with LB do
  begin
    if Items.Objects[ItemIndex] <> nil then
      Image1.Picture.Assign(TPicture(Items.Objects[ItemIndex]))
    else
      Memo1.Lines.Text := Items[ItemIndex];
  end;
end;

procedure TForm1.Delete1Click(Sender: TObject);
begin
  with LB do
    if ItemIndex >= 0 then
      Delete(ItemIndex);
end;

procedure TForm1.cbDrawStyleChange(Sender: TObject);
begin
  LB.DrawStyle := TJvComboListBoxDrawStyle(cbDrawStyle.ItemIndex);
end;

procedure TForm1.Proportional1Click(Sender: TObject);
begin
  cbDrawStyle.ItemIndex := (Sender as TMenuItem).Tag;
  LB.DrawStyle := TJvComboListBoxDrawStyle(cbDrawStyle.ItemIndex);
  (Sender as TMenuItem).Checked := true;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var i:integer;
begin
  for i := 0 to PopupMenu1.Items.Count - 1 do
    if PopupMenu1.Items[i].GroupIndex = 1 then
      PopupMenu1.Items[i].Checked := PopupMenu1.Items[i].Tag = cbDrawStyle.ItemIndex;
end;

end.

