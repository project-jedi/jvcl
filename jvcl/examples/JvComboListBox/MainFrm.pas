unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvComponent, JvClipboardViewer, ExtCtrls, ExtDlgs,
  ComCtrls, Menus, JvComboListBox, JvListBox;

type

  TForm1 = class(TForm)
    JvClipboardViewer1: TJvClipboardViewer;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    pnlImage: TPanel;
    Image1: TImage;
    btnLoadImage: TButton;
    btnCopyImage: TButton;
    btnCopyText: TButton;
    edItemHeight: TEdit;
    udItemHeight: TUpDown;
    cbDrawStyle: TComboBox;
    edButtonWidth: TEdit;
    udButtonWidth: TUpDown;
    btnLoadText: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    PopupMenu1: TPopupMenu;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Original1: TMenuItem;
    Stretch1: TMenuItem;
    Proportional1: TMenuItem;
    OpenDialog1: TOpenDialog;
    chkHotTrackCombo: TCheckBox;
    edColumns: TEdit;
    Label5: TLabel;
    udColumns: TUpDown;
    chkInsert: TCheckBox;
    procedure JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
    procedure JvClipboardViewer1Text(Sender: TObject; Text: string);
    procedure btnCopyTextClick(Sender: TObject);
    procedure btnCopyImageClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure udItemHeightClick(Sender: TObject; Button: TUDBtnType);
    procedure FormCreate(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure cbDrawStyleChange(Sender: TObject);
    procedure Proportional1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure udButtonWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure btnLoadTextClick(Sender: TObject);
    procedure chkHotTrackComboClick(Sender: TObject);
    procedure udColumnsClick(Sender: TObject; Button: TUDBtnType);
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
  try
    P.Assign(Image);
    if chkInsert.Checked then
      LB.InsertImage(0, P)
    else
      LB.AddImage(P);
  finally
    P.Free; // AddImage creates a copy, so we can free this instance
  end;
//  LB.ItemHeight := Max(LB.ItemHeight, B.Height + 8);
  udItemHeight.Position := LB.ItemHeight;
  Caption := Format('Count: %d', [LB.Items.Count]);
end;

procedure TForm1.JvClipboardViewer1Text(Sender: TObject; Text: string);
begin
  if chkInsert.Checked then
    LB.InsertText(0, StringReplace(Text, #13#10, ' ', [rfReplaceAll]))
  else
    LB.AddText(StringReplace(Text, #13#10, ' ', [rfReplaceAll]));
  Caption := Format('Clipboard count: %d', [LB.Items.Count]);
end;

procedure TForm1.btnCopyTextClick(Sender: TObject);
begin
  if Memo1.SelLength = 0 then
    Memo1.SelectAll;
  Memo1.CopyToClipboard;
end;

procedure TForm1.btnCopyImageClick(Sender: TObject);
var
  AFormat: Word;
  AData: Cardinal;
  APalette: HPALETTE;
  il: TImageList;
begin
  if Image1.Picture.Graphic is TIcon then
  begin
    // convert ico to bmp
    il := TImageList.CreateSize(Image1.Picture.Width, Image1.Picture.Height);
    try
      il.AddIcon(Image1.Picture.Icon);
      il.GetBitmap(0, Image1.Picture.Bitmap);
    finally
      il.Free;
    end;
  end;
  Image1.Picture.SaveToClipboardFormat(AFormat, AData, APalette);
  Clipboard.SetAsHandle(AFormat, AData);
end;

procedure TForm1.btnLoadImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TForm1.udItemHeightClick(Sender: TObject; Button: TUDBtnType);
begin
  LB.ItemHeight := udItemHeight.Position;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LB := TJvComboListBox.Create(Self);
  LB.Align := alClient;
  LB.Width := 200;
  LB.Parent := Self;
  LB.DropDownMenu := PopupMenu1;
//  LB.ScrollBars := ssBoth;
//  LB.HotTrack := true;
  Splitter1.Left := LB.Left - 10;
  cbDrawStyle.ItemIndex := Ord(LB.DrawStyle);
  LB.ItemHeight := udItemHeight.Position;
  udButtonWidth.Position := LB.ButtonWidth;
  udColumns.Position := LB.Columns;
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
var
  i: integer;
begin
  for i := 0 to PopupMenu1.Items.Count - 1 do
    if PopupMenu1.Items[i].GroupIndex = 1 then
      PopupMenu1.Items[i].Checked := PopupMenu1.Items[i].Tag = cbDrawStyle.ItemIndex;
end;

procedure TForm1.udButtonWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  LB.ButtonWidth := udButtonWidth.Position;
end;

procedure TForm1.btnLoadTextClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.chkHotTrackComboClick(Sender: TObject);
begin
  LB.HotTrackCombo := chkHotTrackCombo.Checked;
end;

procedure TForm1.udColumnsClick(Sender: TObject; Button: TUDBtnType);
begin
  LB.Columns := udColumns.Position;
  if LB.Columns > 0 then
    LB.ScrollBars := ssHorizontal
  else
    LB.ScrollBars := ssVertical;
end;

end.

