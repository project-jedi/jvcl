unit ListCombMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ImgList, JvListComb, ExtCtrls, JvColorBox, JvColorBtn,
  JvColorCombo, JvComponent, JvCtrls, JvCombobox;

type
  TListCombMainForm = class(TForm)
    ImageList1: TImageList;
    Panel2: TPanel;
    JvListBox1: TJvImageListBox;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Button2: TButton;
    JvComboBox1: TJvImageComboBox;
    CheckBox1: TCheckBox;
    JvColorComboBox1: TJvColorComboBox;
    JvFontComboBox1: TJvFontComboBox;
    JvColorButton1: TJvColorButton;
    Button1: TButton;
    PanelTop: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure TransparentButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure JvColorComboBox1Change(Sender: TObject);
    procedure JvFontComboBox1Change(Sender: TObject);
    procedure JvColorButton1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  ListCombMainForm: TListCombMainForm;

implementation

{$R *.DFM}

procedure TListCombMainForm.TransparentButton1Click(Sender: TObject);
begin
  ShowMessage('Hej Simon!')
end;

procedure TListCombMainForm.Button1Click(Sender: TObject);
var S: TStringlist; i: integer;
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'Text files|*.txt|All files|*.*';
    if Execute then
    begin
      S := TStringlist.Create;
      try
        S.LoadFromFile(Filename);
        JvComboBox1.Items.Assign(S);
        JvListBox1.Items.Assign(S);
        if JvComboBox1.ImageList <> nil then
          for i := 0 to JvComboBox1.Items.Count - 1 do
          begin
            JvComboBox1.Items[i].ImageIndex := i mod JvComboBox1.ImageList.Count;
            JvListBox1.Items[i].ImageIndex := JvComboBox1.Items[i].ImageIndex;
          end;
        JvComboBox1.ItemIndex := 0;
      finally
        S.Free;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TListCombMainForm.Button2Click(Sender: TObject);
begin
  case JvListBox1.Alignment of
    taLeftJustify: JvListBox1.Alignment := taRightJustify;
    taRightJustify: JvListBox1.Alignment := taCenter;
    taCenter: JvListBox1.Alignment := taLeftJustify;
  end;
end;

procedure TListCombMainForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    JvComboBox1.ImageList := ImageList1;
    JvListBox1.ImageList := ImageList1;
  end
  else
  begin
    JvComboBox1.ImageList := nil;
    JvListBox1.ImageList := nil;
  end;
end;

procedure TListCombMainForm.JvColorComboBox1Change(Sender: TObject);
begin
  JvListBox1.Color := JvColorComboBox1.ColorValue;
  JvComboBox1.Color := JvColorComboBox1.ColorValue;
end;

procedure TListCombMainForm.JvFontComboBox1Change(Sender: TObject);
begin
  JvListBox1.Font.Name := JvFontComboBox1.FontName;
  JvComboBox1.Font.Name := JvFontComboBox1.FontName;
end;

procedure TListCombMainForm.JvColorButton1Change(Sender: TObject);
begin
  JvComboBox1.Font.Color := JvColorButton1.Color;
  JvListBox1.Font.Color := JvColorButton1.Color;
end;

procedure TListCombMainForm.FormCreate(Sender: TObject);
begin
  JvFontComboBox1.FontName := JvComboBox1.Font.Name;
end;

end.

