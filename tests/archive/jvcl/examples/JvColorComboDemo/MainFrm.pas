unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvColorCombo, StdCtrls, JvCombobox;

type
  TfrmMain = class(TForm)
    JvColorComboBox1: TJvColorComboBox;
    Memo1: TMemo;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure JvColorComboBox1NewColor(Sender: TObject; Color: TColor;
      var DisplayName: string; var AllowAdd: Boolean);
    procedure CheckBox1Click(Sender: TObject);
  private
    procedure DoBeforeCustom(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.DoBeforeCustom(Sender: TObject);
begin
  JvColorComboBox1.ColorNameMap.Add('$00CECECE=Custom');
  JvColorComboBox1.AddColor($00CECECE, '');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  JvColorComboBox1.Options := [coText,coSysColors,coCustomColors];
  JvColorComboBox1.OnBeforeCustom := DoBeforeCustom;
  JvColorComboBox1.GetColors;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Memo1.Lines := JvColorComboBox1.ColorNameMap;
end;

procedure TfrmMain.JvColorComboBox1NewColor(Sender: TObject; Color: TColor;
  var DisplayName: string; var AllowAdd: Boolean);
begin
  if Edit1.Text <> '' then
    DisplayName := Format(Edit1.Text, [JvColorComboBox1.CustomColorCount + 1]);
  JvColorComboBox1.ColorNameMap.Add(Format('$%s=%s', [IntToHex(Color, 8), DisplayName]));
end;

procedure TfrmMain.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    JvColorComboBox1.Options := JvColorComboBox1.Options + [coCustomColors]
  else
    JvColorComboBox1.Options := JvColorComboBox1.Options - [coCustomColors];
end;

end.

