unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvAppXMLStorage, JvFormPlacementSelectList, JvFormPlacement,
  JvAppDBStorage, JvAppStorageSelectList, JvAppStorage, JvAppIniStorage,
  JvComponent, JvAppRegistryStorage, ExtCtrls, ComCtrls, ToolWin, StdCtrls,
  ShellAPI, Menus;

type
  TMainFormDlg = class(TForm, IAppStorageHandler)
    StatusBar1: TStatusBar;
    JvAppIniFileStorage1: TJvAppIniFileStorage;
    JvFormStorage1: TJvFormStorage;
    PopupMenu1: TPopupMenu;
    AnotherOptiopn1: TMenuItem;
    YetAnotherOption1: TMenuItem;
    Option1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    CheckBox2: TCheckBox;
    TrackBar1: TTrackBar;
    DateTimePicker1: TDateTimePicker;
    Panel3: TPanel;
    Memo2: TMemo;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    JvFormStorageSelectList1: TJvFormStorageSelectList;
    JvAppRegistryStorage1: TJvAppRegistryStorage;
    procedure YetAnotherOption1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; BasePath: string);

  public
    { Public declarations }
  end;

var
  MainFormDlg: TMainFormDlg;

implementation

{$R *.dfm}

procedure TMainFormDlg.YetAnotherOption1Click(Sender: TObject);
begin
  if Sender is TMenuItem
     then TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;


procedure TMainFormDlg.ReadFromAppStorage(AppStorage: TJvCustomAppStorage; BasePath: string);
begin
  CheckBox1.Checked := AppStorage.ReadBoolean(AppStorage.ConcatPaths([BasePath, 'MyCheckBox1']), CheckBox1.Checked);
  CheckBox2.Checked := AppStorage.ReadBoolean(AppStorage.ConcatPaths([BasePath, 'MyCheckBox2']), CheckBox2.Checked);
end;

procedure TMainFormDlg.WriteToAppStorage(AppStorage: TJvCustomAppStorage; BasePath: string);
begin
  AppStorage.WriteBoolean(AppStorage.ConcatPaths([BasePath, 'MyCheckBox1']), CheckBox1.Checked);
  AppStorage.WriteBoolean(AppStorage.ConcatPaths([BasePath, 'MyCheckBox2']), CheckBox2.Checked);
end;


end.
