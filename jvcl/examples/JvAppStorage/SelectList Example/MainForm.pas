unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvAppXMLStorage, JvFormPlacementSelectList, JvFormPlacement,
  JvAppDBStorage, JvAppStorageSelectList, JvAppStorage, JvAppIniStorage,
  JvComponent, JvAppRegistryStorage, ExtCtrls, ComCtrls, ToolWin, StdCtrls,
  ShellAPI, Menus, jvDynControlEngine, jvDynControlEngineJVCL;

type
  TMainFormDlg = class(TForm)
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
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure YetAnotherOption1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
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

procedure TMainFormDlg.ToolButton1Click(Sender: TObject);
begin
  JvFormStorage1.SaveFormPlacement;
end;

procedure TMainFormDlg.ToolButton2Click(Sender: TObject);
begin
  JvFormStorage1.RestoreFormPlacement;
end;

procedure TMainFormDlg.Button1Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
  FormStorageSelectList := tJvFormStorageSelectList.Create(Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage1;
    FormStorageSelectList.AppStorage := JvAppINIFileStorage1;
    FormStorageSelectList.SelectPath  := 'SelectTest';
    FormStorageSelectList.RestoreFormStorage;
  finally
    FormStorageSelectList.Free;
  end;
end;

procedure TMainFormDlg.Button2Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
  FormStorageSelectList := tJvFormStorageSelectList.Create(Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage1;
    FormStorageSelectList.AppStorage := JvAppINIFileStorage1;
    FormStorageSelectList.SelectPath  := 'SelectTest';
//    if CheckBox1.Checked
    SetDefaultDynControlEngine(DynControlEngineJVCL);
    FormStorageSelectList.SaveFormStorage;
      finally
    FormStorageSelectList.Free;
  end;
end;

end.
