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
    Panel3: TPanel;
    Memo2: TMemo;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    JvFormStorageSelectList1: TJvFormStorageSelectList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    JvAppXMLFileStorage1: TJvAppXMLFileStorage;
    JvAppRegistryStorage1: TJvAppRegistryStorage;
    JvAppStorage1: TJvAppStorage;
    Panel2: TPanel;
    Label1: TLabel;
    rbXML: TRadioButton;
    rbReg: TRadioButton;
    rbINI: TRadioButton;
    Edit1: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    Label3: TLabel;
    procedure YetAnotherOption1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure jvStorageKindClick(Sender: TObject);
    procedure JvFormStorage1RestorePlacement(Sender: TObject);
  private
    procedure CheckStorageKind;
  protected
    procedure loaded; override;
    { Private declarations }
  public
    { Public declarations }
    StoragePath: String;
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
    FormStorageSelectList.SaveFormStorage;
      finally
    FormStorageSelectList.Free;
  end;
end;

procedure TMainFormDlg.ToolButton5Click(Sender: TObject);
begin
  jvAppStorage1.WriteInteger(StoragePath+'ListBox1Selected', ListBox1.ItemIndex);
end;

procedure TMainFormDlg.ToolButton6Click(Sender: TObject);
begin
  ListBox1.ItemIndex := jvAppStorage1.ReadInteger(StoragePath+'ListBox1Selected', 0);
end;

procedure TMainFormDlg.CheckStorageKind;
begin
  if rbXML.Checked
     then StoragePath := '\XML\';
  if rbINI.Checked
     then StoragePath := '\INI\';
  if rbReg.Checked
     then StoragePath := '\REG\';
  jvFormStorage1.AppStoragePath := StoragePath;
end;

procedure TMainFormDlg.jvStorageKindClick(Sender: TObject);
begin
  CheckStorageKind;
end;

procedure TMainFormDlg.JvFormStorage1RestorePlacement(Sender: TObject);
begin
  CheckStorageKind;
  // here is an excellent place to restore values for a form if they
  // are the sort of properties that are not published, such as
  // selection values etc...
end;

procedure TMainFormDlg.loaded;
begin
  CheckStorageKind;
end;

end.
