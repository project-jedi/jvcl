unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, JvPageListTreeView, ExtCtrls, StdCtrls, ActnList,
  JvButton, JvFooter, JvComponent, JvGroupHeader, JvCombobox, JvColorCombo,
  Buttons, JvBitBtn;

type
  TfrmMain = class(TForm)
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Splitter1: TSplitter;
    JvPagedTreeView1: TJvSettingsTreeView;
    ActionList1: TActionList;
    JvStandardPage2: TJvStandardPage;
    JvStandardPage4: TJvStandardPage;
    JvStandardPage1: TJvStandardPage;
    JvStandardPage5: TJvStandardPage;
    JvFooter1: TJvFooter;
    JvFooterBtn2: TJvFooterBtn;
    JvFooterBtn3: TJvFooterBtn;
    JvFooterBtn1: TJvFooterBtn;
    JvPageList1: TJvPageList;
    pgEnvironmentGeneral: TJvStandardPage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    JvGroupHeader3: TJvGroupHeader;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button1: TButton;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    TrackBar1: TTrackBar;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    pgDocuments: TJvStandardPage;
    Label7: TLabel;
    Label8: TLabel;
    JvGroupHeader4: TJvGroupHeader;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    Edit3: TEdit;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    pgDynamicHelp: TJvStandardPage;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    ListView1: TListView;
    ListView2: TListView;
    Button2: TButton;
    Button3: TButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    CheckBox14: TCheckBox;
    Edit4: TEdit;
    pgFontsColors: TJvStandardPage;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Shape1: TShape;
    Label19: TLabel;
    ComboBox2: TComboBox;
    Button4: TButton;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ListBox1: TListBox;
    JvColorComboBox1: TJvColorComboBox;
    JvColorComboBox2: TJvColorComboBox;
    CheckBox15: TCheckBox;
    Button5: TButton;
    Button6: TButton;
    pgHelp: TJvStandardPage;
    pgInternational: TJvStandardPage;
    pgKeyboard: TJvStandardPage;
    pgProjSolutions: TJvStandardPage;
    pgTaskList: TJvStandardPage;
    pgWebBrowser: TJvStandardPage;
    JvGroupHeader1: TJvGroupHeader;
    JvGroupHeader2: TJvGroupHeader;
    Button7: TButton;
    ImageList2: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure JvFooterBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  JvColorComboBox1.InsertColor(0, clBlack, 'Automatic');
  JvColorComboBox2.InsertColor(0, clWhite, 'Automatic');
end;

procedure TfrmMain.JvFooterBtn2Click(Sender: TObject);
begin
  Close;
end;

end.

