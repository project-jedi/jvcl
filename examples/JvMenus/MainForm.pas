unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, JvMenus, ImgList, ExtCtrls, ToolWin, ComCtrls, JvToolBar,
  StdCtrls;

type
  TfrmMain = class(TForm)
    jmnMain: TJvMainMenu;
    File1: TMenuItem;
    Try1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Other1: TMenuItem;
    Sub11: TMenuItem;
    Hello1: TMenuItem;
    Plaf1: TMenuItem;
    Yop1: TMenuItem;
    Nice1: TMenuItem;
    Checked1: TMenuItem;
    N11: TMenuItem;
    Radio11: TMenuItem;
    Radio21: TMenuItem;
    Radio31: TMenuItem;
    imlImages: TImageList;
    jpmPopup: TJvPopupMenu;
    pnlPopup: TPanel;
    Popup11: TMenuItem;
    Popup21: TMenuItem;
    PopupSub1: TMenuItem;
    Yop2: TMenuItem;
    Yip1: TMenuItem;
    N2: TMenuItem;
    SUb1: TMenuItem;
    SubAgain1: TMenuItem;
    Checked2: TMenuItem;
    CheckedInSub1: TMenuItem;
    AfterPSub1: TMenuItem;
    JvToolBar1: TJvToolBar;
    btnAddItems: TButton;
    Button1: TButton;
    procedure Exit1Click(Sender: TObject);
    procedure btnAddItemsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Try1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.btnAddItemsClick(Sender: TObject);
var item : TMenuItem;
begin
  item := TMenuItem.Create(Self);
  item.Caption := 'Add';
  jmnMain.Items.Add(item);
  item := TMenuItem.Create(Self);
  item.Caption := 'Below Add';
  jmnMain.Items[3].Add(item);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Try1.Caption := 'ReFile';
end;

procedure TfrmMain.Try1Click(Sender: TObject);
begin
  ShowMessage('Nice try !');
end;

end.
