unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, ComCtrls, CheckLst, Mask,
  JvDotNetControls, JvDBDotNetControls;

type
  TMainForm = class(TForm)
    dxDNCheckListBox1: TJvDotNetCheckListBox;
    JvDotNetDbEdit1: TJvDotNetDBEdit;
    JvDotNetDbListBox1: TJvDotNetDBListBox;
    JvDotNetDbMemo1: TJvDotNetDBMemo;
    JvDotNetDbRichEdit1: TJvDotNetDBRichEdit;
    JvDotNetEdit1: TJvDotNetEdit;
    dxDNHotKey1: TJvDotNetHotKey;
    dxDNListBox1: TJvDotNetListBox;
    JvDotNetMaskEdit1: TJvDotNetMaskEdit;
    JvDotNetMemo1: TJvDotNetMemo;
    dxDNScrollBox1: TJvDotNetScrollBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Image2: TImage;
    Label8: TLabel;
    PageControl1: TPageControl;
    Shape1: TPanel;
    TabSheet4: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  PageControl1.ActivePageIndex := 0;
end;

end.

