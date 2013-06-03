unit fTreeViewMenu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, JvPageControl2, JvTreeView2
//  , JvComCtrls
  ;
type
  TForm1 = class(TForm)
    JvTreeView1: TJvTreeView2;
    JvPageControl1: TJvPageControl2;
    TabSheet1: TTabSheet;
    Button1: TButton;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    ListBox1: TListBox;
    ListBox2: TListBox;
    RadioGroup1: TRadioGroup;
    Panel1: TPanel;
    procedure JvTreeView1PageChanged(Sender: TObject; Item: TTreeNode;
      Page: TTabSheet);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.JvTreeView1PageChanged(Sender: TObject; Item: TTreeNode;
  Page: TTabSheet);
begin
  Caption := Item.Text+' - '+Page.Caption;
end;

end.
