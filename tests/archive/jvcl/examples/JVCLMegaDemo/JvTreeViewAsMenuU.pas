unit JvTreeViewAsMenuU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, JvComCtrls, JvComponent, JvCaptionPanel;
  
type
  TJvTreeViewAsMenu = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    JvPageControl1: TJvPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    TabSheet2: TTabSheet;
    ListBox1: TListBox;
    TabSheet3: TTabSheet;
    ListBox2: TListBox;
    TabSheet4: TTabSheet;
    RadioGroup1: TRadioGroup;
    TabSheet5: TTabSheet;
    Panel1: TPanel;
    JvTreeView1: TJvTreeView;
    Label1: TLabel;
    procedure JvTreeView1PageChanged(Sender: TObject; Item: TTreeNode;
      Page: TTabSheet);
  private
  public
  end;

var
  JvTreeViewAsMenu: TJvTreeViewAsMenu;

implementation

{$R *.DFM}

procedure TJvTreeViewAsMenu.JvTreeView1PageChanged(Sender: TObject; Item: TTreeNode;
  Page: TTabSheet);
begin
  Caption := Item.Text+' - '+Page.Caption;
end;

end.
