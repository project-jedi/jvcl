unit JvTreeViewAsMenuMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, JvComCtrls, JvComponent, JvCaptionPanel;
  
type
  TJvTreeViewAsMenuMainForm = class(TForm)
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
    procedure JvTreeView1PageChanged(Sender: TObject; Item: TTreeNode; Page: TTabSheet);
  end;

var
  JvTreeViewAsMenuMainForm: TJvTreeViewAsMenuMainForm;

implementation

{$R *.DFM}

procedure TJvTreeViewAsMenuMainForm.JvTreeView1PageChanged(Sender: TObject; Item: TTreeNode;
  Page: TTabSheet);
begin
  Caption := Item.Text+' - '+Page.Caption;
end;

end.
