unit ContentsFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls, ImgList;

type
  TContentsForm = class(TForm)
    lbDockClient1: TJvDockClient;
    TreeView1: TTreeView;
    Panel1: TPanel;
    Label2: TLabel;
    FilteredbyComboBox: TComboBox;
    ImageList1: TImageList;
    procedure Panel1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ContentsForm: TContentsForm;

implementation

uses MSDN2002MainUnit;

{$R *.DFM}

procedure TContentsForm.Panel1Resize(Sender: TObject);
begin
  FilteredbyComboBox.Width := Panel1.ClientWidth - 0;
end;

procedure TContentsForm.FormCreate(Sender: TObject);
begin
  FilteredbyComboBox.ItemIndex := 0;
end;

end.
