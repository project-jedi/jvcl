unit IndexFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TIndexForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel1: TPanel;
    Label1: TLabel;
    LookforComboBox: TComboBox;
    ListBox1: TListBox;
    Label2: TLabel;
    FilteredbyComboBox: TComboBox;
    procedure Panel1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IndexForm: TIndexForm;

implementation

uses MSDN2002MainUnit;

{$R *.DFM}

procedure TIndexForm.Panel1Resize(Sender: TObject);
begin
  LookforComboBox.Width := Panel1.ClientWidth - 0;
  FilteredbyComboBox.Width := Panel1.ClientWidth - 0;
end;

procedure TIndexForm.FormCreate(Sender: TObject);
begin
  FilteredbyComboBox.ItemIndex := 0;
end;

end.
