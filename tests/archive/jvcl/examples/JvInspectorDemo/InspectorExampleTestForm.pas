unit InspectorExampleTestForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrmTest = class(TForm)
    PanelForLabel: TPanel;
    lblTest: TLabel;
    Edit1: TEdit;
    mmChanges: TMemo;
    procedure Edit1Change1(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTest: TfrmTest;

implementation

{$R *.DFM}

procedure TfrmTest.Edit1Change1(Sender: TObject);
begin
  mmChanges.Lines.Add('Edit1Change1 event');
end;

end.
