unit InspectorExampleTestForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvComponent;

type
  TfrmTest = class(TForm)
    PanelForLabel: TPanel;
    lblTest: TLabel;
    ListBox1: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTest: TfrmTest;

implementation

{$R *.DFM}

end.
