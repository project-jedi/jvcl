unit DocumentOutlineUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TDocumentOutlineForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DocumentOutlineForm: TDocumentOutlineForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TDocumentOutlineForm.FormCreate(Sender: TObject);
begin
  Memo1.Text := 'There are no items to show for the selected document.';
end;

end.
