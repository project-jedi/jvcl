unit AdvancedBCBForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmAdvancedBCB = class(TForm)
    bbtOk: TBitBtn;
    lblExpl: TLabel;
    edtBCB5: TEdit;
    lblBCB5: TLabel;
    edtBCB6: TEdit;
    lblBCB6: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAdvancedBCB: TfrmAdvancedBCB;

implementation

{$R *.dfm}

end.
