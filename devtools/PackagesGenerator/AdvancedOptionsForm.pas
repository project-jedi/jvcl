unit AdvancedOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmAdvancedOptions = class(TForm)
    bbtOk: TBitBtn;
    lblExpl: TLabel;
    edtBCB5: TEdit;
    lblBCB5: TLabel;
    edtBCB6: TEdit;
    lblBCB6: TLabel;
    lblImageBase: TLabel;
    lblVersionMajorNumber: TLabel;
    lblVersionMinorNumber: TLabel;
    lblReleaseNumber: TLabel;
    lblBuildNumber: TLabel;
    edtImageBase: TEdit;
    edtVersionMajorNumber: TEdit;
    edtVersionMinorNumber: TEdit;
    edtReleaseNumber: TEdit;
    edtBuildNumber: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAdvancedOptions: TfrmAdvancedOptions;

implementation

{$R *.dfm}

end.
