unit JvSIMDModifyForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TJvSIMDModifyFrm = class(TForm)
    ComboBoxDisplay: TComboBox;
    ComboBoxFormat: TComboBox;
    LabelDisplay: TLabel;
    LabelFormat: TLabel;
    LabelBlank: TLabel;
    Panel1: TPanel;
  private
  public
    function Execute:Boolean;
  end;

implementation

{$R *.dfm}

{ TJvSIMDModifyFrm }

function TJvSIMDModifyFrm.Execute: Boolean;
begin
  Result := ShowModal = mrOk;
end;

end.
