unit DeepEmbeddedFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvEmbeddedForms, JvComponent;

type
  TDeepEmbeddedForm = class(TForm)
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    JvEmbeddedFormLink1: TJvEmbeddedFormLink;
    RadioButton1: TRadioButton;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var DeepEmbeddedForm: TDeepEmbeddedForm;

implementation

{$R *.DFM}

procedure TDeepEmbeddedForm.FormShow(Sender: TObject);
begin
//  ShowMessage ('Deep Embedded Form OnShow');
  OnShow := nil;
end;

procedure TDeepEmbeddedForm.FormCreate(Sender: TObject);
begin
//  ShowMessage ('Deep Embedded Form OnCreate');
end;

end.

