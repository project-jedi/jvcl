unit EmbeddedFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvEmbeddedForms, JvComponent,
  JvExControls;

type
  TFirstEmbeddedForm = class(TForm)
    JvEmbeddedFormLink1: TJvEmbeddedFormLink;
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    EmbededFormPanel1: TJvEmbeddedFormPanel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var FirstEmbeddedForm: TFirstEmbeddedForm;

implementation

uses DeepEmbeddedFormUnit;

{$R *.DFM}

procedure TFirstEmbeddedForm.FormShow(Sender: TObject);
begin
//  ShowMessage ('Embedded Form OnShow');
  OnShow := nil;
end;

procedure TFirstEmbeddedForm.FormCreate(Sender: TObject);
begin
//  ShowMessage ('Embedded Form OnCreate');
end;

end.


 

 
 
