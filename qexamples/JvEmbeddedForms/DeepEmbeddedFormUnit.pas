{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

unit DeepEmbeddedFormUnit;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, JvQEmbeddedForms, JvQComponent;

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

{$R *.xfm}

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

