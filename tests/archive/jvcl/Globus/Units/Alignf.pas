unit AlignF;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, gltypes;

type

  TAlignForm = class(TForm)
    g_Horz: TRadioGroup;
    g_Vert: TRadioGroup;
    B_Ok: TButton;
    B_Cancel: TButton;
    procedure B_OkClick(Sender: TObject);
    procedure B_CancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    Horz: TglHComponentAlign;
    Vert: TglVComponentAlign;
  end;

var
  AlignForm: TAlignForm;

implementation

{$R *.DFM}

procedure TAlignForm.B_OkClick(Sender: TObject);
begin
  Horz := TglHComponentAlign( g_Horz.ItemIndex);
  Vert := TglVComponentAlign( g_Vert.ItemIndex);
  ModalResult := mrOK;
end;

procedure TAlignForm.B_CancelClick(Sender: TObject);
begin
  ModalResult := mrCANCEL;
end;

end.
