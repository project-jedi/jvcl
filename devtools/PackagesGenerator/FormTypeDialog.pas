unit FormTypeDialog;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmFormType = class(TForm)
    bbtOk: TBitBtn;
    lblUnable: TLabel;
    lblFileName: TLabel;
    lblDeclarationIs: TLabel;
    lblDeclaration: TLabel;
    cmbType: TComboBox;
    lblPlease: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure EnsureCorrectType(var TypeName : string; FileName : string; declaration : string);
  end;

var
  frmFormType: TfrmFormType;

implementation

{$R *.dfm}

{ TfrmFormType }

procedure TfrmFormType.EnsureCorrectType(var TypeName: string; FileName : string; declaration : string);
begin
  if (TypeName <> 'TForm') and
     (TypeName <> 'TJvForm') and
     (TypeName <> 'TFrame') and
     (TypeName <> 'TDataModule') then
  begin
    lblFileName.Caption := FileName;
    lblDeclaration.Caption := declaration;
    // try to guess from the filename
    if Copy(FileName, Length(FileName)-8, 5)  = 'Frame' then
      cmbType.Text := 'TFrame';
    if (Copy(FileName, Length(FileName)-7, 4) = 'Form') or
       (Copy(FileName, Length(FileName)-9, 6) = 'Dialog') then
      cmbType.Text := 'TForm';

    // show dialog
    ShowModal;
    TypeName := cmbType.Text;
  end;
end;

end.
