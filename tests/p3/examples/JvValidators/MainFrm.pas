{$I JVCL.INC}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvValidators, JvErrProvider, ImgList;

type
  TfrmMain = class(TForm)
    Label1: TLabel;
    edRequired: TEdit;
    Label2: TLabel;
    edRequired10Chars: TEdit;
    Label3: TLabel;
    edRegExpr: TEdit;
    Label4: TLabel;
    edRange0to100: TEdit;
    udRange0to100: TUpDown;
    btnCheck: TButton;
    Label5: TLabel;
    btnProviderCheck: TButton;
    il16: TImageList;
    reResults: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnProviderCheckClick(Sender: TObject);
  private
    // custom validation event
    procedure Do10CharValidate(Sender: TObject; ValueToValidate: Variant;
      var IsValid: boolean);
    // validation fail event
    procedure DoValidateFailed(Sender: TObject;
      Validator: TJvBaseValidator; var Continue: boolean);
    // validation fail event
    procedure DoErrProviderValidateFailed(Sender: TObject;
      Validator: TJvBaseValidator; var Continue: boolean);
    { Private declarations }
  public
    { Public declarations }
    JvValidator:TJvValidator;
    JvErrProv:TJvErrorProvider;
  end;

var
  frmMain: TfrmMain;

implementation
{$IFDEF COMPILER6_UP}
uses
  Variants;
{$ENDIF}

{$R *.DFM}

procedure TfrmMain.Do10CharValidate(Sender: TObject; ValueToValidate: Variant;
  var IsValid: boolean);
begin
  IsValid := not VarIsNull(ValueToValidate) and (Length(string(ValueToValidate)) >= 10);
end;
procedure TfrmMain.DoErrProviderValidateFailed(Sender:TObject; Validator:TJvBaseValidator;var Continue:boolean);
begin
  JvErrProv.Error[Validator.ObjectToValidate] := Validator.ErrorMessage;
end;

procedure TfrmMain.DoValidateFailed(Sender:TObject; Validator:TJvBaseValidator;var Continue:boolean);
begin
  reResults.Lines.Add('-' + Validator.ErrorMessage);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var Jv:TJvBaseValidator;
begin
  JvValidator := TJvValidator.Create(self);
  JvErrProv   := TJvErrorProvider.Create(self);
  JvErrProv.ImageList := il16;
  Jv := JvValidator.Validators.Add(TJvRequiredFieldValidator);
  with Jv do
  begin
    ErrorMessage := 'edRequired cannot be empty!';
    ObjectToValidate := edRequired;
    PropertyToValidate := 'Text';
  end;

  Jv := JvValidator.Validators.Add(TJvCustomValidator);
  with Jv do
  begin
    ErrorMessage := 'edRequired10Chars requires at least 10 characters !';
    ObjectToValidate := edRequired10Chars;
    PropertyToValidate := 'Text';
    TJvCustomValidator(Jv).OnValidate := Do10CharValidate;
  end;

  Jv := JvValidator.Validators.Add(TJvRegularExpressionValidator);
  with Jv do
  begin
    ErrorMessage := 'edRegExpr does not match "A.B.C."!';
    ObjectToValidate := edRegExpr;
    PropertyToValidate := 'Text';
    TJvRegularExpressionValidator(Jv).ValidationExpression := 'A.B.C.*';
  end;

  Jv := JvValidator.Validators.Add(TJvRangeValidator);
  with Jv do
  begin
    ErrorMessage := 'Value in udRange0to100 must be between 0 and 100!';
    ObjectToValidate := udRange0to100;
    PropertyToValidate := 'Position';
    TJvRangeValidator(Jv).MinimumValue := 0;
    TJvRangeValidator(Jv).MaximumValue := 100;
  end;
end;

procedure TfrmMain.btnCheckClick(Sender: TObject);
begin
  reResults.Lines.Clear;
  JvValidator.OnValidateFailed := DoValidateFailed;
  if not JvValidator.Validate then
    ShowMessage('Please correct errors and try again!')
  else
    reResults.Lines.Add('All OK!');
end;

procedure TfrmMain.btnProviderCheckClick(Sender: TObject);
begin
  reResults.Lines.Clear;
  JvErrProv.BeginUpdate;
  try
    JvErrProv.ClearErrors;
    JvValidator.OnValidateFailed := DoErrProviderValidateFailed;
    if JvValidator.Validate then
      reResults.Lines.Add('All OK!');
  finally
    JvErrProv.EndUpdate;
  end;
end;

end.
