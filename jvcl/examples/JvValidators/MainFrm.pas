{$I JVCL.INC}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvValidators, JvErrProvider, ImgList, JvComponent;

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
    reResults: TRichEdit;
    btnValSum: TButton;
    JvValidators1: TJvValidators;
    JvErrorProvider1: TJvErrorProvider;
    JvValidationSummary1: TJvValidationSummary;
    procedure FormCreate(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnProviderCheckClick(Sender: TObject);
    procedure btnValSumClick(Sender: TObject);
    procedure reResultsEnter(Sender: TObject);
  private
    { Private declarations }
    // custom validation event
    procedure Do10CharValidate(Sender: TObject; ValueToValidate: Variant;
      var IsValid: boolean);
    // validation fail event
    procedure DoValidateFailed(Sender: TObject;
      Validator: TJvBaseValidator; var Continue: boolean);
    // validation fail event
    procedure DoErrProviderValidateFailed(Sender: TObject;
      Validator: TJvBaseValidator; var Continue: boolean);
    procedure SetUpValidators;
    procedure DoSummaryChange(Sender: TObject);
  public
    { Public declarations }
    JvValidator:TJvValidators;
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
  JvErrProv.Error[Validator.ControlToValidate] := Validator.ErrorMessage;
  reResults.Lines.Add(Format('%d: %s',[reResults.Lines.Count+1,Validator.ErrorMessage]));
end;

procedure TfrmMain.DoValidateFailed(Sender:TObject; Validator:TJvBaseValidator;var Continue:boolean);
begin
  reResults.Lines.Add(Format('%s: %s',[Char(Ord('A') + reResults.Lines.Count),Validator.ErrorMessage]));
end;

procedure TfrmMain.SetUpValidators;
var Jv:TJvBaseValidator;
begin
  Jv := TJvRequiredFieldValidator.Create(self);
  with Jv do
  begin
    ErrorMessage := 'Value in edRequired cannot be empty';
    ControlToValidate := edRequired;
    PropertyToValidate := 'Text';
  end;
  JvValidator.Insert(Jv);

  Jv := TJvCustomValidator.Create(self);
  with Jv do
  begin
    ErrorMessage := 'Value in "edRequired10Chars" requires at least 10 characters';
    ControlToValidate := edRequired10Chars;
    PropertyToValidate := 'Text';
    TJvCustomValidator(Jv).OnValidate := Do10CharValidate;
  end;
  JvValidator.Insert(Jv);

  Jv := TJvRegularExpressionValidator.Create(self);
  with Jv do
  begin
    ErrorMessage := 'Value in "edRegExpr" does not match "A.B.C."';
    ControlToValidate := edRegExpr;
    PropertyToValidate := 'Text';
    TJvRegularExpressionValidator(Jv).ValidationExpression := '^A.B.C.*';
  end;
  JvValidator.Insert(Jv);

  Jv := TJvRangeValidator.Create(self);
  with Jv do
  begin
    ErrorMessage := 'Value in "udRange0to100" must be between 0 and 100';
    ControlToValidate := udRange0to100;
    PropertyToValidate := 'Position';
    TJvRangeValidator(Jv).MinimumValue := 0;
    TJvRangeValidator(Jv).MaximumValue := 100;
  end;
  JvValidator.Insert(Jv);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  JvValidator := TJvValidators.Create(self);
  JvErrProv   := TJvErrorProvider.Create(self);
//  JvErrProv.ImageList := il16;
  SetUpValidators;
  reResults.WordWrap := true;
end;

procedure TfrmMain.btnCheckClick(Sender: TObject);
begin
  reResults.Lines.Clear;
  reResults.WordWrap := false;
  JvErrProv.ClearErrors;
  JvValidator.ErrorProvider := nil;
  JvValidator.OnValidateFailed := DoValidateFailed;
  JvValidator.Validate;
end;

procedure TfrmMain.btnProviderCheckClick(Sender: TObject);
begin
  reResults.Lines.Clear;
  reResults.WordWrap := false;
  JvErrProv.BeginUpdate;
  try
    JvErrProv.ClearErrors;
    JvValidator.OnValidateFailed := DoErrProviderValidateFailed;
    JvValidator.Validate;
  finally
    JvErrProv.EndUpdate;
  end;
end;

procedure TfrmMain.DoSummaryChange(Sender: TObject);
begin
  reResults.Lines.Text := AnsiUpperCase(TJvValidationSummary(Sender).Summaries.Text);
end;

procedure TfrmMain.btnValSumClick(Sender: TObject);
var V:TJvValidationSummary;
begin
  reResults.Lines.Clear;
  reResults.WordWrap := false;
  JvErrProv.ClearErrors;
  JvValidator.OnValidateFailed := nil;
  V := TJvValidationSummary.Create(nil);
  try
    // Setting the ValidationSummary for TJvValidators will delay
    // triggering the OnChange event until after Validate has completed:
    JvValidator.ValidationSummary := V;
    JvValidator.ErrorProvider := JvErrProv;
    V.OnChange := DoSummaryChange;
    JvValidator.Validate;
  finally
    V.Free;
  end;
end;

procedure TfrmMain.reResultsEnter(Sender: TObject);
begin
  SelectNext(reResults,true,true);
end;

end.
