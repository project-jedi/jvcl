{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{$I jvcl.inc}

unit MainFrm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, JvQValidators, JvQErrorIndicator, QImgList, JvQComponent,
  QComCtrlsEx;

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
    btnValSum: TButton;
    JvValidators1: TJvValidators;
    JvErrorIndicator1: TJvErrorIndicator;
    JvValidationSummary1: TJvValidationSummary;
    JvRequiredFieldValidator1: TJvRequiredFieldValidator;
    JvCustomValidator1: TJvCustomValidator;
    JvRegularExpressionValidator1: TJvRegularExpressionValidator;
    JvRangeValidator1: TJvRangeValidator;
    reResults: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnProviderCheckClick(Sender: TObject);
    procedure btnValSumClick(Sender: TObject);
    procedure reResultsEnter(Sender: TObject);
    procedure JvCustomValidator1Validate(Sender: TObject;
      ValueToValidate: Variant; var Valid: Boolean);
    procedure JvValidators1ValidateFailed(Sender: TObject;
      BaseValidator: TJvBaseValidator; var Continue: Boolean);
    procedure JvValidationSummary1Change(Sender: TObject);
  private
    { Private declarations }
    procedure ProviderErrorValidateFailed(Sender: TObject;
      BaseValidator: TJvBaseValidator; var Continue: Boolean);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Variants;


{$R *.xfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  reResults.WordWrap := true;
end;

procedure TfrmMain.btnCheckClick(Sender: TObject);
begin
  reResults.Lines.Clear;
  reResults.WordWrap := false;
  JvErrorIndicator1.ClearErrors;
  JvValidators1.ValidationSummary := nil;
  JvValidators1.ErrorIndicator := nil;
  JvValidators1.OnValidateFailed := JvValidators1ValidateFailed;
  JvValidators1.Validate;
end;

procedure TfrmMain.btnProviderCheckClick(Sender: TObject);
begin
  reResults.Lines.Clear;
  reResults.WordWrap := false;
  // calling BeginUpdate/EndUpdate delays the error reporting until all controls have been validated
  JvErrorIndicator1.BeginUpdate;
  try
    JvErrorIndicator1.ClearErrors;
    JvValidators1.ValidationSummary := nil;
    // custom error messages for this type of check
    JvValidators1.OnValidateFailed := ProviderErrorValidateFailed;
    JvValidators1.Validate;
  finally
    JvErrorIndicator1.EndUpdate;
  end;
end;

procedure TfrmMain.btnValSumClick(Sender: TObject);
begin
  reResults.Lines.Clear;
  reResults.WordWrap := false;
  JvErrorIndicator1.ClearErrors;
  JvValidators1.OnValidateFailed := nil;
  JvValidators1.ErrorIndicator := nil;
  // Setting the ValidationSummary for TJvValidators will delay
  // triggering the OnChange event until after Validate has completed
  JvValidationSummary1.Summaries.Clear;
  JvValidators1.ValidationSummary := JvValidationSummary1;
  JvValidators1.Validate;
end;

procedure TfrmMain.reResultsEnter(Sender: TObject);
begin
  SelectNext(reResults,true,true);
end;

procedure TfrmMain.JvCustomValidator1Validate(Sender: TObject;
  ValueToValidate: Variant; var Valid: Boolean);
begin
  // custom validation
  Valid := not VarIsNull(ValueToValidate) and (Length(string(ValueToValidate)) >= 10);
end;

procedure TfrmMain.JvValidators1ValidateFailed(Sender: TObject;
  BaseValidator: TJvBaseValidator; var Continue: Boolean);
begin
  // using the OnValidateFailed event
  reResults.Lines.Add(Format('FAILED: %s',[BaseValidator.ErrorMessage]));
end;

procedure TfrmMain.ProviderErrorValidateFailed(Sender: TObject;
  BaseValidator: TJvBaseValidator; var Continue: Boolean);
begin
  JvErrorIndicator1.Error[BaseValidator.ControlToValidate] := BaseValidator.ErrorMessage;
  reResults.Lines.Add(Format('PROVIDER: %s',[BaseValidator.ErrorMessage]));
end;
procedure TfrmMain.JvValidationSummary1Change(Sender: TObject);
var i:integer;
begin
  // update all at once
  reResults.Lines.Text := TJvValidationSummary(Sender).Summaries.Text;
  for i := 0 to reResults.Lines.Count - 1 do
    reResults.Lines[i] := 'SUMMARY: ' + reResults.Lines[i];
end;

end.
