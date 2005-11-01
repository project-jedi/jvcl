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

unit DSAExamplesCustom2;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, JvQComponent, JvQDSADialogs;

type
  TfrmDSAExamplesCustomDlg2 = class(TForm)
    lblMessage: TLabel;
    rbNone: TRadioButton;
    rbUnlock: TRadioButton;
    rbReportError: TRadioButton;
    cbSuppress: TCheckBox;
    btnOK: TButton;
    JvDSADialog: TJvDSADialog;
    procedure JvDSADialogApplyKeys(Sender: TObject;
      const DSAInfo: TDSARegItem; const Storage: TDSAStorage);
    procedure JvDSADialogUpdateKeys(Sender: TObject;
      const DSAInfo: TDSARegItem; const Storage: TDSAStorage);
    procedure UpdateBtnState(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    { Public declarations }
    procedure SelectRBByIndex(Value: Integer);
    function SelectedRBIndex: Integer;
  end;

function DoCustomDSA2: string;

implementation

{$R *.xfm}

function DoCustomDSA2: string;
begin
  with TfrmDSAExamplesCustomDlg2.Create(Screen.ActiveForm) do
  try
    ShowModal;
    case SelectedRBIndex of
      0:
        Result := 'Do nothing';
      1:
        Result := 'Unlock file';
      2:
        Result := 'Report error and stop';
      else
        Result := 'Unknown state: ' + IntToStr(SelectedRBIndex);
    end;
  finally
    Free;
  end;
end;

{ TfrmDSAExamplesCustomDlg2 }

procedure TfrmDSAExamplesCustomDlg2.SelectRBByIndex(Value: Integer);
begin
  case Value of
    0:
      rbNone.Checked := True;
    1:
      rbUnlock.Checked := True;
    2:
      rbReportError.Checked := True;
    else
      ShowMessage('Unknown Action value read: ' + IntToStr(Value));
  end;
end;

function TfrmDSAExamplesCustomDlg2.SelectedRBIndex: Integer;
begin
  if rbNone.Checked then
    Result := 0
  else if rbUnlock.Checked then
    Result := 1
  else if rbReportError.Checked then
    Result := 2
  else
    Result := -1;
end;

procedure TfrmDSAExamplesCustomDlg2.JvDSADialogApplyKeys(Sender: TObject;
  const DSAInfo: TDSARegItem; const Storage: TDSAStorage);
begin
  // Read the index of the checked radio button and apply it.
  SelectRBByIndex(Storage.ReadInteger(DSAInfo, 'Action'));
end;

procedure TfrmDSAExamplesCustomDlg2.JvDSADialogUpdateKeys(Sender: TObject;
  const DSAInfo: TDSARegItem; const Storage: TDSAStorage);
begin
  // Store the index of the radio button that is checked.
  Storage.WriteInteger(DSAInfo, 'Action', SelectedRBIndex);
end;

procedure TfrmDSAExamplesCustomDlg2.UpdateBtnState(Sender: TObject);
begin
  btnOK.Enabled := rbNone.Checked or rbUnlock.Checked or rbReportError.Checked;
end;

procedure TfrmDSAExamplesCustomDlg2.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := btnOK.Enabled;
end;

end.
