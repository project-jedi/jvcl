unit DSAExamplesCustom2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvComponent, JvDSADialogs;

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

{$R *.DFM}

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
