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

unit MessageDlgEditorSelectIcon;

interface

uses
  Windows,
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QMask, JvQToolEdit, JvQExMask, QComboEdits, JvQExComboEdits;

type
  TfrmMessageDlgEditorSelectIcon = class(TForm)
    rbWarning: TRadioButton;
    rbError: TRadioButton;
    rbInformation: TRadioButton;
    rbConfirmation: TRadioButton;
    rbNone: TRadioButton;
    rbIconRes: TRadioButton;
    rbBMPRes: TRadioButton;
    rbLoadFile: TRadioButton;
    edIconRes: TEdit;
    edBMPRes: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    edFileName: TJvFilenameEdit;
    procedure FormCreate(Sender: TObject);
    procedure ResNameChanged(Sender: TObject);
    procedure edFileNameChange(Sender: TObject);
  private
    { Private declarations }
    procedure PicSettingChanged;
  public
    { Public declarations }
    procedure ApplyUI;
    procedure UpdateUI;
  end;

function DoSelectPicture: Boolean;

implementation

{$R *.xfm}

uses
  MessageDlgEditorMain;

function DoSelectPicture: Boolean;
begin
  with TfrmMessageDlgEditorSelectIcon.Create(Screen.ActiveCustomForm) do
  try
    UpdateUI;
    Result := ShowModal = mrOK;
    if Result then
      ApplyUI;
  finally
    Free;
  end;
end;

function CheckedRBIndex(const RadioButtons: array of TRadioButton): Integer;
begin
  Result := High(RadioButtons);
  while (Result >= 0) and not RadioButtons[Result].Checked do
    Dec(Result);
end;

{ TfrmDSADlgLoadPic }

procedure TfrmMessageDlgEditorSelectIcon.PicSettingChanged;
var
  I: Integer;
begin
  edIconRes.Enabled := rbIconRes.Checked;
  edBMPRes.Enabled := rbBMPRes.Checked;
  edFileName.Enabled := rbLoadFile.Checked;

  I := CheckedRBIndex([rbNone, rbWarning, rbError, rbInformation, rbConfirmation, rbIconRes,
    rbBMPRes, rbLoadFile]);
  btnOK.Enabled := (I <> -1) and (
    (I < 5) or (
      (I = 5) and
      (edIconRes.Text <> '')
    ) or (
      (I = 6) and
      (edBMPRes.Text <> '')
    ) or (
      (I = 7) and
      (edFileName.FileName <> '')
    )
  ); 
end;

procedure TfrmMessageDlgEditorSelectIcon.ApplyUI;
begin
  frmMessageDlgEditor.PicType := CheckedRBIndex([rbNone, rbWarning, rbError, rbInformation,
    rbConfirmation, rbIconRes, rbBMPRes, rbLoadFile]);
  case frmMessageDlgEditor.PicType of
    1:
      begin
        frmMessageDlgEditor.PicID := IDI_EXCLAMATION;
        frmMessageDlgEditor.PicName := 'IDI_EXCLAMATION';
      end;
    2:
      begin
        frmMessageDlgEditor.PicID := IDI_HAND;
        frmMessageDlgEditor.PicName := 'IDI_HAND';
      end;
    3:
      begin
        frmMessageDlgEditor.PicID := IDI_ASTERISK;
        frmMessageDlgEditor.PicName := 'IDI_ASTERISK';
      end;
    4:
      begin
        frmMessageDlgEditor.PicID := IDI_QUESTION;
        frmMessageDlgEditor.PicName := 'IDI_QUESTION';
      end;
    5: { if rbIconRes.Checked then }
      frmMessageDlgEditor.PicName := edIconRes.Text;
    6: { if rbBMPRes.Checked then }
      frmMessageDlgEditor.PicName := edBMPRes.Text;
    7: { if rbLoadFile.Checked then }
      frmMessageDlgEditor.PicName := edFileName.FileName;
  end;
end;

procedure TfrmMessageDlgEditorSelectIcon.UpdateUI;
begin
  case frmMessageDlgEditor.PicType of
    0:
      rbNone.Checked := True;
    1:
      rbWarning.Checked := True;
    2:
      rbError.Checked := True;
    3:
      rbInformation.Checked := True;
    4:
      rbConfirmation.Checked := True;
    5:
      begin
        rbIconRes.Checked := True;
        edIconRes.Text := frmMessageDlgEditor.PicName;
      end;
    6:
      begin
        rbBMPRes.Checked := True;
        edBMPRes.Text := frmMessageDlgEditor.PicName;
      end;
    7:
      begin
        rbLoadFile.Checked := True;
        edFileName.FileName := frmMessageDlgEditor.PicName;
        edFileName.InitialDir := ExtractFilePath(frmMessageDlgEditor.PicName);
        if edFileName.InitialDir = '' then
          edFileName.InitialDir := ExtractFilePath(Application.ExeName);
      end;
    else
      raise Exception.CreateFmt('Invalid PicType value %d', [frmMessageDlgEditor.PicType]);
  end;
end;

procedure TfrmMessageDlgEditorSelectIcon.FormCreate(Sender: TObject);
begin
  if edFileName.InitialDir = '' then
    edFileName.InitialDir := ExtractFilePath(Application.ExeName);
end;

procedure TfrmMessageDlgEditorSelectIcon.ResNameChanged(Sender: TObject);
begin
  PicSettingChanged;
end;

procedure TfrmMessageDlgEditorSelectIcon.edFileNameChange(Sender: TObject);
begin
  PicSettingChanged;
end;

end.
