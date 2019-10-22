{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit MessageDlgEditorMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, CheckLst,
  JclBase, JvDSADialogs, JvAppStorage, JvAppRegistryStorage, JvComponentBase, JvFormPlacement;

type
  TfrmMessageDlgEditor = class(TForm)
    btnClose: TButton;
    gbDlgType: TGroupBox;
    imgWarning: TImage;
    imgError: TImage;
    imgInformation: TImage;
    imgConfirmation: TImage;
    imgCustom: TImage;
    rbWarning: TRadioButton;
    rbError: TRadioButton;
    rbInformation: TRadioButton;
    rbConfirmation: TRadioButton;
    rbCustom: TRadioButton;
    btnSelectIcon: TButton;
    edCustomTitle: TEdit;
    cxCustomTitle: TCheckBox;
    gbButtons: TGroupBox;
    lblDefaultButton: TLabel;
    lblCancelButton: TLabel;
    lblHelpButton: TLabel;
    rbStdButtons: TRadioButton;
    rbCustomButtons: TRadioButton;
    clbStdButtons: TCheckListBox;
    mmCustomButtons: TMemo;
    cbDefaultButton: TComboBox;
    cbCancelButton: TComboBox;
    cbHelpButton: TComboBox;
    gbOther: TGroupBox;
    lblHelpContext: TLabel;
    lblMessage: TLabel;
    rbCenterScreen: TRadioButton;
    rbMainFormCenter: TRadioButton;
    rbActiveFormCenter: TRadioButton;
    edHelpCtx: TEdit;
    cxIsDSADialog: TCheckBox;
    edDSA_ID: TEdit;
    mmMessage: TMemo;
    lblSource: TLabel;
    mmSource: TMemo;
    btnTest: TButton;
    cxAutoClose: TCheckBox;
    edAutoCloseDelay: TEdit;
    lblAutoCloseUnit: TLabel;
    cxAutoCloseShow: TCheckBox;
    JvFormStorage1: TJvFormStorage;
    JvAppRegistryStorage1: TJvAppRegistryStorage;
    procedure FormCreate(Sender: TObject);
    procedure imgWarningClick(Sender: TObject);
    procedure rbWarningClick(Sender: TObject);
    procedure imgErrorClick(Sender: TObject);
    procedure rbErrorClick(Sender: TObject);
    procedure imgInformationClick(Sender: TObject);
    procedure rbInformationClick(Sender: TObject);
    procedure imgConfirmationClick(Sender: TObject);
    procedure rbConfirmationClick(Sender: TObject);
    procedure imgCustomClick(Sender: TObject);
    procedure rbCustomClick(Sender: TObject);
    procedure btnSelectIconClick(Sender: TObject);
    procedure edCustomTitleChange(Sender: TObject);
    procedure rbStdButtonsClick(Sender: TObject);
    procedure rbCustomButtonsClick(Sender: TObject);
    procedure clbStdButtonsClickCheck(Sender: TObject);
    procedure mmCustomButtonsChange(Sender: TObject);
    procedure cbDefaultButtonChange(Sender: TObject);
    procedure cbCancelButtonChange(Sender: TObject);
    procedure cbHelpButtonChange(Sender: TObject);
    procedure rbCenterScreenClick(Sender: TObject);
    procedure rbActiveFormCenterClick(Sender: TObject);
    procedure rbMainFormCenterClick(Sender: TObject);
    procedure edHelpCtxChange(Sender: TObject);
    procedure cxIsDSADialogClick(Sender: TObject);
    procedure edDSA_IDChange(Sender: TObject);
    procedure mmMessageChange(Sender: TObject);
    procedure cxCustomTitleClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cxAutoCloseClick(Sender: TObject);
    procedure edAutoCloseDelayChange(Sender: TObject);
    procedure cxAutoCloseShowClick(Sender: TObject);
  private
    { Private declarations }
    procedure DlgSettingChanged;
    procedure HandleDSAMessageDlg;
    procedure HandleDSAMessageDlgEx;
    procedure HandleMessageDlg;
    procedure HandleMessageDlgEx;
    function GenerateSource: string;
    function GetCenter: TDlgCenterKind;
    function GetTimeout: Integer;
    function GetTimeoutValue: string;
    function GetCustomButtonNames: TDynStringArray;
    function GetCustomButtonResults: TDynIntegerArray;
    function GetCustomCancelButton: Integer;
    function GetCustomDefaultButton: Integer;
    function GetCustomHelpButton: Integer;
    function GetDlgType: TMsgDlgType;
    function GetStdButtons: TMsgDlgButtons;
    function GetStdCancelButton: TMsgDlgBtn;
    function GetStdDefaultButton: TMsgDlgBtn;
    function GetStdHelpButton: TMsgDlgBtn;
    procedure InitDefaultLists;
    procedure TempDSARegCreate;
    procedure TempDSARegDestroy;
  public
    { Public declarations }
    TempDSA_ID: Integer;
    TmpStorage: TDSAQueueStorage;
    PicType: Integer;
    PicName: string;
    PicID: PChar;
  end;

var
  frmMessageDlgEditor: TfrmMessageDlgEditor;

implementation

{$R *.DFM}

uses
  TypInfo,
  MessageDlgEditorSelectIcon, JvDynControlEngineJVCL;

function MsgToSource(const S: TStrings): string;
begin
  Result := S.Text;
  Result := QuotedStr(Result);
  Result := StringReplace(StringReplace(Result, #13#10, ''' + #13#10 + ''', [rfReplaceAll]), '#13#10 + '''' + ', '#13#10', [rfReplaceAll]);
end;

function StdBtnsToSource(ChkLst: TCheckListBox): string;
var
  Prefix: string;
  I: Integer;
begin
  Result := '[';
  Prefix := '';
  for I := 0 to ChkLst.Items.Count - 1 do
  begin
    if ChkLst.Checked[I] then
    begin
      Result := Result + Prefix + ChkLst.Items[I];
      Prefix := ', ';
    end;
  end;
  Result := Result + ']';
end;

function CustomButtonsToSource(SL: TStrings): string;
var
  Prefix: string;
  Values: string;
  I: Integer;
  TmpName: string;
begin
  Result := '[';
  Values := '[';
  for I := 0 to SL.Count - 1 do
  begin
    if (Trim(SL[I]) <> '') then
    begin
      TmpName := SL.Names[I];
      if (Trim(SL.Values[TmpName]) <> '') and (TmpName = Trim(TmpName)) then
      begin
        Result := Result + Prefix + QuotedStr(TmpName);
        Values := Values + Prefix + Trim(SL.Values[TmpName]);
        Prefix := ', ';
      end;
    end;
  end;
  Result := Result + '], ' + Values + ']';
end;

procedure TfrmMessageDlgEditor.DlgSettingChanged;
begin
  // Setup Default lists
  InitDefaultLists;

  // Assert settings
  cxCustomTitle.Checked := cxCustomTitle.Checked or (
    (rbCustom.Checked) and (PicType <> 0)
  );
  edAutoCloseDelay.Enabled := cxAutoClose.Checked;
  cxAutoCloseShow.Enabled := cxAutoClose.Checked;

  // Enable/disable controls
  btnSelectIcon.Enabled := rbCustom.Checked;
  edCustomTitle.Enabled := cxCustomTitle.Checked;
  edDSA_ID.Enabled := cxIsDSADialog.Checked;
  clbStdButtons.Enabled := rbStdButtons.Checked;
  mmCustomButtons.Enabled := rbCustomButtons.Checked;

  // Generate source
  mmSource.Lines.Text := GenerateSource;
end;

procedure TfrmMessageDlgEditor.HandleDSAMessageDlg;
begin
  TempDSARegCreate;
  try
    if cxCustomTitle.Checked then
    begin
      if rbCustom.Checked and (PicType <> 0) then { custom picture }
        DSAMessageDlg(TempDSA_ID, edCustomTitle.Text, mmMessage.Lines.Text,
          imgCustom.Picture.Graphic, GetStdButtons, 0, GetCenter, GetTimeout, GetStdDefaultButton,
            GetStdCancelButton, GetStdHelpButton)
      else
        DSAMessageDlg(TempDSA_ID, edCustomTitle.Text, mmMessage.Lines.Text, GetDlgType,
          GetStdButtons, 0, GetCenter, GetTimeout, GetStdDefaultButton, GetStdCancelButton,
          GetStdHelpButton);
    end
    else { if not cxCustomTitle.Checked then }
      DSAMessageDlg(TempDSA_ID, mmMessage.Lines.Text, GetDlgType, GetStdButtons, 0, GetCenter,
        GetTimeout, GetStdDefaultButton, GetStdCancelButton, GetStdHelpButton);
  finally
    TempDSARegDestroy;
  end;
end;

procedure TfrmMessageDlgEditor.HandleDSAMessageDlgEx;
begin
  TempDSARegCreate;
  try
    if cxCustomTitle.Checked then
    begin
      if rbCustom.Checked and (PicType <> 0) then { custom picture }
        DSAMessageDlgEx(TempDSA_ID, edCustomTitle.Text, mmMessage.Lines.Text,
          imgCustom.Picture.Graphic, GetCustomButtonNames, GetCustomButtonResults, 0, GetCenter,
          GetTimeout, GetCustomDefaultButton, GetCustomCancelButton, GetCustomHelpButton)
      else
        DSAMessageDlgEx(TempDSA_ID, edCustomTitle.Text, mmMessage.Lines.Text, GetDlgType,
          GetCustomButtonNames, GetCustomButtonResults, 0, GetCenter, GetTimeout,
          GetCustomDefaultButton, GetCustomCancelButton, GetCustomHelpButton);
    end
    else { if not cxCustomTitle.Checked then }
      DSAMessageDlgEx(TempDSA_ID, mmMessage.Lines.Text, GetDlgType, GetCustomButtonNames,
        GetCustomButtonResults, 0, GetCenter, GetTimeout, GetCustomDefaultButton,
        GetCustomCancelButton, GetCustomHelpButton);
  finally
    TempDSARegDestroy;
  end;
end;

procedure TfrmMessageDlgEditor.HandleMessageDlg;
begin
  if cxCustomTitle.Checked then
  begin
    if rbCustom.Checked and (PicType <> 0) then { custom picture }
      MessageDlg(edCustomTitle.Text, mmMessage.Lines.Text, imgCustom.Picture.Graphic, GetStdButtons,
        0, GetCenter, GetTimeout, GetStdDefaultButton, GetStdCancelButton, GetStdHelpButton)
    else
      MessageDlg(edCustomTitle.Text, mmMessage.Lines.Text, GetDlgType, GetStdButtons, 0, GetCenter,
        GetTimeout, GetStdDefaultButton, GetStdCancelButton, GetStdHelpButton);
  end
  else { if not cxCustomTitle.Checked then }
    MessageDlg(mmMessage.Lines.Text, GetDlgType, GetStdButtons, 0, GetCenter, GetTimeout,
      GetStdDefaultButton, GetStdCancelButton, GetStdHelpButton);
end;

procedure TfrmMessageDlgEditor.HandleMessageDlgEx;
begin
  if cxCustomTitle.Checked then
  begin
    if rbCustom.Checked and (PicType <> 0) then { custom picture }
      MessageDlgEx(edCustomTitle.Text, mmMessage.Lines.Text, imgCustom.Picture.Graphic,
        GetCustomButtonNames, GetCustomButtonResults, 0, GetCenter, GetTimeout,
        GetCustomDefaultButton, GetCustomCancelButton, GetCustomHelpButton)
    else
      MessageDlgEx(edCustomTitle.Text, mmMessage.Lines.Text, GetDlgType, GetCustomButtonNames,
        GetCustomButtonResults, 0, GetCenter, GetTimeout, GetCustomDefaultButton,
        GetCustomCancelButton, GetCustomHelpButton);
  end
  else { if not cxCustomTitle.Checked then }
    MessageDlgEx(mmMessage.Lines.Text, GetDlgType, GetCustomButtonNames, GetCustomButtonResults, 0,
      GetCenter, GetTimeout, GetCustomDefaultButton, GetCustomCancelButton, GetCustomHelpButton);
end;

function TfrmMessageDlgEditor.GenerateSource: string;
  function HasGraphic: Boolean;
  begin
    Result := (
      rbCustom.Checked and (PicType <> 0)
    );
  end;

  function GetPicLoadSource: string;
  begin
    case PicType of
      1 .. 4:
        Result := 'Pic.Icon.Handle := LoadIcon(0, ' + PicName + ');';
      5:
        Result := 'Pic.Icon.Handle := LoadIcon(0, ''' + PicName + ''');';
      6:
        Result := 'Pic.Bitmap.Handle := LoadBitmap(0, ''' + PicName + ''');';
      7:
        Result := 'Pic.LoadFromFile(''' + PicName + ''');';
      else
        Result := '';
    end;
    if Result <> '' then
      Result := Result + #13#10 + '  ';
  end;

  function RequireCenter: Boolean;
  begin
    Result := not rbCenterScreen.Checked or cxAutoClose.Checked or (
      // for non-extended dialogs: button defaults are not met
      rbStdButtons.Checked and (
        (cbDefaultButton.ItemIndex <> 0) or
        (cbCancelButton.ItemIndex <> 0) or (
          (cbHelpButton.ItemIndex <> 0) and
          (cbHelpButton.Text <> 'mbHelp')
        )
      )
    ) or (
      // for extended dialogs: button defaults are not met
      rbCustomButtons.Checked and (
        (cbDefaultButton.ItemIndex <> 1) or
        (cbCancelButton.ItemIndex <> 2) or
        (cbHelpButton.ItemIndex <> 0)
      )
    );
  end;

  function RequireAutoClose: Boolean;
  begin
    Result := cxAutoClose.Checked or (
      // for non-extended dialogs: button defaults are not met
      rbStdButtons.Checked and (
        (cbDefaultButton.ItemIndex <> 0) or
        (cbCancelButton.ItemIndex <> 0) or (
          (cbHelpButton.ItemIndex <> 0) and
          (cbHelpButton.Text <> 'mbHelp')
        )
      )
    ) or (
      // for extended dialogs: button defaults are not met
      rbCustomButtons.Checked and (
        (cbDefaultButton.ItemIndex <> 1) or
        (cbCancelButton.ItemIndex <> 2) or
        (cbHelpButton.ItemIndex <> 0)
      )
    );
  end;

  function GetCenterName: string;
  begin
    if rbCenterScreen.Checked then
      Result := 'dckScreen'
    else if rbMainFormCenter.Checked then
      Result := 'dckMainForm'
    else if rbActiveFormCenter.Checked then
      Result := 'dckActiveForm'
    else
      Result := '??? error ???';
  end;

  function RequireDefaultButton: Boolean;
  begin
    Result := (rbStdButtons.Checked and (
      // Non extended dialogs: Either button has a non default value
      (cbDefaultButton.ItemIndex <> 0) or
      (cbCancelButton.ItemIndex <> 0) or (
        (cbHelpButton.ItemIndex <> 0) and
        (cbHelpButton.Text <> 'mbHelp')
      )
    )) or (rbCustomButtons.Checked and (
      // Extended dialogs: Either button has a non default value
      (cbDefaultButton.ItemIndex <> 1) or
      (cbCancelButton.ItemIndex <> 2) or
      (cbHelpButton.ItemIndex <> 0)
    ));
  end;

  function DefaultButtonName: string;
  begin
    if rbStdButtons.Checked then
    begin
      if cbDefaultButton.ItemIndex = 0 then
        Result := 'mbDefault'
      else if cbDefaultButton.ItemIndex = 1 then
        Result := 'mbNone'
      else
        Result := cbDefaultButton.Text;
    end
    else if rbCustomButtons.Checked then
    begin
      if cbDefaultButton.ItemIndex = 0 then
        Result := 'mbNone'
      else
        Result := IntToStr(cbDefaultButton.ItemIndex - 1);
    end;
  end;

  function RequireCancelButton: Boolean;
  begin
    Result := (rbStdButtons.Checked and (
      // Non extended dialogs: Either cancel or help button has a non default value
      (cbCancelButton.ItemIndex <> 0) or (
        (cbHelpButton.ItemIndex <> 0) and
        (cbHelpButton.Text <> 'mbHelp')
      )
    )) or (rbCustomButtons.Checked and (
      // Extended dialogs: Either cancel or help button has a non default value
      (cbCancelButton.ItemIndex <> 2) or
      (cbHelpButton.ItemIndex <> 0)
    ));
  end;

  function CancelButtonName: string;
  begin
    if rbStdButtons.Checked then
    begin
      if cbCancelButton.ItemIndex = 0 then
        Result := 'mbDefault'
      else if cbCancelButton.ItemIndex = 1 then
        Result := 'mbNone'
      else
        Result := cbCancelButton.Text;
    end
    else if rbCustomButtons.Checked then
    begin
      if cbCancelButton.ItemIndex = 0 then
        Result := 'mbNone'
      else
        Result := IntToStr(cbCancelButton.ItemIndex - 1);
    end;
  end;

  function RequireHelpButton: Boolean;
  begin
    Result := (rbStdButtons.Checked and (
      // Non extended dialogs: Help button has a non default value
      (cbHelpButton.ItemIndex <> 0) and
      (cbHelpButton.Text <> 'mbHelp')
    )) or (rbCustomButtons.Checked and (
      // Extended dialogs: Help button has a non default value
      (cbHelpButton.ItemIndex <> 0)
    ));
  end;

  function HelpButtonName: string;
  begin
    if rbStdButtons.Checked then
    begin
      if cbHelpButton.ItemIndex = 0 then
        Result := 'mbDefault'
      else if cbHelpButton.ItemIndex = 1 then
        Result := 'mbNone'
      else
        Result := cbHelpButton.Text;
    end
    else if rbCustomButtons.Checked then
    begin
      if cbHelpButton.ItemIndex = 0 then
        Result := 'mbNone'
      else
        Result := IntToStr(cbHelpButton.ItemIndex - 1);
    end;
  end;

begin
  if HasGraphic then
    Result := 'Pic := TPicture.Create;' + #13#10 + 'try'+ #13#10 + '  ' + GetPicLoadSource
  else
    Result := '';
  if cxIsDSADialog.Checked then
    Result := Result + 'DSAMessageDlg'
  else
    Result := Result + 'MessageDlg';

  if rbCustomButtons.Checked then
    Result := Result + 'Ex';

  Result := Result + '(';

  if cxIsDSADialog.Checked then
    Result := Result + Trim(edDSA_ID.Text) + ', ';

  if cxCustomTitle.Checked then
    Result := Result + QuotedStr(edCustomTitle.Text) + ', ';

  Result := Result + MsgToSource(mmMessage.Lines) + ', ';

  if rbWarning.Checked then
    Result := Result + 'mtWarning, ';
  if rbError.Checked then
    Result := Result + 'mtError, ';
  if rbInformation.Checked then
    Result := Result + 'mtInformation, ';
  if rbConfirmation.Checked then
    Result := Result + 'mtConfirmation, ';
  if rbCustom.Checked then
  begin
    if HasGraphic then
      Result := Result + 'Pic, '
    else
      Result := Result + 'mtCustom, ';
  end;

  if not rbCustomButtons.Checked then
    Result := Result + StdBtnsToSource(clbStdButtons) + ', '
  else
    Result := Result + CustomButtonsToSource(mmCustomButtons.Lines) + ', ';

  Result := Result + edHelpCtx.Text;

  if RequireCenter then
    Result := Result + ', ' + GetCenterName;

  if RequireAutoClose then
    Result := Result + ', ' + GetTimeoutValue;

  if RequireDefaultButton then
    Result := Result + ', ' + DefaultButtonName;

  if RequireCancelButton then
    Result := Result + ', ' + CancelButtonName;

  if RequireHelpButton then
    Result := Result + ', ' + HelpButtonName;

  Result := Result + ')';

  if HasGraphic then
    Result := Result + ';' + #13#10 + 'finally'+ #13#10 + '  Pict.Free;';
end;

function TfrmMessageDlgEditor.GetCenter: TDlgCenterKind;
begin
  if rbCenterScreen.Checked then
    Result := dckScreen
  else if rbMainFormCenter.Checked then
    Result := dckMainForm
  else if rbActiveFormCenter.Checked then
    Result := dckActiveForm
  else
    Result := dckScreen;
end;

function TfrmMessageDlgEditor.GetTimeout: Integer;
begin
  if cxAutoClose.Checked then
  begin
    Result := StrToInt(edAutoCloseDelay.Text);
    if not cxAutoCloseShow.Checked then
      Result := -Result;
  end
  else
    Result := 0;
end;

function TfrmMessageDlgEditor.GetTimeoutValue: string;
begin
  Result := IntToStr(GetTimeout);
end;

function TfrmMessageDlgEditor.GetCustomButtonNames: TDynStringArray;
var
  J: Integer;
  I: Integer;
  S: string;
begin
  SetLength(Result, mmCustomButtons.Lines.Count);
  J := 0;
  for I := 0 to mmCustomButtons.Lines.Count - 1 do
  begin
    if (Trim(mmCustomButtons.Lines[I]) <> '') then
    begin
      S := mmCustomButtons.Lines.Names[I];
      if (Trim(mmCustomButtons.Lines.Values[S]) <> '') and (S = Trim(S)) then
      begin
        Result[J] := S;
        Inc(J);
      end;
    end;
  end;
  SetLength(Result, J);
end;

function TfrmMessageDlgEditor.GetCustomButtonResults: TDynIntegerArray;
var
  Names: TDynStringArray;
  I: Integer;
begin
  Names := GetCustomButtonNames;
  SetLength(Result, Length(Names));
  for I := Low(Result) to High(Result) do
    Result[I] := I + 1;
end;

function TfrmMessageDlgEditor.GetCustomCancelButton: Integer;
begin
  Result := cbCancelButton.ItemIndex - 1;
end;

function TfrmMessageDlgEditor.GetCustomDefaultButton: Integer;
begin
  Result := cbDefaultButton.ItemIndex - 1;
end;

function TfrmMessageDlgEditor.GetCustomHelpButton: Integer;
begin
  Result := cbHelpButton.ItemIndex - 1;
end;

function TfrmMessageDlgEditor.GetDlgType: TMsgDlgType;
begin
  if rbWarning.Checked then
    Result := mtWarning
  else if rbError.Checked then
    Result := mtError
  else if rbInformation.Checked then
    Result := mtInformation
  else if rbConfirmation.Checked then
    Result := mtConfirmation
  else if rbCustom.Checked then
    Result := mtCustom
  else
    Result := mtCustom;
end;

function TfrmMessageDlgEditor.GetStdButtons: TMsgDlgButtons;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to clbStdButtons.Items.Count - 1 do
  begin
    if clbStdButtons.Checked[I] then
      Include(Result, TMsgDlgBtn(GetEnumValue(TypeInfo(TMsgDlgBtn), clbStdButtons.Items[I])));
  end;
end;

function TfrmMessageDlgEditor.GetStdCancelButton: TMsgDlgBtn;
begin
  if cbCancelButton.ItemIndex < 2 then
    Result := TMsgDlgBtn(cbCancelButton.ItemIndex - 2)
  else
    Result := TMsgDlgBtn(GetEnumValue(TypeInfo(TMsgDlgBtn), cbCancelButton.Text));
end;

function TfrmMessageDlgEditor.GetStdDefaultButton: TMsgDlgBtn;
begin
  if cbDefaultButton.ItemIndex < 2 then
    Result := TMsgDlgBtn(cbDefaultButton.ItemIndex - 2)
  else
    Result := TMsgDlgBtn(GetEnumValue(TypeInfo(TMsgDlgBtn), cbDefaultButton.Text));
end;

function TfrmMessageDlgEditor.GetStdHelpButton: TMsgDlgBtn;
begin
  if cbHelpButton.ItemIndex < 2 then
    Result := TMsgDlgBtn(cbHelpButton.ItemIndex - 2)
  else
    Result := TMsgDlgBtn(GetEnumValue(TypeInfo(TMsgDlgBtn), cbHelpButton.Text));
end;

procedure TfrmMessageDlgEditor.InitDefaultLists;
var
  SL: TStrings;
  I: Integer;
  CurDefault: string;
  CurCancel: string;
  CurHelp: string;
begin
  SL := TStringList.Create;
  try
    if rbStdButtons.Checked then
    begin
      SL.Add('[VCL default]');
      SL.Add('[none]');
      for I := 0 to clbStdButtons.Items.Count - 1 do
      begin
        if clbStdButtons.Checked[I] then
          SL.Add(clbStdButtons.Items[I]);
      end;
    end
    else
    begin
      SL.Add('[none]');
      for I := 0 to mmCustomButtons.Lines.Count - 1 do
      begin
        if (Trim(mmCustomButtons.Lines[I]) <> '') then
        begin
          CurDefault := mmCustomButtons.Lines.Names[I];
          if (Trim(mmCustomButtons.Lines.Values[CurDefault]) <> '') and (CurDefault = Trim(CurDefault)) then
            SL.Add(Trim(CurDefault));
        end;
      end;
    end;
    CurDefault := cbDefaultButton.Text;
    CurCancel := cbCancelButton.Text;
    CurHelp := cbHelpButton.Text;

    cbDefaultButton.Items.Assign(SL);
    cbCancelButton.Items.Assign(SL);
    cbHelpButton.Items.Assign(SL);

    I := SL.IndexOf(CurDefault);
    if I < 0 then
      I := SL.IndexOf('[VCL default]');
    if I < 0 then
      I := SL.IndexOf('[none]');
    cbDefaultButton.ItemIndex := I;

    I := SL.IndexOf(CurCancel);
    if I < 0 then
      I := SL.IndexOf('[VCL default]');
    if I < 0 then
      I := SL.IndexOf('[none]');
    cbCancelButton.ItemIndex := I;

    I := SL.IndexOf(CurHelp);
    if I < 0 then
      I := SL.IndexOf('[VCL default]');
    if I < 0 then
      I := SL.IndexOf('[none]');
    cbHelpButton.ItemIndex := I;
  finally
    SL.Free;
  end;
end;

procedure TfrmMessageDlgEditor.TempDSARegCreate;
begin
  TmpStorage := TDSAQueueStorage.Create;
  TempDSA_ID := 1;
  try
    TmpStorage.CheckMarkTextSuffix := '';
    RegisterDSA(TempDSA_ID, 'Temp' + IntToStr(TempDSA_ID), 'Temporary DSA dialog', TmpStorage,
      ctkShow);
  except
    TempDSA_ID := 0;
    FreeAndNil(TmpStorage);
    raise;
  end;
end;

procedure TfrmMessageDlgEditor.TempDSARegDestroy;
begin
  if TempDSA_ID <> 0 then
  begin
    UnregisterDSA(TempDSA_ID);
    FreeAndNil(TmpStorage);
    TempDSA_ID := 0;
  end;
end;

procedure TfrmMessageDlgEditor.FormCreate(Sender: TObject);
begin
  imgWarning.Picture.Icon.Handle := LoadIcon(0, IDI_EXCLAMATION);
  imgError.Picture.Icon.Handle := LoadIcon(0, IDI_HAND);
  imgInformation.Picture.Icon.Handle := LoadIcon(0, IDI_ASTERISK);
  imgConfirmation.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
  DlgSettingChanged;
  frmMessageDlgEditor := self; // is needed when loading as part of the MegaDemo
end;

procedure TfrmMessageDlgEditor.imgWarningClick(Sender: TObject);
begin
  rbWarning.Checked := True;
end;

procedure TfrmMessageDlgEditor.rbWarningClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.imgErrorClick(Sender: TObject);
begin
  rbError.Checked := True;
end;

procedure TfrmMessageDlgEditor.rbErrorClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.imgInformationClick(Sender: TObject);
begin
  rbInformation.Checked := True;
end;

procedure TfrmMessageDlgEditor.rbInformationClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.imgConfirmationClick(Sender: TObject);
begin
  rbConfirmation.Checked := True;
end;

procedure TfrmMessageDlgEditor.rbConfirmationClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.imgCustomClick(Sender: TObject);
begin
  rbCustom.Checked := True;
end;

procedure TfrmMessageDlgEditor.rbCustomClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.btnSelectIconClick(Sender: TObject);
var
  Pict: TPicture;
begin
  if DoSelectPicture then
  begin
    Pict := TPicture.Create;
    try
      Pict.Assign(imgCustom.Picture);
      case PicType of
        0:
          imgCustom.Picture.Graphic := nil;
        1 .. 4:
          imgCustom.Picture.Icon.Handle := LoadIcon(0, PicID);
        5:
          try
            imgCustom.Picture.Icon.Handle := LoadIcon(0, PChar(PicName));
          except
            imgCustom.Picture.Graphic := nil;
          end;
        6:
          try
            imgCustom.Picture.Bitmap.Handle := LoadBitmap(0, PChar(PicName));
          except
            imgCustom.Picture.Graphic := nil;
          end;
        7:
          imgCustom.Picture.LoadFromFile(PicName);
      end;
    finally
      Pict.Free;
    end;
    DlgSettingChanged;
  end;
end;

procedure TfrmMessageDlgEditor.edCustomTitleChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.rbStdButtonsClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.rbCustomButtonsClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.clbStdButtonsClickCheck(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.mmCustomButtonsChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.cbDefaultButtonChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.cbCancelButtonChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.cbHelpButtonChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.rbCenterScreenClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.rbActiveFormCenterClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.rbMainFormCenterClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.edHelpCtxChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.cxIsDSADialogClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.edDSA_IDChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.mmMessageChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.cxCustomTitleClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.btnTestClick(Sender: TObject);
  function IsMessageDlg: Boolean;
  begin
    Result := not cxIsDSADialog.Checked and not rbCustomButtons.Checked;
  end;

  function IsMessageDlgEx: Boolean;
  begin
    Result := not cxIsDSADialog.Checked and rbCustomButtons.Checked;
  end;

  function IsDSAMessageDlg: Boolean;
  begin
    Result := cxIsDSADialog.Checked and not rbCustomButtons.Checked;
  end;

  function IsDSAMessageDlgEx: Boolean;
  begin
    Result := cxIsDSADialog.Checked and rbCustomButtons.Checked;
  end;

begin
  if IsMessageDlg then
    HandleMessageDlg
  else if IsMessageDlgEx then
    HandleMessageDlgEx
  else if IsDSAMessageDlg then
    HandleDSAMessageDlg
  else if IsDSAMessageDlgEx then
    HandleDSAMessageDlgEx;
end;

procedure TfrmMessageDlgEditor.btnCloseClick(Sender: TObject);
begin
  Close
end;

procedure TfrmMessageDlgEditor.cxAutoCloseClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.edAutoCloseDelayChange(Sender: TObject);
begin
  DlgSettingChanged;
end;

procedure TfrmMessageDlgEditor.cxAutoCloseShowClick(Sender: TObject);
begin
  DlgSettingChanged;
end;

end.