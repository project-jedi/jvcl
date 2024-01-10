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

unit DSADialogsMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, JvDSADialogs, Menus, StdCtrls;

type
  TDSADialogsMainForm = class(TForm)
    lvDSAInfo: TListView;
    pmDSAList: TPopupMenu;
    miExecuteDlg: TMenuItem;
    miBreak1: TMenuItem;
    miReset: TMenuItem;
    stbMain: TStatusBar;
    btnClose: TButton;
    procedure lvDSAInfoResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvDSAInfoChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure pmDSAListPopup(Sender: TObject);
    procedure miExecuteDlgClick(Sender: TObject);
    procedure miResetClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    procedure SetStatus(Value: string);
  public
    procedure DoDlg1;
    procedure DoDlg2;
    procedure DoDlg3;
    procedure DoDlg4;
    procedure DoDlg5;
    procedure DoDlg6;
    procedure DoDlg7;
    procedure DoDlg8;
    procedure DoDlg9;
    function DoDlg9Callback(const Position, Max: Integer): Integer;
    procedure DoDlg10;
    function DoDlg10Callback(const Position, Max: Integer): Integer;
    procedure DoDlg11;
    procedure DoExecute(const DlgID: Integer);
    procedure FillListView;
    procedure RefreshDSAState(DlgID: Integer = 0; UpdateStatus: Boolean = False; ResCode: Integer = Integer($80000000); ResStr: string = '');

    property Status: string write SetStatus;
  end;

var
  DSADialogsMainForm: TDSADialogsMainForm;

implementation

{$R *.DFM}

uses
  DSAExamplesCustom1, DSAExamplesCustom2, DSAExamplesProgressDlg,
//  JvDynControlEngineDevExpcx,
  JclBase;

const
  ctkMyMark: TDSACheckTextKind = 25;

procedure TDSADialogsMainForm.SetStatus(Value: string);
begin
  if Value = '' then
    Value := '(unknown)';
  stbMain.SimpleText := 'Status: ' + Value;
end;

procedure TDSADialogsMainForm.DoDlg1;
begin
  DSAShowMessage(1, 'Test ShowMessage with DSA support.');
  RefreshDSAState(1, True);
end;

procedure TDSADialogsMainForm.DoDlg2;
begin
  RefreshDSAState(2, True, DSAMessageDlg(
    2,
    'Simple warning box, standard title, VCL buttons and image.'#13#10+
    'The warning text is a little bit longer'#13#10+
    'and has a lot of lines'#13#10+
    '...'#13#10+
    '...'#13#10+
    '...'#13#10+
    '...'#13#10+
    '...'#13#10+
    'Stop',
    mtWarning,
    [mbOK],
    0)
  );
end;

procedure TDSADialogsMainForm.DoDlg3;
var
  Pic: TPicture;
  BtnCap: TDynStringArray;
begin
  Pic := TPicture.Create;
  try
    Pic.Icon.Assign(Application.Icon);
    SetLength(BtnCap, 1);
    BtnCap[0] := 'I don''t care!';
    RefreshDSAState(3, True, DSAMessageDlgEx(
      3,
      'Test warning',
      'Extended warning box, custom title, buttons and image.',
      Pic.Graphic,
      BtnCap,
      [50],
      0)
    );
  finally
    Pic.Free;
  end;
end;

procedure TDSADialogsMainForm.DoDlg4;
begin
  RefreshDSAState(4, True, DSAMessageDlg(
    4,
    'Simple confirmation box, standard title, VCL buttons and image.',
    mtConfirmation,
    [mbYes, mbNo],
    0)
  );
end;

procedure TDSADialogsMainForm.DoDlg5;
var
  Pic: TPicture;
  BtnCap: TDynStringArray;
begin
  Pic := TPicture.Create;
  try
    Pic.Icon.Assign(Application.Icon);
    SetLength(BtnCap, 2);
    BtnCap[0] := 'Sure';
    BtnCap[1] := 'No way';
    RefreshDSAState(5, True, DSAMessageDlgEx(
      5,
      'Test warning',
      'Extended confirmation box, custom title, buttons and image.',
      Pic.Graphic,
      BtnCap,
      [10, 20],
      0)
    );
  finally
    Pic.Free;
  end;
end;

procedure TDSADialogsMainForm.DoDlg6;
begin
  DSAShowMessage(6, 'Test ShowMessage with custom checkmark text.');
  RefreshDSAState(6, True);
end;

procedure TDSADialogsMainForm.DoDlg7;
begin
  DoCustomDSA1;
  RefreshDSAState(7, True);
end;

procedure TDSADialogsMainForm.DoDlg8;
var
  ResStr: string;
begin
  ResStr := DoCustomDSA2;
  RefreshDSAState(8, True, Integer($80000000), ResStr);
end;

procedure TDSADialogsMainForm.DoDlg9;
begin
  DoProgress(DoDlg9Callback);
  RefreshDSAState(9, True);
end;

function TDSADialogsMainForm.DoDlg9Callback(const Position, Max: Integer): Integer;
begin
  DSAShowMessageFmt(9, 'Processing %d of %d.', [Position, Max], dckActiveForm);
  Result := mrOK;
end;

procedure TDSADialogsMainForm.DoDlg10;
begin
  DoProgress(DoDlg10Callback);
  RefreshDSAState(10, True);
end;

function TDSADialogsMainForm.DoDlg10Callback(const Position, Max: Integer): Integer;
begin
  Result := DSAMessageDlg(10, Format('About to process %d of %d. Continue?', [Position, Max]),
    mtConfirmation, [mbYes, mbNo], 0, dckActiveForm);
end;

procedure TDSADialogsMainForm.DoDlg11;
begin
  DSAMessageDlg(11, 'Test MessageDlg with timeout.', mtWarning, [mbOK], 0,dckScreen, 5);
  RefreshDSAState(11, True);
end;

procedure TDSADialogsMainForm.DoExecute(const DlgID: Integer);
begin
  Status := 'Executing...';
  case DlgID of
    1: DoDlg1;
    2: DoDlg2;
    3: DoDlg3;
    4: DoDlg4;
    5: DoDlg5;
    6: DoDlg6;
    7: DoDlg7;
    8: DoDlg8;
    9: DoDlg9;
    10: DoDlg10;
    11: DoDlg11;
    else Status := 'Error: invalid dialog ID (' + IntToStr(DlgID) + ')';
  end;
end;

procedure TDSADialogsMainForm.FillListView;
var
  I: Integer;
  LI: TListItem;
begin
  lvDSAInfo.Items.BeginUpdate;
  try
    lvDSAInfo.Items.Clear;
    for I := 0 to DSACount - 1 do
    begin
      LI := lvDSAInfo.Items.Add;
      LI.Data := Pointer(DSAItem(I).ID);
      LI.Caption := '';
      LI.Checked := GetDSAState(DSAItem(I).ID);
      LI.SubItems.Add(IntToStr(DSAItem(I).ID));
      LI.SubItems.Add(DSAItem(I).Name);
      LI.SubItems.Add(DSAItem(I).Description);
    end;
  finally
    lvDSAInfo.Items.EndUpdate;
  end;
end;

procedure TDSADialogsMainForm.RefreshDSAState(DlgID: Integer; UpdateStatus: Boolean; ResCode: Integer; ResStr: string);
var
  I: Integer;
  S: string;
begin
  if DlgID <> 0 then
  begin
    I := lvDSAInfo.Items.Count - 1;
    while (I >= 0) and (Integer(lvDSAInfo.Items[I].Data) <> DlgID) do
      Dec(I);
    if I > -1 then
      lvDSAInfo.Items[I].Checked := GetDSAState(DlgID);
    if UpdateStatus then
    begin
      if GetDSAState(DlgID) then
        S := Format('Dialog %d has been suppressed.', [DlgID])
      else
        S := Format('Dialog %d has not been suppressed.', [DlgID]);
      if ResCode <> Integer($80000000) then
        S := S + ' Result value: ' + IntToStr(ResCode) + '.';
      if ResStr <> '' then
        S := S + ' Custom result: ' + ResStr + '.';
      Status := S;
    end;
  end
  else
  begin
    for I := lvDSAInfo.Items.Count - 1 downto 0 do
      lvDSAInfo.Items[I].Checked := GetDSAState(Integer(lvDSAInfo.Items[I].Data));
  end;
end;

procedure TDSADialogsMainForm.lvDSAInfoResize(Sender: TObject);
var
  AvailWidth: Integer;
  I: Integer;
begin
  AvailWidth := lvDSAInfo.ClientWidth;
  for I := 0 to lvDSAInfo.Columns.Count - 2 do
    Dec(AvailWidth, lvDSAInfo.Columns[I].Width);
  with lvDSAInfo.Columns[lvDSAInfo.Columns.Count - 1] do
  begin
    MinWidth := AvailWidth;
    MaxWidth := AvailWidth;
    Width := AVailWidth;
  end;
end;

procedure TDSADialogsMainForm.FormCreate(Sender: TObject);
begin
  // Alter registry location
  DSARegStore.Key := 'Software\JEDI-VCL\DSAExamples';

  // Add custom checkmark text
  RegisterDSACheckMarkText(ctkMyMark, 'Check to suppress this dialog');

  // Create DSAShowMessage example
  RegisterDSA(1, 'ShowMessage', 'ShowMessage example', DSARegStore, ctkShow);

  // Create DSAMessageDlg(Ex) examples
  RegisterDSA(2, 'Warning', 'MessageDlg warning', DSARegStore, ctkWarn);
  RegisterDSA(3, 'Warning2', 'MessageDlgEx warning', DSARegStore, ctkWarn);
  RegisterDSA(4, 'Confirm', 'MessageDlg confirmation', DSARegStore, ctkAsk);
  RegisterDSA(5, 'Confirm2', 'MessageDlgEx confirmation', DSARegStore, ctkAsk);
  RegisterDSA(6, 'CustomChk', 'Custom checkmark text', DSARegStore, ctkMyMark);

  // Create TJvDSADialog examples
  RegisterDSA(7, 'TJvDSADialog', 'Simple TJvDSADialog example', DSARegStore, ctkShow);
  RegisterDSA(8, 'TJvDSADialog2', 'TJvDSADialog example with custom data', DSARegStore, ctkAsk);

  // Create Queue tests
  RegisterDSA(9, 'Queue1', 'Simple queue ShowMessage', DSAQueueStore, ctkShow);
  RegisterDSA(10, 'Queue2', 'Queue MessageDlg confirmation', DSAQueueStore, ctkAsk);

  RegisterDSA(11, 'TimeOut', 'Simple TimeOut Example', DSARegStore, ctkWarn);

  // Fill the user interface.
  FillListview;

  // Unknown status
  Status := '';
end;

procedure TDSADialogsMainForm.FormDestroy(Sender: TObject);
begin
  { It would be better to unregister the dialogs here, but it's not necessary; the registration is
    removed on application shut down. }
end;

procedure TDSADialogsMainForm.FormShow(Sender: TObject);
begin
  lvDSAInfoResize(lvDSAInfo);
end;

procedure TDSADialogsMainForm.lvDSAInfoChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  DSAState: Boolean;
begin
  if (Item.Data <> nil) and (Change = ctState) then
  begin
    DSAState := GetDSAState(Integer(Item.Data));
    if (Item.Checked <> DSAState) and not Item.Checked then
      SetDSAState(Integer(Item.Data), Item.Checked);
    RefreshDSAState(Integer(Item.Data));
  end;
end;

procedure TDSADialogsMainForm.pmDSAListPopup(Sender: TObject);
begin
  miExecuteDlg.Enabled := lvDSAInfo.ItemFocused <> nil;
  miReset.Enabled := (lvDSAInfo.ItemFocused <> nil) and lvDSAInfo.ItemFocused.Checked;
end;

procedure TDSADialogsMainForm.miExecuteDlgClick(Sender: TObject);
begin
  if lvDSAInfo.ItemFocused <> nil then
    DoExecute(Integer(lvDSAInfo.ItemFocused.Data));
end;

procedure TDSADialogsMainForm.miResetClick(Sender: TObject);
begin
  if (lvDSAInfo.ItemFocused <> nil) and lvDSAInfo.ItemFocused.Checked then
    lvDSAInfo.ItemFocused.Checked := False;
end;

procedure TDSADialogsMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.