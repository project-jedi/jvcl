{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

unit ChangeNotificationMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ComCtrls, ExtCtrls,
  JvChangeNotify, JvComponent;

type
  TChangeNotificationMainForm = class(TForm)
    JvChangeNotify1: TJvChangeNotify;
    ListBox2: TListBox;
    Label3: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    btnStart: TSpeedButton;
    Label2: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    Edit1: TEdit;
    udInterval: TUpDown;
    btnClear: TButton;
    Label4: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure JvChangeNotify1ChangeNotify(Sender: TObject; Dir: string; Actions: TJvChangeActions);
    procedure EditItem(li: TListItem);
    procedure DeleteItem(li: TListItem);
    procedure ListView1DblClick(Sender: TObject);
  private
    procedure ResetCaptions(Invert: boolean);
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  end;

var
  ChangeNotificationMainForm: TChangeNotificationMainForm;

implementation

uses ChangeNotificationDirDlgU;

{$R *.dfm}

procedure TChangeNotificationMainForm.ResetCaptions(Invert: boolean);
const
  aCap: array[boolean] of string = ('TJvChangeNotification demo', 'Checking...');
begin
  if Invert then
    Caption := aCap[not JvChangeNotify1.Active]
  else
    Caption := aCap[JvChangeNotify1.Active];
  Application.Title := Caption;
end;

procedure TChangeNotificationMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  JvChangeNotify1.Active := false;
end;

procedure TChangeNotificationMainForm.btnDeleteClick(Sender: TObject);
begin
  DeleteItem(ListView1.Selected);
end;

function OptionsToStr(Options: TJvChangeActions): string;
begin
  Result := '';
  if caChangeFileName in Options then
    Result := Result + 'Rename Files,';
  if caChangeDirName in Options then
    Result := Result + 'Rename Folders,';
  if caChangeAttributes in Options then
    Result := Result + 'Change Attributes,';
  if caChangeSize in Options then
    Result := Result + 'Change Size,';
  if caChangeLastWrite in Options then
    Result := Result + 'Change Content,';
  if caChangeSecurity in Options then
    Result := Result + 'Change Security,';
  if Length(Result) > 0 then
  begin
    SetLength(Result, Length(Result) - 1);
    Result := '(' + Result + ')';
  end;
end;

procedure TChangeNotificationMainForm.btnAddClick(Sender: TObject);
begin
  EditItem(nil);
end;

procedure TChangeNotificationMainForm.btnStartClick(Sender: TObject);
var b: boolean;
begin
  if JvChangeNotify1.Notifications.Count = 0 then
  begin
    ShowMessage('No notifications to monitor!');
    btnStart.Down := false;
    Exit;
  end;

  b := btnStart.Down;
  btnAdd.Enabled := not b;
  btnDelete.Enabled := not b;
  ResetCaptions(true);
  { do this *after* setting buttons }
  JvChangeNotify1.Active := b;
end;

procedure TChangeNotificationMainForm.btnClearClick(Sender: TObject);
begin
  ListBox2.Clear;
  ResetCaptions(false);
end;

procedure TChangeNotificationMainForm.JvChangeNotify1ChangeNotify(Sender: TObject; Dir: string;
  Actions: TJvChangeActions);
begin
  Application.Title := Format('Change in %s (%s)', [Dir, ActionsToString(Actions)]);
  ListBox2.Items.Add(Application.Title);
  FlashWindow(ChangeNotificationMainForm.Handle, true);
  MessageBeep(DWORD(-1));
end;

procedure TChangeNotificationMainForm.WMGetMinMaxINfo(var Msg: TWMGetMinMaxInfo);
begin
  with Msg.MinMaxInfo^.ptMinTrackSize do
  begin
    X := 392;
    Y := 295;
  end;
  Msg.Result := 0;
end;

procedure TChangeNotificationMainForm.EditItem(li: TListItem);
var ADirectory: string;
  AOptions: TJvChangeActions;
  AIncludeSubDirs: boolean;
begin
  if (li = nil) or (li.Data = nil) then
  begin
    ADirectory := GetCurrentDir;
    AIncludeSubDirs := true;
    AOptions := [caChangeFileName, caChangeDirName];
  end
  else
    with TJvChangeItem(li.Data) do
    begin
      ADirectory := Directory;
      AIncludeSubDirs := IncludeSubTrees;
      AOptions := Actions;
    end;

  if TChangeNotificationDirDlg.Execute(ADirectory, AOptions, AIncludeSubDirs) then
  begin
    if li = nil then
    begin
      li := ListView1.Items.Add;
      li.Caption := ADirectory;
      if AIncludeSubDirs and (Win32Platform = VER_PLATFORM_WIN32_NT) then
        li.SubItems.Add('Yes')
      else
        li.SubItems.Add('No');
      li.SubItems.Add(OptionsToStr(AOptions));
    end
    else
    begin
      li.Caption := ADirectory;
      if AIncludeSubDirs and (Win32Platform = VER_PLATFORM_WIN32_NT) then
        li.SubItems[0] := 'Yes'
      else
        li.SubItems[0] := 'No';
      li.SubItems[1] := OptionsToStr(AOptions);
    end;
    if li.Data = nil then
      li.Data := JvChangeNotify1.Notifications.Add;
    with TJvChangeItem(li.Data) do
    begin
      IncludeSubTrees := AIncludeSubDirs and (Win32Platform = VER_PLATFORM_WIN32_NT);
      Directory := ADirectory;
      Actions := AOptions;
    end;
  end;
end;

procedure TChangeNotificationMainForm.DeleteItem(li: TListItem);
begin
  if li = nil then
    Exit;
  if li.Data <> nil then
    JvChangeNotify1.Notifications.Delete(li.Index);
  li.Delete;
end;

procedure TChangeNotificationMainForm.ListView1DblClick(Sender: TObject);
begin
  EditItem(ListView1.Selected);
end;

end.

