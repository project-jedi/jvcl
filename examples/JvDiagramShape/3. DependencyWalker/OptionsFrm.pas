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

unit OptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvCombobox, JvColorCombo, ComCtrls, ActnList, ExtCtrls,
  JvBaseDlg, JvBrowseFolder, PersistForm, PersistSettings, Menus,
  JvComponent, JvExStdCtrls;

type
  // a TEdit that doesn't allow pasting of non-numeric text if Numeric is true
  TEdit = class(StdCtrls.TEdit)
  private
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    function GetNumeric:boolean;
    procedure SetNumeric(Value:boolean);
  published
    property Numeric:boolean read GetNumeric write SetNumeric;
  end;

  TfrmOptions = class(TfrmPersistable)
    btnOK: TButton;
    btnCancel: TButton;
    pcOptions: TPageControl;
    tabGeneral: TTabSheet;
    tabPaths: TTabSheet;
    lvPaths: TListView;
    edLibPath: TEdit;
    btnPathBrowse: TButton;
    btnReplace: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    alOptions: TActionList;
    Label9: TLabel;
    JvBrowseFolder1: TJvBrowseForFolderDialog;
    acReplace: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acBrowse: TAction;
    popPaths: TPopupMenu;
    acDelInvalidPaths: TAction;
    acGetD5Path: TAction;
    acGetD6Path: TAction;
    acGetD7Path: TAction;
    acGetBCB5Path: TAction;
    acGetBCB6Path: TAction;
    Add1: TMenuItem;
    Replace1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    InsertLibraryPath1: TMenuItem;
    CBuilder51: TMenuItem;
    CBuilder61: TMenuItem;
    N2: TMenuItem;
    Delphi51: TMenuItem;
    Delphi61: TMenuItem;
    Delphi71: TMenuItem;
    DeleteInvalidPaths1: TMenuItem;
    gbConnectors: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    cbIntfColor: TJvColorComboBox;
    cbIntfSelColor: TJvColorComboBox;
    cbImplColor: TJvColorComboBox;
    cbImplSelColor: TJvColorComboBox;
    acSystemPath: TAction;
    N3: TMenuItem;
    SystemPath1: TMenuItem;
    acSelectAll: TAction;
    acInvertSelect: TAction;
    acUnselectAll: TAction;
    Select1: TMenuItem;
    SelectAll1: TMenuItem;
    UnselectAll1: TMenuItem;
    InvertSelection1: TMenuItem;
    N4: TMenuItem;
    gbShapes: TGroupBox;
    edShapeWidth: TEdit;
    edShapeHeight: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure acBrowseExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acReplaceExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure alOptionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acGetD5PathExecute(Sender: TObject);
    procedure acGetD6PathExecute(Sender: TObject);
    procedure acGetD7PathExecute(Sender: TObject);
    procedure acGetBCB5PathExecute(Sender: TObject);
    procedure acGetBCB6PathExecute(Sender: TObject);
    procedure acDelInvalidPathsExecute(Sender: TObject);
    procedure lvPathsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvPathsEnter(Sender: TObject);
    procedure tabPathsShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acSystemPathExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acInvertSelectExecute(Sender: TObject);
    procedure acUnselectAllExecute(Sender: TObject);
  private
    { Private declarations }
    { IPersistSettings }
    procedure ListViewAddPath(const S: string);
    procedure ListViewAddPaths(Version: integer; ForDelphi: boolean);
    procedure ListViewAddSystemPaths;
  protected
    procedure Load(Storage: TPersistStorage);override;
    procedure Save(Storage: TPersistStorage);override;
  public
    { Public declarations }
    class function Execute: boolean;
  end;


implementation
uses
{$IFNDEF COMPILER6_UP}
  FileCtrl,
{$ENDIF }
  DepWalkUtils, Registry;

{$R *.DFM}


{ TEdit }

function TEdit.GetNumeric: boolean;
begin
  HandleNeeded;
  if HandleAllocated then
    Result := GetWindowLong(Handle, GWL_STYLE) and ES_NUMBER = ES_NUMBER
  else
    Result := false;
end;

procedure TEdit.SetNumeric(Value: boolean);
begin
  HandleNeeded;
  if HandleAllocated then
  begin
    if Value then
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or ES_NUMBER)
    else
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not ES_NUMBER);
  end;
end;

procedure TEdit.WMPaste(var Msg: TMessage);
var
  S: string;
begin
  S := Text;
  inherited;
  if Numeric then
    try
      StrToInt(Text);
    except
      Text := S;
    end;
end;

{ TfrmOptions }

class function TfrmOptions.Execute: boolean;
var Storage: TPersistStorage;
begin
  with self.Create(Application) do
  try
    pcOptions.ActivePageIndex := 0;
    Storage := PersistSettings.GetStorage;
    try
      Load(Storage);
      Result := ShowModal = mrOK;
      if Result then
      begin
        Save(Storage);
        Storage.UpdateFile;
      end;
    finally
      Storage.Free;
    end;
  finally
    Free;
  end;
end;

procedure TfrmOptions.acBrowseExecute(Sender: TObject);
begin
  JvBrowseFolder1.Directory := edLibPath.Text;
  if JvBrowseFolder1.Execute then
    edLibPath.Text := JvBrowseFolder1.Directory;
end;

procedure TfrmOptions.acAddExecute(Sender: TObject);
begin
  with lvPaths.Items.Add do
  begin

    Caption := edLibPath.Text;
    MakeVisible(true);
    Selected := true;
    Focused := true;
  end;
end;

procedure TfrmOptions.acReplaceExecute(Sender: TObject);
begin
  with lvPaths.Selected do
    Caption := edLibPath.Text;
end;

procedure TfrmOptions.acDeleteExecute(Sender: TObject);
var i, j: integer;
begin
  j := lvPaths.Items.Count;
  for i := lvPaths.Items.Count - 1 downto 0 do
    if lvPaths.Items[i].Selected then
    begin
      lvPaths.Items[i].Delete;
      j := i;
    end;
  if (j >= 0) and (j < lvPaths.Items.Count) then
  begin
    lvPaths.Items[j].MakeVisible(true);
    lvPaths.Items[j].Selected := true;
    lvPaths.Items[j].Focused := true;
  end;
  if lvPaths.CanFocus then lvPaths.SetFocus;
end;

procedure TfrmOptions.Load(Storage: TPersistStorage);
var S: TStringlist; i: integer;
begin
  inherited;

  edShapeHeight.Text := IntToStr(Storage.ReadInteger('Options', 'ShapeHeight', 50));
  edShapeWidth.Text := IntToStr(Storage.ReadInteger('Options', 'ShapeWidth', 100));
  cbIntfColor.ColorValue := Storage.ReadInteger('Options', 'IntfColor', clBlack);
  cbIntfSelColor.ColorValue := Storage.ReadInteger('Options', 'IntfSelColor', clRed);
  cbImplColor.ColorValue := Storage.ReadInteger('Options', 'ImplColor', clBtnShadow);
  cbImplSelColor.ColorValue := Storage.ReadInteger('Options', 'ImplSelColor', clBlue);
  lvPaths.Items.Clear;
  S := TStringlist.Create;
  try
    Storage.ReadSection('Library Paths', S);
    for i := 0 to S.Count - 1 do
      ListViewAddPath(S[i]);
  finally
    S.Free;
  end;
end;

procedure TfrmOptions.Save(Storage: TPersistStorage);
var i: integer;
begin
  inherited;
  i := StrToIntDef(edShapeHeight.Text, 50);
  if i < 40 then i := 40;
  Storage.WriteInteger('Options', 'ShapeHeight', i);
  i := StrToIntDef(edShapeWidth.Text, 100);
  if i < 50 then i := 50;
  Storage.WriteInteger('Options', 'ShapeWidth', i);

  Storage.WriteInteger('Options', 'IntfColor', cbIntfColor.ColorValue);
  Storage.WriteInteger('Options', 'IntfSelColor', cbIntfSelColor.ColorValue);
  Storage.WriteInteger('Options', 'ImplColor', cbImplColor.ColorValue);
  Storage.WriteInteger('Options', 'ImplSelColor', cbImplSelColor.ColorValue);

  Storage.EraseSection('Library Paths');
  for i := 0 to lvPaths.Items.Count - 1 do
    Storage.WriteString('Library Paths', lvPaths.Items[i].Caption, '');
end;

procedure TfrmOptions.ListViewAddPath(const S: string);
begin
  if (S <> '') and (lvPaths.FindCaption(0, S, false, true, true) = nil) then
    lvPaths.Items.Add.Caption := S;
end;

procedure TfrmOptions.ListViewAddPaths(Version: integer; ForDelphi: boolean);
var S: TStringlist; i: integer;
begin
  S := TStringlist.Create;
  try
    GetPathList(Version, ForDelphi, S);
    for i := 0 to S.Count - 1 do
      ListViewAddPath(S[i]);
  finally
    S.Free;
  end;
end;

procedure TfrmOptions.alOptionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acAdd.Enabled := DirectoryExists(edLibPath.Text)
    and (lvPaths.FindCaption(0, edLibPath.Text, false, true, true) = nil);
  acDelete.Enabled := (lvPaths.Selected <> nil);
  acReplace.Enabled := acAdd.Enabled and acDelete.Enabled;
  acDelInvalidPaths.Enabled := lvPaths.Items.Count > 0;
end;

procedure TfrmOptions.acGetD5PathExecute(Sender: TObject);
begin
  ListViewAddPaths(5, true);
end;


procedure TfrmOptions.acGetD6PathExecute(Sender: TObject);
begin
  ListViewAddPaths(6, true);
end;

procedure TfrmOptions.acGetD7PathExecute(Sender: TObject);
begin
  ListViewAddPaths(7, true);
end;

procedure TfrmOptions.acGetBCB5PathExecute(Sender: TObject);
begin
  ListViewAddPaths(5, false);
end;

procedure TfrmOptions.acGetBCB6PathExecute(Sender: TObject);
begin
  ListViewAddPaths(6, false);
end;

procedure TfrmOptions.acDelInvalidPathsExecute(Sender: TObject);
var i: integer;
begin
  for i := lvPaths.Items.Count - 1 downto 0 do
    if not DirectoryExists(lvPaths.Items[i].Caption) then
      lvPaths.Items[i].Delete;
end;

procedure TfrmOptions.lvPathsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    edLibPath.Text := Item.Caption;
end;

procedure TfrmOptions.lvPathsEnter(Sender: TObject);
begin
  if (lvPaths.Selected = nil) and (lvPaths.Items.Count > 0) then
  begin
    lvPaths.Items[0].Selected := true;
    lvPaths.Items[0].Focused := true;
  end;
end;

procedure TfrmOptions.tabPathsShow(Sender: TObject);
begin
  if lvPaths.CanFocus then lvPaths.SetFocus;
end;


procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  if edShapeWidth <> nil then
    edShapeWidth.Numeric := true;
  if edShapeHeight <> nil then
    edShapeHeight.Numeric := true;
end;

procedure TfrmOptions.acSystemPathExecute(Sender: TObject);
begin
  ListViewAddSystemPaths;
end;

procedure TfrmOptions.ListViewAddSystemPaths;
var S: TStringlist; i: integer;
begin
  S := TStringlist.Create;
  try
    GetSystemPaths(S);
    for i := 0 to S.Count - 1 do
      ListViewAddPath(S[i]);
  finally
    S.Free;
  end;
end;

procedure TfrmOptions.acSelectAllExecute(Sender: TObject);
var i: integer;
begin
  for i := 0 to lvPaths.Items.Count - 1 do
    lvPaths.Items[i].Selected := true;
end;

procedure TfrmOptions.acInvertSelectExecute(Sender: TObject);
var i: integer;
begin
  for i := 0 to lvPaths.Items.Count - 1 do
    lvPaths.Items[i].Selected := not lvPaths.Items[i].Selected;
end;

procedure TfrmOptions.acUnselectAllExecute(Sender: TObject);
var i: integer;
begin
  for i := 0 to lvPaths.Items.Count - 1 do
    lvPaths.Items[i].Selected := false;
end;

end.

