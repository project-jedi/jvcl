{$I JVCL.INC}
unit OptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvCombobox, JvColorCombo, ComCtrls, ActnList, ExtCtrls,
  JvBaseDlg, JvBrowseFolder, PersistSettings, Menus;

type
  // a TEdit that doesn't allow pasting of non-numeric text if ES_NUMBER is in GWL_STYLE
  TEdit = class(StdCtrls.TEdit)
  private
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
  end;

  TfrmOptions = class(TForm, IUnknown, IPersistSettings)
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
    JvBrowseFolder1: TJvBrowseFolder;
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

    procedure Load(Storage: TPersistSettings);
    procedure Save(Storage: TPersistSettings);
  public
    { Public declarations }
    class function Execute(Storage: TPersistSettings): boolean;
  end;


implementation
uses
{$IFNDEF COMPILER6_UP}
  FileCtrl,
{$ENDIF }
  Registry;

{$R *.DFM}

procedure strTokenize(const S: string; Delims: TSysCharSet; Results: TStrings);
var I, J: integer; tmp: string;
begin
  I := 1;
  J := 1;
  while true do
  begin
    while (I <= Length(S)) and not (S[i] in Delims) do
      Inc(I);
    tmp := trim(Copy(S, J, I - J));
    if tmp <> '' then
      Results.Add(tmp);
    if (I <= Length(S)) and (S[I] in Delims) then
      Inc(I); // skip the delimiter
    J := I;
    if i > Length(S) then
      Break;
  end;
end;

function GetBorlandLibPath(Version: integer; ForDelphi: boolean): string;
const
  cLibPath: array[boolean] of PChar = ('\Software\Borland\C++Builder\%d.0\Library',
    '\Software\Borland\Delphi\%d.0\Library');
var ALibPath: string;
begin
  ALibPath := Format(cLibPath[ForDelphi], [Version]);
  with TRegistry.Create do // defaults to HKCU - just what we want
  try
    if OpenKeyReadOnly(ALibPath) and ValueExists('Search Path') then
      Result := ReadString('Search Path')
    else
      Result := '';
  finally
    Free;
  end;
end;

function GetExpandedLibRoot(Version: integer; ForDelphi: boolean): string;
const
  cLibPath: array[boolean] of PChar = ('\Software\Borland\C++Builder\%d.0',
    '\Software\Borland\Delphi\%d.0');
var ALibPath: string;
begin
  ALibPath := Format(cLibPath[ForDelphi], [Version]);
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(ALibPath) and ValueExists('RootDir') then
      Result := ReadString('RootDir')
    else
      Result := '';
  finally
    Free;
  end;
end;

procedure GetPathList(Version: integer; ForDelphi: boolean; Strings: TStrings);
const
  cRootDirMacro: array[boolean] of PChar = ('$(BCB)', '$(DELPHI)');
var S, T: string;
begin
  S := GetBorlandLibPath(Version, ForDelphi);
  T := GetExpandedLibRoot(Version, ForDelphi);
  S := StringReplace(S, cRootDirMacro[ForDelphi], T, [rfReplaceAll, rfIgnoreCase]);
  StrTokenize(S, [';'], Strings);
end;

procedure GetSystemPaths(Strings:TStrings);
var lpBuffer:PChar;nSize:Cardinal;
begin
  nSize := GetEnvironmentVariable('PATH',nil,0);
  if nSize > 0 then
  begin
    lpBuffer := AllocMem(nSize);
    try
      GetEnvironmentVariable('PATH',lpBuffer,nSize);
      strTokenize(lpBuffer,[';'],Strings);
    finally
      FreeMem(lpBuffer);
    end;
  end;
end;
{ TEdit }

{$UNDEF RPLUS}
{$IFOPT R+}
  {$R-}
  {$DEFINE RPLUS}
{$ENDIF}
procedure TEdit.WMPaste(var Msg: TMessage);
var S: string; V, C: integer;
begin
  S := Text;
  inherited;
  if GetWindowLong(Handle, GWL_STYLE) and ES_NUMBER = ES_NUMBER then
  begin
    Val(Text, V, C);
    if C <> 0 then
      Text := S;
  end;
end;
{$IFDEF RPLUS}
  {$UNDEF RPLUS}
  {$R+}
{$ENDIF}

procedure MakeEditNumeric(EditHandle: integer);
begin
  SetWindowLong(EditHandle, GWL_STYLE, GetWindowLong(EditHandle, GWL_STYLE) or ES_NUMBER);
end;

{ TfrmOptions }

class function TfrmOptions.Execute(Storage: TPersistSettings): boolean;
begin
  with self.Create(Application) do
  try
    pcOptions.ActivePageIndex := 0;
    Load(Storage);
    Result := ShowModal = mrOK;
    if Result then Save(Storage);
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
var i: integer;
begin
  for i := lvPaths.Items.Count - 1 downto 0 do
    if lvPaths.Items[i].Selected then
      lvPaths.Items[i].Delete;
end;

procedure TfrmOptions.Load(Storage: TPersistSettings);
var S: TStringlist; i: integer;
begin
// TODO
  edShapeHeight.Text := Storage.ReadString('Options', 'ShapeHeight', '50');
  edShapeWidth.Text := Storage.ReadString('Options', 'ShapeWidth', '100');
  cbIntfColor.ColorValue := Storage.ReadInteger('Options', 'IntfColor', clBlack);
  cbIntfSelColor.ColorValue := Storage.ReadInteger('Options', 'IntfSelColor', clRed);
  cbImplColor.ColorValue := Storage.ReadInteger('Options', 'ImplColor', clBtnShadow);
  cbImplSelColor.ColorValue := Storage.ReadInteger('Options', 'ImplSelColor', clBlue);
  lvPaths.Items.Clear;
  S := TStringlist.Create;
  try
    Storage.ReadSection('Options.Paths', S);
    for i := 0 to S.Count - 1 do
      ListViewAddPath(S[i]);
  finally
    S.Free;
  end;
  Top := Storage.ReadInteger(ClassName, 'Top', (Screen.Height - ClientHeight) div 2);
  Left := Storage.ReadInteger(ClassName, 'Left', (Screen.Width - ClientWidth) div 2);
end;

procedure TfrmOptions.Save(Storage: TPersistSettings);
var i: integer;
begin
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

  Storage.EraseSection('Options.Paths');
  for i := 0 to lvPaths.Items.Count - 1 do
    Storage.WriteString('Options.Paths', lvPaths.Items[i].Caption, '');
  if not IsZoomed(Handle) and not IsIconic(Application.Handle) then
  begin
    Storage.WriteInteger(ClassName, 'Top', Top);
    Storage.WriteInteger(ClassName, 'Left', Left);
  end;
end;

procedure TfrmOptions.ListViewAddPath(const S: string);
begin
  if lvPaths.FindCaption(0, S, false, true, true) = nil then
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
    MakeEditNumeric(edShapeWidth.Handle);
  if edShapeHeight <> nil then
    MakeEditNumeric(edShapeHeight.Handle);
end;

procedure TfrmOptions.acSystemPathExecute(Sender: TObject);
begin
  ListViewAddSystemPaths;
end;

procedure TfrmOptions.ListViewAddSystemPaths;
var S:TStringlist;i:integer;
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
var i:integer;
begin
  for i := 0 to lvPaths.Items.Count - 1 do
    lvPaths.Items[i].Selected := true;
end;

procedure TfrmOptions.acInvertSelectExecute(Sender: TObject);
var i:integer;
begin
  for i := 0 to lvPaths.Items.Count - 1 do
    lvPaths.Items[i].Selected := not lvPaths.Items[i].Selected;
end;

procedure TfrmOptions.acUnselectAllExecute(Sender: TObject);
var i:integer;
begin
  for i := 0 to lvPaths.Items.Count - 1 do
    lvPaths.Items[i].Selected := false;
end;

end.

