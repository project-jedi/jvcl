unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvChangeNotify, Buttons, StdCtrls, ComCtrls, ExtCtrls, JvComponent;

type
  TForm1 = class(TForm)
    CN1: TJvChangeNotify;
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
    procedure CN1ChangeNotify(Sender: TObject; Dir: string;
      Actions: TJvChangeActions);
    procedure EditItem(li: TListItem);
    procedure DeleteItem(li: TListItem);
    procedure ListView1DblClick(Sender: TObject);
  private
    { Private declarations }
    procedure ResetCaptions(Invert: boolean);
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

procedure TForm1.ResetCaptions(Invert: boolean);
const
  aCap: array[boolean] of string = ('TJvChangeNotification demo', 'Checking...');
begin
  if Invert then
    Caption := aCap[not CN1.Active]
  else
    Caption := aCap[CN1.Active];
  Application.Title := Caption;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CN1.Active := false;
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
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

procedure TForm1.btnAddClick(Sender: TObject);
begin
  EditItem(nil);
end;

procedure TForm1.btnStartClick(Sender: TObject);
var b: boolean;
begin
  if CN1.Notifications.Count = 0 then
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
  CN1.Active := b;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  ListBox2.Clear;
  ResetCaptions(false);
end;

procedure TForm1.CN1ChangeNotify(Sender: TObject; Dir: string;
  Actions: TJvChangeActions);
begin
  Application.Title := Format('Change in %s (%s)', [Dir, ActionsToString(Actions)]);
  ListBox2.Items.Add(Application.Title);
  FlashWindow(Form1.Handle, true);
  MessageBeep(DWORD(-1));
end;

procedure TForm1.WMGetMinMaxINfo(var Msg: TWMGetMinMaxInfo);
begin
  with Msg.MinMaxInfo^.ptMinTrackSize do
  begin
    X := 392;
    Y := 295;
  end;
  Msg.Result := 0;
end;

procedure TForm1.EditItem(li: TListItem);
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

  if TForm2.Execute(ADirectory, AOptions, AIncludeSubDirs) then
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
      li.Data := CN1.Notifications.Add;
    with TJvChangeItem(li.Data) do
    begin
      IncludeSubTrees := AIncludeSubDirs and (Win32Platform = VER_PLATFORM_WIN32_NT);
      Directory := ADirectory;
      Actions := AOptions;
    end;
  end;
end;

procedure TForm1.DeleteItem(li: TListItem);
begin
  if li = nil then
    Exit;
  if li.Data <> nil then
    CN1.Notifications.Delete(li.Index);
  li.Delete;
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
begin
  EditItem(ListView1.Selected);
end;

end.

