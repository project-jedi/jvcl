unit fSearchFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvListbox, JvComponent, JvCtrls, JvSearchFiles, Mask, JvToolEdit,
  ComCtrls, JvEdit, JvPlacemnt;

type
  TMainFrm = class(TForm)
    JvSearchFile1: TJvSearchFiles;
    GroupBox1: TGroupBox;
    JvDirectoryBox1: TJvDirectoryEdit;
    btnSearch: TButton;
    Label1: TLabel;
    chkRecursive: TCheckBox;
    Label2: TLabel;
    edFileMask: TEdit;
    GroupBox2: TGroupBox;
    btnCancel: TButton;
    chkContains: TCheckBox;
    reFoundFiles: TRichEdit;
    edContainText: TJvEdit;
    chkVirtual: TCheckBox;
    JvFormStorage1: TJvFormStorage;
    StatusBar1: TStatusBar;
    procedure btnSearchClick(Sender: TObject);
    procedure JvSearchFile1FindFile(Sender: TObject; const AName: string);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure chkContainsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvSearchFile1BeginScanDir(Sender: TObject;
      const AName: String);
    procedure OptionsChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation
uses
  JvFunctions;

{$R *.DFM}

procedure TMainFrm.btnSearchClick(Sender: TObject);
begin
  btnSearch.Enabled := false;
  btnCancel.Enabled := true;
  Screen.Cursor := crHourGlass;
  try
    reFoundFiles.Lines.Clear;
    JvSearchFile1.FileParams.FileMasks.Text := edFileMask.Text;
    if chkRecursive.Checked then
      JvSearchFile1.DirOption := doIncludeSubDirs
    else
      JvSearchFile1.DirOption := doExcludeSubDirs;
    if chkVirtual.Checked then
      JvSearchFile1.Options := JvSearchFile1.Options + [soOwnerData]
    else
      JvSearchFile1.Options := JvSearchFile1.Options - [soOwnerData];
    JvSearchFile1.RootDirectory := JvDirectoryBox1.EditText;
    JvSearchFile1.Search;
  finally
    StatusBar1.Panels[0].Text := Format('(%d matching items found)',[reFoundFiles.Lines.Count]);
    btnSearch.Enabled := true;
    btnCancel.Enabled := false;
    Screen.Cursor := crDefault;
  end;
end;

function ContainsText(const Filename,AText:string):boolean;
var S:TMemoryStream;tmp:string;
begin
  Result := false;
  S := TMemoryStream.Create;
  try
    S.LoadFromFile(Filename);
    if S.Memory <> nil then
    begin
      tmp := PChar(S.Memory);
      tmp := AnsiLowerCase(tmp);
      Result := Pos(AnsiLowerCase(AText),tmp) > 0;
    end;
  finally
    S.Free;
  end;
end;

procedure TMainFrm.JvSearchFile1FindFile(Sender: TObject;
  const AName: string);
begin
  StatusBar1.Panels[0].Text := Format('Searching in %s...',[AName]);
  StatusBar1.Update;
  if chkContains.Checked and (edContainText.Text <> '') then
    if not ContainsText(AName,edContainText.Text) then Exit;
  reFoundFiles.Lines.Add(AName);
end;

procedure TMainFrm.btnCancelClick(Sender: TObject);
begin
  JvSearchFile1.Abort;
  btnCancel.Enabled := false;
end;

procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  btnCancel.Click;
end;

procedure TMainFrm.chkContainsClick(Sender: TObject);
begin
  edContainText.Enabled := chkContains.Checked;
  OptionsChange(Sender);
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  JvFormStorage1.IniFileName := ChangeFileExt(Application.ExeName,'.ini');
end;

procedure TMainFrm.JvSearchFile1BeginScanDir(Sender: TObject;
  const AName: String);
begin
  StatusBar1.Panels[0].Text := Format('Searching in %s...',[ExcludeTrailingPathDelimiter(AName)]);
  StatusBar1.Update;
end;

procedure TMainFrm.OptionsChange(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Ready';
  StatusBar1.Update;
end;

end.

