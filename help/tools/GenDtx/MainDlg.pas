unit MainDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,

  ParserTypes, MainCtrl, Settings, JvComponent, JvProgressComponent;

type
  TForm1 = class(TForm)
    lsbMessages: TListBox;
    lblInDirDesc: TLabel;
    lblOutDirDesc: TLabel;
    lblInDir: TLabel;
    lblOutDir: TLabel;
    btnSettings: TButton;
    lsbSource: TListBox;
    lsbDest: TListBox;
    btnInclude: TButton;
    btnIncludeAll: TButton;
    btnExclude: TButton;
    btnExcludeAll: TButton;
    ActionList1: TActionList;
    actIncludeAll: TAction;
    actExcludeAll: TAction;
    actInclude: TAction;
    actExclude: TAction;
    actSettings: TAction;
    btnProcess: TButton;
    actProcess: TAction;
    Button1: TButton;
    actSave: TAction;
    chbDontIncludeIgnoredFiles: TCheckBox;
    Button2: TButton;
    actAddToIgnoreList: TAction;
    OpenDialog1: TOpenDialog;
    JvProgressComponent1: TJvProgressComponent;
    { Form }
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    { Actions }
    procedure actIncludeExecute(Sender: TObject);
    procedure actIncludeAllExecute(Sender: TObject);
    procedure actExcludeExecute(Sender: TObject);
    procedure actExcludeAllExecute(Sender: TObject);
    procedure actIncludeAllUpdate(Sender: TObject);
    procedure actExcludeAllUpdate(Sender: TObject);
    procedure actIncludeUpdate(Sender: TObject);
    procedure actExcludeUpdate(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actProcessExecute(Sender: TObject);
    procedure actProcessUpdate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure chbDontIncludeIgnoredFilesClick(Sender: TObject);
    procedure actAddToIgnoreListExecute(Sender: TObject);
  private
    FMainCtrl: TMainCtrl;
  protected
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;

    procedure SettingsChanged(Sender: TObject; ChangeType: TSettingsChangeType);
    procedure UpdateLabels;
  end;

var
  Form1: TForm1;

implementation

uses
  JclFileUtils, SettingsDlg, DelphiParser;

{$R *.dfm}

{ DONE: dubbel classes eruit halen }
{ TODO: default values uitbreiden }
{ TODO: %code toevoegen }
{ TODO: %value toevoegen }
{ DONE: constructors, destructors voor methods sorteren }
{ TODO: %scope toevoegen voor properties/methods }
{ DONE: Record fields met , ook herkennen }
{ DONE: Param fields met , ook herkennen }
{ TODO: Set -> Enumeration; Set zelf toevoegen }

procedure TForm1.actIncludeExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(lsbSource);
  MoveSelected(lsbSource, lsbDest.Items);
  SetItem(lsbSource, Index);
end;

procedure TForm1.actIncludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  lsbDest.Items.BeginUpdate;
  try
    for I := 0 to lsbSource.Items.Count - 1 do
      lsbDest.Items.AddObject(lsbSource.Items[I],
        lsbSource.Items.Objects[I]);
  finally
    lsbDest.Items.EndUpdate;
  end;
  lsbSource.Items.Clear;
  SetItem(lsbSource, 0);
end;

procedure TForm1.actExcludeExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(lsbDest);
  MoveSelected(lsbDest, lsbSource.Items);
  SetItem(lsbDest, Index);
end;

procedure TForm1.actExcludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to lsbDest.Items.Count - 1 do
    lsbSource.Items.AddObject(lsbDest.Items[I], lsbDest.Items.Objects[I]);
  lsbDest.Items.Clear;
  SetItem(lsbDest, 0);
end;

function TForm1.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then
      Exit;
  Result := LB_ERR;
end;

procedure TForm1.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    List.Items.BeginUpdate;
    try
      for I := List.Items.Count - 1 downto 0 do
        if List.Selected[I] then
        begin
          Items.AddObject(List.Items[I], List.Items.Objects[I]);
          List.Items.Delete(I);
        end;
    finally
      List.Items.EndUpdate;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TForm1.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then
      Index := 0
    else
      if Index > MaxIndex then
      Index := MaxIndex;
    Selected[Index] := True;
  end;
end;

procedure TForm1.actIncludeAllUpdate(Sender: TObject);
begin
  actIncludeAll.Enabled := lsbSource.Items.Count > 0;
end;

procedure TForm1.actExcludeAllUpdate(Sender: TObject);
begin
  actExcludeAll.Enabled := lsbDest.Items.Count > 0;
end;

procedure TForm1.actIncludeUpdate(Sender: TObject);
begin
  actInclude.Enabled := lsbSource.SelCount > 0;
end;

procedure TForm1.actExcludeUpdate(Sender: TObject);
begin
  actExclude.Enabled := lsbDest.SelCount > 0;
end;

procedure TForm1.UpdateLabels;
begin
  with TSettings.Instance do
  begin
    lblInDir.Caption := InDir;
    lblOutDir.Caption := OutDir;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  UpdateLabels;
end;

procedure TForm1.actSettingsExecute(Sender: TObject);
begin
  TfrmSettings.Execute;
end;

procedure TForm1.actProcessExecute(Sender: TObject);
begin
  FMainCtrl.Process;
end;

procedure TForm1.actProcessUpdate(Sender: TObject);
begin
  actProcess.Enabled := lsbDest.Items.Count > 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMainCtrl := TMainCtrl.Create;
  FMainCtrl.SkipList := lsbSource.Items;
  FMainCtrl.ProcessList := lsbDest.Items;
  FMainCtrl.MessagesList := lsbMessages.Items;

  FMainCtrl.UpdateSourceFiles;

  TSettings.Instance.RegisterObserver(Self, SettingsChanged);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  TSettings.Instance.UnRegisterObserver(Self);
  FMainCtrl.Free;
end;

procedure TForm1.SettingsChanged(Sender: TObject;
  ChangeType: TSettingsChangeType);
begin
  case ChangeType of
    ctInDirectory, ctOutDirectory:
      UpdateLabels;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  with TFileStream.Create('C:\Temp\allfilestemp.txt', fmCreate) do
  try
    for I := 0 to lsbDest.Items.Count - 1 do
    begin
      S := lsbDest.Items[I] + #13#10;
      Write(PChar(S)^, Length(S));
    end;
  finally
    Free;
  end;
end;

procedure TForm1.actSaveExecute(Sender: TObject);
begin
  with TSaveDialog.Create(Application) do
  try
    if Execute then
      lsbDest.Items.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TForm1.chbDontIncludeIgnoredFilesClick(Sender: TObject);
begin
  FMainCtrl.IgnoreFiles := chbDontIncludeIgnoredFiles.Checked;
end;

procedure TForm1.actAddToIgnoreListExecute(Sender: TObject);
var
  I: Integer;
begin
  with lsbDest do
  begin
    Items.BeginUpdate;
    try
      for I := Count - 1 downto 0 do
        if Selected[I] then
        begin
          TSettings.Instance.IgnoredUnits.Add(Items[I]);
          if FMainCtrl.IgnoreFiles then
            Items.Delete(I);
        end;
    finally
      Items.EndUpdate;
    end;
  end;

  TSettings.Instance.Save;
end;

end.

