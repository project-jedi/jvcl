unit MainDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,

  ParserTypes, MainCtrl, Settings, JvComponent, JvProgressComponent,
  ComCtrls, ToolWin, ExtCtrls;

type
  TForm1 = class(TForm)
    lsbMessages: TListBox;
    ActionList1: TActionList;
    actIncludeAll: TAction;
    actExcludeAll: TAction;
    actInclude: TAction;
    actExclude: TAction;
    actSettings: TAction;
    actProcess: TAction;
    actSave: TAction;
    actAddToIgnoreList: TAction;
    OpenDialog1: TOpenDialog;
    actAddToCompletedList: TAction;
    actUnitStatus: TAction;
    Panel1: TPanel;
    lsbSource: TListBox;
    btnInclude: TButton;
    btnIncludeAll: TButton;
    btnExclude: TButton;
    btnExcludeAll: TButton;
    lsbDest: TListBox;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Panel2: TPanel;
    lblInDirDesc: TLabel;
    lblPasDir: TLabel;
    lblOutDirDesc: TLabel;
    lblGeneratedDtxDir: TLabel;
    Button2: TButton;
    Button3: TButton;
    ToolBar2: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    actShowCompleted: TAction;
    actShowIgnored: TAction;
    actShowOther: TAction;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    actRefresh: TAction;
    Label1: TLabel;
    lblRealDtxDir: TLabel;
    ToolButton9: TToolButton;
    actShowGenerated: TAction;
    { Form }
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    procedure actAddToIgnoreListExecute(Sender: TObject);
    procedure actAddToCompletedListExecute(Sender: TObject);
    procedure actUnitStatusExecute(Sender: TObject);
    procedure actShowCompletedExecute(Sender: TObject);
    procedure actShowIgnoredExecute(Sender: TObject);
    procedure actShowOtherExecute(Sender: TObject);
    procedure actShowCompletedUpdate(Sender: TObject);
    procedure actShowIgnoredUpdate(Sender: TObject);
    procedure actShowOtherUpdate(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actShowGeneratedExecute(Sender: TObject);
    procedure actShowGeneratedUpdate(Sender: TObject);
  private
    FMainCtrl: TMainCtrl;
  protected
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure MoveSelectedToCompletedList(List: TCustomListBox);
    procedure MoveSelectedToIgnoredList(List: TCustomListBox);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;

    procedure SettingsChanged(Sender: TObject; ChangeType: TSettingsChangeType);
    procedure UpdateLabels;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

uses
  JclFileUtils, SettingsDlg, DelphiParser, UnitStatusDlg;

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
    lblPasDir.Caption := PasDir;
    lblGeneratedDtxDir.Caption := GeneratedDtxDir;
    lblRealDtxDir.Caption := RealDtxDir;
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
  FMainCtrl.SkipList := lsbSource.Items;
  FMainCtrl.ProcessList := lsbDest.Items;
  FMainCtrl.MessagesList := lsbMessages.Items;

  FMainCtrl.RefreshFiles;

  TSettings.Instance.RegisterObserver(Self, SettingsChanged);
end;

procedure TForm1.SettingsChanged(Sender: TObject;
  ChangeType: TSettingsChangeType);
begin
  case ChangeType of
    ctPasDirectory, ctGeneratedDtxDirectory, ctRealDtxDirectory:
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

procedure TForm1.actAddToIgnoreListExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(lsbDest);
  MoveSelectedToIgnoredList(lsbDest);
  SetItem(lsbDest, Index);
end;

procedure TForm1.actAddToCompletedListExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(lsbDest);
  MoveSelectedToCompletedList(lsbDest);
  SetItem(lsbDest, Index);
end;

procedure TForm1.actUnitStatusExecute(Sender: TObject);
begin
  TfrmUnitStatus.Execute(FMainCtrl);
end;

procedure TForm1.actShowCompletedExecute(Sender: TObject);
begin
  FMainCtrl.ShowCompletedFiles := not FMainCtrl.ShowCompletedFiles;
end;

procedure TForm1.actShowIgnoredExecute(Sender: TObject);
begin
  FMainCtrl.ShowIgnoredFiles := not FMainCtrl.ShowIgnoredFiles;
end;

procedure TForm1.actShowOtherExecute(Sender: TObject);
begin
  FMainCtrl.ShowOtherFiles := not FMainCtrl.ShowOtherFiles;
end;

procedure TForm1.actShowCompletedUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FMainCtrl.ShowCompletedFiles;
end;

procedure TForm1.actShowIgnoredUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FMainCtrl.ShowIgnoredFiles;
end;

procedure TForm1.actShowOtherUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FMainCtrl.ShowOtherFiles
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainCtrl := TMainCtrl.Create;
end;

destructor TForm1.Destroy;
begin
  TSettings.Instance.UnRegisterObserver(Self);
  FMainCtrl.Free;

  inherited Destroy;
end;

procedure TForm1.actRefreshExecute(Sender: TObject);
begin
  FMainCtrl.RefreshFiles;
end;

procedure TForm1.MoveSelectedToCompletedList(List: TCustomListBox);
var
  I: Integer;
begin
  with List do
  begin
    Items.BeginUpdate;
    try
      for I := Items.Count - 1 downto 0 do
        if Selected[I] then
          FMainCtrl.AddToCompletedList(Items[I]);
    finally
      Items.EndUpdate;
    end;
  end;

  TSettings.Instance.SaveUnitStatus(usCompleted);
end;

procedure TForm1.MoveSelectedToIgnoredList(List: TCustomListBox);
var
  I: Integer;
begin
  with List do
  begin
    Items.BeginUpdate;
    try
      for I := Items.Count - 1 downto 0 do
        if Selected[I] then
          FMainCtrl.AddToIgnoreList(Items[I]);
    finally
      Items.EndUpdate;
    end;
  end;

  TSettings.Instance.SaveUnitStatus(usIgnored);
end;

procedure TForm1.actShowGeneratedExecute(Sender: TObject);
begin
  FMainCtrl.ShowGeneratedFiles := not FMainCtrl.ShowGeneratedFiles;
end;

procedure TForm1.actShowGeneratedUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FMainCtrl.ShowGeneratedFiles;
end;

end.

