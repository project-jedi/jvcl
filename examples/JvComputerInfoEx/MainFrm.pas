unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComputerInfoEx, Menus, JvMenus, StdCtrls, JvExStdCtrls,
  ActnList, ComCtrls, JvDialogs, JvRichEdit, JvComponent,
  JvInspector, JvExControls, ExtCtrls, ImgList;

type
  TfrmMain = class(TForm)
    JvComputerInfoEx1: TJvComputerInfoEx;
    mmMain: TJvMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Refresh1: TMenuItem;
    SaveAs1: TMenuItem;
    PrinterSetup1: TMenuItem;
    Print1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    alMain: TActionList;
    acSaveAs: TAction;
    acPrinterSetup: TAction;
    acPrint: TAction;
    acExit: TAction;
    acRefresh: TAction;
    acAbout: TAction;
    StatusBar1: TStatusBar;
    JvSaveDialog1: TJvSaveDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintDialog1: TPrintDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    reInfo: TJvRichEdit;
    lvEvents: TListView;
    acClearEvents: TAction;
    ClearEvents1: TMenuItem;
    acReloadIcons: TAction;
    acReloadCursors: TAction;
    N2: TMenuItem;
    ReloadIcons1: TMenuItem;
    ReloadCursors1: TMenuItem;
    TabSheet3: TTabSheet;
    JvInspector1: TJvInspector;
    JvInspectorBorlandPainter1: TJvInspectorBorlandPainter;
    Panel1: TPanel;
    chkReadOnly: TCheckBox;
    acTrailingPathDelimiter: TAction;
    INcludeTrailingPathDelimiter1: TMenuItem;
    procedure acSaveAsExecute(Sender: TObject);
    procedure acPrinterSetupExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure JvComputerInfoEx1Compacting(Sender: TObject; Ratio: Integer);
    procedure JvComputerInfoEx1DeviceAdded(Sender: TObject; Drive: Char);
    procedure JvComputerInfoEx1DeviceChange(Sender: TObject;
      Event: Cardinal; Data: Pointer);
    procedure JvComputerInfoEx1DeviceModeChange(Sender: TObject;
      Device: string);
    procedure JvComputerInfoEx1DeviceRemoved(Sender: TObject; Drive: Char);
    procedure JvComputerInfoEx1DisplayChange(Sender: TObject; BitsPerPixel,
      ScreenWidth, ScreenHeight: Integer);
    procedure JvComputerInfoEx1FontChange(Sender: TObject);
    procedure JvComputerInfoEx1PaletteChanged(Sender: TObject; Wnd: HWND);
    procedure JvComputerInfoEx1PaletteChanging(Sender: TObject; Wnd: HWND);
    procedure JvComputerInfoEx1PowerBroadcast(Sender: TObject; Event,
      Data: Integer);
    procedure JvComputerInfoEx1SettingChange(Sender: TObject;
      Flag: Integer; const Section: string);
    procedure JvComputerInfoEx1SpoolerStatusChange(Sender: TObject;
      JobStatus, JobsLeft: Integer);
    procedure JvComputerInfoEx1SysColorChange(Sender: TObject);
    procedure JvComputerInfoEx1TimeChange(Sender: TObject);
    procedure JvComputerInfoEx1UserChanged(Sender: TObject);
    procedure acClearEventsExecute(Sender: TObject);
    procedure acReloadIconsExecute(Sender: TObject);
    procedure acReloadCursorsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkReadOnlyClick(Sender: TObject);
    procedure acTrailingPathDelimiterExecute(Sender: TObject);
  private
    { Private declarations }
    LastCategory: string;
    function Parse(AnObject: TObject; const Category: string; Indent: integer): boolean;
    procedure AddItem(const Category, Name, Value: string; Indent: integer);
    procedure AddEvent(const EventName, Info: string);
  public
    { Public declarations }
  end;
  
var
  frmMain: TfrmMain;

implementation
uses
  TypInfo, JvJVCLUtils;

{$R *.dfm}

procedure TfrmMain.acSaveAsExecute(Sender: TObject);
begin
  if JvSaveDialog1.Execute then
  begin
    reInfo.PlainText := true;
    reInfo.Lines.SaveToFile(JvSaveDialog1.Filename);
    reInfo.PlainText := false;
  end;
end;

procedure TfrmMain.acPrinterSetupExecute(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TfrmMain.acPrintExecute(Sender: TObject);
begin
  if PrintDialog1.Execute then
    reInfo.Print(Format('Computer info for %s', [JvComputerInfoEx1.Identification.LocalComputerName]));
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acRefreshExecute(Sender: TObject);
begin
  reInfo.Lines.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    reInfo.Lines.Clear;
    Parse(JvComputerInfoEx1, '', -1);
  finally
    reInfo.Lines.EndUpdate;
    reInfo.SelStart := 0;
    SendMessage(reInfo.Handle, EM_SCROLLCARET, 0, 0);
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  MsgAbout(Handle, 'JvComputerInfoEx Demo. Part of JVCL (http://jvcl.sf.net)', 'About...', 'MAINICON', 0);
end;

procedure TfrmMain.AddItem(const Category, Name, Value: string; Indent: integer);
  function DefaultValue(const S: string): string;
  begin
    if S = '' then
      Result := '(none)'
    else
      Result := S;
  end;
  function IndentStr(const S: string; Indent: integer): string;
  begin
    if Indent > 0 then
      Result := StringOfChar(' ', Indent * 4) + S
    else
      Result := S;
  end;
begin
  if Category <> '' then
  begin
    if reInfo.GetTextLen > 0 then
      reInfo.Lines.Add('');
    if Category <> LastCategory then
    begin
      reInfo.AddFormatText(IndentStr(Category + ':', Indent), [fsBold], '', clMaroon, 14);
      reInfo.Lines.Add('');
      LastCategory := Category;
    end;
    reInfo.AddFormatText(IndentStr('  ' + Name + ':' + #9, Indent), [fsBold], '', clBlack, 8);
    reInfo.AddFormatText(DefaultValue(Value), [], '', clBlack, 8);
  end;
end;

function TfrmMain.Parse(AnObject: TObject; const Category: string; Indent: integer): boolean;
var
  i, Count: integer;
  PropName: string;
  PropList: PPropList;
  PropInfo: PPropInfo;
  SubItems: TStringList;
begin
  LastCategory := '';
  SubItems := nil;
  Count := GetPropList(AnObject.ClassInfo, tkAny, nil);
  GetMem(PropList, Count * sizeof(PropList));
  GetPropList(AnObject.ClassInfo, tkAny, PropList);
  try
    for i := 0 to Count - 1 do
    begin
      PropInfo := PropList[i];
      PropName := PropInfo^.Name;
      case PropInfo^.PropType^.Kind of
        tkInteger, tkInt64:
          AddItem(Category, PropName, IntToStr(GetOrdProp(AnObject, PropName)), Indent);
        tkFloat:
          AddItem(Category, PropName, FloatToStr(GetFloatProp(AnObject, PropName)), Indent);
        tkEnumeration:
          AddItem(Category, PropName, GetEnumProp(AnObject, PropName), Indent);
        tkSet:
          AddItem(Category, PropName, GetSetProp(AnObject, PropName), Indent);
        tkClass:
          begin
            if SubItems = nil then
              SubItems := TStringList.Create;
            SubItems.AddObject(PropName, GetObjectProp(AnObject, PropName));
          end;
      else
        AddItem(Category, PropName, GetStrProp(AnObject, PropName), Indent);
      end;
    end;
    Result := SubItems <> nil;
    if Result then
    begin
      for i := 0 to SubItems.Count - 1 do
      begin
        Parse(SubItems.Objects[i], SubItems[i], Indent + 1);
//        reInfo.Lines.Add('');
      end;
    end;
//  if not Result then
//    reInfo.Lines.Add('');
    SubItems.Free;
  finally
    FreeMem(PropList);
  end;
end;

procedure TfrmMain.JvComputerInfoEx1Compacting(Sender: TObject;
  Ratio: Integer);
begin
  AddEvent('Compacting', Format('Ratio: %d', [Ratio]));
end;

procedure TfrmMain.AddEvent(const EventName, Info: string);
begin
  with lvEvents.Items.Insert(0) do
  begin
    Caption := DateTimeToStr(Now);
    SubItems.Add(EventName);
    SubItems.Add(Info);
  end;
end;

procedure TfrmMain.JvComputerInfoEx1DeviceAdded(Sender: TObject;
  Drive: Char);
begin
  AddEvent('Device Added', Format('Drive: %s', [Drive]));
end;

procedure TfrmMain.JvComputerInfoEx1DeviceChange(Sender: TObject;
  Event: Cardinal; Data: Pointer);
begin
  if Data <> nil then
    AddEvent('Device Changed', Format('Event ID: %d, Data Address: %x', [Event, Data]))
  else
    AddEvent('Device Changed', Format('Event ID: %d', [Event]));
end;

procedure TfrmMain.JvComputerInfoEx1DeviceModeChange(Sender: TObject;
  Device: string);
begin
  AddEvent('Device Mode Changed', Device);
end;

procedure TfrmMain.JvComputerInfoEx1DeviceRemoved(Sender: TObject;
  Drive: Char);
begin
  AddEvent('Device Removed', Format('Drive: %s', [Drive]));
end;

procedure TfrmMain.JvComputerInfoEx1DisplayChange(Sender: TObject;
  BitsPerPixel, ScreenWidth, ScreenHeight: Integer);
begin
  AddEvent('Display Changed', Format('BPP: %d, Width: %dpx, Height: %dpx', [BitsPerPixel, ScreenWidth, ScreenHeight]));
end;

procedure TfrmMain.JvComputerInfoEx1FontChange(Sender: TObject);
begin
  AddEvent('Font added or removed', '');
end;

function GetWndText(Wnd:HWND):String;
var buf: array[0..255] of char;
begin
  if GetWindowText(Wnd, buf, sizeof(buf)) = 0 then
    buf := '(no name)';
  Result := buf;
end;

procedure TfrmMain.JvComputerInfoEx1PaletteChanged(Sender: TObject; Wnd: HWND);
begin
  AddEvent('Palette Changed',
    Format('Window: %s, Handle: %d', [GetWndText(Wnd), Wnd]));
end;

procedure TfrmMain.JvComputerInfoEx1PaletteChanging(Sender: TObject; Wnd: HWND);
begin
  AddEvent('Palette Changing',
    Format('Window: %s, Handle: %d', [GetWndText(Wnd), Wnd]));
end;

procedure TfrmMain.JvComputerInfoEx1PowerBroadcast(Sender: TObject; Event, Data: Integer);
begin
  AddEvent('Power Broadcast',
    Format('Event: %d, Date: %d', [Event, Data]));
end;

procedure TfrmMain.JvComputerInfoEx1SettingChange(Sender: TObject;
  Flag: Integer; const Section: string);
begin
  AddEvent('System Setting Changed',
    Format('Flags: %d, Section: "%s"', [Flag, Section]));
end;

procedure TfrmMain.JvComputerInfoEx1SpoolerStatusChange(Sender: TObject;
  JobStatus, JobsLeft: Integer);
begin
  AddEvent('Spooler Status Changed',
    Format('Job Status: %d, Jobs Left: %d', [JobStatus, JobsLeft]));
end;

procedure TfrmMain.JvComputerInfoEx1SysColorChange(Sender: TObject);
begin
  AddEvent('System Color Changed', '');
end;

procedure TfrmMain.JvComputerInfoEx1TimeChange(Sender: TObject);
begin
  AddEvent('System Time Changed', '');
end;

procedure TfrmMain.JvComputerInfoEx1UserChanged(Sender: TObject);
begin
  AddEvent('User logged on or off', '');
end;

procedure TfrmMain.acClearEventsExecute(Sender: TObject);
begin
  lvEvents.Items.Clear;
end;

procedure TfrmMain.acReloadIconsExecute(Sender: TObject);
begin
  if not JvComputerInfoEx1.ResetSystemIcons then
    MsgError(Handle, 'Could not reload system icons', 'Error');
end;

procedure TfrmMain.acReloadCursorsExecute(Sender: TObject);
begin
  if not JvComputerInfoEx1.ResetSystemCursors then
    MsgError(Handle, 'Could not reload system cursors', 'Error');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  JvInspector1.InspectObject := JvComputerInfoEx1;
end;

procedure TfrmMain.chkReadOnlyClick(Sender: TObject);
begin
  JvInspector1.ReadOnly := chkReadOnly.Checked;
  JvInspector1.Refresh;
  JvComputerInfoEx1.ReadOnly := chkReadOnly.Checked;
end;

procedure TfrmMain.acTrailingPathDelimiterExecute(Sender: TObject);
begin
  acTrailingPathDelimiter.Checked := not acTrailingPathDelimiter.Checked;
  JvComputerInfoEx1.Folders.TrailingPathDelimiter := acTrailingPathDelimiter.Checked;
  if reInfo.Lines.Count > 0 then
    acRefresh.Execute;
end;

end.

