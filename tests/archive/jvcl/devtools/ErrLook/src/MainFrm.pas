unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList;

type
  // a list of TStrings
  TStringsList = class(TObject)
  private
    FNames: TStrings;
    procedure SetString(Index: integer; Strings: TStrings);
    function GetString(Index: integer): TStrings;
    function GetName(const Name: string): TStrings;
    procedure SetName(const Name: string; const Value: TStrings);
    function GetCount: integer;
    function NameOfIndex(Index: integer): string;
    function IndexOfName(const Name: string): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Name:string;Strings:TStrings):integer;
    procedure Delete(Index:integer);
    property Name[Index: integer]: string read NameOfIndex;
    property Index[const Name:string]:integer read IndexOfName;
    property Names[const Name: string]: TStrings read GetName write SetName;
    property Strings[Index: integer]: TStrings read GetString write SetString;default;
    property Count: integer read GetCount;
  end;

  TfrmMain = class(TForm)
    edValue: TEdit;
    Label1: TLabel;
    reErrMsg: TRichEdit;
    btnModules: TButton;
    btnLookup: TButton;
    btnClose: TButton;
    btnHelp: TButton;
    lblHex: TLabel;
    lblInt: TLabel;
    UpDown1: TUpDown;
    alMain: TActionList;
    acModules: TAction;
    acLookUp: TAction;
    acClose: TAction;
    acHelp: TAction;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acModulesExecute(Sender: TObject);
    procedure acLookUpExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure lblHexClick(Sender: TObject);
    procedure lblIntClick(Sender: TObject);
  private
    { Private declarations }
    FModules,FFoundInis: TStringlist;
    FNamedErrors: TStringsList;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetupHelp;
    function GetDelphiErrors(const ErrorValue: string; Errors: TStringsList): integer;
    function GetModuleErrors(ErrorCode: integer; Filenames: TStrings): integer;
    procedure DoInsertMessage(const Category, Msg: string);
    procedure ReadIniSettings(const Filename: string;FoundFiles:TStrings);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  Registry, IniFiles,
  ModulesFrm,
  HtmlHlp; // download from http://delphi-jedi.org (API Library Files)

{$R *.dfm}
{$I popups.inc}

resourcestring
  SUnknownSource = 'Unknown source:';
  SMessageSeparator = '----';
  SSystemErrorHeader = 'System Error';
  SFmtNoMessageFound = ' No message for error value "%s" found';
  SHelpFile = 'ErrLook.chm';

  { utility }

// MyStrToIntDef: converts standard int strings as well as 0x hex style strings
function MyStrToIntDef(const S: string;Default:integer=0): integer;
begin
  if Pos('0x', AnsiLowerCase(S)) = 1 then
    Result := StrToIntDef('$' + Copy(S, 3, MaxInt), Default)
  else
    Result := StrToIntDef(S, Default);
end;

function GetSysErrorMessage(ErrorCode: integer; HModule: Cardinal): string;
var
  Len: Integer;
  Buffer: PChar;
  dwFlags: Cardinal;
begin
  dwFlags := FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_FROM_SYSTEM;
  if HModule <> 0 then
    dwFlags := dwFlags or FORMAT_MESSAGE_FROM_HMODULE;
  Len := FormatMessage(dwFlags, Pointer(HModule), ErrorCode, 0, @Buffer,
    0, nil);
  try
    while (Len > 0) and (Buffer[Len - 1] in [#0..#32, '.']) do Dec(Len);
    SetString(Result, Buffer, Len);
  finally
    if Len > 0 then
      LocalFree(Cardinal(Buffer));
  end;
end;

{ TStringsList }

function TStringsList.Add(const Name: string; Strings: TStrings): integer;
begin
  SetName(Name,Strings);
  Result := IndexOfName(Name);
end;

procedure TStringsList.Clear;
var i: integer;
begin
  for i := 0 to FNames.Count - 1 do
    FNames.Objects[i].Free;
  FNames.Clear;
end;

constructor TStringsList.Create;
begin
  inherited Create;
  FNames := TStringlist.Create;
end;

procedure TStringsList.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt('Invalid index (%d)',[Index]);
  FNames.Objects[Index].Free;
  FNames.Delete(Index);
end;

destructor TStringsList.Destroy;
begin
  Clear;
  FNames.Free;
  inherited;
end;

function TStringsList.GetCount: integer;
begin
  Result := FNames.Count;
end;

function TStringsList.GetString(Index: integer): TStrings;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt('Invalid index (%d)',[Index]);
  Result := TStrings(FNames.Objects[Index]);
end;

function TStringsList.GetName(const Name: string): TStrings;
var i: integer;
begin
  i := FNames.IndexOf(Name);
  if i < 0 then
    i := FNames.AddObject(Name, TStringList.Create);
  Result := TStrings(FNames.Objects[i]);
end;

function TStringsList.IndexOfName(const Name: string): integer;
begin
  Result := FNames.IndexOf(Name);
end;

function TStringsList.NameOfIndex(Index: integer): string;
begin
  Result := FNames[Index];
end;

procedure TStringsList.SetString(Index: integer; Strings: TStrings);
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt('Invalid index (%d)',[Index]);
  TStrings(FNames.Objects[Index]).Assign(Strings);
end;

procedure TStringsList.SetName(const Name: string; const Value: TStrings);
begin
  if Value = nil then
    Names[Name].Clear
  else
    Names[Name].Assign(Value);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FModules := TStringlist.Create;
  FNamedErrors := TStringsList.Create;
  LoadSettings;
  UpDown1.Width := 0;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveSettings;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FModules.Free;
  FNamedErrors.Free;
end;

procedure TfrmMain.ReadIniSettings(const Filename:string;FoundFiles:TStrings);
var FSections,FValues:TStringlist;i,j:integer;
begin
  if not FileExists(Filename) or (FoundFiles.IndexOf(Filename) > -1 )then Exit;
  FoundFiles.Add(Filename);
  with TIniFile.Create(Filename) do
  try
    FSections := TStringlist.Create;
    try
      ReadSections(FSections);
      for i := 0 to FSections.Count - 1 do
      begin
        if AnsiSameText(FSections[i],'Externals') then
        begin
          // recursively read another file
          FValues := TStringlist.Create;
          try
            ReadSection('Externals',FValues);
            for j := 0 to FValues.Count - 1 do
              ReadIniSettings(ExpandUNCFileName(ReadString('Externals',FValues[j],'')),FoundFiles);
          finally
            FValues.Free;
          end;
          Continue;
        end;
        // read in the categorized error codes:
        j := FNamedErrors.Add(FSections[i],nil);
        ReadSectionValues(FSections[i], FNamedErrors[j]);
      end;
    finally
      FSections.Free;
    end;
  finally
    Free;
  end;
end;

procedure TfrmMain.LoadSettings;
begin
  with TRegIniFile.Create('') do
  try
    OpenKey('\Software\JVCL\ErrLook', true);
    ReadSection('Modules', FModules);
    if (BorderStyle in [bsSizeable, bsSizeToolWin]) then
    begin
      ClientWidth := ReadInteger('General', 'Width', ClientWidth);
      ClientHeight := ReadInteger('General', 'Height', ClientHeight);
    end;
    // read these after setting width/height or the anchors will be f***d up
    Top := ReadInteger('General', 'Top', (Screen.Height - Height) div 2);
    Left := ReadInteger('General', 'Left', (Screen.Width - Width) div 2);
  finally
    Free;
  end;
  FNamedErrors.Clear;
  // used to avoid recursion
  FFoundInis := TStringlist.Create;
  try
    ReadIniSettings(ExtractFilePath(Application.ExeName) + 'errors.ini',FFoundInis);
  finally
    FFoundInis.Free;
  end;
  SetupHelp;
end;

procedure TfrmMain.SaveSettings;
var i: integer;
begin
  with TRegIniFile.Create('') do
  try
    OpenKey('\Software\JVCL\ErrLook', true);
    EraseSection('Modules');
    for i := 0 to FModules.Count - 1 do
      WriteString('Modules', FModules[i], '');
    if not IsZoomed(Handle) and not IsIconic(Application.Handle) then
    begin
      WriteInteger('General', 'Top', Top);
      WriteInteger('General', 'Left', Left);
      if (BorderStyle in [bsSizeable, bsSizeToolWin]) then
      begin
        WriteInteger('General', 'Width', ClientWidth);
        WriteInteger('General', 'Height', ClientHeight);
      end;
    end;
  finally
    Free;
  end;
end;

function TfrmMain.GetModuleErrors(ErrorCode: integer; Filenames: TStrings): integer;
var i: integer; S, T: string; AModule: HModule;
begin
  Result := 0;
  // get the system error message:
  S := trim(GetSysErrorMessage(ErrorCode, 0));
  if S <> '' then Inc(Result);
  DoInsertMessage(SSystemErrorHeader, S);
  // get any module error messages:
  for i := 0 to Filenames.Count - 1 do
  begin
    AModule := LoadLibraryEx(PChar(Filenames[i]), 0, LOAD_LIBRARY_AS_DATAFILE);
    if AModule <> 0 then
    try
      T := GetSysErrorMessage(ErrorCode, AModule);
      if (T <> '') and not AnsiSameText(S, T) then
      begin
        Inc(Result);
        DoInsertMessage(ExtractFileName(Filenames[i]), T);
      end;
    finally
      FreeLibrary(AModule);
    end;
  end;
end;

function TfrmMain.GetDelphiErrors(const ErrorValue: string; Errors: TStringsList): integer;
var i, j: integer;
begin
  Result := 0;
  if ErrorValue <> '' then
  begin
    for i := 0 to Errors.Count - 1 do
    begin
      j := Errors.Strings[i].IndexOfName(ErrorValue);
      if j > -1 then
      begin
        Inc(Result);
        DoInsertMessage(Errors.Name[i], Errors.Strings[i].Values[Errors.Strings[i].Names[j]]);
      end;
    end;
  end;
end;

procedure TfrmMain.DoInsertMessage(const Category, Msg: string);
var S: string;
begin
  S := Category;
  if S = '' then
    S := SUnknownSource
  else
    S := S + ':';
  if Msg <> '' then
  begin
    reErrMsg.SelAttributes.Style := [];
    reErrMsg.SelAttributes.Color := reErrMsg.Font.Color;
    if reErrMsg.Lines.Count > 0 then
      reErrMsg.Lines.Add(SMessageSeparator);

    // bug in richedit?, resets to clMaroon here by itself sometimes...
    reErrMsg.SelAttributes.Style := [fsBold];
    reErrMsg.SelAttributes.Color := reErrMsg.Font.Color;
    reErrMsg.Lines.Add(S);

    reErrMsg.SelAttributes.Style := [];
    reErrMsg.SelAttributes.Color := clMaroon;
    reErrMsg.Lines.Add(Msg);
  end;
end;

procedure TfrmMain.acModulesExecute(Sender: TObject);
begin
  with TfrmModules.Create(Application) do
  try
    Modules := FModules;
    if ShowModal = mrOK then
      FModules.Assign(Modules);
  finally
    Free;
  end;
end;
             
procedure TfrmMain.acLookUpExecute(Sender: TObject);
var i: integer;S:string;
begin
  reErrMsg.Lines.Clear;
  S := edValue.Text;
  i := MyStrToIntDef(S,-1);
  lblInt.Caption := IntToStr(i);
  lblHex.Caption := '$' + IntToHex(i, 2);
  if GetModuleErrors(i, FModules) + GetDelphiErrors(S, FNamedErrors) = 0 then
    reErrMsg.Lines.Text := Format(SFmtNoMessageFound,[S]);
  // scroll to top
  reErrMsg.SelStart := 0;
  reErrMsg.Perform(EM_SCROLLCARET, 0, 0);
  // prepare for next code
  edValue.SetFocus;
  edValue.SelectAll;
end;

procedure TfrmMain.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acHelpExecute(Sender: TObject);
begin
  ModulesFrm.ShowHelp(SHelpFile,ActiveControl);
end;

function TfrmMain.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := false;
  Result := true;
end;

procedure TfrmMain.SetupHelp;
begin
  edValue.HelpContext := IDH_VALUE;
  reErrMsg.HelpContext := IDH_ERRORMESSAGE;
  btnModules.HelpContext := IDH_MODULES;
  btnLookUp.HelpContext := IDH_LOOKUP;
end;

procedure TfrmMain.lblHexClick(Sender: TObject);
begin
  edValue.Text := lblHex.Caption;
end;

procedure TfrmMain.lblIntClick(Sender: TObject);
begin
  edValue.Text := lblInt.Caption;
end;

end.

