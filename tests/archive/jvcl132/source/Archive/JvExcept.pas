unit JvExcept;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, {$IFDEF DELPHI5_UP}Contnrs, {$ENDIF}JclBase;

const
  UM_CREATEDETAILS = WM_USER + $100;

type
  TJvExceptionHandleEvent = procedure (E: Exception; var DefaultHandling: Boolean) of object;

  TJvExceptionLog = class(TObject)
  private
    FCurrentException: Exception;
    FEnvironmentDetails: TStrings;
    FExceptionDetails: TStrings;
    FFileName: TFileName;
    FLastActiveControl: TWinControl;
    FLastActiveForm: TCustomForm;
    FLogExceptions: Boolean;
    FLogHandle: THandle;
    FSupportAddress: string;
    FSupportSubject: string;
    FOnDetails: TNotifyEvent;
    FOnException: TJvExceptionHandleEvent;
    FOnSupport: TNotifyEvent;
    function GetEnvironmentDetails: TStrings;
    function GetExceptionDetails: TStrings;
    function GetExceptionText: string;
  protected
    procedure DoSupport; dynamic;
    function GetProcessorInfo: string;
    procedure InitProperties;
    procedure SendMailReport;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearDetails;
    procedure CloseLog;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    function GetComponentInfo(C: TComponent): string;
    function GetFileVersion(const FileName: TFileName): string;
    function IsSupportAvailable: Boolean;
    procedure LogException;
    procedure ShowException(E: Exception);
    procedure OpenLog;
    procedure WriteLogTimeStamp;
    procedure WriteLogString(S: string; AddCRLF: Boolean = True);
    procedure WriteLogStrings(Strings: TStrings);
    property CurrentException: Exception read FCurrentException;
    property EnvironmentDetails: TStrings read GetEnvironmentDetails;
    property ExceptionDetails: TStrings read GetExceptionDetails;
    property ExceptionText: string read GetExceptionText;
    property FileName: TFileName read FFileName write FFileName;
    property LogExceptions: Boolean read FLogExceptions write FLogExceptions;
    property SupportAddress: string read FSupportAddress write FSupportAddress;
    property SupportSubject: string read FSupportSubject write FSupportSubject;
    property OnDetails: TNotifyEvent read FOnDetails write FOnDetails;
    property OnException: TJvExceptionHandleEvent read FOnException write FOnException;
    property OnSupport: TNotifyEvent read FOnSupport write FOnSupport;
  end;

  TJvExceptionDialog = class(TForm)
    OkBtn: TButton;
    DetailsMemo: TMemo;
    DetailsBtn: TButton;
    Bevel1: TBevel;
    TextLabel: TMemo;
    SupportBtn: TButton;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
  private
    FDetailsVisible: Boolean;
    FFullHeight: Integer;
    procedure CreateDetails;
    procedure SetDetailsVisible(const Value: Boolean);
    procedure UMCreateDetails(var Message: TMessage); message UM_CREATEDETAILS;
  public
    property DetailsVisible: Boolean read FDetailsVisible write SetDetailsVisible;
  end;

var
  JvExceptionLog: TJvExceptionLog = nil;

implementation

{$R *.DFM}

uses
  JclDebug, JclStrings, JclFileUtils, JclSysInfo, JclMapi, JclSysUtils;

resourcestring
  RsAppError = '%s - application error';
  RsLineNumber = 'Line number: %d';
  RsModuleName = 'Module: %s';
  RsFileName = 'File: %s';
  RsProcedure = 'Procedure: %s';
  RsExceptionClass = 'Exception class: %s';
  RsExceptionAddr = 'Exception address: %p';
  RsDiskSpace = 'Free disk space [%s:]: %d';
  RsScreenRes = 'Screen: %dx%d pixels, %d bpp';
  RsActiveControl = 'Active control: %s';
  RsActiveForm = 'Active form: %s';
  RsOSVersion = 'OS: Win%s, Version: %d.%d, Build: %x, "%s"';
  RsFileVersion = '%s version: %s [%s]';
  RsVersionInfoNotFound = '%s - not found.';
  RsProcessor = 'Processor: %s %s %d %s%s';

procedure InitializeHandler;
begin
  JvExceptionLog := TJvExceptionLog.Create;
  Application.OnException := JvExceptionLog.ExceptionHandler;
end;

{ TJvExceptionLog }

procedure TJvExceptionLog.ClearDetails;
begin
  FEnvironmentDetails.Clear;
  FExceptionDetails.Clear;
end;

procedure TJvExceptionLog.CloseLog;
begin
  if FLogHandle <> INVALID_HANDLE_VALUE then
  begin
    FileClose(FLogHandle);
    FLogHandle := INVALID_HANDLE_VALUE;
  end;
end;

constructor TJvExceptionLog.Create;
begin
  FFileName := ChangeFileExt(ParamStr(0), '') + '_ErrLog.txt';
  FEnvironmentDetails := TStringList.Create;
  FExceptionDetails := TStringList.Create;
  FLogHandle := INVALID_HANDLE_VALUE;
  FLogExceptions := True;
  InitProperties;
end;

destructor TJvExceptionLog.Destroy;
begin
  FreeAndNil(FEnvironmentDetails);
  FreeAndNil(FExceptionDetails);
  inherited;
end;

procedure TJvExceptionLog.DoSupport;
begin
  if Assigned(FOnSupport) then
    FOnSupport(Self)
  else
    SendMailReport;
end;

procedure TJvExceptionLog.ExceptionHandler(Sender: TObject; E: Exception);
begin
  ShowException(E);
end;

function TJvExceptionLog.GetComponentInfo(C: TComponent): string;
begin
  Result := '';
  if C <> nil then
  try
    Result := Format('[%s] %s', [C.ClassName, C.Name]);
  except
  end;  
end;

function TJvExceptionLog.GetEnvironmentDetails: TStrings;
const
  OSNames: array[TWindowsVersion] of PChar =
    ('?', '95', '95OSR2', '98', '98SE', 'NT3', 'NT4', 'W2000');
var
  D: Byte;
  S: string;

  function GetBPP: Integer;
  var
    DC: HDC;
  begin
    DC := GetDC(0);
    Result := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
    ReleaseDC(0, DC);
  end;

begin
  with FEnvironmentDetails do
    if Count = 0 then
    begin
      Add(Format(RsOSVersion, [OSNames[GetWindowsVersion], Win32MajorVersion,
        Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
      S := GetWindowsFolder;
      if Length(S) > 0 then
      begin
        D := Ord(S[1]) - 64;
        Add(Format(RsDiskSpace, [S[1], DiskFree(D)]));
      end;
      Add(Format(RsScreenRes, [Screen.Width, Screen.Height, GetBPP]));
      Add(Format(RsActiveControl, [GetComponentInfo(FLastActiveControl)]));
      Add(Format(RsActiveForm, [GetComponentInfo(FLastActiveForm)]));
      Add(GetProcessorInfo);
      Add(GetFileVersion(Application.ExeName));
      Add(GetFileVersion('COMCTL32.DLL'));
      Add(GetFileVersion('KERNEL32.DLL'));
      Add(GetFileVersion('MAPI32.DLL'));
      Add(GetFileVersion('SHDOCVW.DLL'));
      Add(GetFileVersion('SHELL32.DLL'));
      Add(GetFileVersion('WININET.DLL'));
      Add(GetFileVersion('WSOCK32.DLL'));
      Add(GetFileVersion('VREDIR.VXD'));
      if Assigned(FOnDetails) then FOnDetails(Self); 
    end;
  Result := FEnvironmentDetails;
end;

function TJvExceptionLog.GetExceptionDetails: TStrings;
var
  EAddr: Pointer;
  LineNumber: Integer;
begin
  with FExceptionDetails do
    if Count = 0 then
    begin
      EAddr := JclDebug.ExceptionAddr;
      Add(Format(RsExceptionClass, [FCurrentException.ClassName]));
      Add(Format(RsExceptionAddr, [EAddr]));
      LineNumber := __LINE_OF_ADDR__(EAddr);
      if LineNumber > 0 then
      begin
        Add(Format(RsFileName, [__FILE_OF_ADDR__(EAddr)]));
        Add(Format(RsModuleName, [__MODULE_OF_ADDR__(EAddr)]));
        Add(Format(RsLineNumber, [LineNumber]));
        Add(Format(RsProcedure, [__PROC_OF_ADDR__(EAddr)]));
      end;
    end;
  Result := FExceptionDetails;
end;

function TJvExceptionLog.GetExceptionText: string;
begin
  if Assigned(FCurrentException) then
    Result := AdjustLineBreaks(StrEnsureSuffix('.', FCurrentException.Message))
  else
    Result := '';
end;

function TJvExceptionLog.GetFileVersion(const FileName: TFileName): string;
begin
  Result := '';
  try
    if VersionResourceAvailable(FileName) then
      with TJclFileVersionInfo.Create(FileName) do
      try
        Result := Format(RsFileVersion,
          [ExtractFileName(FileName), FileVersion, ProductVersion]);
      finally
        Free;
      end
    else
      Result := Format(RsVersionInfoNotFound, [ExtractFileName(FileName)]);
  except
  end;
end;

function TJvExceptionLog.GetProcessorInfo: string;
const
  MMXText: array[Boolean] of PChar = ('', 'MMX');
  FDIVText: array[Boolean] of PChar = (' [FDIV Bug]', '');
var
  CpuInfo: TCpuInfo;
begin
  Result := '';
  try
    GetCpuInfo(CpuInfo);
    with CpuInfo do
      Result := Format(RsProcessor, [Manufacturer, CpuName,
        RoundFrequency(FrequencyInfo.NormFreq), MMXText[MMX], FDIVText[IsFDIVOK]]);
  except
  end;
end;

procedure TJvExceptionLog.InitProperties;
begin
  if VersionResourceAvailable(ParamStr(0)) then
  try
    with TJclFileVersionInfo.Create(ParamStr(0)) do
    try
      FSupportAddress := UserKeys['SupportAddress'];
      FSupportSubject := UserKeys['SupportSubject'];
    finally
      Free;
    end;
  except
  end;
end;

function TJvExceptionLog.IsSupportAvailable: Boolean;
begin
  Result := Assigned(FOnSupport) or (Length(FSupportAddress) > 0);
end;

procedure TJvExceptionLog.LogException;
begin
  OpenLog;
  WriteLogTimeStamp;
  WriteLogString(ExceptionText);
  WriteLogStrings(ExceptionDetails);
  WriteLogStrings(EnvironmentDetails);
  CloseLog;
end;

procedure TJvExceptionLog.OpenLog;
begin
  if LogExceptions and (FLogHandle = INVALID_HANDLE_VALUE) then
    FLogHandle := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ,
      nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

procedure TJvExceptionLog.SendMailReport;
var
  Body: string;
begin
  Body := ExceptionText + AnsiCrLf + ExceptionDetails.Text + EnvironmentDetails.Text;
  TJclEmail.SimpleSendMail(FSupportAddress, FSupportAddress, FSupportSubject,
    Body, Application.Handle);
end;

procedure TJvExceptionLog.ShowException(E: Exception);
var
  DefaultHandling: Boolean;
begin
  ClearDetails;
  FCurrentException := E;
  DefaultHandling := False;
  try
    FLastActiveControl := Screen.ActiveControl;
    FLastActiveForm := Screen.ActiveCustomForm;
    if Assigned(FOnException) then FOnException(E, DefaultHandling);
    if DefaultHandling then
      Application.ShowException(E)
    else
    with TJvExceptionDialog.Create(Application) do
    try
      if ShowModal = mrYes then DoSupport;
    finally
      Free;
    end;
  except
    on E: Exception do Application.ShowException(E);
  end;
end;

procedure TJvExceptionLog.WriteLogString(S: string; AddCRLF: Boolean);
begin
  if FLogHandle <> INVALID_HANDLE_VALUE then
  begin
    if AddCRLF then S := S + AnsiCrLf;
    SetFilePointer(FLogHandle, 0, nil, soFromEnd);
    FileWrite(FLogHandle, Pointer(S)^, Length(S));
  end;
end;

procedure TJvExceptionLog.WriteLogStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do WriteLogString(Strings[I]);
end;

procedure TJvExceptionLog.WriteLogTimeStamp;
var
  S: string;
begin
  S := DateTimeToStr(Now);
  S := AnsiCrLf + S + ' ' + StringOfChar('-', 79 - Length(S));
  WriteLogString(S);
end;

{ TJvExceptionDialog }

procedure TJvExceptionDialog.CreateDetails;
begin
  if DetailsMemo.Lines.Count > 0 then Exit;
  Screen.Cursor := crHourGlass;
  try
    JvExceptionLog.LogException;
    DetailsMemo.Lines.Assign(JvExceptionLog.ExceptionDetails);
    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
    OkBtn.Enabled := True;
    DetailsBtn.Enabled := True;
    SupportBtn.Enabled := True;
    OkBtn.SetFocus;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvExceptionDialog.DetailsBtnClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

procedure TJvExceptionDialog.FormCreate(Sender: TObject);
begin
  DetailsMemo.Anchors := [akLeft, akTop];
  Bevel1.Anchors := [akLeft, akTop];
  TextLabel.Anchors := [akLeft, akTop];
  FFullHeight := ClientHeight;
  DetailsVisible := False;
  Caption := Format(RsAppError, [Application.Title]);
  SupportBtn.Visible := JvExceptionLog.IsSupportAvailable;
  TextLabel.Text := JvExceptionLog.ExceptionText;
end;

procedure TJvExceptionDialog.FormPaint(Sender: TObject);
begin
  DrawIcon(Canvas.Handle, TextLabel.Left - GetSystemMetrics(SM_CXICON) - 15,
    TextLabel.Top, LoadIcon(0, IDI_ERROR));
end;

procedure TJvExceptionDialog.FormShow(Sender: TObject);
begin
  MessageBeep(MB_ICONERROR);
  PostMessage(Handle, UM_CREATEDETAILS, 0, 0);
end;

procedure TJvExceptionDialog.SetDetailsVisible(const Value: Boolean);
var
  DetailsCaption: string;
begin
  FDetailsVisible := Value;
  DetailsCaption := Trim(StrRemoveChars(DetailsBtn.Caption, ['<', '>']));
  if Value then
  begin
    CreateDetails;
    DetailsCaption := '<< ' + DetailsCaption;
    ClientHeight := FFullHeight;
  end else
  begin
    DetailsCaption := DetailsCaption + ' >>';
    with TextLabel do Self.ClientHeight := Top + Height + 7;
  end;
  DetailsBtn.Caption := DetailsCaption;
  DetailsMemo.Enabled := Value;
end;

procedure TJvExceptionDialog.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

initialization
  InitializeHandler;

finalization
  FreeAndNil(JvExceptionLog);

end.
