unit iJvMTracer;
{ интерфейс к Tracer'у  через сообщения}

interface
{$DEFINE DEBUG}

{$IFDEF DEBUG}
uses Windows, Messages, SysUtils {, Forms};

type
  PTracerInfo = ^TTracerInfo;
  TTracerCommand = word;

  TJvTracerSocket  = class
  private
    FPDI : PTracerInfo;
    hFileMap : THandle;
    FEnabled : boolean;
    procedure Post(Command : TTracerCommand; Param : Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Writeln(const S : string);
    procedure Clear;
    procedure TimerStart(Timer : integer);
    procedure TimerStop(Timer : integer);
    property PDI : PTracerInfo read FPDI;
    property Enabled : boolean read FEnabled;
  end;

  TTracerInfo = record
    RemoteAppName : string[255];
    S : array[0..1024*8-1] of Char;
    Data : boolean;
  end;

  TWMTracer = record
    Msg: Cardinal;
    Command : TTracerCommand;
    wParam  : Word;
    Unused: array[0..1] of Word;
    Result: Longint;
  end;

const
  TracerVersion = 2;
  wm_Tracer = wm_User+200 + TracerVersion;
  RATracerSignatur = 'JVCL Tracer Info';

 {Tracer Commands}
  dc_Writeln         = 1;
  dc_Clear           = 2;
  dc_RemoteAppClosed = 3;
  dc_TimerStart      = 4;
  dc_TimerStop       = dc_TimerStart + 1;

  function Tracer : TJvTracerSocket ;

var
  TracerApp : boolean = false;
{$ENDIF DEBUG}

  procedure ODS(S : string);

implementation

{$IFDEF DEBUG}
var
  fJvTracer : TJvTracerSocket  = nil;

constructor TJvTracerSocket .Create;
begin
  inherited Create;
  hFileMap := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, sizeof(TTracerInfo), RATracerSignatur);
  if hFileMap = 0 then exit;
  FPDI := MapViewOfFile(hFileMap, FILE_MAP_WRITE, 0, 0, 0);
  FEnabled := PDI <> nil;
end;

destructor TJvTracerSocket .Destroy;
begin
  if FEnabled then Post(dc_RemoteAppClosed, 0);
  if FPDI <> nil then UnMapViewOfFile(FPDI);
  if hFileMap <> 0 then CloseHandle(hFileMap);
  inherited Destroy;
end;

procedure TJvTracerSocket .Post(Command : TTracerCommand; Param : Word);
var
  H : HWnd;
begin
  H := FindWindow('TJvTracerMain ', nil);
  if H > 0 then begin
    if not TracerApp then FPDI.RemoteAppName := ParamStr(0);
    SendMessage(H, wm_Tracer, Param shl 16 + Command, 0);
    { приходится использовать SendMessage а не PostMessage,
      т.к. оба приложения разделяют один участок памяти
      и если Tracer не будет успевать обрабатывать сообщения
      то они будут затираться новыми.
        Для устранения недостатка можно попробывать организовать
      список необработанных сообщений в разделяемой памяти... }
  end;
end;

procedure TJvTracerSocket .Writeln(const S : string);
begin
  if not FEnabled then exit;
  StrLCopy(FPDI.S, PChar(S), SizeOf(FPDI.S));
  if Length(S) < SizeOf(FPDI.S) then
    FPDI.S[Length(S)] := #00
  else
    FPDI.S[SizeOf(FPDI.S) - 1] := #00;
  Post(dc_Writeln, 0);
end;

procedure TJvTracerSocket .Clear;
begin
  if not FEnabled then exit;
  Post(dc_Clear, 0);
end;

procedure TJvTracerSocket .TimerStart(Timer : integer);
begin
  if not FEnabled then exit;
  Post(dc_TimerStart, Timer);
end;

procedure TJvTracerSocket .TimerStop(Timer : integer);
begin
  if not FEnabled then exit;
  Post(dc_TimerStop, Timer);
end;
{-----------------------------------------------}

function Tracer : TJvTracerSocket ;
begin
  if fJvTracer = nil then
    fJvTracer := TJvTracerSocket .Create;
  Result := fJvTracer;
end;
{$ENDIF DEBUG}

procedure ODS(S : string);
begin
{$IFDEF DEBUG}
  Tracer.Writeln(S);
{$ENDIF DEBUG}
end;

{$IFDEF DEBUG}
initialization
finalization
  fJvTracer.Free;
{$ENDIF DEBUG}

end.
