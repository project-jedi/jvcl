////////////////////////////////////////////////////
//   DSMixer                                      //
//  -------------------------------------------   //
//   ReWrited by Pavel Bibergal                   //
//   cyberkm@barak-online.net                     //
//  --------------------------------------------  //
//   based on Work of Carlos Barbadosa            //
//   Copyright (C) 1999, 2002 Carlos Barbosa      //
////////////////////////////////////////////////////


unit cbDSMixer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  DirectSound, ComObj, MMSystem, cbAudioFileRead,
  Math,dxerr9;

const
  conBUFFER_BREAKS_MIN = 2;
  conBUFFER_BREAKS_MAX = 50;
  conBUFFER_BREAKS_DEFAULT = 10;

type
  TacahLevel = (lNormal, lPriority, lExclusive);
  TacahBitsPerSample = (bs8Bit, bs16Bit);
  TChangedDSEvent = procedure (Sender: TObject; Value: IDirectSound) of object;
  TacfStatus = (sStopped, sPlaying, sPaused);

  TcbDSMixerChannel = class;

  EDirectXError = class(Exception)
  private
    FErrorCode: HRESULT;
  public
    constructor Create( const Msg: String; const ErrorCode: HRESULT ); virtual;
    property ErrorCode: HRESULT read FErrorCode;
  end;

  TPositionTimes = class(TObject)
  private
    FCurrentIdx: Integer;
    FPositions: array [0..conBUFFER_BREAKS_MAX-1, 0..1] of Longint;
    FTimes: array [0..conBUFFER_BREAKS_MAX-1, 0..1] of Int64;
  public
    procedure AddTimes(PosStart, PosEnd: Longint; TimeStart, TimeEnd: Int64);
    function GetMsTime(Pos: Longint): Longint;
  end;

  TcbDSMixer = class(TComponent)
  private
    FDirectSound8: IDirectSound8;
    FPrimaryBuffer: IDirectSoundBuffer;
    FStereo,
    FEnabled: Boolean;
    FDeviceIndex,
    FSamplesPerSec: Integer;
    FCooperationWindow: THandle;
    FSpeakerConfig: DWord;
    FBitsPerSample: TacahBitsPerSample;
    FLevel: TacahLevel;
    FChannelList: TList;

    procedure NeedDirectSound;
    function GetChannelCount: Integer;
    function GetChannels(Index: Integer): TcbDSMixerChannel;
    function GetDeviceCount: Integer;
    function GetDevices(Index: Integer): String;
    function GetDirectSound: IDirectSound;
    function GetPrimaryBuffer: IDirectSoundBuffer;
    function GetCooperationWindow: THandle;
    procedure SetDeviceIndex(Value: Integer);
    procedure SetSamplesPerSec(Value: Longint);
    procedure SetSpeakerConfig(Value: DWord);

    procedure InsertChannel(Channel: TcbDSMixerChannel);
    procedure RemoveChannel(Channel: TcbDSMixerChannel);

  protected
    procedure DoChangedDS; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyDirectSound;

    property Channels[Index: Integer]: TcbDSMixerChannel read GetChannels;
    property ChannelCount: Longint read GetChannelCount;
    property Devices[Index: Integer]: String read GetDevices;
    property DeviceCount: Longint read GetDeviceCount;
    property DeviceIndex: Longint read FDeviceIndex write SetDeviceIndex;
    property DirectSound: IDirectSound read GetDirectSound;
    property PrimaryBuffer: IDirectSoundBuffer read GetPrimaryBuffer;
    property CooperationWindow: THandle read GetCooperationWindow
      write FCooperationWindow;

  published
    property BitsPerSample: TacahBitsPerSample read FBitsPerSample write FBitsPerSample;
    property Stereo: Boolean read FStereo write FStereo;
    property Frequency: Longint read FSamplesPerSec write SetSamplesPerSec;
    property Level: TacahLevel read FLevel write FLevel;
    property SpeakerConfig: DWord read FSpeakerConfig write SetSpeakerConfig;
  end;

  TcbUpdateChannelThread = class(TThread)
  private
    FOwner: TcbDSMixerChannel;
    FInterval: Cardinal;
    FStopEvent: THandle;
    FEnabled: Boolean;
    FMutexObj: THandle;

    procedure SetEnabled(Value: Boolean);

  public
    constructor Create(AOwner: TcbDSMixerChannel; const MutexName: String);
    destructor Destroy; override;
    procedure Execute; override;

    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write FInterval;
  end;

  TcbDSMixerChannel = class(TObject)
  private
    FAOwner: TcbDSMixer;
    FDSBuffer8: IDirectSoundBuffer8;
    FFileName: String;
    FLoop,
    FLocked,
    FPrepared,
    FFading,
    FStopApproach: Boolean;
    FFileSamplesPerSec,
    FFileNumChannels,
    FFileBitsPerSample,
    FFileBlockAlign,
    FFileDuration,
    FRangeStart,
    FRangeEnd,
    FBufferLength,
    FBufferSize,
    FBufferApproachSize,
    FBufferMinFillSize,
    FWritePosition,
    FLastTime,
    FFrequency,
    FFadeTimeStart,
    FFadeTimeEnd: DWord;
    FVolume,
    FPan,
    FTag,
    FFadeVolStart,
    FFadeVolEnd,
    FStopPos: Longint;
    FWaveReader: TcbAudioFileReader;
    FStatus: TacfStatus;
    FTimes: TPositionTimes;
    FUpdater: TcbUpdateChannelThread;
    FBuffer: PChar;
    FOnEndPlay,
    FOnLostBuffer,
    FOnEndFade: TNotifyEvent;
    FMutexObj: THandle;
    FWindowHandle: THandle;

    function GetInterval: DWord;
    function GetPosition: DWord;
    procedure SetBufferLength(Value: DWord);
    procedure SetInterval(Value: DWord);
    procedure SetFileName(Value: String);
    procedure SetFrequency(Value: DWord);
    procedure SetLoop(Value: Boolean);
    procedure SetPan(Value: Longint);
    procedure SetPosition(Value: DWord);
    procedure SetRangeStart(Value: DWord);
    procedure SetRangeEnd(Value: DWord);
    procedure SetVolume(Value: Longint);
    procedure WndProc(var Msg: TMessage);

    procedure OnUpdater;
    procedure InitBufferVars;
    procedure InitFileVars;
    procedure FreeBuffer;
    procedure FreeFile;
    procedure FillBuffer;
    procedure ReadFileToBuffer(Pos, Size: DWord);
    procedure FillSilenceToBuffer(Pos, Size: DWord);
    procedure NeedBuffer;
    procedure NeedFile;

    protected
    procedure DoBufferLost;

  public
    constructor Create(AOwner: TcbDSMixer);
    destructor Destroy; override;

    procedure Fade(Time: DWord; EndVolume: Longint);
    procedure StopFade;
    procedure Pause;
    procedure Play;
    procedure Prepare;
    procedure Stop;
    procedure Lock(Start, Size: DWord;
      var Audio1Ptr: Pointer; var Size1: DWord; var Audio2Ptr: Pointer;
      var Size2: DWord);
    procedure Unlock;

    property FileSamplesPerSec: DWord read FFileSamplesPerSec;
    property FileNumChannels: DWord read FFileNumChannels;
    property FileBitsPerSample: DWord read FFileBitsPerSample;
    property FileBlockAlign: DWord read FFileBlockAlign;
    property FileDuration: DWord read FFileDuration;
    property Status: TacfStatus read FStatus;
    property DSBuffer: IDirectSoundBuffer8 read FDSBuffer8;


    property Position: DWord read GetPosition write SetPosition;

  published
    property BufferLength: DWord read FBufferLength write SetBufferLength;
    property FileName: String read FFileName write SetFileName;
    property Frequency: DWord read FFrequency write SetFrequency;
    property Interval: DWord read GetInterval write SetInterval;
    property Loop: Boolean read FLoop write SetLoop;
    property Pan: Longint read FPan write SetPan;
    property RangeStart: DWord read FRangeStart write SetRangeStart;
    property RangeEnd: DWord read FRangeEnd write SetRangeEnd;
    property Tag: Longint read FTag write FTag;
    property Volume: Longint read FVolume write SetVolume;

    property OnEndFade: TNotifyEvent read FOnEndFade write FOnEndFade;
    property OnEndPlay: TNotifyEvent read FOnEndPlay write FOnEndPlay;
    property OnLostBuffer: TNotifyEvent read FOnLostBuffer write FOnLostBuffer;
    property OnVolumeChange: TNotifyEvent read FOnLostBuffer write FOnLostBuffer;
  end;
  const
  conNUM_SFX = 7;
  
type
  TSFXType = (eSFX_chorus, eSFX_compressor, {eSFX_distortion,} eSFX_echo,
            eSFX_flanger, eSFX_gargle, eSFX_parameq, eSFX_reverb, eSFX_all);


type
  TDSFXManager = class(TComponent)
  private
   FNumberOFEffects: Byte;
   FDSoundBuffer8: IDirectSoundBuffer8;
   FFXDescArray: array [0..conNUM_SFX-1] of TDSEffectDesc;
   FEffectLoadedArray: array [0..conNUM_SFX-1] of bool;
   FGuidesReferences: array [0..conNUM_SFX-1] of TGUID;
   FTypeArray: array [0..conNUM_SFX-1] of TSFXType;
   //----------------Effect Interfaces ---------------------
   FIChorus: IDirectSoundFXChorus8;
   FIEcho: IDirectSoundFXEcho8;
   FICompressor: IDirectSoundFXCompressor8;
   FIReverb: IDirectSoundFXWavesReverb8;
   FIFlanger: IDirectSoundFXFlanger8;
   //FIDistortion: IDirectSoundFXDistortion8;
   FIGargle: IDirectSoundFXGargle8;
   FIParamEQ: IDirectSoundFXParamEq8;

    Function EnableGenericFX(GuidSFXClass: TGuid; GuidInterface: TGUID;
                               ppObject: Pointer; EffectType:TSFXType): Bool;
    procedure SetDSBuffer(const Value: IDirectSoundBuffer8);
    { Private declarations }
  protected
    { Protected declarations }
  public
   Echo: TDSFXEcho;
   Chorus: TDSFXChorus;
   Compressor: TDSFXCompressor;
   Reverb: TDSFXWavesReverb;
   Flanger: TDSFXFlanger;
   //Distortion: TDSFXDistortion;
   Gargle: TDSFXGargle;
   ParamEQ: TDSFXParamEq;

    constructor Create(Aowner: Tcomponent); override;
    destructor Destroy; override;
    procedure SetCurrentParameters(FXType: TSFXType);
    procedure EnableEffect(EffectType: TSFXType);
    procedure LoadDefaultParameters;
    procedure ActivateFX;
    procedure DisableAllFX;
    property DSBuffer: IDirectSoundBuffer8 read FDSoundBuffer8 write SetDSBuffer;

    { Public declarations }
  published




    { Published declarations}
  end;


const
  conBITSPERSAMPLE8 = 8;
  conBITSPERSAMPLE16 = 16;
  conFREQUENCY_MINIMUM = 100;
  conFREQUENCY_MAXIMUM = 100000;
  conSAMPLESPERSEC_MINIMUM = 100;
  conSAMPLESPERSEC_MAXIMUM = 100000;
  conVOLUME_MINIMUM = -10000;
  conMAX_FADE_TIME = 60000;
  conDEFAULT_FREQUENCY = 44100;
  conDEFAULT_SAMPLESPERSEC = 44100;
  conDEFAULT_STEREO = True;
  conDEFAULT_BITSPERSAMPLE = 16;
  conDEFAULT_LEVEL = lPriority;
  conDEFAULT_SPEAKERCONFIG = DSSPEAKER_STEREO;
  conBUFFER_LENGTH_MIN = 1000;
  conBUFFER_LENGTH_MAX = 60000;
  conBUFFER_LENGTH_DEFAULT = 4000;
  conDEFAULT_VOLUME = 0;
  conDEFAULT_PAN = 0;
  conDEFAULT_LOOP = False;
  conDEFAULT_RANGESTART = 0;
  conDEFAULT_RANGEEND = 0;
  conBUFFER_APPROX_LENGTH = 40;    // percentage of buffer, time before end where a break will identify an approach tho stop point
  conBUFFER_MIN_FILL_SIZE = 500; // percentage of interval, should be higher than 100

  conMUTEX_NAME_PREFIX = 'cbDSMCUpd';
  conMUTEX_WAIT = 250;

  conWMUSER_ENDPLAY = WM_USER + 1;
  conWMUSER_ENDFADE = WM_USER + 2;


procedure Register;

implementation

var
  DevicesDescription,
  DevicesGUID: TStringList;


procedure Register;
begin
  RegisterComponents('Additional', [TcbDSMixer]);
end;

procedure DSCheck(Value: HRESULT);
begin
  if Value <> DS_OK then
    raise EDirectXError.Create(DXGetErrorString9(Value), Value );
end;

function DSEnumCallback( GUIDPtr: PGUID ; lpstrDescription: LPSTR ;
  lpstrModule: LPSTR ; lpContext: Pointer ): BOOL ; stdcall;
begin
  // add device to the list
  DevicesDescription.Add(StrPas(lpstrDescription));

  if GUIDPtr = nil then
    DevicesGUID.Add('')
  else
    DevicesGUID.Add(GUIDToString(GUIDPtr^));

  // Next please
  Result := True;
end;

constructor EDirectXError.Create( const Msg: String; const ErrorCode: HRESULT );
begin
  inherited Create( Msg );
  FErrorCode := ErrorCode;
end;

constructor TcbDSMixer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDeviceIndex := 0;
  FEnabled := True;
  FSamplesPerSec := conDEFAULT_SAMPLESPERSEC;
  FStereo := conDEFAULT_STEREO;
  FBitsPerSample := bs16Bit;
  FLevel := conDEFAULT_LEVEL;
  FSpeakerConfig := conDEFAULT_SPEAKERCONFIG;

  FChannelList := TList.Create;
end;

destructor TcbDSMixer.Destroy;
var
  Cnt: Integer;
begin
  DestroyDirectSound;

  for Cnt := 0 to FChannelList.Count-1 do
    TcbDSMixerChannel(FChannelList[Cnt]).Free;
  FChannelList.Free;

  inherited;
end;

procedure TcbDSMixer.InsertChannel(Channel: TcbDSMixerChannel);
begin
  FChannelList.Add(Channel);
end;

procedure TcbDSMixer.RemoveChannel(Channel: TcbDSMixerChannel);
begin
  FChannelList.Remove(Channel);
end;

procedure TcbDSMixer.NeedDirectSound;
var
  g: TGUID;
  pg: PGUID;
  ds: TDSBufferDesc;
  wfx: TWaveFormatEx;
  Level: Integer;
begin
  if (csDesigning in ComponentState) or
    (not FEnabled) or
    Assigned(FDirectSound8) then
    exit;

  { Initialize the DirectSound system }
//  if not Assigned(DirectSoundCreate) then
//    raise EDirectXError.Create('DirectSoundCreate pointer not assigned.', 0);

  if DevicesGUID[FDeviceIndex] = '' then
    pg := nil
  else
  begin
    g := StringToGUID(DevicesGUID[FDeviceIndex]);
    pg := @g;
  end;

  DSCheck(DirectSoundCreate8(pg, FDirectSound8, nil) );

  DoChangedDS;

  case FLevel of
    lNormal: Level := DSSCL_NORMAL;
    lExclusive: Level := DSSCL_EXCLUSIVE;
    else Level := DSSCL_PRIORITY;
  end;
  DSCheck( FDirectSound8.SetCooperativeLevel(GetCooperationWindow, Level) );

  // create primary buffer
  ZeroMemory(@ds, sizeof(ds));
  ds.dwSize := sizeof(ds);
  ds.dwFlags := DSBCAPS_PRIMARYBUFFER;
  DSCheck(FDirectSound8.CreateSoundBuffer(ds, FPrimaryBuffer, nil));

  if (FLevel <> lNormal) then
  begin
    // set primary buffer format
    DSCheck( FPrimaryBuffer.GetFormat(@wfx, sizeof(wfx), nil) );
    if FStereo then
      wfx.nChannels := 2
    else
      wfx.nChannels := 1;
    wfx.wFormatTag:=WAVE_FORMAT_PCM;
    wfx.wBitsPerSample := (8 shl Ord(FBitsPerSample));
    wfx.nSamplesPerSec := FSamplesPerSec;
    wfx.nBlockAlign := (wfx.wBitsPerSample * wfx.nChannels) shr 3;
    wfx.nAvgBytesPerSec := wfx.nSamplesPerSec * wfx.nBlockAlign;
    DSCheck( FPrimaryBuffer.SetFormat(@wfx) );

    // get real format
    DSCheck( FPrimaryBuffer.GetFormat(@wfx, sizeof(wfx), nil) );
    FStereo := wfx.nChannels = 2;
    FBitsPerSample := TacahBitsPerSample(wfx.wBitsPerSample shr 4);
    FSamplesPerSec := wfx.nSamplesPerSec;
  end;

  SetSpeakerConfig(FSpeakerConfig);

  FPrimaryBuffer.Play(0, 0, DSBPLAY_LOOPING);
end;

procedure TcbDSMixer.DestroyDirectSound;
var
  Cnt: Integer;
begin
  for Cnt := 0 to FChannelList.Count-1 do
    TcbDSMixerChannel(FChannelList.Items[Cnt]).FreeBuffer;

  FPrimaryBuffer := nil;

  if Assigned(FDirectSound8) then
  begin
    FDirectSound8 := nil;

    DoChangedDS;
  end;
end;

function TcbDSMixer.GetDirectSound: IDirectSound;
begin
  NeedDirectSound;
  Result := FDirectSound8;
end;

function TcbDSMixer.GetPrimaryBuffer: IDirectSoundBuffer;
begin
  NeedDirectSound;
  Result := FPrimaryBuffer;
end;

function TcbDSMixer.GetCooperationWindow: THandle;
begin
  if FCooperationWindow = 0 then
    FCooperationWindow := GetDesktopWindow;

  Result := FCooperationWindow;
end;

function TcbDSMixer.GetChannelCount: Integer;
begin
  Result := FChannelList.Count;
end;

function TcbDSMixer.GetChannels(Index: Integer): TcbDSMixerChannel;
begin
  Result := TcbDSMixerChannel(FChannelList.Items[Index]);
end;

function TcbDSMixer.GetDeviceCount: Integer;
begin
  Result := DevicesDescription.Count;
end;

function TcbDSMixer.GetDevices(Index: Integer): String;
begin
  Result := DevicesDescription[Index];
end;

procedure TcbDSMixer.SetDeviceIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < DevicesGUID.Count) then
    FDeviceIndex := Value;
end;

procedure TcbDSMixer.SetSamplesPerSec(Value: Longint);
begin
  if (Value < conSAMPLESPERSEC_MINIMUM) then
    FSamplesPerSec := conSAMPLESPERSEC_MINIMUM
  else
    if (Value > conSAMPLESPERSEC_MAXIMUM) then
      FSamplesPerSec := conSAMPLESPERSEC_MAXIMUM
    else
      FSamplesPerSec := Value;
end;

procedure TcbDSMixer.SetSpeakerConfig(Value: DWord);
begin
  if Assigned(FDirectSound8) then
    DSCheck( FDirectSound8.SetSpeakerConfig(Value) );

  FSpeakerConfig := Value;
end;

procedure TcbDSMixer.DoChangedDS;
begin
end;

{ TAudioPositions }
procedure TPositionTimes.AddTimes(PosStart, PosEnd: Longint;
  TimeStart, TimeEnd: Int64);
begin
  FPositions[FCurrentIdx, 0] := PosStart;
  FPositions[FCurrentIdx, 1] := PosEnd;
  FTimes[FCurrentIdx, 0] := TimeStart;
  FTimes[FCurrentIdx, 1] := TimeEnd;

  Assert(TimeStart <= TimeEnd, 'Internal Error: Time Start higher then Time End');
  Assert(PosStart < posEnd, 'Internal Error: Position Start not inferior to Position End');

  FCurrentIdx := (FCurrentIdx+1) mod conBUFFER_BREAKS_MAX;
end;

function TPositionTimes.GetMsTime(Pos: Longint): Integer;
var
  Idx: Integer;
  LLResult: Int64;
begin
  Idx := FCurrentIdx;
  repeat
    Dec(Idx);
    if Idx < 0 then Idx := conBUFFER_BREAKS_MAX - 1;
    Assert(Idx <> FCurrentIdx, 'Internal Error: unable to GetPosTime');
  until (FPositions[Idx,0] <= Pos) and (FPositions[Idx,1] > Pos);

  // Result = ti + (tf-ti)*(p-pi)/(pf-pi)
  LLResult := FTimes[Idx, 1] - FTimes[Idx, 0];
  Result := (Trunc(((Pos - FPositions[Idx, 0]) /
    (FPositions[Idx, 1] - FPositions[Idx, 0])) * LLResult) +
    FTimes[Idx, 0]) div MUNITS_PER_MSEC;
end;


{ TcbUpdateChannelThread }

constructor TcbUpdateChannelThread.Create(AOwner: TcbDSMixerChannel;
  const MutexName: String);
begin
  inherited Create(True);

  FOwner := AOwner;

  FStopEvent := CreateEvent(nil, False, False, nil);

  FMutexObj := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MutexName));

  Resume;
end;

destructor TcbUpdateChannelThread.Destroy;
begin
  // order to terminate
  Terminate;

  // trigger stop event
  if (FStopEvent <> 0) then
    SetEvent(FStopEvent);

  // wait till thread ends
  if Suspended then
    Resume;
  WaitFor;

  // close mutex object
  if (FMutexObj <> 0) then
    CloseHandle(FMutexObj);

  // close event
  if (FStopEvent <> 0) then
    CloseHandle(FStopEvent);

  inherited Destroy;
end;

procedure TcbUpdateChannelThread.Execute;
begin
  repeat
    if not FEnabled then
      WaitForSingleObject(FStopEvent, INFINITE)
    else
      if (WaitForSingleObject(FStopEvent, FInterval) = WAIT_TIMEOUT) then
        if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
        try
          if FEnabled then
            FOwner.OnUpdater;
        finally
          ReleaseMutex(FMutexObj);
        end;
  until Terminated;
end;

procedure TcbUpdateChannelThread.SetEnabled(Value: Boolean);
begin
  if (Value <> FEnabled) then
  begin
    FEnabled := Value;
    if FEnabled then
      SetEvent(FStopEvent);
  end;
end;


{ TcbDSMixerChannel }

constructor TcbDSMixerChannel.Create(AOwner: TcbDSMixer);
var
  MId: Integer;
  MutexName: String;
begin
  inherited Create;

  FAOwner := AOwner;

  // Create object to maintain time points
  FTimes := TPositionTimes.Create;

  InitBufferVars;
  InitFileVars;

  // create window to receive messages from updater thread
  FWindowHandle := AllocateHWnd(WndProc);

  // create mutex object to control access from user and updater thread
  MId := 0;
  while (FMutexObj = 0) do
  begin
    MutexName := conMUTEX_NAME_PREFIX + IntToStr(MId);
    FMutexObj := CreateMutex(nil, False, PChar(MutexName));
    Inc(MId);
  end;

  FUpdater := TcbUpdateChannelThread.Create(Self, MutexName);
  FUpdater.Enabled := False;

  FBufferLength := conBUFFER_LENGTH_DEFAULT;
  FUpdater.Interval := FBufferLength div conBUFFER_BREAKS_DEFAULT;

  FAOwner.InsertChannel(Self);
end;

destructor TcbDSMixerChannel.Destroy;
begin
  FAOwner.RemoveChannel(Self);
  FUpdater.Enabled := False;

  FreeBuffer;
  FreeFile;

  if (FMutexObj <> 0) then
    CloseHandle(FMutexObj);
    
  FreeAndNil(FUpdater);

  FreeAndNil(FTimes);

  if (FWindowHandle <> 0) then
    DeallocateHWnd(FWindowHandle);

  inherited Destroy;
end;

procedure TcbDSMixerChannel.InitBufferVars;
begin
  FBuffer := nil;
  FBufferApproachSize := 0;
  FBufferMinFillSize := 0;
  FStatus := sStopped;
  FWritePosition := 0;
  FPrepared := False;
  FStopPos := -1;
  FStopApproach := False;
  FLocked := False;
end;

procedure TcbDSMixerChannel.InitFileVars;
begin
  FVolume := conDEFAULT_VOLUME;
  FFrequency := conDEFAULT_FREQUENCY;
  FPan := conDEFAULT_PAN;
  FLoop := conDEFAULT_LOOP;
  FRangeStart := conDEFAULT_RANGESTART;
  FRangeEnd := conDEFAULT_RANGEEND;
  FFileName := '';
end;

procedure TcbDSMixerChannel.FreeBuffer;
begin
  if Assigned(FDSBuffer8) then
  begin
    Stop;
    FDSBuffer8 := nil;
  end;

  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;

  InitBufferVars;
end;

procedure TcbDSMixerChannel.FreeFile;
begin
  FreeBuffer;

  if Assigned(FWaveReader) then
  begin
    FWaveReader.Free;
    FWaveReader := nil;
  end;

  InitFileVars;
end;

//function TcbDSMixerChannel.MSecToByte(Time: DWord): DWord;
//begin
//  Result := ((Time * FWaveReader.Format.nAvgBytesPerSec) div 1000)
//    and (not (FWaveReader.Format.nBlockAlign-1));
//end;

procedure TcbDSMixerChannel.OnUpdater;
var
  CurrentTime: DWord;
  PosDif: DWord;

  function GetPosDiff: Longint;
  var
    CurrentPos: Longint;
  begin
    DSCheck( FDSBuffer8.GetCurrentPosition(@CurrentPos, nil) );
    Result := Integer(FStopPos) - CurrentPos;
    if Result <= 0 then
      Inc(Result, FBufferSize);
  end;

begin
  PosDif := 0;

  if FStopApproach then
  // approaching stop point, check if it should stop
  begin
    PosDif := GetPosDiff;

    if PosDif > FBufferApproachSize then
    // Over stop point
    begin
      Stop;
      PostMessage(FWindowHandle, conWMUSER_ENDPLAY, 0, 0);
      exit;
    end;
  end;

  CurrentTime := GetTickCount();
  if CurrentTime - FLastTime >= FUpdater.Interval then
  begin
    FLastTime := CurrentTime;

    if FStopPos <> -1 then
    begin
      if PosDif = 0 then
        PosDif := GetPosDiff;

      if PosDif <= FBufferApproachSize then
        FStopApproach := True;
    end;

    FillBuffer;
  end;

  // Adjust volume to achieve fading
  if FFading and (FStatus = sPlaying) and Assigned(FDSBuffer8) then
  begin
    if (CurrentTime >= FFadeTimeEnd) then
    begin
      FFading := False;

      SetVolume(FFadeVolEnd);

      // finished fading
      PostMessage(FWindowHandle, conWMUSER_ENDFADE, 0, 0);
    end
    else
      SetVolume(FFadeVolStart + (FFadeVolEnd - FFadeVolStart) *
        Longint(CurrentTime - FFadeTimeStart) div
        Longint(FFadeTimeEnd - FFadeTimeStart));
  end;
end;

procedure TcbDSMixerChannel.NeedFile;
begin
  if not Assigned(FWaveReader) then
    raise Exception.Create('No file opened');
end;

procedure TcbDSMixerChannel.NeedBuffer;
var
  dsbd: TDSBufferDesc;
  dsbuffer: IDirectSoundBuffer;
begin
  // if DirectSoundBuffer already created exit because nothing to do
  if Assigned(FDSBuffer8) then exit;

  // dependent of file and DirectSound interface
  NeedFile;

   //Create the secondary DirectSoundBuffer object to receive our sound data.
  dsbd.dwSize := sizeof( TDSBufferDesc );

  // Use new GetCurrentPosition() accuracy (DirectX 2 feature)
  FBufferSize := (FBufferLength * FWaveReader.Format.nAvgBytesPerSec) div
    1000;

  FBufferApproachSize := (conBUFFER_APPROX_LENGTH * FBufferSize) div 100;
  FBufferMinFillSize := (conBUFFER_MIN_FILL_SIZE * FUpdater.Interval *
    FWaveReader.Format.nAvgBytesPerSec) div (1000 * 100);
  GetMem(FBuffer, FBufferSize);
  dsbd.dwFlags := DSBCAPS_CTRLFX or DSBCAPS_CTRLPAN or DSBCAPS_CTRLVOLUME
  or DSBCAPS_CTRLFREQUENCY or DSBCAPS_GETCURRENTPOSITION2
  or  DSBCAPS_GLOBALFOCUS;
  dsbd.dwBufferBytes := FBufferSize;
  dsbd.dwReserved := 0;

  //Set Format properties according to the WAVE file we just opened
  dsbd.lpwfxFormat := @FWaveReader.Format;

  // create buffer interface
  DSCheck( FAOwner.DirectSound.CreateSoundBuffer( dsbd, dsbuffer, nil ) );
  dsbuffer.QueryInterface(IID_IDirectSoundBuffer8, FDSBuffer8);

  // set initial params
  SetVolume(FVolume);
  SetPan(FPan);
end;

procedure TcbDSMixerChannel.FillBuffer;
// Fills the buffer with data from the audio file
var
  Diff: Integer;
  CurrentPos: DWord;
begin
  NeedBuffer;

  if FPrepared then
  begin
    DSCheck( FDSBuffer8.GetCurrentPosition(@CurrentPos, nil) );

    if FLocked then
    // can't overwrite buffer
    begin
      Diff := FWritePosition - CurrentPos;
      if Diff <= 0 then
        Inc(Diff, FBufferSize);

      if DWord(Diff) < FBufferMinFillSize then
      // keep a minimum buffer ahead filled with silence
      begin
        if FWritePosition + FBufferMinFillSize > FBufferSize then
        begin
          FillSilenceToBuffer(FWritePosition, FBufferSize-FWritePosition);
          FillSilenceToBuffer(0, FBufferMinFillSize -
            (FBufferSize-FWritePosition));
        end
        else
          FillSilenceToBuffer(FWritePosition, FBufferMinFillSize);

        Inc(FWritePosition, FBufferMinFillSize);
        if FWritePosition >= FBufferSize then
          Dec(FWritePosition, FBufferSize);
      end;
    end
    else
    // fill buffer
    begin
      if CurrentPos >= FWritePosition then
        ReadFileToBuffer(FWritePosition, CurrentPos - FWritePosition)
      else
      begin
        ReadFileToBuffer(FWritePosition, FBufferSize - FWritePosition);
        ReadFileToBuffer(0, CurrentPos);
      end;

      FWritePosition := CurrentPos;
    end;
  end
  else
  begin
    ReadFileToBuffer(0, FBufferSize);

    FPrepared := True;
  end;
end;


procedure TcbDSMixerChannel.ReadFileToBuffer(Pos, Size: DWord);
// Pos: position on buffer in bytes
// Size: size in bytes
var
  ToWrite: Longint;
  AudioPtr,
  Audio2Ptr: Pointer;
  cbActual,
  AudioBytes,
  Audio2Bytes,
  PosWrite: DWord;
begin
  if Size = 0 then
    exit;

  // Fill local buffer
  PosWrite := Pos;
  ToWrite := Size;
  repeat
    Assert(ToWrite > 0, 'Read from file not higher than 0');

    AudioPtr := FBuffer + PosWrite;

    if FStopPos = -1 then
    // no stop point defined yet
    begin
      cbActual := FWaveReader.Read(AudioPtr^, ToWrite);

      if cbActual > 0 then
      begin
        // save position times
        FTimes.AddTimes(PosWrite, PosWrite + cbActual,
          FWaveReader.LastReadingStartTime,
          FWaveReader.LastReadingEndTime);

        Dec(ToWrite, cbActual);
      end;

      if ToWrite > 0 then
      begin
        Inc(PosWrite, cbActual);

        // set stop position
        if not FLoop then
          FStopPos := PosWrite;

        // rewind file
        FWaveReader.Position := 0;
      end;
    end
    else
    begin
      // fill with silence
      if FWaveReader.Format.wBitsPerSample = 8 then
        FillMemory(AudioPtr, ToWrite, 128)
      else
        ZeroMemory(AudioPtr, ToWrite);

      FTimes.AddTimes(PosWrite, Longint(PosWrite) + ToWrite,
        FWaveReader.LastReadingEndTime, FWaveReader.LastReadingEndTime);
      ToWrite := 0;
    end;
  until ToWrite = 0;

  // Copy from local buffer to DirectSound secondary buffer
  try
    DSCheck( FDSBuffer8.Lock(Pos, Size, AudioPtr, AudioBytes,
      Audio2Ptr, Audio2Bytes, 0) );
  except
    on e: EDirectXError do
      if e.ErrorCode = DSERR_BUFFERLOST then
      begin
        DoBufferLost;
        exit;
      end
      else
        raise;
  end;
  try
    Assert(AudioBytes = Size, 'Locked different size from requested');
    CopyMemory(AudioPtr, FBuffer + Pos, Size);
  finally
    FDSBuffer8.Unlock(AudioPtr, AudioBytes, nil, 0);
  end;
end;

procedure TcbDSMixerChannel.FillSilenceToBuffer(Pos, Size: DWord);
// Pos: position on buffer in bytes
// Size: size in bytes
var
  AudioPtr,
  Audio2Ptr: Pointer;
  AudioBytes,
  Audio2Bytes: DWord;
begin
  try
    DSCheck( FDSBuffer8.Lock(Pos, Size, AudioPtr, AudioBytes,
      Audio2Ptr, Audio2Bytes, 0) );
  except
    on e: EDirectXError do
      if e.ErrorCode = DSERR_BUFFERLOST then
      begin
        DoBufferLost;
        exit;
      end
      else
        raise;
  end;
  try
    Assert(AudioBytes = Size, 'Locked different size from requested');
    ZeroMemory(AudioPtr, Size);
  finally
    FDSBuffer8.Unlock(AudioPtr, AudioBytes, nil, 0);
  end;

  FTimes.AddTimes(Pos, Pos + Size,
    FWaveReader.LastReadingEndTime, FWaveReader.LastReadingEndTime);
end;

procedure TcbDSMixerChannel.DoBufferLost;
begin
  FreeBuffer;
  if Assigned(FOnLostBuffer) then
    FOnLostBuffer(self);
end;

function TcbDSMixerChannel.GetInterval: DWord;
begin
  Result := FUpdater.Interval;
end;

function TcbDSMixerChannel.GetPosition: DWord;
// Returns the audio position in miliseconds
var
  CurrentPos: DWord;
begin
  Result := 0;
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if (FPrepared) then
    begin
      Assert(Assigned(FDSBuffer8));
      DSCheck( FDSBuffer8.GetCurrentPosition(@CurrentPos, nil) );
      Result := FTimes.GetMsTime(CurrentPos);
    end
    else
      if Assigned(FWaveReader) then
        Result := Floor(FWaveReader.Position div MUNITS_PER_MSEC);
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetBufferLength(Value: DWord);
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if not Assigned(FBuffer) then
    begin
      if (Value > conBUFFER_LENGTH_MAX) then
        Value := conBUFFER_LENGTH_MAX;
      if (Value < conBUFFER_LENGTH_MIN) then
        Value := conBUFFER_LENGTH_MIN;
      FBufferLength := Value;

      // adjust interval to make sure it's within imposed limits
      SetInterval(FUpdater.Interval);
    end;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetFrequency(Value: DWord);
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    FFrequency := Value;

    if Assigned(FDSBuffer8) then
      DSCheck( FDSBuffer8.SetFrequency(FFrequency) );
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetInterval(Value: DWord);
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if not Assigned(FBuffer) and (Value > 0) then
    begin
      if ((FBufferLength div Value) > conBUFFER_BREAKS_MAX) then
        Value := FBufferLength div conBUFFER_BREAKS_MAX;
      if ((FBufferLength div Value) < conBUFFER_BREAKS_MIN) then
        Value := FBufferLength div conBUFFER_BREAKS_MIN;

      FUpdater.Interval := Value;
    end;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetPosition(Value: DWord);
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if Assigned(FWaveReader) then
    begin
      Stop;

      FWaveReader.Position := Value * MUNITS_PER_MSEC;
    end;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetRangeStart(Value: DWord);
var
  Pos: Int64;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if Assigned(FWaveReader) then
    begin
      Pos := Value * MUNITS_PER_MSEC;
      if (FWaveReader.SelStart <> Pos) then
      begin
        FWaveReader.SelStart := Pos;
        FRangeStart := Pos div MUNITS_PER_MSEC;

        SetRangeEnd(FRangeEnd);
      end;
    end
    else
      FRangeStart := Value;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetRangeEnd(Value: DWord);
var
  Pos: Int64;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if Assigned(FWaveReader) then
    begin
      Pos := Value * MUNITS_PER_MSEC - FWaveReader.SelStart;
      if (FWaveReader.SelLength <> Pos) then
      begin
        FWaveReader.SelLength := Pos;
        FRangeEnd := FRangeStart + (Pos div MUNITS_PER_MSEC);
      end;
    end
    else
      FRangeEnd := Value;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetLoop( Value: Boolean );
begin
  FLoop := Value;
end;

procedure TcbDSMixerChannel.SetFileName(Value: String);
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    FreeBuffer;

    FreeFile;

    if Value <> '' then
    begin
      // Open audio file
      FWaveReader := TcbAudioFileReader.Create(Value);

      // Set size of audio file in miliseconds
      FFileDuration := FWaveReader.Duration div MUNITS_PER_MSEC;
      if (FFileDuration = 0) then
        raise Exception.Create('Audio file is empty.');

      // validates
      if (FWaveReader.Format.nChannels <> 1) and
        (FWaveReader.Format.nChannels <> 2) then
        raise Exception.Create('Invalid number of channels.');

      if (FWaveReader.Format.wBitsPerSample <> 8) and
        (FWaveReader.Format.wBitsPerSample <> 16) then
        raise Exception.Create('Invalid number of bits per sample.');

      // Get audio file format
      FFileSamplesPerSec := FWaveReader.Format.nSamplesPerSec;
      FFileNumChannels := FWaveReader.Format.nChannels;
      FFileBitsPerSample := FWaveReader.Format.wBitsPerSample;
      FFileBlockAlign := FWaveReader.Format.nBlockAlign;
    end;

    SetRangeStart(FRangeStart);
    SetRangeEnd(FRangeEnd);

    FFileName := Value;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetPan( Value: Longint );
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    FPan := Value;
    if Assigned(FDSBuffer8) then
      DSCheck( FDSBuffer8.SetPan(FPan) );
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.SetVolume(Value: Longint);
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    FVolume := Value;

    if Assigned(FDSBuffer8) then
      DSCheck( FDSBuffer8.SetVolume(FVolume) );
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.Fade(Time: DWord; EndVolume: Longint);
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if (EndVolume > 0) or (EndVolume < conVOLUME_MINIMUM) then
      raise Exception.Create('Fade error: EndVolume out of range');

    if (Time > conMAX_FADE_TIME) then
      raise Exception.Create('Fade error: Too long fade time');

    if (FStatus = sPlaying) then
    begin
      FFadeTimeStart := GetTickCount;
      FFadeTimeEnd := FFadeTimeStart + Time;
      FFadeVolStart := FVolume;
      FFadeVolEnd := EndVolume;
      FFading := True;
    end;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.StopFade;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if (FFading) then
      FFading := False;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.Pause;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    NeedBuffer;

    if FStatus <> sPlaying then
      exit;

    FUpdater.Enabled := False;
    DSCheck( FDSBuffer8.Stop );
    FStatus := sPaused;
    FFading := False;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.Play;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if FStatus = sPlaying then
      exit;

    NeedBuffer;
    Prepare;
    try
      DSCheck( FDSBuffer8.Play(0, 0, DSBPLAY_LOOPING) );
    except
      on e: EDirectXError do
        if e.ErrorCode = DSERR_BUFFERLOST then
        begin
          DoBufferLost;
          exit;
        end
        else
          raise;
    end;
    FStatus := sPlaying;
    FUpdater.Enabled := True;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.Prepare;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    if not FPrepared then
      FillBuffer;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.Stop;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    // if no buffer just leave
    if not Assigned(FDSBuffer8) then exit;

    FUpdater.Enabled := False;
    DSCheck( FDSBuffer8.Stop );
    FStatus := sStopped;
    FFading := False;
    DSCheck( FDSBuffer8.SetCurrentPosition(0) );
    FWritePosition := 0;
    FWaveReader.Position := 0;
    FPrepared := False;
    FStopPos := -1;
    FStopApproach := False;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.Lock(Start, Size: DWord;
  var Audio1Ptr: Pointer; var Size1: DWord; var Audio2Ptr: Pointer;
  var Size2: DWord);
// Obtains a read pointer to the sound buffer's audio data starting
// at the current play position + <Start> bytes of <Size> bytes
var
  ValidSize: Longint;
  CurrentPos,
  InitPosition: DWord;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    FLocked := False;
    Audio1Ptr := nil;
    Audio2Ptr := nil;
    Size1 := 0;
    Size2 := 0;

    if not FPrepared then
      exit;

    // get's current play position
    DSCheck( FDSBuffer8.GetCurrentPosition(@CurrentPos, nil) );

    // calc size between play and write position
    if CurrentPos > FWritePosition then
      ValidSize := FWritePosition  + FBufferSize - CurrentPos
    else
      ValidSize := FWritePosition - CurrentPos;

    // decrement start bytes
    Dec(ValidSize, Start);
    if ValidSize <= 0 then
      exit;

    if Size > DWord(ValidSize) then
      Size := DWord(ValidSize);

    // Calc start buffer position
    InitPosition := (CurrentPos + Start);
    if (InitPosition >= FBufferSize) then
      Dec(InitPosition, FBufferSize);

    // Set output variables
    Audio1Ptr := FBuffer + InitPosition;
    Size1 := FBufferSize - InitPosition;
    if Size1 >= Size then
      Size1 := Size
    else
    begin
      Audio2Ptr := FBuffer;
      Size2 := Size - Size1;
    end;

    FLocked := True;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.Unlock;
begin
  if (WaitForSingleObject(FMutexObj, conMUTEX_WAIT) <> WAIT_TIMEOUT) then
  try
    FLocked := False;
  finally
    ReleaseMutex(FMutexObj);
  end;
end;

procedure TcbDSMixerChannel.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    conWMUSER_ENDPLAY:
      if Assigned(FOnEndPlay) then
        FOnEndPlay(Self);
    conWMUSER_ENDFADE:
      if Assigned(FOnEndFade) then
        FOnEndFade(Self);
  end;
end;

procedure TDSFXManager.ActivateFX;
var dwResults: array [0..conNUM_SFX-1] of dword;
    i: Byte;

begin
 if not Assigned(FDSoundBuffer8) then Exit;
 if FNumberOFEffects = 0 then Exit;
 DSCheck(FDSoundBuffer8.SetFX(FNumberOFEffects, @FFXDescArray[0], @dwResults));
 for i:= 0 to FNumberOFEffects-1 do
 begin
   case FTypeArray[i] of
   eSFX_chorus:
   dscheck(FDSoundBuffer8.GetObjectInPath(FFXDescArray[i].guidDSFXClass,0,
                          FGuidesReferences[i],FIChorus));
   eSFX_compressor:
   dscheck(FDSoundBuffer8.GetObjectInPath(FFXDescArray[i].guidDSFXClass,0,
                          FGuidesReferences[i],FICompressor));
   eSFX_echo:
   dscheck(FDSoundBuffer8.GetObjectInPath(FFXDescArray[i].guidDSFXClass,0,
                          FGuidesReferences[i],FIEcho));
   eSFX_flanger:
   dscheck(FDSoundBuffer8.GetObjectInPath(FFXDescArray[i].guidDSFXClass,0,
                          FGuidesReferences[i],FIFlanger));
   eSFX_gargle:
   dscheck(FDSoundBuffer8.GetObjectInPath(FFXDescArray[i].guidDSFXClass,0,
                          FGuidesReferences[i],FIGargle));
   eSFX_parameq:
   dscheck(FDSoundBuffer8.GetObjectInPath(FFXDescArray[i].guidDSFXClass,0,
                          FGuidesReferences[i],FIParamEQ));
   eSFX_reverb:
   dscheck(FDSoundBuffer8.GetObjectInPath(FFXDescArray[i].guidDSFXClass,0,
                          FGuidesReferences[i],FIReverb));

 end;
 end;
end;

constructor TDSFXManager.Create;

begin
  inherited;
  ZeroMemory(@FFXDescArray[0], SizeOf(TDSEFFECTDESC)* conNUM_SFX);
  ZeroMemory(@FGuidesReferences[0], SizeOf(TGUID)*conNUM_SFX);
  ZeroMemory(@FTypeArray[0], SizeOf(TSFXType)*conNUM_SFX);
  ZeroMemory(@FEffectLoadedArray[0], SizeOf(bool)*conNUM_SFX);

end;

destructor TDSFXManager.Destroy;

begin
 DisableAllFX;
 inherited;
end;

procedure TDSFXManager.DisableAllFX;
begin
if not Assigned(FDSoundBuffer8) then Exit;
FIChorus:=nil;
FIEcho:=nil;
FICompressor:=nil;
FIReverb:=nil;
FIFlanger:=nil;
//FIDistortion:=nil;
FIGargle:=nil;
FIParamEQ:=nil;

ZeroMemory(@FFXDescArray[0], SizeOf(TDSEFFECTDESC)* conNUM_SFX);
ZeroMemory(@FGuidesReferences[0], SizeOf(TGUID)*conNUM_SFX);
ZeroMemory(@FTypeArray[0], SizeOf(TSFXType)*conNUM_SFX);
ZeroMemory(@FEffectLoadedArray[0], SizeOf(bool)*conNUM_SFX);

FNumberOFEffects:=0;
FDSoundBuffer8.SetFX(0,nil,nil);
end;

Function TDSFXManager.EnableGenericFX(GuidSFXClass, GuidInterface: TGUID;
  ppObject: Pointer; EffectType: TSFXType): Bool;
begin
 Result:=False;
 if not assigned(ppObject) then Exit;
 if FNumberOFEffects >= conNUM_SFX then Exit;
 ZeroMemory(@FFXDescArray[FNumberOfEffects], SizeOf(TDSEFFECTDESC));
 FFXDescArray[FNumberOFEffects].dwSize:=SizeOf(TDSEFFECTDESC);
 FFXDescArray[FNumberOFEffects].dwFlags:=0;
 FFXDescArray[FNumberOFEffects].guidDSFXClass:=GuidSFXClass;
 FGuidesReferences[FNumberOFEffects]:=GuidInterface;
 FTypeArray[FNumberOFEffects]:=EffectType;
 Inc(FNumberOFEffects);
 Result:=True;
end;

procedure TDSFXManager.LoadDefaultParameters;
var i: Byte;
begin
if not Assigned(FDSoundBuffer8) then Exit;
 for i:= 0 to conNUM_SFX-1 do EnableEffect(TSFXType(i));
 ActivateFX;
 if Assigned(FIChorus) then FIChorus.GetAllParameters(Chorus);
 if Assigned(FICompressor) then FICompressor.GetAllParameters(Compressor);
// if Assigned(FIDistortion) then FIDistortion.SetAllParameters(FPDistortion);
 if Assigned(FIEcho) then FIEcho.GetAllParameters(Echo);
 if Assigned(FIFlanger) then FIFlanger.GetAllParameters(Flanger);
 if Assigned(FIGargle) then FIGargle.GetAllParameters(Gargle);
 if Assigned(FIParamEQ) then FIParamEQ.GetAllParameters(ParamEQ);
 if Assigned(FIReverb) then FIReverb.GetAllParameters(Reverb);
 DisableAllFX;
end;

procedure TDSFXManager.SetCurrentParameters(FXType: TSFXType);
begin
 if Assigned(FIChorus) then FIChorus.SetAllParameters(Chorus);
 if Assigned(FICompressor) then FICompressor.SetAllParameters(Compressor);
// if Assigned(FIDistortion) then FIDistortion.SetAllParameters(FPDistortion);
 if Assigned(FIEcho) then FIEcho.SetAllParameters(Echo);
 if Assigned(FIFlanger) then FIFlanger.SetAllParameters(Flanger);
 if Assigned(FIGargle) then FIGargle.SetAllParameters(Gargle);
 if Assigned(FIParamEQ) then FIParamEQ.SetAllParameters(ParamEQ);
 if Assigned(FIReverb) then FIReverb.SetAllParameters(Reverb);
end;

procedure TDSFXManager.SetDSBuffer(const Value: IDirectSoundBuffer8);
begin
if Assigned(Value) then
   FDSoundBuffer8:=Value;
end;

procedure TDSFXManager.EnableEffect(EffectType: TSFXType);
begin

if Integer(EffectType) > conNUM_SFX then Exit;
if FEffectLoadedArray[Integer(EffectType)] then Exit
  else FEffectLoadedArray[Integer(EffectType)]:=True;

case EffectType of
 eSFX_chorus:
   EnableGenericFX(GUID_DSFX_STANDARD_CHORUS,IID_IDirectSoundFXChorus8,
                      @FIChorus, TSFXType(effecttype));
 eSFX_compressor:
   EnableGenericFX(GUID_DSFX_STANDARD_COMPRESSOR, IID_IDirectSoundFXCompressor8,
                      @FICompressor, TSFXType(effecttype));
// eSFX_distortion:
//   EnableGenericFX(GUID_DSFX_STANDARD_DISTORTION, IID_IDirectSoundFXDistortion8,
//                      @FPDistortion);
 eSFX_echo:
   EnableGenericFX(GUID_DSFX_STANDARD_ECHO, IID_IDirectSoundFXEcho8,
                      @FIEcho, TSFXType(effecttype));

 eSFX_flanger:
   EnableGenericFX(GUID_DSFX_STANDARD_FLANGER, IID_IDirectSoundFXFlanger8,
                      @FIFlanger, TSFXType(effecttype));
 eSFX_gargle:
   EnableGenericFX(GUID_DSFX_STANDARD_GARGLE, IID_IDirectSoundFXGargle8,
                      @FIGargle, TSFXType(effecttype));
 eSFX_parameq:
   EnableGenericFX(GUID_DSFX_STANDARD_PARAMEQ, IID_IDirectSoundFXParamEq8,
                      @FIParamEQ, TSFXType(effecttype));
 eSFX_reverb:
   EnableGenericFX(GUID_DSFX_WAVES_REVERB, IID_IDirectSoundFXWavesReverb8,
                      @FIReverb, TSFXType(effecttype));
end;
end;


initialization
DevicesDescription := TStringList.Create;
DevicesGUID := TStringList.Create;
DSCheck( DirectSoundEnumerate(DSEnumCallback, nil) );

finalization
DevicesGUID.Free;
DevicesDescription.Free;

end.
