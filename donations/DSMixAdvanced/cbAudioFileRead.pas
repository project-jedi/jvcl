////////////////////////////////////////////////////
//                                                //
//   cbDSMixer 1.5                                //
//                                                //
//   Copyright (C) 1999, 2001 Carlos Barbosa      //
//   email: delphi@carlosb.com                    //
//   Home Page: http://www.carlosb.com            //
//                                                //
//   Last updated: September 6, 2001              //
//                                                //
////////////////////////////////////////////////////

unit cbAudioFileRead;

interface

uses
  Windows, Classes, MMSystem, SysUtils, DirectShow9, Dialogs, ActiveX, Math;

const
  MUNITS_PER_MSEC: int64 = 10000;
  MIN_SEL_LENGTH: int64 = 100000;  // 0.01 secs
  MAX_SEL_LENGTH: int64 = 1000000000000000;

  DSUSER_HRESULT = HResult($08000000);
  DSUSER_INVALIDSIZE = DSUSER_HRESULT + 1;

type
  EAudioReadError = class(Exception)
  private
    FErrorCode: HRESULT;
  public
    constructor Create( const Msg: String; const ErrorCode: HRESULT ); virtual;
    property ErrorCode: HRESULT read FErrorCode;
  end;

  TcbAudioFileReader = class(TObject)
  private
    FFormat: TWaveFormatEx;
    FFileName: TFileName;
    FAMMultiMediaStream: IAMMultiMediaStream;
    FGraphBuilder: IGraphBuilder;
    FMediaSeeking: IMediaSeeking;
    FMediaControl: IMediaControl;
    FAudioMediaStream: IAudioMediaStream;
    FMediaStream: IMediaStream;
    FAudioStreamSample: IAudioStreamSample;
    FAudioData: IAudioData;
    FBuffer: Pointer;
    FBufferSize: DWord;
    FDuration,
    FPosition,
    FSelStart,
    FSelLength,
    FLastReadingStartTime,
    FLastReadingEndTime: int64;

    procedure SetPosition( Value: int64 );
    procedure SetSelLength( Value: int64 );
    procedure SetSelStart( Value: int64 );

  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    function Read( const Buffer; Size: DWord ): DWord;

    property Duration: int64 read FDuration;
    property FileName: TFileName read FFileName;
    property Format: TWaveFormatEx read FFormat;
    property LastReadingEndTime: int64 read FLastReadingEndTime;
    property LastReadingStartTime: int64 read FLastReadingStartTime;
    property Position: int64 read FPosition write SetPosition;
    property SelLength: int64 read FSelLength write SetSelLength;
    property SelStart: int64 read FSelStart write SetSelStart;
  end;

function DirectShowPresent: Boolean;


implementation

constructor EAudioReadError.Create( const Msg: String; const ErrorCode: HRESULT );
begin
  inherited Create( Msg );
  FErrorCode := ErrorCode;
end;

function SCheck( Value: HRESULT ): HRESULT; { Check the result of a COM operation }
var
  S: String;
  S2: array [0..300] of Char;
begin
  Result := Value;

  if (Value <> S_OK) then
  begin
    Case DWord(Value) of
      DSUSER_INVALIDSIZE: S:='Invalid buffer size.';
      DWord(REGDB_E_CLASSNOTREG): S:='A specified class is not registered in the registration database.';
      DWord(CLASS_E_NOAGGREGATION): S:='This class cannot be created as part of an aggregate.';
      DWord(E_ABORT): S:='The update aborted.';
      DWOrd(E_INVALIDARG): S:='One of the parameters is invalid.';
      DWord(E_POINTER): S:='This method tried to access an invalid pointer.';
      DWord(E_NOINTERFACE): S:='No interface.';
      MS_S_PENDING: S:='The asynchronous update is pending.';
      MS_S_NOUPDATE: S:='Sample was not updated after forced completion.';
      MS_S_ENDOFSTREAM: S:='Reached the end of the stream; the sample wasn''t updated.';
      MS_E_SAMPLEALLOC: S:='An IMediaStream object could not be removed from an IMultiMediaStream object because it still contains at least one allocated sample.';
      MS_E_PURPOSEID: S:='The specified purpose ID can''t be used for the call.';
      MS_E_NOSTREAM: S:='No stream can be found with the specified attributes.';
      MS_E_NOSEEKING: S:='One or more media streams don''t support seeking.';
      MS_E_INCOMPATIBLE: S:='The stream formats are not compatible.';
      MS_E_BUSY: S:='This sample already has a pending update.';
      MS_E_NOTINIT: S:='The object can''t accept the call because its initialize function or equivalent has not been called.';
      MS_E_SOURCEALREADYDEFINED: S:='Source already defined.';
      MS_E_INVALIDSTREAMTYPE: S:='The stream type is not valid for this operation.';
      MS_E_NOTRUNNING: S:='The IMultiMediaStream object is not in running state.';
      Else
        begin
          if AMGetErrorText( Value, s2, High(s2) ) = 0 then
            S:='Unrecognized error value.'
          else
            S:=StrPas( s2 );
        end;
    end;

    raise EAudioReadError.Create(S, Value );
  end;
end ;

function DirectShowPresent: Boolean;
var
  AMovie: IGraphBuilder;
begin
  Result := CoCreateInstance( CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
    IID_IGraphBuilder, AMovie ) = S_OK;
end;

constructor TcbAudioFileReader.Create(const FileName: string);
var
  v: WideString;
begin
  inherited Create;

  // creates IAMMultiMediaStream instance
  SCheck( CoCreateInstance( CLSID_AMMultiMediaStream, nil, CLSCTX_INPROC_SERVER,
    IID_IAMMultiMediaStream, FAMMultiMediaStream ) );

  SCheck( FAMMultiMediaStream.Initialize(STREAMTYPE_READ, AMMSF_NOGRAPHTHREAD, nil) );

  // creates IMediaStream instance
  SCheck( FAMMultiMediaStream.AddMediaStream(nil, @MSPID_PrimaryAudio, 0, FMediaStream) );

  SCheck( FAMMultiMediaStream.GetMediaStream(MSPID_PrimaryAudio, FMediaStream) );

  // opens the file
  v := FileName;
  SCheck( FAMMultiMediaStream.OpenFile(PWideChar(v), 0) );

  // Get IMediaControl instance
  SCheck( FAMMultiMediaStream.GetFilterGraph(FGraphBuilder) );
  SCheck( FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl) );
  SCheck( FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking) );

  // creates IAudioMediaStream instance
  SCheck( FMediaStream.QueryInterface(IID_IAudioMediaStream, FAudioMediaStream) );

  SCheck( FAudioMediaStream.GetFormat(FFormat) );

  // creates IAudioData instance
  SCheck( CoCreateInstance(CLSID_AMAudioData, nil, CLSCTX_INPROC_SERVER,
    IID_IAudioData, FAudioData) );

  SCheck( FAudioData.SetFormat(FFormat) );

  // creates IAudioStreamSample instance
  SCheck( FAudioMediaStream.CreateSample(FAudioData, 0, FAudioStreamSample) );

  SCheck( FAMMultiMediaStream.GetDuration(FDuration) );

  SCheck( FAMMultiMediaStream.SetState( STREAMSTATE_RUN ) );

  FFileName := FileName;
  FSelLength := 0;
end;


destructor TcbAudioFileReader.Destroy;
begin
  if Assigned(FAMMultiMediaStream) then
    SCheck( FAMMultiMediaStream.SetState( STREAMSTATE_STOP ) );

  FAudioStreamSample := nil;
  FAudioData := nil;
  FAudioMediaStream := nil;
  FMediaStream := nil;
  FMediaSeeking := nil;
  FMediaControl := nil;
  FGraphBuilder := nil;
  FAMMultiMediaStream := nil;

  inherited Destroy;
end;


function TcbAudioFileReader.Read(const Buffer; Size: DWord): DWord;
// Reads <Size> bytes from audio file to <Buffer>
// limited by the range [SelStart, SelStart + SelLength[
//
// returns:   number of actual bytes read
var
  hr: DWord;
  Tmp: int64;
begin
  Result := 0;

  if (Size <= 0) then
    SCheck(DSUSER_INVALIDSIZE);

  // if end reached exit without reading a thing
  if (FSelLength <> 0) and (FPosition >= FSelStart + FSelLength) then
    exit;

  // if buffer changed since last access changes the reference to it
  if (@Buffer <> FBuffer) or (Size <> FBufferSize) then
  begin
    FBuffer := @Buffer;
    FBufferSize := Size;
    SCheck( FAudioData.SetBuffer( FBufferSize, FBuffer, 0 ) );
  end;

  // Read samples
  hr := FAudioStreamSample.Update(0, 0, nil, 0);

  // if end reached exit without reading a thing
  if (hr = MS_S_ENDOFSTREAM) or (hr = MS_E_NOSTREAM) then
    exit;

  // check for errors
  SCheck(hr);

  // get number of bytes read
  SCheck( FAudioData.GetInfo(FBufferSize, FBuffer, Result) );

  // get position of samples read
  SCheck( FAudioStreamSample.GetSampleTimes( FLastReadingStartTime,
    FLastReadingEndTime, Tmp ) );

  if FLastReadingStartTime > FLastReadingEndTime then  // don't know why but this happens
    FLastReadingStartTime := FLastReadingEndTime;

  // something happened and start and end time are equal
  // Limit to selection upper limit
  if (FSelLength <> 0) and (FLastReadingEndTime > FLastReadingStartTime) and
    (FSelStart + FSelLength < FLastReadingEndTime) then
  begin
    Result := DWord(Trunc(((Result *
      (FSelStart + FSelLength - FLastReadingStartTime)) /
      (FLastReadingEndTime - FLastReadingStartTime)))) and
      (not(FFormat.nBlockAlign-1));

    FLastReadingEndTime := FSelStart + FSelLength;
  end;

  FPosition := FLastReadingEndTime;
end;


procedure TcbAudioFileReader.SetPosition( Value: int64 );
var
  pfs: TFilter_State;
begin
  if (Value <> FPosition) then
  begin
    if (Value < FSelStart) then
      Value := FSelStart
    else
      if (Value > FDuration) then
        Value := FDuration
      else
        if (FSelLength <> 0) and (Value > FSelStart + FSelLength) then
          Value := FSelStart + FSelLength;

    // Seeks to position
    SCheck( FMediaControl.StopWhenReady );
    SCheck( FMediaSeeking.SetPositions(Value,
      AM_SEEKING_AbsolutePositioning, Value, AM_SEEKING_NoPositioning));
    SCheck( FMediaControl.Run );
    SCheck( FMediaControl.GetState(INFINITE, pfs) );

    FPosition := Value;
  end;
end;

procedure TcbAudioFileReader.SetSelStart( Value: int64 );
begin
  if (Value <> FSelStart) then
  begin
    if (Value < 0) then
      Value := 0;

    if (FPosition < Value) then
      SetPosition( Value );

    FSelStart := Value;
  end;
end;


procedure TcbAudioFileReader.SetSelLength( Value: int64 );
begin
  if (Value <> FSelLength) then
  begin
    if (Value <> 0) and (Value < MIN_SEL_LENGTH) then
      Value := MIN_SEL_LENGTH;

    if (Value > MAX_SEL_LENGTH) then
      Value := MAX_SEL_LENGTH;  // usefull to avoid overflows

    if (FPosition > FSelStart + Value) then
      SetPosition( FSelStart );

    FSelLength := Value;
  end;
end;


initialization
  CoInitialize(nil);

finalization
  CoUninitialize();

end.
