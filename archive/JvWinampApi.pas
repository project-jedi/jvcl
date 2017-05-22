{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWinampApi.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvWinampApi;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,
  Winamp,
  JvTypes, JvComponent;

type
  TWStatus = (wsNotAvailable, wsStopped, wsPlaying, wsPaused);

  TWinampEqualizer = record
    Bands: array [0..9] of Integer;
    Preamp: Integer;
    Enabled, AutoLoad: Boolean;
  end;

  TJvPlayOption = (poRepeat, poShuffle);
  TJvPlayOptions = set of TJvPlayOption;

  TJvWinampApi = class(TJvComponent)
  private
    FWinampHandle: THandle;
    FDummy: Boolean;
    FDummyI: Integer;
    FDummyW: TWStatus;
    FDummyT: TTime;
    function GetBitRate: Integer;
    function GetChannels: Integer;
    function GetListLength: Integer;
    function GetMajorVersion: Integer;
    function GetMinorVersion: Integer;
    function GetPlaylistPos: Integer;
    function GetPlayOptions: TJvPlayOptions;
    function GetSampleRate: Integer;
    function GetEqualizer: TWinampEqualizer;
    function GetSongLength: TTime;
    function GetSongPosition: TTime;
    function GetWinampHandle: THandle;
    function GetWinampPresent: Boolean;
    function GetWinampStatus: TWStatus;
    procedure SetEqualizer(const Value: TWinampEqualizer);
    procedure SetPlaylistPos(const Value: Integer);
    procedure SetPlayOptions(const Value: TJvPlayOptions);
    procedure SetSongPosition(const Value: TTime);
    function CheckWinampHigher(Major, Minor: Integer): Boolean;
    procedure SendCommand(Value: Integer);
  protected
    { Refreshes the winamp handle }
    procedure RefreshHandle;
    { Raises an error when the winamp handle = 0 }
    procedure CheckHandle;
    // (rom) added helper
    procedure RefreshAndCheckHandle;
  public
    procedure ClearPlaylist;
    procedure SavePlaylist;
    procedure Play;
    procedure SetVolume(Value: Byte = 122);
    procedure SetPanning(Value: Byte = 122);
    procedure ShowOptions;
    procedure ShowAbout;
    procedure OpenFiles;
    procedure ToggleAlwaysOnTop;
    procedure ToggleEqualizer;
    procedure TogglePlaylist;
    procedure OpenFile(FileName: string);
    procedure SetDirectory(Directory: string);
    procedure VolumeInc;
    procedure VolumeDec;
    procedure FastForward;
    procedure FastRewind;
    procedure OpenLocation;
    procedure PreviousTrack;
    procedure NextTrack;
    procedure Stop;
    procedure Pause;
    procedure StartOfList;
    procedure EndOfList;
    procedure StopWithFadeout;
    property WinampHandle: THandle read GetWinampHandle;
    property Equalizer: TWinampEqualizer read GetEqualizer write SetEqualizer;
  published
    property WinampPresent: Boolean read GetWinampPresent write FDummy stored False;
    property WinampMajorVersion: Integer read GetMajorVersion write FDummyI stored False;
    property WinampMinorVersion: Integer read GetMinorVersion write FDummyI stored False;
    property WinampStatus: TWStatus read GetWinampStatus write FDummyW stored False;
    property SongPosition: TTime read GetSongPosition write SetSongPosition stored False;
    property SongLength: TTime read GetSongLength write FDummyT stored False;
    property PlaylistPos: Integer read GetPlaylistPos write SetPlaylistPos stored False;
    property SampleRate: Integer read GetSampleRate write FDummyI stored False;
    property BitRate: Integer read GetBitRate write FDummyI stored False;
    property Channels: Integer read GetChannels write FDummyI stored False;
    property ListLength: Integer read GetListLength write FDummyI stored False;
    property PlayOptions: TJvPlayOptions read GetPlayOptions write SetPlayOptions stored False;
  end;

  EWinampError = class(EJVCLException);

implementation

resourcestring
  { (rb) prefix resourcestrings consistently }
  RC_WinampWindow = 'Winamp v1.x';
  RC_WinampFormat = 'You must have Winamp %d.%d or higher to execute this Api';
  RC_ErrorFinding = 'Could not find winamp window';

procedure TJvWinampApi.ClearPlaylist;
begin
  RefreshAndCheckHandle;
  SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_DELETE);
end;

function TJvWinampApi.GetBitRate: Integer;
begin
  if WinampPresent then
    Result := SendMessage(WinampHandle, WM_WA_IPC, 1, IPC_GETINFO)
  else
    Result := 0;
end;

function TJvWinampApi.GetChannels: Integer;
begin
  if WinampPresent then
    Result := SendMessage(WinampHandle, WM_WA_IPC, 2, IPC_GETINFO)
  else
    Result := 0;
end;

function TJvWinampApi.GetSongLength: TTime;
var
  I: Longint;
  TimeStamp: TTimeStamp;
begin
  TimeStamp.Time := 0;
  TimeStamp.Date := 1;
  if WinampPresent then
  begin
    I := SendMessage(WinampHandle, WM_WA_IPC, 1, IPC_GETOUTPUTTIME);
    if I <> -1 then
      TimeStamp.Time := I * 1000;
  end;
  Result := TimeStampToDateTime(TimeStamp);
end;

function TJvWinampApi.GetMajorVersion: Integer;
var
  I: Longint;
begin
  if WinampPresent then
  begin
    I := SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_GETVERSION);
    Result := (I and $F000) div $1000;
  end
  else
    Result := -1;
end;

function TJvWinampApi.GetMinorVersion: Integer;
var
  I: Longint;
begin
  if WinampPresent then
  begin
    I := SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_GETVERSION);
    if WinampMajorVersion = 1 then
      Result := ((I and $0F00) div (16 * 16 * 16)) * 10 + (I and $000F)
    else
      Result := StrToInt(IntToHex(((I div 16) and $00FF), 2));
  end
  else
    Result := -1;
end;

function TJvWinampApi.GetPlaylistPos: Integer;
begin
  if WinampPresent then
    Result := SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_GETLISTPOS)
  else
    Result := -1;
end;

function TJvWinampApi.GetSampleRate: Integer;
begin
  if WinampPresent then
    Result := SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_GETINFO)
  else
    Result := 0;
end;

function TJvWinampApi.GetSongPosition: TTime;
var
  I: Longint;
  TimeStamp: TTimeStamp;
begin
  TimeStamp.Time := 0;
  TimeStamp.Date := 1;
  if WinampPresent then
  begin
    I := SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_GETOUTPUTTIME);
    if I <> -1 then
      TimeStamp.Time := I;
  end;
  Result := TimeStampToDateTime(TimeStamp);
end;

function TJvWinampApi.GetWinampPresent: Boolean;
begin
  RefreshHandle;
  Result := FWinampHandle <> 0;
end;

function TJvWinampApi.GetWinampStatus: TWStatus;
var
  I: Longint;
begin
  if WinampPresent then
  begin
    I := SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_ISPLAYING);
    case I of
      0:
        Result := wsStopped;
      1:
        Result := wsPlaying;
      3:
        Result := wsPaused;
    else
      Result := wsNotAvailable;
    end;
  end
  else
    Result := wsNotAvailable;
end;

procedure TJvWinampApi.Play;
begin
  RefreshAndCheckHandle;
  SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_STARTPLAY)
end;

procedure TJvWinampApi.SavePlaylist;
begin
  RefreshAndCheckHandle;
  if CheckWinampHigher(1, 66) then
    SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_WRITEPLAYLIST);
end;

procedure TJvWinampApi.SetPlaylistPos(const Value: Integer);
begin
  if WinampPresent and CheckWinampHigher(2, 0) then
    SendMessage(WinampHandle, WM_WA_IPC, Value, IPC_SETPLAYLISTPOS);
end;

procedure TJvWinampApi.SetPanning(Value: Byte);
begin
  RefreshAndCheckHandle;
  if CheckWinampHigher(2, 0) then
    SendMessage(WinampHandle, WM_WA_IPC, Value, IPC_SETPANNING);
end;

procedure TJvWinampApi.SetSongPosition(const Value: TTime);
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Value);
  if WinampPresent and CheckWinampHigher(1, 60) then
    SendMessage(WinampHandle, WM_WA_IPC, TimeStamp.Time, IPC_JUMPTOTIME);
end;

procedure TJvWinampApi.SetVolume(Value: Byte);
begin
  RefreshAndCheckHandle;
  if CheckWinampHigher(2, 0) then
    SendMessage(WinampHandle, WM_WA_IPC, Value, IPC_SETVOLUME);
end;

function TJvWinampApi.CheckWinampHigher(Major, Minor: Integer): Boolean;
begin
  Result := (WinampMajorVersion > Major) or
    ((WinampMajorVersion = Major) and (WinampMinorVersion >= Minor));
  if not Result and not (csDesigning in ComponentState) then
    raise EWinampError.Create(Format(RC_WinampFormat, [Major, Minor]));
end;

function TJvWinampApi.GetListLength: Integer;
begin
  if WinampPresent and CheckWinampHigher(2, 0) then
    Result := SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_GETLISTLENGTH)
  else
    Result := 0;
end;

procedure TJvWinampApi.ShowAbout;
begin
  SendCommand(WINAMP_HELP_ABOUT);
end;

procedure TJvWinampApi.ShowOptions;
begin
  SendCommand(WINAMP_OPTIONS_PREFS);
end;

procedure TJvWinampApi.OpenFiles;
begin
  SendCommand(WINAMP_FILE_PLAY);
end;

procedure TJvWinampApi.ToggleAlwaysOnTop;
begin
  SendCommand(WINAMP_OPTIONS_AOT);
end;

procedure TJvWinampApi.ToggleEqualizer;
begin
  SendCommand(WINAMP_OPTIONS_EQ);
end;

procedure TJvWinampApi.TogglePlaylist;
begin
  SendCommand(WINAMP_OPTIONS_PLEDIT);
end;

procedure TJvWinampApi.OpenFile(FileName: string);
var
  CopyData: TCopyDataStruct;
  Data: array [0..255] of Char;
begin
  RefreshAndCheckHandle;
  CopyData.dwData := IPC_PLAYFILE;
  FillChar(Data, SizeOf(Data), 0); // (ahuser) otherwise WinAmp will get strange symbols
  StrPCopy(Data, FileName);
  CopyData.lpData := @Data;
  CopyData.cbData := Length(FileName);
  SendMessage(WinampHandle, WM_COPYDATA, 0, Longint(@CopyData))
end;

procedure TJvWinampApi.SetDirectory(Directory: string);
var
  CopyData: TCopyDataStruct;
  Data: array [0..255] of Char;
begin
  RefreshAndCheckHandle;
  CopyData.dwData := IPC_CHDIR;
  StrPCopy(Data, Directory);
  CopyData.lpData := @Data;
  CopyData.cbData := Length(Directory);

  SendMessage(WinampHandle, WM_COPYDATA, 0, Longint(@CopyData))
end;

procedure TJvWinampApi.VolumeDec;
begin
  SendCommand(WINAMP_VOLUMEDOWN);
end;

procedure TJvWinampApi.VolumeInc;
begin
  SendCommand(WINAMP_VOLUMEUP);
end;

procedure TJvWinampApi.FastForward;
begin
  SendCommand(WINAMP_FFWD5S);
end;

procedure TJvWinampApi.FastRewind;
begin
  SendCommand(WINAMP_REW5S);
end;

procedure TJvWinampApi.OpenLocation;
begin
  SendCommand(WINAMP_BUTTON2_CTRL);
end;

procedure TJvWinampApi.NextTrack;
begin
  SendCommand(WINAMP_BUTTON5);
end;

procedure TJvWinampApi.PreviousTrack;
begin
  SendCommand(WINAMP_BUTTON1);
end;

procedure TJvWinampApi.Pause;
begin
  SendCommand(WINAMP_BUTTON3);
end;

procedure TJvWinampApi.Stop;
begin
  SendCommand(WINAMP_BUTTON4);
end;

procedure TJvWinampApi.EndOfList;
begin
  SendCommand(WINAMP_BUTTON5_CTRL);
end;

procedure TJvWinampApi.StartOfList;
begin
  SendCommand(WINAMP_BUTTON1_CTRL);
end;

procedure TJvWinampApi.StopWithFadeout;
begin
  SendCommand(WINAMP_BUTTON4_SHIFT);
end;

function TJvWinampApi.GetEqualizer: TWinampEqualizer;
var
  I: Integer;
begin
  RefreshAndCheckHandle;
  if CheckWinampHigher(2, 5) then
  begin
    for I := 0 to 9 do
      Result.Bands[I] := SendMessage(WinampHandle, WM_WA_IPC, I, IPC_GETEQDATA);
    Result.PreAmp := SendMessage(WinampHandle, WM_WA_IPC, 10, IPC_GETEQDATA);
    Result.Enabled := SendMessage(WinampHandle, WM_WA_IPC, 11, IPC_GETEQDATA) <> 0;
    Result.AutoLoad := SendMessage(WinampHandle, WM_WA_IPC, 12, IPC_GETEQDATA) <> 0;
  end;
end;

procedure TJvWinampApi.SetEqualizer(const Value: TWinampEqualizer);
const
  CBool: array [Boolean] of Integer = (0, 1);
var
  I: Integer;
begin
  RefreshAndCheckHandle;
  if not CheckWinampHigher(2, 5) then
    Exit;
  for I := 0 to 9 do
  begin
    SendMessage(WinampHandle, WM_WA_IPC, I, IPC_GETEQDATA);
    SendMessage(WinampHandle, WM_WA_IPC, Value.Bands[I], IPC_SETEQDATA);
  end;

  SendMessage(WinampHandle, WM_WA_IPC, 10, IPC_GETEQDATA);
  SendMessage(WinampHandle, WM_WA_IPC, Value.PreAmp, IPC_SETEQDATA);

  SendMessage(WinampHandle, WM_WA_IPC, 11, IPC_GETEQDATA);
  SendMessage(WinampHandle, WM_WA_IPC, CBool[Value.Enabled], IPC_SETEQDATA);

  SendMessage(WinampHandle, WM_WA_IPC, 12, IPC_GETEQDATA);
  SendMessage(WinampHandle, WM_WA_IPC, CBool[Value.Enabled], IPC_SETEQDATA);
end;

procedure TJvWinampApi.SendCommand(Value: Integer);
begin
  RefreshAndCheckHandle;
  SendMessage(WinampHandle, WM_COMMAND, Value, 0)
end;

function TJvWinampApi.GetWinampHandle: THandle;
begin
  if FWinampHandle = 0 then
    RefreshAndCheckHandle;
  Result := FWinampHandle;
end;

procedure TJvWinampApi.RefreshHandle;
begin
  FWinampHandle := FindWindow(PChar(RC_WinampWindow), nil);
end;

procedure TJvWinampApi.CheckHandle;
begin
  if FWinampHandle = 0 then
    raise EWinampError.Create(RC_ErrorFinding);
end;

procedure TJvWinampApi.RefreshAndCheckHandle;
begin
  RefreshHandle;
  CheckHandle;
end;

procedure TJvWinampApi.SetPlayOptions(const Value: TJvPlayOptions);
const
  CBool: array [Boolean] of Integer = (0, 1);
begin
  CheckWinampHigher(2, 40);

  SendMessage(WinampHandle, WM_WA_IPC, CBool[poShuffle in Value], IPC_SET_SHUFFLE);
  SendMessage(WinampHandle, WM_WA_IPC, CBool[poRepeat in Value], IPC_SET_REPEAT);
end;

function TJvWinampApi.GetPlayOptions: TJvPlayOptions;
begin
  CheckWinampHigher(2, 40);

  Result := [];
  if SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_GET_SHUFFLE) = 1 then
    Include(Result, poShuffle);
  if SendMessage(WinampHandle, WM_WA_IPC, 0, IPC_GET_REPEAT) = 1 then
    Include(Result, poRepeat);
end;

end.

