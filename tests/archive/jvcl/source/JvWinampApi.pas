{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWinampApi.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvWinampApi;



interface

// (rom) this file definitely needs some local helper functions

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvHWinamp, JvTypes, JvComponent;

type
  TWStatus = (wsNotAvailable, wsStopped, wsPlaying, wsPaused);

  TWinampEqualizer = record
    Bands: array[0..9] of Integer;
    Preamp: Integer;
    Enabled, Autoload: Boolean;
  end;

  TJvWinampApi = class(TJvComponent)
  private
    FBidon: Boolean;
    FBidoni: Integer;
    FBidonW: TWStatus;
    FBidonT: TTime;
    function GetWinampPresent: Boolean;
    function GetMajorVersion: Integer;
    function GetMinorVersion: Integer;
    function GetWStatus: TWStatus;
    function GetLength: TTime;
    function GetTime: TTime;
    procedure SetTime(const Value: TTime);
    function GetPos: Integer;
    procedure SetPos(const Value: Integer);
    function GetBitRate: Integer;
    function GetChannels: Integer;
    function GetSampleRate: Integer;
    function GetListLength: Integer;
    function WinampHigher(Major, Minor: Integer): Boolean;
    procedure SendCommand(Value: Integer);
  published
    property WinampPresent: Boolean read GetWinampPresent write FBidon;
    property WinampMajorVersion: Integer read GetMajorVersion write FBidoni;
    property WinampMinorVersion: Integer read GetMinorVersion write FBidoni;
    property WinampStatus: TWStatus read GetWStatus write FBidonW;
    property SongPosition: TTime read GetTime write SetTime;
    property SongLength: TTime read GetLength write FBidonT;
    property PlaylistPos: Integer read GetPos write SetPos;
    property SampleRate: Integer read GetSampleRate write FBidoni;
    property BitRate: Integer read GetBitRate write FBidoni;
    property Channels: Integer read GetChannels write FBidoni;
    property ListLength: Integer read GetListLength write FBidonI;
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
    procedure StopWithFadout;
    function GetEqualizer: TWinampEqualizer;
    procedure SetEqualizer(Value: TWinampEqualizer);
  end;

  EWinampError = class(EJVCLException);

implementation

resourcestring
  RC_WinampWindow = 'Winamp v1.x';
  RC_WinampFormat = 'You must have Winamp %d.%d or higher to execute this Api';
  RC_ErrorFinding = 'Could not find winamp window';

  {**************************************************}

procedure TJvWinampApi.ClearPlaylist;
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    SendMessage(h, WM_WA_IPC, 0, IPC_DELETE)
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

function TJvWinampApi.GetBitRate: Integer;
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    Result := SendMessage(h, WM_WA_IPC, 1, IPC_GETINFO)
  else
    Result := 0;
end;

{**************************************************}

function TJvWinampApi.GetChannels: Integer;
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    Result := SendMessage(h, WM_WA_IPC, 2, IPC_GETINFO)
  else
    Result := 0;
end;

{**************************************************}

function TJvWinampApi.GetLength: TTime;
var
  h: THandle;
  i: Integer;
  tstamp: TTimeStamp;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  tstamp.Time := 0;
  tstamp.Date := 1;
  if h <> 0 then
  begin
    i := SendMessage(h, WM_WA_IPC, 1, IPC_GETOUTPUTTime);
    if i <> -1 then
      tstamp.Time := i * 1000;
  end;
  Result := TimeStampToDateTime(tstamp);
end;

{**************************************************}

function TJvWinampApi.GetMajorVersion: Integer;
var
  h: THandle;
  i: Integer;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
  begin
    i := SendMessage(h, WM_WA_IPC, 0, IPC_GETVERSION);
    Result := (i and $F000) div $1000;
  end
  else
    Result := -1;
end;

{**************************************************}

function TJvWinampApi.GetMinorVersion: Integer;
var
  h: THandle;
  i: Integer;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
  begin
    i := SendMessage(h, WM_WA_IPC, 0, IPC_GETVERSION);
    if WinampMajorVersion = 1 then
      Result := ((i and $0F00) div (16 * 16 * 16)) * 10 + (i and $000F)
    else
      Result := StrToInt(IntToHex(((i div 16) and $00FF), 2));
  end
  else
    Result := -1;
end;

{**************************************************}

function TJvWinampApi.GetPos: Integer;
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    Result := SendMessage(h, WM_WA_IPC, 0, IPC_GETLISTPOS)
  else
    Result := -1;
end;

{**************************************************}

function TJvWinampApi.GetSampleRate: Integer;
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    Result := SendMessage(h, WM_WA_IPC, 0, IPC_GETINFO)
  else
    Result := 0;
end;

{**************************************************}

function TJvWinampApi.GetTime: TTime;
var
  h: THandle;
  i: Integer;
  tstamp: TTimeStamp;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  tstamp.Time := 0;
  tstamp.Date := 1;
  if h <> 0 then
  begin
    i := SendMessage(h, WM_WA_IPC, 0, IPC_GETOUTPUTTime);
    if i <> -1 then
      tstamp.Time := i;
  end;
  Result := TimeStampToDateTime(tstamp);
end;

{**************************************************}

function TJvWinampApi.GetWinampPresent: Boolean;
begin
  Result := FindWindow(PChar(RC_WinampWindow), nil) <> 0;
end;

{**************************************************}

function TJvWinampApi.GetWStatus: TWStatus;
var
  h: THandle;
  i: Integer;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
  begin
    i := SendMessage(h, WM_WA_IPC, 0, IPC_ISPLAYING);
    case i of
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

{**************************************************}

procedure TJvWinampApi.Play;
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    SendMessage(h, WM_WA_IPC, 0, IPC_STARTPLAY)
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

procedure TJvWinampApi.SavePlaylist;
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
  begin
    if WinampHigher(1, 66) then
      SendMessage(h, WM_WA_IPC, 0, IPC_WRITEPLAYLIST);
  end
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

procedure TJvWinampApi.SetPos(const Value: Integer);
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if (h <> 0) and WinampHigher(2, 0) then
    SendMessage(h, WM_WA_IPC, Value, IPC_SETPLAYLISTPOS);
end;

{**************************************************}

procedure TJvWinampApi.SetPanning(Value: Byte);
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
  begin
    if WinampHigher(2, 0) then
      SendMessage(h, WM_WA_IPC, Value, IPC_SETPANNING);
  end
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

procedure TJvWinampApi.SetTime(const Value: TTime);
var
  h: THandle;
  tstamp: TTimeStamp;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  tstamp := DateTimeToTimeStamp(Value);
  if h <> 0 then
    if WinampHigher(1, 60) then
      SendMessage(h, WM_WA_IPC, tstamp.Time, IPC_JUMPTOTIME);
end;

{**************************************************}

procedure TJvWinampApi.SetVolume(Value: Byte);
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
  begin
    if WinampHigher(2, 0) then
      SendMessage(h, WM_WA_IPC, Value, IPC_SETVOLUME);
  end
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

function TJvWinampApi.WinampHigher(Major, Minor: Integer): Boolean;
begin
  if (WinampMajorVersion > Major) or ((Major = WinampMajorVersion) and (Minor <= WinampMinorVersion)) then
    Result := True
  else
  begin
    Result := False;
    if not (csDesigning in ComponentState) then
      raise EWinampError.Create(Format(RC_WinampFormat, [Major, Minor]));
  end;
end;

{**************************************************}

function TJvWinampApi.GetListLength: Integer;
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if (h <> 0) and (WinampHigher(2, 0)) then
    Result := SendMessage(h, WM_WA_IPC, 0, IPC_GETLISTLENGTH)
  else
    Result := 0;
end;

{**************************************************}

procedure TJvWinampApi.ShowAbout;
begin
  SendCommand(WINAMP_HELP_ABOUT);
end;

{**************************************************}

procedure TJvWinampApi.ShowOptions;
begin
  SendCommand(WINAMP_OPTIONS_PREFS);
end;

{**************************************************}

procedure TJvWinampApi.OpenFiles;
begin
  SendCommand(WINAMP_FILE_PLAY);
end;

{**************************************************}

procedure TJvWinampApi.ToggleAlwaysOnTop;
begin
  SendCommand(WINAMP_OPTIONS_AOT);
end;

{**************************************************}

procedure TJvWinampApi.ToggleEqualizer;
begin
  SendCommand(WINAMP_OPTIONS_EQ);
end;

{**************************************************}

procedure TJvWinampApi.TogglePlaylist;
begin
  SendCommand(WINAMP_OPTIONS_PLEDIT);
end;

{**************************************************}

procedure TJvWinampApi.OpenFile(FileName: string);
var
  cds: TCopyDataStruct;
  dat: array[0..255] of Char;
  h: THandle;
begin
  cds.dwData := IPC_PLAYFILE;
  StrPCopy(dat, FileName);
  cds.lpData := @dat;
  cds.cbData := Length(FileName);
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    SendMessage(h, WM_COPYDATA, 0, Longint(@cds))
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

procedure TJvWinampApi.SetDirectory(Directory: string);
var
  cds: TCopyDataStruct;
  dat: array[0..255] of Char;
  h: THandle;
begin
  cds.dwData := IPC_CHDIR;
  StrPCopy(dat, Directory);
  cds.lpData := @dat;
  cds.cbData := Length(Directory);
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    SendMessage(h, WM_COPYDATA, 0, Longint(@cds))
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

procedure TJvWinampApi.VolumeDec;
begin
  SendCommand(WINAMP_VOLUMEDOWN);
end;

{**************************************************}

procedure TJvWinampApi.VolumeInc;
begin
  SendCommand(WINAMP_VOLUMEUP);
end;

{**************************************************}

procedure TJvWinampApi.FastForward;
begin
  SendCommand(WINAMP_FFWD5S);
end;

{**************************************************}

procedure TJvWinampApi.FastRewind;
begin
  SendCommand(WINAMP_REW5S);
end;

{**************************************************}

procedure TJvWinampApi.OpenLocation;
begin
  SendCommand(WINAMP_BUTTON2_CTRL);
end;

{**************************************************}

procedure TJvWinampApi.NextTrack;
begin
  SendCommand(WINAMP_BUTTON5);
end;

{**************************************************}

procedure TJvWinampApi.PreviousTrack;
begin
  SendCommand(WINAMP_BUTTON1);
end;

{**************************************************}

procedure TJvWinampApi.Pause;
begin
  SendCommand(WINAMP_BUTTON3);
end;

{**************************************************}

procedure TJvWinampApi.Stop;
begin
  SendCommand(WINAMP_BUTTON4);
end;

{**************************************************}

procedure TJvWinampApi.EndOfList;
begin
  SendCommand(WINAMP_BUTTON5_CTRL);
end;

{**************************************************}

procedure TJvWinampApi.StartOfList;
begin
  SendCommand(WINAMP_BUTTON1_CTRL);
end;

{**************************************************}

procedure TJvWinampApi.StopWithFadout;
begin
  SendCommand(WINAMP_BUTTON4_SHIFT);
end;

{**************************************************}

function TJvWinampApi.GetEqualizer: TWinampEqualizer;
var
  h: THandle;
  I: Integer;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
  begin
    if WinampHigher(2, 5) then
    begin
      for I := 0 to 9 do
        Result.Bands[I] := SendMessage(h, WM_WA_IPC, I, IPC_GETEQDATA);
      Result.PreAmp := SendMessage(h, WM_WA_IPC, 10, IPC_GETEQDATA);
      Result.Enabled := SendMessage(h, WM_WA_IPC, 11, IPC_GETEQDATA) <> 0;
      Result.Autoload := SendMessage(h, WM_WA_IPC, 12, IPC_GETEQDATA) <> 0;
    end;
  end
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

procedure TJvWinampApi.SetEqualizer(Value: TWinampEqualizer);
var
  h: THandle;
  I: Integer;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
  begin
    if WinampHigher(2, 5) then
    begin
      for I := 0 to 9 do
      begin
        SendMessage(h, WM_WA_IPC, I, IPC_GETEQDATA);
        SendMessage(h, WM_WA_IPC, Value.Bands[I], IPC_SETEQDATA);
      end;

      SendMessage(h, WM_WA_IPC, 10, IPC_GETEQDATA);
      SendMessage(h, WM_WA_IPC, Value.PreAmp, IPC_SETEQDATA);

      SendMessage(h, WM_WA_IPC, 11, IPC_GETEQDATA);
      if Value.Enabled then
        SendMessage(h, WM_WA_IPC, 1, IPC_SETEQDATA)
      else
        SendMessage(h, WM_WA_IPC, 0, IPC_SETEQDATA);

      SendMessage(h, WM_WA_IPC, 12, IPC_GETEQDATA);
      if Value.Autoload then
        SendMessage(h, WM_WA_IPC, 1, IPC_SETEQDATA)
      else
        SendMessage(h, WM_WA_IPC, 0, IPC_SETEQDATA);
    end;
  end
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

{**************************************************}

procedure TJvWinampApi.SendCommand(Value: Integer);
var
  h: THandle;
begin
  h := FindWindow(PChar(RC_WinampWindow), nil);
  if h <> 0 then
    SendMessage(h, WM_COMMAND, Value, 0)
  else
    raise EWinampError.Create(RC_ErrorFinding);
end;

end.
