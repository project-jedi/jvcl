{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSoundControl.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvSoundControl;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, MMSystem, JvTypes, JvComponent;

type
  TJvSoundValue = class(TPersistent)
  private
    FHandle: Integer;
    FOnRefresh: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    FBalance: Integer;
    FVolume: Integer;
    function GetBalance: TBalance;
    function GetVolume: Byte;
    procedure SetBalance(const Value: TBalance);
    procedure SetVolume(const Value: Byte);
  protected
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property Handle: Integer read FHandle write FHandle;
    procedure SetValue(Vol: TVolumeRec);
    function GetValue: TVolumeRec;
  public
    constructor Create;
  published
    property Volume: Byte read GetVolume write SetVolume stored False;
    property Balance: TBalance read GetBalance write SetBalance stored False;
  end;

  TJvSoundControl = class(TJvComponent)
  private
    FMidi: TJvSoundValue;
    FCd: TJvSoundValue;
    FWave: TJvSoundValue;
    FLastError: Integer;
    procedure OnCdRefresh(Sender: TObject);
    procedure OnWaveRefresh(Sender: TObject);
    procedure OnMidiRefresh(Sender: TObject);
    procedure OnCdUpdate(Sender: TObject);
    procedure OnWaveUpdate(Sender: TObject);
    procedure OnMidiUpdate(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LastError: Integer read FLastError;
  published
    property Wave: TJvSoundValue read FWave write FWave;
    property Midi: TJvSoundValue read FMidi write FMidi;
    property Cd: TJvSoundValue read FCd write FCd;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvSoundControl
///////////////////////////////////////////////////////////

constructor TJvSoundControl.Create(AOwner: TComponent);
var
  AuxCaps: TAuxCaps;
  WaveOutCaps: TWaveOutCaps;
  MidiOutCaps: TMidiOutCaps;
  i: Integer;
begin
  inherited;
  FLastError := 0;

  FMidi := TJvSoundValue.Create;
  FCd := TJvSoundValue.Create;
  FWave := TJvSoundValue.Create;

  FCd.OnRefresh := OnCdRefresh;
  FWave.OnRefresh := OnWaveRefresh;
  FMidi.OnRefresh := OnMidiRefresh;

  FCd.OnUpdate := OnCdUpdate;
  FWave.OnUpdate := OnWaveUpdate;
  FMidi.OnUpdate := OnMidiUpdate;

  for i := 0 to auxGetNumDevs - 1 do
  begin
    auxGetDevCaps(I, @AuxCaps, SizeOf(AuxCaps));
    if (AuxCaps.dwSupport and AUXCAPS_VOLUME) <> 0 then
    begin
      FCd.Handle := I;
      Break;
    end;
  end;

  for i := 0 to waveOutGetNumDevs - 1 do
  begin
    waveOutGetDevCaps(I, @WaveOutCaps, SizeOf(WaveOutCaps));
    if (WaveOutCaps.dwSupport and WAVECAPS_VOLUME) <> 0 then
    begin
      FWave.Handle := I;
      Break;
    end;
  end;

  for i := 0 to midiOutGetNumDevs - 1 do
  begin
    MidiOutGetDevCaps(I, @MidiOutCaps, SizeOf(MidiOutCaps));
    if (MidiOutCaps.dwSupport and MIDICAPS_VOLUME) <> 0 then
    begin
      FMidi.Handle := I;
      Break;
    end;
  end;
end;

{**********************************************************}

destructor TJvSoundControl.Destroy;
begin
  FMidi.Free;
  FCd.Free;
  FWave.Free;
  inherited;
end;

{**********************************************************}

procedure TJvSoundControl.OnCdRefresh(Sender: TObject);
var
  Vol: TVolumeRec;
begin
  with Sender as TJvSoundValue do
  begin
    FLastError := AuxGetVolume(Handle, @Vol.LongVolume);
    if FLastError = MMSYSERR_NOERROR then
      SetValue(Vol);
  end;
end;

{**********************************************************}

procedure TJvSoundControl.OnCdUpdate(Sender: TObject);
var
  Vol: TVolumeRec;
begin
  with Sender as TJvSoundValue do
  begin
    Vol := GetValue;
    FLastError := AuxSetVolume(Handle, Vol.LongVolume);
  end;
end;

{**********************************************************}

procedure TJvSoundControl.OnMidiRefresh(Sender: TObject);
var
  Vol: TVolumeRec;
begin
  with Sender as TJvSoundValue do
  begin
    FLastError := MidiOutGetVolume(Handle, @Vol.LongVolume);
    if FLastError = MMSYSERR_NOERROR then
      SetValue(Vol);
  end;
end;

{**********************************************************}

procedure TJvSoundControl.OnMidiUpdate(Sender: TObject);
var
  Vol: TVolumeRec;
begin
  with Sender as TJvSoundValue do
  begin
    Vol := GetValue;
    FLastError := MidiOutSetVolume(Handle, Vol.LongVolume);
  end;
end;

{**********************************************************}

procedure TJvSoundControl.OnWaveRefresh(Sender: TObject);
var
  Vol: TVolumeRec;
begin
  with Sender as TJvSoundValue do
  begin
    FLastError := waveOutGetVolume(Handle, @Vol.LongVolume);
    if FLastError = MMSYSERR_NOERROR then
      SetValue(Vol);
  end;
end;

{**********************************************************}

procedure TJvSoundControl.OnWaveUpdate(Sender: TObject);
var
  Vol: TVolumeRec;
begin
  with Sender as TJvSoundValue do
  begin
    Vol := GetValue;
    FLastError := WaveOutSetVolume(Handle, Vol.LongVolume);
  end;
end;

///////////////////////////////////////////////////////////
// TJvSoundValue
///////////////////////////////////////////////////////////

constructor TJvSoundValue.Create;
begin
  FHandle := -1;
end;

{**********************************************************}

function TJvSoundValue.GetBalance: TBalance;
begin
  if Handle = -1 then
    Result := 0
  else
  begin
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);
    Result := FBalance;
  end;
end;

{**********************************************************}

function TJvSoundValue.GetValue: TVolumeRec;
begin
  Result.LeftVolume := Round(((FVolume * FBalance) / 100)) shl 9;
  Result.RightVolume := Round(((FVolume * (100 - FBalance)) / 100)) shl 9;
end;

{**********************************************************}

function TJvSoundValue.GetVolume: Byte;
begin
  if Handle = -1 then
    Result := 0
  else
  begin
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);
    Result := FVolume;
  end;
end;

{**********************************************************}

procedure TJvSoundValue.SetBalance(const Value: TBalance);
begin
  if Handle <> -1 then
  begin
    FBalance := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

{**********************************************************}

procedure TJvSoundValue.SetValue(Vol: TVolumeRec);
var
  Total: Double;
begin
  FVolume := (Vol.LeftVolume + Vol.RightVolume) shr 9;
  Total := (Vol.LeftVolume + Vol.RightVolume) / 100;
  if Total <> 0 then
    FBalance := Round(Vol.LeftVolume / Total);
end;

{**********************************************************}

procedure TJvSoundValue.SetVolume(const Value: Byte);
begin
  if Handle <> -1 then
  begin
    FVolume := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

end.
