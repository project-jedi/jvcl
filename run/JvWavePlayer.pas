{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWavePlayer.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvWavePlayer;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, MMSystem,
  JvTypes, JvComponentBase;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvWavePlayer = class(TJvComponent)
  private
    FAsynchronous: Boolean;
    FLoop: Boolean;
    FFileName: TFileName;
    FWavePointer: Pointer;
    FSourceType: TJvWaveLocation;
    FBeforePlaying: TNotifyEvent;
    FAfterPlaying: TNotifyEvent;
    procedure SetAsynchronous(Value: Boolean);
    procedure SetLoop(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Play: Boolean;
    procedure Stop;
    procedure Silence;
    property WavePointer: Pointer read FWavePointer write FWavePointer;
  published
    property Asynchronous: Boolean read FAsynchronous write SetAsynchronous default True;
    property Loop: Boolean read FLoop write SetLoop default False;
    property SourceType: TJvWaveLocation read FSourceType write FSourceType default frFile;
    property FileName: TFileName read FFileName write FFileName;
    property BeforePlaying: TNotifyEvent read FBeforePlaying write FBeforePlaying;
    property AfterPlaying: TNotifyEvent read FAfterPlaying write FAfterPlaying;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation


const
  CSourceTypes: array [TJvWaveLocation] of DWORD =
    (SND_FILENAME, SND_RESOURCE, SND_MEMORY);

constructor TJvWavePlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsynchronous := True;
  Loop := False;
  FSourceType := frFile;
end;

destructor TJvWavePlayer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TJvWavePlayer.Play: Boolean;
const
  CLoops: array [Boolean] of DWORD = (0, SND_LOOP);
  CAsynchronous: array [Boolean] of DWORD = (SND_SYNC, SND_ASYNC);
var
  Flags: DWORD;
begin
  Result := False;
  case SourceType of
    frRAM:
      if WavePointer = nil then
        Exit;
    frFile, frResource:
      if FileName = '' then
        Exit;
  else
    Exit;
  end;

  if Assigned(FBeforePlaying) then
    FBeforePlaying(Self);

  Flags := CSourceTypes[SourceType] or CLoops[Loop] or CAsynchronous[Asynchronous];

  if FSourceType = frRAM then
    Result := PlaySound(WavePointer, 0, Flags)
  else
    Result := PlaySound(PChar(FileName), 0, Flags);

  if Assigned(FAfterPlaying) and not (Loop or Asynchronous) then
    FAfterPlaying(Self);
end;

procedure TJvWavePlayer.SetAsynchronous(Value: Boolean);
begin
  FAsynchronous := Value;
  if not FAsynchronous then
    FLoop := False;
end;

procedure TJvWavePlayer.SetLoop(Value: Boolean);
begin
  if (FLoop <> Value) and Asynchronous then
    FLoop := Value;
end;

procedure TJvWavePlayer.Stop;
begin
  PlaySound(nil, 0, CSourceTypes[FSourceType]);
  if Assigned(FAfterPlaying) and (Loop or Asynchronous) and
    not (csDestroying in ComponentState) then
    FAfterPlaying(Self);
end;

procedure TJvWavePlayer.Silence;
begin
  // Immediately stops the WAV from playing and purges any remaining WAV audio from the queue.
  PlaySound(nil, 0, SND_PURGE);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
