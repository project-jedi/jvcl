{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWavePlayer.PAS, released on 2001-02-28.

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

unit JvWavePlayer;

interface

uses
  Windows, SysUtils, Classes, MMSystem,
  JvTypes, JvComponent;

type
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
    destructor Destroy; override;
    function Play: Boolean;
    procedure Stop;
    property WavePointer: Pointer read FWavePointer write FWavePointer;
  published
    property Asynchronous: Boolean read FAsynchronous write SetAsynchronous;
    property Loop: Boolean read FLoop write SetLoop;
    property SourceType: TJvWaveLocation read FSourceType write FSourceType default frFile;
    property FileName: TFileName read FFileName write FFileName;
    property BeforePlaying: TNotifyEvent read FBeforePlaying write FBeforePlaying;
    property AfterPlaying: TNotifyEvent read FAfterPlaying write FAfterPlaying;
  end;

implementation

destructor TJvWavePlayer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TJvWavePlayer.Play;
const
  { TJvWaveLocation = (frFile, frResource, frRAM); }
  CSourceTypes: array [TJvWaveLocation] of DWORD =
    (SND_FILENAME, SND_RESOURCE, SND_MEMORY);
  CLoops: array [Boolean] of DWORD = (0, SND_LOOP);
  CAsynchronous: array [Boolean] of DWORD = (SND_SYNC, SND_ASYNC);
var
  Flags: DWORD;
begin
  if Assigned(FBeforePlaying) then
    FBeforePlaying(Self);

  Flags := CSourceTypes[FSourceType] or CLoops[FLoop] or CAsynchronous[FAsynchronous];

  if FSourceType = frRAM then
    Result := PlaySound(FWavePointer, 0, Flags)
  else
    Result := PlaySound(PChar(FFileName), 0, Flags);

  if Assigned(FAfterPlaying) then
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
  if (FLoop <> Value) and FAsynchronous then
    FLoop := Value;
end;

procedure TJvWavePlayer.Stop;
const
  { TJvWaveLocation = (frFile, frResource, frRAM); }
  CSourceTypes: array [TJvWaveLocation] of DWORD =
    (SND_FILENAME, SND_RESOURCE, SND_MEMORY);
begin
  PlaySound(nil, 0, CSourceTypes[FSourceType]);
end;

end.

