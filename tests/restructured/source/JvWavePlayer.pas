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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvWavePlayer;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, MMSystem, JvTypes, JvComponent;

type
  TJvWavePlayer = class(TJvComponent)
  private
    FAsync: Boolean;
    FLoop: Boolean;
    FWaveName: TFileName;
    FWavePointer: Pointer;
    FWaveLocation: TJvWaveLocation;
    FBeforePlay: TNotifyEvent;
    FAfterPlay: TNotifyEvent;
    procedure SetAsync(Value: Boolean);
    procedure SetLoop(Value: Boolean);
  protected
  public
    property WavePointer: Pointer read FWavePointer write FWavePointer;
    destructor Destroy; override;
  published
    property Asynchronous: Boolean read FAsync write SetAsync;
    property Loop: Boolean read FLoop write SetLoop;
    property SourceType: TJvWaveLocation read FWaveLocation write FWaveLocation default frFile;
    property FileName: TFileName read FWaveName write FWaveName;
    property BeforePlaying: TNotifyEvent read FBeforePlay write FBeforePlay;
    property AfterPlaying: TNotifyEvent read FAfterPlay write FAfterPlay;
    function Play: Boolean;
    procedure Stop;
  end;

implementation

{************************************************************}

procedure TJvWavePlayer.SetAsync(Value: Boolean);
begin
  FAsync := Value;
  if not FAsync then
    FLoop := False;
end;

{************************************************************}

procedure TJvWavePlayer.SetLoop(Value: Boolean);
begin
  if (FLoop <> Value) and FAsync then
    FLoop := Value;
end;

{************************************************************}

function TJvWavePlayer.Play;
var
  Flags: DWORD;
begin
  if Assigned(FBeforePlay) then
    FBeforePlay(Self);
  case FWaveLocation of
    frFile:
      Flags := SND_FileName;
    frResource:
      Flags := SND_RESOURCE;
  else
    Flags := SND_MEMORY;
  end;
  if FLoop then
    Flags := Flags or SND_LOOP;
  if FAsync then
    Flags := Flags or SND_ASYNC
  else
    Flags := Flags or SND_SYNC;
  if FWaveLocation = frRAM then
    Result := PlaySound(FWavePointer, 0, Flags)
  else
    Result := PlaySound(PChar(FWaveName), 0, Flags);
  if Assigned(FAfterPlay) then
    FAfterPlay(Self);
end;

{************************************************************}

procedure TJvWavePlayer.Stop;
var
  Flags: DWORD;
begin
  case FWaveLocation of
    frFile:
      Flags := SND_FileName;
    frResource:
      Flags := SND_RESOURCE;
  else
    Flags := SND_MEMORY;
  end;
  PlaySound(nil, 0, Flags);
end;

{************************************************************}

destructor TJvWavePlayer.Destroy;
begin
  Stop;
  inherited;
end;

end.
