{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemoryInfos.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvMemoryInfos;

interface

uses
  Windows, SysUtils, Classes, ExtCtrls, Forms,
  JvTypes, JvComponent;

type
  TJvMemoryInfos = class(TJvComponent)
  private
    FTotalMemory: string;
    FFreeMemory: string;
    FTotalPages: string;
    FDisponiblePages: string;
    FNumberOfRegions: string;
    FDisponibleRegions: string;
    FMemoryLoad: string;
    FTimer: TTimer;
    FRefreshDelay: Integer;
    FAutoRefresh: Boolean;
    FDummy: string;
    procedure SetAuto(Auto: Boolean);
    procedure SetRefreshDelay(Speed: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  published
    procedure Refresh(Sender: TObject);
    property AutoRefresh: Boolean read FAutoRefresh write SetAuto default False;
    property RefreshDelay: Integer read FRefreshDelay write SetRefreshDelay default 500;
    // (rom) i am not sure if these properties should be string
    // (rom) it limits the component to display purposes
    property TotalMemory: string read FTotalMemory write FDummy stored False;
    property FreeMemory: string read FFreeMemory write FDummy stored False;
    property NumberOfPages: string read FTotalPages write FDummy stored False;
    property DisponiblePages: string read FDisponiblePages write FDummy stored False;
    property NumberOfRegions: string read FNumberOfRegions write FDummy stored False;
    property DisponibleRegions: string read FDisponibleRegions write FDummy stored False;
    property MemoryLoad: string read FMemoryLoad write FDummy stored False;
  end;

implementation

constructor TJvMemoryInfos.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRefreshDelay := 500;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := FRefreshDelay;
  FTimer.OnTimer := Refresh;
  FTimer.Enabled := AutoRefresh;
  Refresh(Self);
end;

procedure TJvMemoryInfos.Refresh(Sender: TObject);
var
  MemoryStatus: TMemoryStatus;
begin
  GlobalMemoryStatus(MemoryStatus);
  FTotalMemory := IntToStr(MemoryStatus.dwTotalPhys);
  FFreeMemory := IntToStr(MemoryStatus.dwAvailPhys);
  FTotalPages := IntToStr(MemoryStatus.dwTotalPageFile);
  FDisponiblePages := IntToStr(MemoryStatus.dwAvailPageFile);
  FNumberOfRegions := IntToStr(MemoryStatus.dwTotalVirtual);
  FDisponibleRegions := IntToStr(MemoryStatus.dwAvailVirtual);
  FMemoryLoad := IntToStr(MemoryStatus.dwMemoryLoad);
end;

procedure TJvMemoryInfos.SetRefreshDelay(Speed: Integer);
begin
  FTimer.Interval := Speed;
  FRefreshDelay := Speed;
end;

procedure TJvMemoryInfos.SetAuto(Auto: Boolean);
begin
  FTimer.Enabled := Auto;
  FAutoRefresh := Auto;
end;

end.

