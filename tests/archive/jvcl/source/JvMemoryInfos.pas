{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemoryInfos.PAS, released on 2001-02-28.

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

unit JvMemoryInfos;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms,
  JvTypes, JvComponent;

type
  TJvMemoryInfos = class(TJvComponent)
  private
    FTotalMemory: string;
    FFreeMemory: string;
    FTotalPages: string;
    FDispoPages: string;
    FRegions: string;
    FDispoRegions: string;
    FMemoryLoad: string;
    FTimer: TTimer;
    FDelay: Integer;
    FAutoRefresh: Boolean;
    FDummy: string;
    procedure SetAuto(Auto: Boolean);
    procedure SetDelay(Speed: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    procedure Refresh(Sender: TObject);
    property AutoRefresh: Boolean read FAutoRefresh write SetAuto;
    property RefreshDelay: Integer read FDelay write SetDelay;
    property TotalMemory: string read FTotalMemory write FDummy;
    property FreeMemory: string read FfreeMemory write FDummy;
    property NumberOfPages: string read FTotalPages write FDummy;
    property DisponiblePages: string read FDispoPages write FDummy;
    property NumberOfRegions: string read FRegions write FDummy;
    property DisponibleRegions: string read FDispoRegions write FDummy;
    property MemoryLoad: string read FMemoryLoad write FDummy;
  end;

implementation

{*************************************************}

procedure TJvMemoryInfos.Refresh(Sender: TObject);
var
  MemoryStatus: Tmemorystatus;
begin
  GlobalMemoryStatus(MemoryStatus);
  Application.ProcessMessages;
  FTotalMemory := IntToStr(MemoryStatus.dwTotalPhys);
  FFreeMemory := IntToStr(MemoryStatus.dwAvailPhys);
  FTotalPages := IntToStr(MemoryStatus.dwTotalPageFile);
  FDispoPages := IntToStr(MemoryStatus.dwAvailPageFile);
  FRegions := IntToStr(MemoryStatus.dwTotalVirtual);
  FDispoRegions := IntToStr(MemoryStatus.dwAvailVirtual);
  FMemoryLoad := IntToStr(MemoryStatus.dwMemoryLoad);
  Application.ProcessMessages;
end;

{*************************************************}

procedure TJvMemoryInfos.SetDelay(Speed: Integer);
begin
  FTimer.Interval := Speed;
  FDelay := Speed;
end;

{*************************************************}

procedure TJvMemoryInfos.SetAuto(Auto: Boolean);
begin
  FTimer.Enabled := Auto;
  FAutoRefresh := Auto;
end;

{*************************************************}

constructor TJvMemoryInfos.Create(AOwner: TComponent);
begin
  inherited Create(Aowner);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 500;
  FTimer.OnTimer := Refresh;
  FTimer.Enabled := AutoRefresh;
end;

end.
