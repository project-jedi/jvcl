{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDeviceChanged.PAS, released on 2001-02-28.

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
{$I windowsonly.inc}

unit JvDeviceChanged;

interface

uses
  Windows, Messages, SysUtils, Classes, DBT,
  JvComponent;

type
  TOnDeviceArrived = procedure(Sender: TObject; Drive: Char) of object;
  TOnDeviceQueryRemove = procedure(Sender: TObject; Drive: Char; var CanRemove: Boolean) of object;

  TJvDeviceChanged = class(TJvComponent)
  private
    FHandle: THandle;
    FOnDeviceArrived: TOnDeviceArrived;
    FOnDeviceRemoveCompleted: TOnDeviceArrived;
    function GetFirstDriveLetter(UnitMask: Longint): Char;
    procedure WndProc(var Msg: TMessage);
  public
    procedure WMDeviceChange(var Msg: TWMDeviceChange);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnDeviceArrived: TOnDeviceArrived read FOnDeviceArrived write FOnDeviceArrived;
    property OnDeviceRemoveCompleted: TOnDeviceArrived read FOnDeviceRemoveCompleted write FOnDeviceRemoveCompleted;
  end;

implementation

uses
  JvJVCLUtils;

// (rom) change to RegisterWndProcHook mechanism?

constructor TJvDeviceChanged.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := AllocateHWndEx(WndProc);
end;

destructor TJvDeviceChanged.Destroy;
begin
  DeallocateHWndEx(FHandle);
  inherited Destroy;
end;

procedure TJvDeviceChanged.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_DEVICECHANGE then
    WMDeviceChange(TWMDeviceChange(Msg))
  else
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

function TJvDeviceChanged.GetFirstDriveLetter(UnitMask: Longint): Char;
var
  DriveLetter: Shortint;
begin
  DriveLetter := Ord('A');
  while (UnitMask and 1) = 0 do
  begin
    UnitMask := UnitMask shr 1;
    Inc(DriveLetter);
  end;
  Result := Char(DriveLetter);
end;

procedure TJvDeviceChanged.WMDeviceChange(var Msg: TWMDeviceChange);
var
  lpdb: PDevBroadcastHdr;
  lpdbv: PDevBroadcastVolume;
begin
  lpdb := PDevBroadcastHdr(Msg.dwData);
  case Msg.Event of
    DBT_DEVICEARRIVAL:
      begin
        if lpdb^.dbch_devicetype = DBT_DEVTYP_VOLUME then
        begin
          lpdbv := PDevBroadcastVolume(Msg.dwData);
          if (lpdbv^.dbcv_flags and DBTF_MEDIA) = 1 then
            if Assigned(FOnDeviceArrived) then
              FOnDeviceArrived(Self, GetFirstDriveLetter(lpdbv^.dbcv_unitmask));
        end;
      end;
    DBT_DEVICEREMOVECOMPLETE:
      begin
        if lpdb^.dbch_devicetype = DBT_DEVTYP_VOLUME then
        begin
          lpdbv := PDevBroadcastVolume(Msg.dwData);
          if (lpdbv^.dbcv_flags and DBTF_MEDIA) = 1 then
            if Assigned(FOnDeviceRemoveCompleted) then
              FOnDeviceRemoveCompleted(Self, GetFirstDriveLetter(lpdbv^.dbcv_unitmask));
        end;
      end;
  end
end;

end.

