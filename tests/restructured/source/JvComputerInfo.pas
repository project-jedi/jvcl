{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComputerInfo.PAS, released on 2001-02-28.

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

unit JvComputerInfo;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Registry, MMSystem, JvTypes, JvComponent;

type
  TJvComputerInfo = class(TJvComponent)
  private
    FBidonI: Integer;
    FBidonT: TTime;
    function GetCompany: string;
    function GetComputerName: string;
    function GetUsername: string;
    procedure SetCompany(const Value: string);
    procedure SetComputerName(const Value: string);
    procedure SetUsername(const Value: string);
    function GetComment: string;
    function GetWorkGroup: string;
    procedure SetComment(const Value: string);
    procedure SetWorkGroup(const Value: string);
    function GetDVDRegion: Integer;
    function GetProductID: string;
    function GetProductKey: string;
    function GetProductName: string;
    function GetVersion: string;
    function GetVersionNumber: string;
    procedure SetDVDRegion(const Value: Integer);
    procedure SetProductID(const Value: string);
    procedure SetProductKey(const Value: string);
    procedure SetProductName(const Value: string);
    procedure SetVersion(const Value: string);
    procedure SetVersionNumber(const Value: string);
    function GetDay: Integer;
    function GetTime: TTime;
    procedure WriteReg(Base: HKEY; KeyName, ValueName, Value: string);
    function ReadReg(Base: HKEY; KeyName, ValueName: string): string;
  protected
  public
  published
    property ComputerName: string read GetComputerName write SetComputerName;
    property Username: string read GetUsername write SetUsername;
    property Company: string read GetCompany write SetCompany;
    property Comment: string read GetComment write SetComment;
    property WorkGroup: string read GetWorkGroup write SetWorkGroup;
    property ProductID: string read GetProductID write SetProductID;
    property ProductKey: string read GetProductKey write SetProductKey;
    property ProductName: string read GetProductName write SetProductName;
    property DVDRegion: Integer read GetDVDRegion write SetDVDRegion;
    property VersionNumber: string read GetVersionNumber write SetVersionNumber;
    property Version: string read GetVersion write SetVersion;
    property TimeRunning: TTime read GetTime write FBidonT;
    property DayRunning: Integer read GetDay write FBidonI;
  end;

implementation

resourcestring
  RC_VnetKey = 'System\CurrentControlSet\Services\Vxd\VNETSUP';
  RC_CurrentKey = 'Software\Microsoft\Windows\CurrentVersion';

  {**************************************************}

function TJvComputerInfo.GetComment: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VnetKey, 'Comment');
end;

{**************************************************}

function TJvComputerInfo.GetCompany: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'RegisteredOrganization');
end;

{**************************************************}

function TJvComputerInfo.GetComputerName: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VnetKey, 'ComputerName');
end;

{**************************************************}

function TJvComputerInfo.GetDay: Integer;
begin
  Result := (((timeGetTime div 1000) div 60) div 60) div 24;
end;

{**************************************************}

function TJvComputerInfo.GetDVDRegion: Integer;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(RC_CurrentKey, False);
    try
      if ValueExists('DVD_Region') then
        Result := ReadInteger('DVD_Region')
      else
        Result := -1;
    except
      Result := -1;
    end;
    Free;
  end;
end;

{**************************************************}

function TJvComputerInfo.GetProductID: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'ProductID');
end;

{**************************************************}

function TJvComputerInfo.GetProductKey: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'ProductKey');
end;

{**************************************************}

function TJvComputerInfo.GetProductName: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'ProductName');
end;

{**************************************************}

function TJvComputerInfo.GetTime: TTime;
var
  h, m, s, mi: Integer;
  d: DWord;
begin
  d := timeGetTime;

  mi := d mod 1000;
  d := d div 1000;

  s := d mod 60;
  d := d div 60;

  m := d mod 60;
  d := d div 60;

  h := d mod 24;
  Result := EncodeTime(h, m, s, mi);
end;

{**************************************************}

function TJvComputerInfo.GetUsername: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'RegisteredOwner');
end;

{**************************************************}

function TJvComputerInfo.GetVersion: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'Version');
end;

{**************************************************}

function TJvComputerInfo.GetVersionNumber: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'VersionNumber');
end;

{**************************************************}

function TJvComputerInfo.GetWorkGroup: string;
begin
  Result := ReadReg(HKEY_LOCAL_MACHINE, RC_VnetKey, 'Workgroup');
end;

{**************************************************}

function TJvComputerInfo.ReadReg(Base: HKEY; KeyName, ValueName: string): string;
begin
  with TRegistry.Create do
  begin
    RootKey := Base;
    OpenKey(KeyName, False);
    try
      if ValueExists(ValueName) then
        Result := ReadString(ValueName)
      else
        Result := '';
    except
      Result := '';
    end;
    Free;
  end;
end;

{**************************************************}

procedure TJvComputerInfo.SetComment(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_VnetKey, 'Comment', Value);
end;

{**************************************************}

procedure TJvComputerInfo.SetCompany(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'RegisteredOrganization', Value);
end;

{**************************************************}

procedure TJvComputerInfo.SetComputerName(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_VnetKey, 'ComputerName', Value);
end;

{**************************************************}

procedure TJvComputerInfo.SetDVDRegion(const Value: Integer);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(RC_CurrentKey, False);
    WriteInteger('DVD_Region', Value);
    Free;
  end;
end;

{**************************************************}

procedure TJvComputerInfo.SetProductID(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'ProductId', Value);
end;

{**************************************************}

procedure TJvComputerInfo.SetProductKey(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'ProductKey', Value);
end;

{**************************************************}

procedure TJvComputerInfo.SetProductName(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'ProductName', Value);
end;

{**************************************************}

procedure TJvComputerInfo.SetUsername(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'RegisteredOwner', Value);
end;

{**************************************************}

procedure TJvComputerInfo.SetVersion(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'Version', Value);
end;

{**************************************************}

procedure TJvComputerInfo.SetVersionNumber(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_CurrentKey, 'VersionNumber', Value);
end;

{**************************************************}

procedure TJvComputerInfo.WriteReg(Base: HKEY; KeyName, ValueName, Value: string);
begin
  with TRegistry.Create do
  begin
    RootKey := Base;
    OpenKey(KeyName, False);
    WriteString(ValueName, Value);
    Free;
  end;
end;

{**************************************************}

procedure TJvComputerInfo.SetWorkGroup(const Value: string);
begin
  WriteReg(HKEY_LOCAL_MACHINE, RC_VnetKey, 'Workgroup', Value);
end;

end.
