{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSQLS.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Component   : TJvaSQLScript
Description : db-aware component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBDESQLScript;

{$I jvcl.inc}

interface

uses
  Classes, DBTables,
  JvDBUtils, JvComponent;

type
  TJvBDESQLScript = class;
  TOnScriptProgress = procedure(Sender: TJvBDESQLScript; var Cancel: Boolean; Line: Integer) of object;

  TJvBDESQLScript = class(TJvComponent)
  private
    FOnProgress: TOnScriptProgress;
    FScript: TStringList;
    FCommit: TCommit;
    FDatabase: TDatabase;
    function GetScript: TStrings;
    procedure SetScript(AValue: TStrings);
    procedure Progress(UserData: Integer; var Cancel: Boolean; Line: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure Execute;
    property OnProgress: TOnScriptProgress read FOnProgress write FOnProgress;
    property Script: TStrings read GetScript write SetScript;
    property Commit: TCommit read FCommit write FCommit;
    property Database: TDatabase read FDatabase write FDatabase;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvBDEUtils;

constructor TJvBDESQLScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := TStringList.Create;
end;

destructor TJvBDESQLScript.Destroy;
begin
  FScript.Free;
  inherited Destroy;
end;

function TJvBDESQLScript.GetScript: TStrings;
begin
  Result := FScript;
end;

procedure TJvBDESQLScript.SetScript(AValue: TStrings);
begin
  FScript.Assign(AValue);
end;

procedure TJvBDESQLScript.Execute;
begin
  ExecuteSQLScript(FDatabase, FScript.Text, FCommit, Progress, 0);
end;

procedure TJvBDESQLScript.Progress(UserData: Integer; var Cancel: Boolean; Line: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Cancel, Line);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

