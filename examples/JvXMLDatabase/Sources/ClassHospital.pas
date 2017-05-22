{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit ClassHospital;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ClassUtils, ClassRequest, JvXmlDatabase;

type
  THospital = class
  private
    FRequests: TRequestHandler;
    FDatabase: TJvXmlDatabase;
  public
    constructor Create;
    destructor Destroy;override;

    function GetDataPath: string;

    property Requests: TRequestHandler read FRequests write FRequests;
    property Database: TJvXmlDatabase read FDatabase write FDatabase;
  end;

var
  GHospital: THospital;

implementation


{ THospital }

{**********************************************************************}
constructor THospital.Create;
begin
  FRequests := TRequestHandler.Create;
  FDatabase := TJvXmlDatabase.Create(nil);
  FDatabase.TablesPath := GetDataPath;
end;
{**********************************************************************}
destructor THospital.Destroy;
begin
  FDatabase.Free;
  FRequests.Free;
  inherited;
end;
{**********************************************************************}
function THospital.GetDataPath: string;
begin
  result := ExtractFilePath(Application.ExeName) + 'Data\';
end;
{**********************************************************************}

initialization
  GHospital := THospital.Create;
finalization
  FreeAndNil(GHospital);
end.
