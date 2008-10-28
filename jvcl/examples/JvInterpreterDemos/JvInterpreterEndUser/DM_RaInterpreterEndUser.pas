{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

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

unit DM_RaInterpreterEndUser;

interface

uses
  SysUtils, Windows, Classes, Graphics, Controls,
  Forms, Dialogs, DB, DBTables;

type
  TDMRaIntrEndUsr = class(TDataModule)
    DataSource1: TDataSource;
    Table1: TTable;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  DMRaIntrEndUsr: TDMRaIntrEndUsr;

implementation

{$R *.DFM}

procedure TDMRaIntrEndUsr.DataModuleCreate(Sender: TObject);
begin
  Table1.Open;
end;

end.