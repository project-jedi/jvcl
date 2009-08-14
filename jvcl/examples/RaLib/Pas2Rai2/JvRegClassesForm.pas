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

unit JvRegClassesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvAppStorage, JvAppRegistryStorage, JvComponentBase,
  JvFormPlacement, JvPasImportForm, JvJCLUtils;

type
  TJvRegClasses  = class(TForm)
    memClasses: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    JvFormStorage1: TJvFormStorage;
    procedure RegAuto1AfterSave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
  end;

var
  RegClasses: TJvRegClasses;

implementation

{$R *.DFM}

function ExeDirToSourceDir(const AExeDir: string): string;
begin
  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(AExeDir)) + 'examples\RaLib\Pas2Rai2\';
end;

procedure TJvRegClasses.RegAuto1AfterSave(Sender: TObject);
begin
  memClasses.Lines.SaveToFile(ExeDirToSourceDir(ExtractFilePath(Application.ExeName)) + 'classes.ini');
end;

procedure TJvRegClasses.FormCreate(Sender: TObject);
begin
  try
    memClasses.Lines.LoadFromFile(ExeDirToSourceDir(ExtractFilePath(Application.ExeName)) + 'classes.ini');
  except
    memClasses.Lines.Add('TObject');
    memClasses.Lines.Add('TStream');
  end;
end;

end.