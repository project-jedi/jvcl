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

unit JvRegClassesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TJvRegClasses  = class(TForm)
    memClasses: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure RegAuto1AfterLoad(Sender: TObject);
    procedure RegAuto1AfterSave(Sender: TObject);
  public
  end;

var
  RegClasses: TJvRegClasses;

implementation

{$R *.DFM}

procedure TJvRegClasses.RegAuto1AfterLoad(Sender: TObject);
begin
  try
    memClasses.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'classes.ini');
  except
    memClasses.Lines.Add('TObject');
    memClasses.Lines.Add('TStream');
  end;
end;

procedure TJvRegClasses.RegAuto1AfterSave(Sender: TObject);
begin
  memClasses.Lines.SaveToFile(ExtractFilePath(Application.ExeName) + 'classes.ini');
end;

end.
