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

unit RaInterpreterEndUserMainFormU;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, Forms, DBCtrls, DB, DBGrids, Grids, ExtCtrls, 
  Buttons, JvComponent, JvFormPlacement;

type
  TRaInterpreterEndUserMainForm = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator: TDBNavigator;
    Panel1: TPanel;
    Panel2: TPanel;
    RegAuto1: TJvFormStorage;
    GradButton1: TButton;
    Panel3: TPanel;
    Memo1: TMemo;
    procedure GradButton1Click(Sender: TObject);
  end;

var
  RaInterpreterEndUserMainForm: TRaInterpreterEndUserMainForm;

implementation

{$R *.DFM}

uses fReports;

procedure TRaInterpreterEndUserMainForm.GradButton1Click(Sender: TObject);
begin
  fReports.Show;
end;

end.
