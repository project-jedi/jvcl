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

unit Main;
// (p3) NB! The exe must be saved in the same folder as the source files since they are accessed from the progam directly!
interface

uses SysUtils, Forms, Controls, Menus, Classes, ComCtrls,
  JvInterpreter, JvInterpreterFm, JvInterpreter_all, JvComponent;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    StatusBar: TStatusBar;
    N2: TMenuItem;
    JvInterpreterFm1: TJvInterpreterFm;
    procedure FileNew1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses ChildWin, aboutRaInterpreter;

procedure TMainForm.FileNew1Execute(Sender: TObject);
begin
  JvInterpreterFm1.RunForm(ExtractFilePath(paramstr(0)) + 'childwin.pas');
end;

procedure TMainForm.HelpAbout1Execute(Sender: TObject);
begin
  JvInterpreterRunFormModal(ExtractFilePath(paramstr(0)) + 'aboutRAInterpreter.pas');
end;

procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

end.

