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

unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB, JvComponent;

type
  TMainForm = class(TForm)
    Restore: TJvUIBRestore;
    Log: TMemo;
    Go: TButton;
    procedure RestoreVerbose(Sender: TObject; Message: String);
    procedure GoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.RestoreVerbose(Sender: TObject; Message: String);
begin
  Log.Lines.Add(Message);
end;

procedure TMainForm.GoClick(Sender: TObject);
begin
  Log.Clear;
  Restore.Run;
end;

end.
