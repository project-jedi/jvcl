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

unit PluginTest;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Dialogs,
  Forms,
  JvPlugin,
  JvPlgIntf;

type
  TTest = class(TJvPlugin, IMyPluginInterface)
    procedure JvPluginDestroy(Sender: TObject);
  private
    { Private declarations }
    FMainApp:IMyMainAppInterface;
  public
    { Public declarations }
    procedure ShowPlug(Sender: TObject);
    procedure Init(const MainApp: IMyMainAppInterface);

  end;

function RegisterPlugin: TTest; stdcall;

exports
  RegisterPlugin;

implementation

uses ufrmPluginForm;

{$R *.DFM}

// IMPORTANT NOTE: If you change the name of the Plugin container,
// you must set the type below to the same type. (Delphi changes
// the declaration, but not the procedure itself. Both the return
// type and the type created must be the same as the declared type above.

function RegisterPlugin: TTest;
begin
  Result := TTest.Create(nil);
end;

{ TTest }

procedure TTest.ShowPlug(Sender: TObject);
begin
  if frmPluginForm = nil then
  begin
    frmPluginForm := TfrmPluginForm.Create(Application);
    frmPluginForm.MainApp := FMainApp;
  end;

  frmPluginForm.Show;
  //ShowMessage('ShowPlug was called');
end;

procedure TTest.JvPluginDestroy(Sender: TObject);
begin
  FreeAndNil(frmPluginForm);
end;

procedure TTest.Init(const MainApp: IMyMainAppInterface);
begin
  FMainApp := MainApp;
  if frmPluginForm <> nil then
    frmPluginForm.MainApp := MainApp;
end;

end.

