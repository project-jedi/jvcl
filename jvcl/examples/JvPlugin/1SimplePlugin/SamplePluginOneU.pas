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

unit SamplePluginOneU;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Dialogs,
  Forms,
  JvPlugin;

type
  TSampleUILPlugin = class(TJvPlugin)
    procedure uilPlugin1Commands0Execute(Sender: TObject);
    procedure uilPlugin1Commands1Execute(Sender: TObject);
    procedure uilPlugin1Commands2Execute(Sender: TObject);
    procedure uilPlugin1PluginMessage(Sender: TObject; APluginMessage: Integer; AMessageText: string);
    procedure uilPlugin1Configure(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function RegisterPlugin: TSampleUILPlugin; stdcall;

implementation

{$R *.DFM}

// IMPORTANT NOTE: If you change the name of the Plugin container,
// you must set the type below to the same type. (Delphi changes
// the declaration, but not the procedure itself. Both the return
// type and the type created must be the same as the declared type above.

function RegisterPlugin: TSampleUILPlugin;
begin
  Result := TSampleUILPlugin.Create(nil);
end;

procedure TSampleUILPlugin.uilPlugin1Commands0Execute(Sender: TObject);
begin
  ShowMessage('Command One clicked');
end;

procedure TSampleUILPlugin.uilPlugin1Commands1Execute(Sender: TObject);
begin
  ShowMessage('Command Two clicked');
end;

procedure TSampleUILPlugin.uilPlugin1Commands2Execute(Sender: TObject);
begin
  ShowMessage('Command Three clicked');
end;

procedure TSampleUILPlugin.uilPlugin1PluginMessage(Sender: TObject;
  APluginMessage: Integer; AMessageText: string);
begin
  ShowMessage(Format('Plugin Message number %d received. MessageText: %s', [APluginMessage, AMessageText]));
end;

procedure TSampleUILPlugin.uilPlugin1Configure(Sender: TObject);
begin
  ShowMessage('You could put a configuration dialog here, if your plugin requires one.');
end;

end.

