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

unit ChangePropertiesPluginU;

interface

uses

  Windows,
  Messages,
  SysUtils,
  Classes,
  Dialogs,
  Forms,
  Buttons,
  StdCtrls,
  JvPlugin;

type

  TuilPlugin1 = class(TJvPlugin)
    procedure uilPlugin1Commands0Execute(Sender: TObject);
    procedure uilPlugin1Commands1Execute(Sender: TObject);
    procedure uilPlugin1Commands2Execute(Sender: TObject);

  private

    { Private declarations }

  public

    { Public declarations }

  end;

function RegisterPlugin: TuilPlugin1; stdcall;

implementation

{$R *.DFM}

// IMPORTANT NOTE: If you change the name of the Plugin container,

// you must set the type below to the same type. (Delphi changes

// the declaration, but not the procedure itself. Both the return

// type and the type created must be the same as the declared type above.

function RegisterPlugin: TuilPlugin1;

begin

  Result := TuilPlugin1.Create(nil);

end;

procedure TuilPlugin1.uilPlugin1Commands0Execute(Sender: TObject);
begin
  HostApplication.MainForm.Caption := InputBox('Change Form Caption', 'New Caption:', HostApplication.MainForm.Caption);
end;

procedure TuilPlugin1.uilPlugin1Commands1Execute(Sender: TObject);
var
  Comp: TComponent;
begin
  Comp := HostApplication.MainForm.FindComponent('Button1');
  if assigned(Comp) then
    TButton(Comp).Enabled := not TButton(Comp).Enabled;
end;

procedure TuilPlugin1.uilPlugin1Commands2Execute(Sender: TObject);
var
  Comp: TComponent;
begin
  Comp := HostApplication.MainForm.FindComponent('Listbox1');
  if assigned(Comp) then
    TListBox(Comp).Items.Add(InputBox('Add to listbox', 'Item:', 'Enter item here'));
end;

end.

