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

unit ExceptionPluginU;


interface


uses
   Windows,
   Messages,
   SysUtils,
   Classes,
   Dialogs,
   Forms,
   StdCtrls,
   ComCtrls,
   JvPlugin;

type
  TuilPlugin1 = class(TJvPlugin)

    procedure uilPlugin1Initialize(Sender: TObject;
      var AllowLoad: Boolean);

  private

    procedure ExceptionHandler(Sender: TObject; E: Exception);

  public

    { Public declarations }

  end;


function RegisterPlugin : TuilPlugin1; stdcall;


implementation


{$R *.DFM}


// IMPORTANT NOTE: If you change the name of the Plugin container,

// you must set the type below to the same type. (Delphi changes

// the declaration, but not the procedure itself. Both the return

// type and the type created must be the same as the declared type above.

function RegisterPlugin : TuilPlugin1;

begin

  Result := TuilPlugin1.Create(nil);

end;


procedure TuilPlugin1.ExceptionHandler(Sender: TObject; E: Exception);
var
   Comp : TComponent;
begin
   Comp := HostApplication.MainForm.FindComponent('Listbox1');
   if Comp <> nil then
      TListBox(Comp).Items.Add(e.message);
end;

procedure TuilPlugin1.uilPlugin1Initialize(Sender: TObject;
  var AllowLoad: Boolean);
var
   Comp : TComponent;
begin
   HostApplication.OnException := ExceptionHandler;
   Comp := HostApplication.MainForm.FindComponent('StatusBar1');
   if Comp <> nil then
      TStatusBar(Comp).SimpleText := 'Exception Handler installed.';
end;

end.
