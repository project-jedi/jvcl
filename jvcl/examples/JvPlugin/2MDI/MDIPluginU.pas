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

unit MDIPluginU;


interface


uses

   Windows,
   Messages,
   SysUtils,
   Classes,
   Dialogs,
   Forms,
   JvPlugin,
   MDIPluginFormU;


type


  TuilMDIPluginSample = class( TJvPlugin )

    procedure uilMDIPluginSampleInitialize(Sender: TObject;
      var AllowLoad: Boolean);
    procedure uilMDIPluginSampleDestroy(Sender: TObject);
    procedure uilMDIPluginSampleCommands0Execute(Sender: TObject);
  private
    FMDIChild : TfrmMDIChild;
  protected
    OldApplication : TApplication;
  public
    { Public declarations }
  end;

var
   uilMDIPluginSample: TJvPlugin;


// IMPORTANT: This function should return the same
// type as above. For instance, the default is TuilPlugin1, so
// the declaration below would read:
// function RegisterPlugin : TuilPlugin1; stdcall;
// Hopefully I'll figure out a better way before release!
function RegisterPlugin : TuilMDIPluginSample; stdcall;


implementation




{$R *.DFM}


function RegisterPlugin : TuilMDIPluginSample;

begin

  Result := TuilMDIPluginSample.Create(nil);

end;


procedure TuilMDIPluginSample.uilMDIPluginSampleInitialize(Sender: TObject;
  var AllowLoad: Boolean);
begin
   OldApplication := Application;
   Application := HostApplication;
   FMDIChild := TfrmMDIChild.Create(nil);
end;

procedure TuilMDIPluginSample.uilMDIPluginSampleDestroy(Sender: TObject);
begin
   Application := OldApplication;
end;

procedure TuilMDIPluginSample.uilMDIPluginSampleCommands0Execute(Sender: TObject);
begin
   FMDIChild.WindowState := wsMaximized;
end;

end.
