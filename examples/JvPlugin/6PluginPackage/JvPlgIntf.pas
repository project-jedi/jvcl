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

unit JvPlgIntf;

// holding interfaces for plugins and main application
// remember to always produce unique GUIDs (Ctrl+Shift+G) - never reuse them after changes

interface

uses
  Windows, Classes;

// A common interface for all plugins
type
  IMyMainAppInterface = interface;
  IMyPluginInterface = interface
    ['{A7F68489-2B52-4E59-B5E8-2044C7F67C09}']

    procedure ShowPlug(Sender: TObject);
    // provide a means for the app to make itself known to the plugin
    procedure Init(const MainApp:IMyMainAppInterface);
    // ...
  end;


  // if plugins shall be able to access the mainapplication,
  //   an interface for the main application should be defined as well
  IMyMainAppInterface = interface

    ['{00D251C6-8D61-43F7-88F8-35F7F7EC364D}']
    procedure DoSomethingSpecial(Name: string; OnClick: TNotifyEvent);

    // this way properties can be used:
     function GetPosition : TPoint;
     procedure SetPosition(const NewPos : TPoint);
     property Position : TPoint read GetPosition write SetPosition;
  end;


implementation

end.



