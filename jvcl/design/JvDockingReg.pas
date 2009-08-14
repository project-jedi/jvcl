{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockingReg.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockingReg;

{$I jvcl.inc}

interface

{$R JvDockingReg.dcr}

procedure Register;

implementation

uses
  Classes,
  DesignEditors, DesignIntf, VCLEditors,
  JvDsgnConsts,
  JvDockControlForm, JvDockPropertyEditors, JvDockVIDStyle,
  JvDockDelphiStyle, JvDockVCStyle, JvDockVIDVCStyle, JvDockVSNetStyle;

procedure Register;
begin
  RegisterComponents(RsPaletteDocking, [TJvDockServer, TJvDockClient,
    TJvDockDelphiStyle,TJvDockVCStyle, TJvDockVIDStyle, TJvDockVIDVCStyle, TJvDockVSNetStyle]);
  RegisterComponentEditor(TJvDockVIDTabPageControl, TJvDockVIDTabPageControlEditor);
  RegisterComponentEditor(TJvDockVIDTabSheet, TJvDockVIDTabPageControlEditor);

  RegisterNoIcon([TJvDockVIDTabSheet]);
  RegisterClass(TJvDockVIDTabSheet);
end;

end.