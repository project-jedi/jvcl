{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLinkedControlsEditor.PAS, released on 2004-01-25.

The Initial Developer of the Original Code is Peter Thörnqvist.
Portions created by Peter Thörnqvist are Copyright (C) 2004 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}
unit JvLinkedControlsEditor;

interface
uses
  Windows, Classes, SysUtils, Controls, Forms,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, VCLEditors, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF COMPILER6_UP}
type
  TJvLinkedControlsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation
uses
  TypInfo, JvLinkedControlsEditorForm;

{ TJvLinkedControlsProperty }

procedure TJvLinkedControlsProperty.Edit;
var
  C:TControl;
  S:TStrings;
begin
  C := GetComponent(0) as TControl;
  S := TStrings(GetOrdValue);
  EditLinkedControls(C,S);
end;

function TJvLinkedControlsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paMultiSelect];
end;

end.
