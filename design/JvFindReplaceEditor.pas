{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFindReplaceEditor.pas, released on 2006-12-02.

The Initial Developer of the Original Code is Robert Marquardt [robert_marquardt att gmx dott de].
Portions created by Robert Marquardt are Copyright (C) 2003 Robert Marquardt.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFindReplaceEditor;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils,
  DesignEditors, DesignIntf;

type
  TJvFindReplaceProperty = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  StdCtrls,
  JvFindReplace, JvEditor, JvDsgnTypes;

function TJvFindReplaceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TJvFindReplaceProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  for I := 0 to Designer.GetRoot.ComponentCount - 1 do
  begin
    Component := Designer.GetRoot.Components[I];
    if (Component.Name <> '') and ((Component is TJvCustomEditor) or (Component is TCustomEdit)) then
      Proc(Component.Name);
  end;
end;

end.
