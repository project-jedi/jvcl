{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUrlListGrabberEditors.Pas, released on 2003-11-02.

The Initial Developer of the Original Code is Olivier Sannier [obones@meloo.com]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-11-02

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQUrlListGrabberEditors;

interface

uses
  Windows,
  
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  
  Classes;

type
  TJvUrlGrabberDefaultPropertiesListEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    
    procedure GetProperties(Proc: TGetPropProc); override;
    
  end;

  TJvUrlGrabberDefaultPropertiesEditor = class (TClassProperty)
  public
    function GetName: string; override;
  end;

implementation

uses
  TypInfo,
  JvQUrlListGrabber, JvQUrlGrabbers;

//=== TJvUrlGrabberDefaultPropertiesListEditor ===============================

function TJvUrlGrabberDefaultPropertiesListEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paReadOnly];
end;


procedure TJvUrlGrabberDefaultPropertiesListEditor.GetProperties(Proc: TGetPropProc);
var
  UrlListGrabber: TJvUrlListGrabber;
  I: Integer;
  Components: IDesignerSelections;
begin
  inherited GetProperties(Proc);

  UrlListGrabber := TJvUrlListGrabber(GetComponent(0));
  for I := 0 to UrlListGrabber.DefaultGrabbersProperties.Count - 1 do
  begin
    Components := CreateSelectionList;
    Components.Add(UrlListGrabber.DefaultGrabbersProperties.Items[I].EditorTrick);
    GetComponentProperties(Components, tkAny, Designer, Proc);
  end;
end;


//=== TJvUrlGrabberDefaultPropertiesEditor ===================================

function TJvUrlGrabberDefaultPropertiesEditor.GetName: string;
var
  EditorTrick: TJvUrlGrabberDefPropEdTrick;
begin
  // get Supported URL name from the real default properties
  EditorTrick := TJvUrlGrabberDefPropEdTrick(GetComponent(0));
  Result := EditorTrick.DefaultProperties.SupportedURLName;
end;

end.
