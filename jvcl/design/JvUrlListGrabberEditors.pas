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

unit JvUrlListGrabberEditors;

interface

uses
  Windows,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  Classes;

type
  TJvUrlGrabberDefaultPropertiesListEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    {$IFDEF COMPILER6_UP}
    procedure GetProperties(Proc: TGetPropProc); override;
    {$ELSE}
    procedure GetProperties(Proc: TGetPropEditProc); override;
    {$ENDIF COMPILER6_UP}
  end;

  TJvUrlGrabberDefaultPropertiesEditor = class (TClassProperty)
  public
    function GetName: string; override;
  end;

implementation

uses
  TypInfo,
  JvUrlListGrabber, JvUrlGrabbers;

//=== TJvUrlGrabberDefaultPropertiesListEditor ===============================

function TJvUrlGrabberDefaultPropertiesListEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paReadOnly];
end;

{$IFDEF COMPILER6_UP}
procedure TJvUrlGrabberDefaultPropertiesListEditor.GetProperties(Proc: TGetPropProc);
{$ELSE}
procedure TJvUrlGrabberDefaultPropertiesListEditor.GetProperties(Proc: TGetPropEditProc);
{$ENDIF COMPILER6_UP}
var
  UrlListGrabber: TJvUrlListGrabber;
  I: Integer;
  {$IFDEF COMPILER6_UP}
  Components: IDesignerSelections;
  {$ELSE}
  Components: TDesignerSelectionList;
  {$ENDIF COMPILER6_UP}
begin
  inherited GetProperties(Proc);

  UrlListGrabber := TJvUrlListGrabber(GetComponent(0));
  for I := 0 to UrlListGrabber.DefaultGrabbersProperties.Count - 1 do
  begin
    {$IFDEF COMPILER6_UP}
    Components := CreateSelectionList;
    {$ELSE}
    Components := TDesignerSelectionList.Create;
    {$ENDIF COMPILER6_UP}

    Components.Add(UrlListGrabber.DefaultGrabbersProperties.Items[I].EditorTrick);
    GetComponentProperties(Components, tkAny, Designer, Proc);
  end;
end;

//=== TJvUrlGrabberDefaultPropertiesEditor ===================================

function TJvUrlGrabberDefaultPropertiesEditor.GetName: string;
var
  EditorTrick: TJvUrlGrabberDefPropEdTrick;
begin
  // get classname of real default properties
  EditorTrick := TJvUrlGrabberDefPropEdTrick(GetComponent(0));
  Result := EditorTrick.DefaultProperties.ClassName;

  // remove TJv and everything from Url
  Result := Copy(Result, 4, Pos('Url', Result)-4);
end;

end.
