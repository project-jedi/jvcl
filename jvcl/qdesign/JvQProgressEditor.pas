{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQProgressEditor;

interface

uses
  Classes, SysUtils,  
  QWindows, QControls, QForms,   
  RTLConsts, DesignIntf, DesignEditors; 

type
  TJvProgressControlProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const AName: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses  
  QConsts, QDialogs, 
  JvQConsts, JvQProgressUtils;

procedure TJvProgressControlProperty.CheckComponent(const AName: string);
var
  Component: TComponent;
begin
  Component := Designer.GetComponent(AName);
  if (Component <> nil) and (Component is TControl) and
    SupportsProgressControl(TControl(Component)) and Assigned(FProc) then
    FProc(AName);
end;

procedure TJvProgressControlProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  try
    inherited GetValues(CheckComponent);
  finally
    FProc := nil;
  end;
end;

end.
