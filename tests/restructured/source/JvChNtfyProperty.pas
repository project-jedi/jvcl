{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChNtfyProperty.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ Property editor for the TJvChangeNotify component }

unit JvChNtfyProperty;

interface
uses
  SysUtils,{$IFDEF COMPILER6_UP}DesignEditors,DesignIntf{$ELSE}DsgnIntf{$ENDIF};

type

  TJvChangeNotifyEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index:integer);override;

 {$IFDEF COMPILER6_UP}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
{$ELSE}
 procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
{$ENDIF}

    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

resourcestring
  SEditProperty = 'Notifications...';

implementation

{ TJvChangeNotifyEditor }

 {$IFDEF COMPILER6_UP}
procedure TJvChangeNotifyEditor.EditProperty(const Prop: IProperty;  var Continue: Boolean);
var PropName:string;
begin
  PropName := Prop.GetName;
  if SameText(PropName,'Notifications') then // do not localize
  begin
    Prop.Edit;
    Continue := false;
  end;
end;
{$ELSE}
procedure TJvChangeNotifyEditor.EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean);
var PropName:string;
begin
  PropName := PropertyEditor.GetName;
  if SameText(PropName,'Notifications') then
  begin
    PropertyEditor.Edit;
    Continue := false;
  end;
end;
{$ENDIF}



procedure TJvChangeNotifyEditor.ExecuteVerb(Index: integer);
begin
  if Index = 0 then Edit else inherited;
end;

function TJvChangeNotifyEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := SEditProperty else Result := '';
end;

function TJvChangeNotifyEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
