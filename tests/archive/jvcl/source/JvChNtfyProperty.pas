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

unit JvChNtfyProperty;

{ Property editor for the TJvChangeNotify component }

interface

uses
  SysUtils,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type
  TJvChangeNotifyEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    {$IFDEF COMPILER6_UP}
    procedure EditProperty(const Prop: IProperty; var Cont: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Cont, FreeEditor: Boolean); override;
    {$ENDIF}
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

resourcestring
  SEditProperty = 'Notifications...';

implementation

{$IFDEF COMPILER6_UP}
procedure TJvChangeNotifyEditor.EditProperty(const Prop: IProperty; var Cont: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if SameText(PropName, 'Notifications') then // do not localize
  begin
    Prop.Edit;
    Cont := False;
  end;
end;
{$ELSE}
procedure TJvChangeNotifyEditor.EditProperty(PropertyEditor: TPropertyEditor; var Cont, FreeEditor: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if SameText(PropName, 'Notifications') then
  begin
    PropertyEditor.Edit;
    Cont := False;
  end;
end;
{$ENDIF}

procedure TJvChangeNotifyEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

function TJvChangeNotifyEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := SEditProperty
  else
    Result := '';
end;

function TJvChangeNotifyEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

