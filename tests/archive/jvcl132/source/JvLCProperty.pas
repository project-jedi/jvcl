{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLCProperty.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Property editor for the TJvComboBox and TJvListBox2 components }

unit JvLCProperty;
interface
uses
{$IFDEF Delphi6_UP}DesignEditors, DesignIntf{$ELSE}DsgnIntf{$ENDIF};

type
  { a component editor that by default opens the editor for the Items property in TTimeline }

  TJvListCombProperty = class(TDefaultEditor)
  protected
{$IFDEF Delphi6_UP}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
{$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
{$ENDIF}

  public
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

resourcestring
  SEditProperty = 'Items editor...';

implementation
uses
  SysUtils;

{ TJvListCombProperty }

{$IFDEF Delphi6_UP}

procedure TJvListCombProperty.EditProperty(const Prop: IProperty; var Continue: Boolean);
var PropName: string;
begin
  PropName := Prop.GetName;
  if SameText(PropName, 'Items') then // do not localize
  begin
    Prop.Edit;
    Continue := false;
  end;
end;
{$ELSE}

procedure TJvListCombProperty.EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean);
var PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if SameText(PropName, 'Items') then
  begin
    PropertyEditor.Edit;
    Continue := false;
  end;
end;
{$ENDIF}

procedure TJvListCombProperty.ExecuteVerb(Index: integer);
begin
  if Index = 0 then
    Edit
  else
    inherited;
end;

function TJvListCombProperty.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := SEditProperty
  else
    Result := '';
end;

function TJvListCombProperty.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

