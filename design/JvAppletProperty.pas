{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppletProperty.PAS, released on 2002-05-26.

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

unit JvAppletProperty;

interface

uses
  Windows, Classes, Controls, Forms, Dialogs,
  {$IFNDEF COMPILER6_UP}
  DsgnIntf;
  {$ELSE}
  DesignIntf, DesignEditors;
  {$ENDIF}

type
  // (p3) changed to show the "friendly" names in a list: replaces
  // select value with CPL name
  TJvAppletNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: String); override;
  end;
  TJvAppletIndexProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation
uses
  SysUtils, JvWinDialogs, JclSysInfo, JvFunctions;

var
  FApplets:TStringlist = nil;

procedure Refresh;
var S:string;
begin
  if FApplets = nil then
    FApplets := TStringlist.Create;
  FApplets.Clear;
  S := IncludeTrailingPathDelimiter(GetWindowsSystemFolder);
  GetControlPanelApplets(S,'*.cpl',FApplets,nil);
end;


function TJvAppletNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paSortList,paMultiSelect,paRevertable];
end;

procedure TJvAppletNameProperty.GetValues(Proc: TGetStrProc);
var i:integer;
begin
  if FApplets = nil then
    Refresh;
  for i := 0 to FApplets.Count - 1 do
    Proc(FApplets.Names[i]);
end;

procedure TJvAppletNameProperty.SetValue(const Value: String);
var i:integer;S:string;
begin
  if FApplets = nil then
    Refresh;
  i := FApplets.IndexOfName(Value);
  if i >= 0 then
  begin
    S := FApplets.Values[Value];
    inherited SetValue(Copy(S,1,Pos(',',S)-1));
  end
  else
    inherited SetValue(Value);
end;

{ TJvAppletIndexProperty }

function TJvAppletIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paMultiSelect,paRevertable];
end;

procedure TJvAppletIndexProperty.GetValues(Proc: TGetStrProc);
var i,j:integer;
begin
  j := (GetComponent(0) as TJvAppletDialog).Count;
  for i := 0 to j - 1 do
    Proc(IntToStr(i));
end;

initialization
  Refresh;
finalization
  FreeAndNil(FApplets);

end.
