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

The Original Code is: JvBandsReg.PAS, released on 2002-05-26.

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

unit JvQBehaviorLabelEditor;

interface

uses
  Classes, SysUtils, 
  DesignEditors, DesignIntf; 

type
  TJvLabelBehaviorProperty = class(TStringProperty)
  public
    function AutoFill: Boolean; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

uses
  JvQBehaviorLabel, JvQDsgnTypes;

function TJvLabelBehaviorProperty.AutoFill: Boolean;
begin
  Result := inherited AutoFill;
  // if you want to fix the flickering when double-clicking a value, uncomment line below:
  // Result := False;
end;

function TJvLabelBehaviorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable, paMultiSelect];
end;

procedure TJvLabelBehaviorProperty.GetValues(Proc: TGetStrProc);
var
  S: TStringList;
  I: Integer;
begin
  S := TStringList.Create;
  try
    GetRegisteredLabelBehaviorOptions(S);
    S.Sort;
    for I := 0 to S.Count -1 do
      Proc(S[I]);
  finally
    S.Free;
  end;
end;

procedure TJvLabelBehaviorProperty.SetValue(const Value: string);
var
  List: IDesignerSelections;
  LDesigner: IJvFormDesigner;
begin
  inherited SetValue(Value);
  List := CreateSelectionList;
  Designer.GetSelections(List);
  LDesigner := Designer;  // keep Designer alive
  LDesigner.SetSelections(nil);
  LDesigner.SetSelections(List);
  //Designer.Modified;
end;

end.

