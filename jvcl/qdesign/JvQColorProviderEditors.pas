{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorProviderEditors.pas, released on 2003-09-30.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQColorProviderEditors;

interface

uses
  
  DesignIntf, DesignEditors, DesignMenus,
  
  
  ClxEditors, QConsts,
  
  
  Classes,
  
  JvQColorProvider, JvQDataProviderEditors;

type
  TJvColorProviderMappingProperty = class(TJvDataConsumerExtPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvColorProviderAddColorStyleEditor = class(TJvDataConsumerExtPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  
  TJvColorProviderEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  SysUtils, TypInfo,
  JvQDataProviderIntf, JvQColorProviderDesignerForm, JvQDsgnConsts;

//=== TJvColorProviderMappingProperty ========================================

function TJvColorProviderMappingProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

function TJvColorProviderMappingProperty.GetValue: string;
var
  I: Integer;
begin
  I := GetOrdValue;
  if I = -1 then
    Result := RsNone
  else
    Result := (GetConsumerImpl.ProviderIntf as IJvColorProvider).Get_Mapping(I).Name;
end;

procedure TJvColorProviderMappingProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if AnsiSameStr(Value, RsNone) or (Value = '') then
    SetOrdValue(-1)
  else
  begin
    with GetConsumerImpl.ProviderIntf as IJvColorProvider do
    begin
      I := IndexOfMappingName(Value);
      if I < 0 then
        raise EPropertyError.CreateRes(@RsEMappingDoesNotExistForThisColorProv);
      SetOrdValue(I);
    end;
  end;
end;

procedure TJvColorProviderMappingProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with GetConsumerImpl.ProviderIntf as IJvColorProvider do
  begin
    for I := 0 to Get_MappingCount - 1 do
      Proc(Get_Mapping(I).Name);
  end;
end;

//=== TJvColorProviderAddColorStyleEditor ====================================

function TJvColorProviderAddColorStyleEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

function TJvColorProviderAddColorStyleEditor.GetValue: string;
var
  I: Integer;
begin
  I := GetOrdValue;
  if I > -1 then
    Result := ColorProviderColorAdderRegister.Names(I)
  else
    Result := '';
end;

procedure TJvColorProviderAddColorStyleEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  if Value = '' then
    SetOrdValue(-1)
  else
  begin
    I := ColorProviderColorAdderRegister.IndexOf(Value);
    if I > -1 then
      SetOrdValue(I)
    else
      raise EPropertyError.CreateRes(@RsEInvalidPropertyValue);
  end;
end;

procedure TJvColorProviderAddColorStyleEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to ColorProviderColorAdderRegister.Count - 1 do
    Proc(ColorProviderColorAdderRegister.Names(I));
end;

//=== TJvColorProviderEditor =================================================

procedure TJvColorProviderEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TJvColorProviderEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    DesignColorProvider(TJvColorProvider(Component), Designer)
  else
    inherited ExecuteVerb(Index)
end;

function TJvColorProviderEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RsDesignerEllipsis
  else
   inherited GetVerb(Index);
end;

function TJvColorProviderEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
