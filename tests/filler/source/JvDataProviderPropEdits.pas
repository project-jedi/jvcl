{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderPropEdits.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Peter Thörnqvist

Last Modified: 2003-06-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProviderPropEdits;

interface

procedure RegDataProviderPropEdits;

implementation

uses
  Classes, Consts, {$IFNDEF COMPILER6_UP} DsgnIntf, {$ELSE} DesignIntf, DesignEditors, {$ENDIF}
  SysUtils, TypInfo,
  JvDataProviderImpl, JvDataProvider, JvDataProviderEditor;

type
{$IFNDEF COMPILER6_UP}

{ Since D5 doesn't support published properties of type interface (or rather, the OI/streaming
  system doesn't), D5 will use a simple TComponent property. A property editor is created that
  will only list components that support both the IJvDataProvider as well as the
  IInterfaceComponentRef interfaces. Note: Peter also has a TInterfaceProperty. Merge later. }

  TInterfaceProperty = class(TComponentProperty)
  private
    FOrgStrProc: TGetStrProc;
    function IntfSupported(Component: TComponent): Boolean;
    procedure CheckAndAddComp(const S: string);
  protected
    function GetInterfaceGUID: TGUID; virtual; abstract;
    property OrgStrProc: TGetStrProc read FOrgStrProc write FOrgStrProc;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TJvDataProviderProperty = class(TInterfaceProperty)
  protected
    function GetInterfaceGUID: TGUID; override;
  end;
{$ENDIF COMPILER6_UP}


  TJvDataProviderTreeProperty = class(TEnumProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{$IFNDEF COMPILER6_UP}

{ TInterfaceProperty }

function TInterfaceProperty.IntfSupported(Component: TComponent): Boolean;
var
  Ref: IUnknown;
begin
  with Component do
    Result := GetInterface(IInterfaceComponentReference, Ref) and GetInterface(GetInterfaceGUID, Ref);
end;

procedure TInterfaceProperty.CheckAndAddComp(const S: string);
var
  Comp: TComponent;
begin
  Comp := Designer.GetComponent(S);
  if (Comp <> nil) and IntfSupported(Comp) then
    OrgStrProc(S);
end;

procedure TInterfaceProperty.GetValues(Proc: TGetStrProc);
begin
  OrgStrProc := Proc;
  inherited GetValues(CheckAndAddComp);
end;

procedure TInterfaceProperty.SetValue(const Value: string);
var
  Comp: TComponent;
begin
  if Value = '' then
    Comp := nil
  else
  begin
    Comp := Designer.GetComponent(Value);
    if not (Comp is GetTypeData(GetPropType)^.ClassType) and not IntfSupported(Comp) then
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetOrdValue(Longint(Comp));
end;

{ TJvDataProviderProperty }

function TJvDataProviderProperty.GetInterfaceGUID: TGUID;
begin
  Result := IJvDataProvider;
end;
{$ENDIF COMPILER6_UP}

{ TJvDataProviderTreeProperty }

procedure TJvDataProviderTreeProperty.Edit;
begin
  EditProvider(TJvCustomDataProvider(GetComponent(0)), Designer, GetName);
end;

function TJvDataProviderTreeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvDataProviderTreeProperty.GetValue: string;
begin
  Result := 'Data tree';
end;

procedure TJvDataProviderTreeProperty.SetValue(const Value: string);
begin
end;

procedure RegDataProviderPropEdits;
begin
{$IFNDEF COMPILER6_UP}
  RegisterPropertyEditor(TypeInfo(TComponent), TComponent, 'DataProvider', TJvDataProviderProperty);
{$ENDIF COMPILER6_UP}
  RegisterPropertyEditor(TypeInfo(TJvDataProviderTree), TComponent, '', TJvDataProviderTreeProperty);
end;

end.
