{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I JVCL.INC}

unit JvDBReg;

interface

uses Classes,
{$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
{$ELSE}LibIntf, DsgnIntf,{$ENDIF} SysUtils, DB;

{ Register data aware custom controls and components }

procedure Register;

implementation

{.$IFDEF WIN32}
{.$R *.Res}
{.$ELSE}
{.$R *.D16}
{.$ENDIF}

uses TypInfo, JvLConst, JvDBCtrl, JvLookup, JvxLogin, JvDBComb, JvVCLUtils,
{$IFNDEF COMPILER3_UP}DBTables, {$ENDIF}{$IFDEF DCS}JvSelDSFrm, {$ENDIF}
{$IFDEF COMPILER3_UP}JvMemDS, {$ENDIF}{$IFDEF WIN32}JvDBRichEd, {$ENDIF}
  Consts, LibHelp, JvDsgn;

{ TJvFieldProperty }
{ For TJvDBLookupList, TJvDBLookupCombo components }

type
  TJvFieldProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
    function GetDataSourcePropName: string; virtual;
  end;

function TJvFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'LookupSource';
end;

procedure TJvFieldProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := TComponent(GetComponent(0));
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

{$IFDEF DCS}
{$IFDEF COMPILER3_UP}

{ TJvMemoryDataEditor }

type
  TJvMemoryDataEditor = class(TJvMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;

function TJvMemoryDataEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TJvMemoryData;
  if Result then
    TJvMemoryData(Dest).CopyStructure(Source);
end;

{$ENDIF COMPILER3_UP}
{$ENDIF DCS}

{ Designer registration }

procedure Register;
begin
{$IFDEF COMPILER4_UP}
  { Database Components are excluded from the STD SKU }
  if GDAL = LongWord(-16) then
    Exit;
{$ENDIF}

  { Data aware components and controls }
  RegisterComponents('Jv Data Controls', [
    TJvDBGrid, TJvDBLookupList, TJvDBLookupCombo, TJvLookupEdit, TJvDBDateEdit,
      TJvDBCalcEdit, TJvDBComboEdit, {$IFDEF WIN32}TJvDBRichEdit, {$ENDIF}
      TJvDBStatusLabel, TJvDBComboBox]);

  RegisterComponents('Jv Data Access', [{$IFDEF COMPILER3_UP}TJvMemoryData, {$ENDIF}TJvLoginDialog]);

{$IFDEF COMPILER3_UP}
  RegisterNonActiveX([TJvMemoryData, TJvDBGrid, TJvDBDateEdit,
    TJvDBStatusLabel, TJvDBComboBox, TJvDBLookupList,
      TJvDBLookupCombo, TJvLookupEdit, TJvDBComboEdit, TJvDBCalcEdit,
      TJvDBRichEdit, TJvCustomDBComboBox, TJvLookupControl, TJvLoginDialog],
      axrComponentOnly);
{$ENDIF COMPILER3_UP}
  { Property and component editors for data aware components }
  RegisterPropertyEditor(TypeInfo(string), TJvLookupControl, 'LookupField',
    TJvFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvLookupEdit, 'LookupField',
    TJvFieldProperty);
{$IFDEF COMPILER3_UP}
  RegisterPropertyEditor(TypeInfo(Integer), TJvDBGrid, 'RowsHeight', nil);
{$IFDEF DCS}
  RegisterComponentEditor(TJvMemoryData, TJvMemoryDataEditor);
{$ENDIF DCS}
{$ENDIF COMPILER3_UP}

end;

end.

