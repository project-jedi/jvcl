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

unit JvDBReg;

{$IFDEF DelphiPersonalEdition}

interface

implementation

end.

{$ELSE}

interface

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF}
  SysUtils, DB;

{ Register data aware custom controls and components }

procedure Register;

implementation

uses
  TypInfo,
  JvSelDSFrm, JvMemTableEditor, JvDBEditors,  
  JvMemDS,
  JvDBRichEd,
  JvDBCtrl, JvLookup, JvDBComb, JvVCLUtils, JvxDConst;

{ Designer registration }

procedure Register;
begin
  { Database Components are excluded from the STD SKU }
  if GDAL = LongWord(-16) then
    Exit;

  { Data aware components and controls }
  RegisterComponents(srJvDataAccessPalette,
    [TJvMemoryData]);

  RegisterComponents(srJvDataControlsPalette,
    [TJvDBGrid, TJvDBLookupList, TJvDBLookupCombo, TJvLookupEdit, TJvDBDateEdit,
     TJvDBCalcEdit, TJvDBComboEdit, TJvDBRichEdit,
     TJvDBStatusLabel, TJvDBComboBox]);

  RegisterNonActiveX([TJvMemoryData, TJvDBGrid, TJvDBDateEdit,
    TJvDBStatusLabel, TJvDBComboBox, TJvDBLookupList,
    TJvDBLookupCombo, TJvLookupEdit, TJvDBComboEdit, TJvDBCalcEdit,
    TJvDBRichEdit, TJvCustomDBComboBox, TJvLookupControl],
    axrComponentOnly);
  { Property and component editors for data aware components }
  RegisterPropertyEditor(TypeInfo(string), TJvLookupControl, 'LookupField',
    TJvLookupSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvLookupEdit, 'LookupField',
    TJvLookupSourceProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TJvDBGrid, 'RowsHeight', nil);
  RegisterComponentEditor(TJvMemoryData, TJvMemDataSetEditor);
end;

{$ENDIF DelphiPersonalEdition}

end.

