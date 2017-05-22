{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDynControlEngineDBIntf;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, DB;

type
  IJvDynControlDatabase = interface
    ['{E9F43566-9D52-4DB3-8D58-ABC3366FA1BA}']
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
    property ControlDatasource: TDatasource read ControlGetDatasource write ControlSetDatasource;
    property ControlDataField: string read ControlGetDataField write ControlSetDataField;
  end;

  IJvDynControlDBCheckbox = interface
    ['{5C3BAC5F-1340-43BF-8D59-A9A56A73A3C4}']
    procedure ControlSetValueChecked(Value: Variant);
    procedure ControlSetValueUnChecked(Value: Variant);
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.