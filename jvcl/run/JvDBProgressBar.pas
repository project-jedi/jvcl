{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBProgressBar.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Thornqvist[peter3@peter3.com]
  Moved here from JvProgressBar to support D& Personal

Last Modified: 2002-07-13

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDBProgressBar;

interface
uses
  SysUtils, Classes, Controls, ComCtrls,
  DB, DBCtrls,
  JvProgressBar;

type
  TJvDBProgressBar = class(TJvProgressBar)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    procedure SetDataField(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetField: TField;
  public
    procedure DataChange(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

constructor TJvDBProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TJvDBProgressBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TJvDBProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBProgressBar.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TJvDBProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBProgressBar.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TJvDBProgressBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBProgressBar.DataChange(Sender: TObject);
begin
  if (FDataLink.Field <> nil) and (FDataLink.Field is TNumericField) then
    Position := FDataLink.Field.AsInteger
  else
    Position := Min;
end;

end.

