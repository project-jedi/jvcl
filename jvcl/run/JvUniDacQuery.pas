{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUniDacQuery.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Oracle Dataset with Threaded Functions

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvUniDacQuery;

{$I jvcl.inc}

interface

{$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Forms, Controls, DB,
  Uni, DBaccess,
  JvBaseDBThreadedDataset;
{$ENDIF USE_3RDPARTY_DEVART_UNIDAC}

{$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
type
  TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions = class(TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions)
  public
    constructor Create; override;
  published
    property All;
  End;

  TJvUniDacThreadedDatasetEnhancedOptions = Class(TJvBaseThreadedDatasetEnhancedOptions)
  private
    function GetAllowedContinueRecordFetchOptions: TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions;
    procedure SetAllowedContinueRecordFetchOptions(
      const Value: TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions);
  protected
    function CreateAllowedContinueRecordFetchOptions: TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
      override;
  published
    property AllowedContinueRecordFetchOptions: TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions read
      GetAllowedContinueRecordFetchOptions write SetAllowedContinueRecordFetchOptions;
  end;

  TJvUniDacDatasetThreadHandler = class(TJvBaseDatasetThreadHandler)
  private
    FRefreshKeyFields: string;
    FRefreshKeyValues: Variant;
  protected
    function CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions; override;
    property RefreshKeyFields: string read FRefreshKeyFields write
        FRefreshKeyFields;
    property RefreshKeyValues: Variant read FRefreshKeyValues write
        FRefreshKeyValues;
  public
    constructor Create(AOwner: TComponent; ADataset: TDataSet); reintroduce;
        override;
    procedure AfterRefresh; override;
    procedure BeforeRefresh; override;
    procedure RestoreRefreshKeyFields;
    procedure SaveRefreshKeyFields;
  End;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvUniDacUniQuery = class(TUniQuery, IJvThreadedDatasetInterface)
    procedure BreakExecution;
    procedure BringThreadDialogToFront;
    function DoGetInheritedNextRecord: Boolean;
    procedure DoInheritedAfterOpen;
    procedure DoInheritedAfterRefresh;
    procedure DoInheritedAfterScroll;
    procedure DoInheritedBeforeOpen;
    procedure DoInheritedBeforeRefresh;
    procedure DoInheritedInternalLast;
    procedure DoInheritedInternalRefresh;
    procedure DoInheritedSetActive(Active: Boolean);
    procedure DoInternalOpen;
    function GetDatasetFetchAllRecords: Boolean;
    function IsThreadAllowed: Boolean;
    procedure SetDatasetFetchAllRecords(const Value: Boolean);
  strict private
  private
    FBeforeFetch: TBeforeFetchEvent;
    FThreadHandler: TJvBaseDatasetThreadHandler;
    function GetAfterOpenFetch: TDataSetNotifyEvent;
    function GetAfterOpenFetch1: TDataSetNotifyEvent;
    function GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetEnhancedOptions: TJvUniDacThreadedDatasetEnhancedOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetAfterOpenFetch1(const Value: TDataSetNotifyEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(const Value:
        TJvUniDacThreadedDatasetEnhancedOptions);
    procedure SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
    property ThreadHandler: TJvBaseDatasetThreadHandler read FThreadHandler;
  protected
    procedure DoAfterOpen; override;
    procedure DoAfterRefresh; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforeRefresh; override;
    function GetNextRecord: Boolean; override;
    function GetOnThreadException: TJvThreadedDatasetThreadExceptionEvent;
    procedure InternalLast; override;
    procedure InternalRefresh; override;
    procedure ReplaceBeforeFetch(Dataset: TCustomDADataSet; var Cancel: Boolean);
    procedure SetActive(Value: Boolean); override;
    procedure SetOnThreadException(const Value:
        TJvThreadedDatasetThreadExceptionEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CurrentFetchDuration: TDateTime;
    function CurrentOpenDuration: TDateTime;
    function EofReached: Boolean;
    function ErrorException: Exception;
    function ErrorMessage: string;
    function ThreadIsActive: Boolean;
  published
    property AfterOpenFetch: TDataSetNotifyEvent read GetAfterOpenFetch1 write
        SetAfterOpenFetch1;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read
        GetAfterThreadExecution write SetAfterThreadExecution;
    property BeforeFetch: TBeforeFetchEvent read FBeforeFetch write FBeforeFetch;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read
        GetBeforeThreadExecution write SetBeforeThreadExecution;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property EnhancedOptions: TJvUniDacThreadedDatasetEnhancedOptions read GetEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions write SetThreadOptions;
    property OnThreadException: TJvThreadedDatasetThreadExceptionEvent read
        GetOnThreadException write SetOnThreadException;
  end;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvUniDacUniTable = class(TUniTable, IJvThreadedDatasetInterface)
    procedure BreakExecution;
    procedure BringThreadDialogToFront;
    function DoGetInheritedNextRecord: Boolean;
    procedure DoInheritedAfterOpen;
    procedure DoInheritedAfterRefresh;
    procedure DoInheritedAfterScroll;
    procedure DoInheritedBeforeOpen;
    procedure DoInheritedBeforeRefresh;
    procedure DoInheritedInternalLast;
    procedure DoInheritedInternalRefresh;
    procedure DoInheritedSetActive(Active: Boolean);
    procedure DoInternalOpen;
    function GetDatasetFetchAllRecords: Boolean;
    function IsThreadAllowed: Boolean;
  private
    FBeforeFetch: TBeforeFetchEvent;
    FThreadHandler: TJvBaseDatasetThreadHandler;
    function GetAfterOpenFetch: TDataSetNotifyEvent;
    function GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetEnhancedOptions: TJvUniDacThreadedDatasetEnhancedOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDatasetFetchAllRecords(const Value: Boolean);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(const Value:
        TJvUniDacThreadedDatasetEnhancedOptions);
    procedure SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
    property ThreadHandler: TJvBaseDatasetThreadHandler read FThreadHandler;
  protected
    procedure DoAfterOpen; override;
    procedure DoAfterRefresh; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforeRefresh; override;
    function GetNextRecord: Boolean; override;
    function GetOnThreadException: TJvThreadedDatasetThreadExceptionEvent;
    procedure InternalLast; override;
    procedure InternalRefresh; override;
    procedure ReplaceBeforeFetch(Dataset: TCustomDADataSet; var Cancel: Boolean);
    procedure SetActive(Value: Boolean); override;
    procedure SetOnThreadException(const Value: TJvThreadedDatasetThreadExceptionEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CurrentFetchDuration: TDateTime;
    function CurrentOpenDuration: TDateTime;
    function EofReached: Boolean;
    function ErrorException: Exception;
    function ErrorMessage: string;
    function ThreadIsActive: Boolean;
  published
    property AfterOpenFetch: TDataSetNotifyEvent read GetAfterOpenFetch write SetAfterOpenFetch;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read GetAfterThreadExecution write SetAfterThreadExecution;
    property BeforeFetch: TBeforeFetchEvent read FBeforeFetch write FBeforeFetch;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read GetBeforeThreadExecution write
        SetBeforeThreadExecution;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property EnhancedOptions: TJvUniDacThreadedDatasetEnhancedOptions read GetEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions write SetThreadOptions;
    property OnThreadException: TJvThreadedDatasetThreadExceptionEvent read GetOnThreadException write SetOnThreadException;
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
{$ENDIF USE_3RDPARTY_DEVART_UNIDAC}

implementation

{$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
uses Variants, MemData;

constructor TJvUniDacDatasetThreadHandler.Create(AOwner: TComponent; ADataset:
    TDataSet);
begin
  inherited Create(AOwner, ADataset);
end;

procedure TJvUniDacDatasetThreadHandler.AfterRefresh;
begin
  inherited AfterRefresh;
  if EnhancedOptions.RefreshLastPosition then
    RestoreRefreshKeyFields;
end;

procedure TJvUniDacDatasetThreadHandler.BeforeRefresh;
begin
  if EnhancedOptions.RefreshLastPosition then
    SaveRefreshKeyFields;
  inherited BeforeRefresh;
end;

//=== { TJvUniDacDatasetThreadHandler } ========================================

function TJvUniDacDatasetThreadHandler.CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions;
begin
  Result := TJvUniDacThreadedDatasetEnhancedOptions.Create;
end;

procedure TJvUniDacDatasetThreadHandler.RestoreRefreshKeyFields;
begin
  if Not (Dataset.Active and (Dataset is TCustomUniDataSet) and
    (TCustomUniDataSet(Dataset).KeyFields <> '') and (RefreshKeyFields <> '')) then
    Exit;
  TCustomUniDataSet(Dataset).LocateEx(RefreshKeyFields, RefreshKeyValues, [lxNearest])
end;

procedure TJvUniDacDatasetThreadHandler.SaveRefreshKeyFields;
var KeyFields : String;
  Fields:TStringList;
  Key : string;
  p: Integer;
  Field: TField;
  i: Integer;
begin
  RefreshKeyFields := '';
  if Not (Dataset.Active and (Dataset is TCustomUniDataSet) and (TCustomUniDataSet(Dataset).KeyFields <> '')) then
    Exit;
  Fields := tStringList.create;
  try
    KeyFields := trim(TCustomUniDataSet(Dataset).KeyFields);
    while KeyFields <> '' do
    begin
      p := Pos(';', KeyFields);
      if p > 0 then
      begin
        key := trim(Copy (KeyFields, 1, p-1));
        KeyFields := trim(Copy(KeyFields, p+1, Length(KeyFields)-p));
      end
      else
      begin
        key := KeyFields;
        KeyFields := '';
      end;
      if (Key <> '') and Assigned(Dataset.FindField(Key)) then
        Fields.Add(Key);
    end;
    FRefreshKeyValues := VarArrayCreate([0,Fields.Count-1], varVariant  );
    for i := 0 to Fields.Count - 1 do
    begin
      Field := Dataset.FindField(Fields[i]);
      if Assigned (Field) then
      begin
        RefreshKeyFields := RefreshKeyFields+Key+';';
        FRefreshKeyValues[i] := Field.AsVariant;
      end;
    end;
  finally
    Fields.Free;
  end;
end;

//=== { TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions } ============

constructor TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions.Create;
begin
  inherited Create;
  All := True;
end;

function
  TJvUniDacThreadedDatasetEnhancedOptions.CreateAllowedContinueRecordFetchOptions:
    TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
begin
  Result := TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions.Create;
end;

function
  TJvUniDacThreadedDatasetEnhancedOptions.GetAllowedContinueRecordFetchOptions:
    TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions;
begin
  Result := TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions(inherited AllowedContinueRecordFetchOptions);
end;

procedure
  TJvUniDacThreadedDatasetEnhancedOptions.SetAllowedContinueRecordFetchOptions(
    const Value: TJvUniDacThreadedDatasetAllowedContinueRecordFetchOptions);
begin
  inherited AllowedContinueRecordFetchOptions := Value;
end;

//=== { TJvUniDacSmartQuery } ==================================================

constructor TJvUniDacUniQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadHandler := TJvUniDacDatasetThreadHandler.Create(Self, Self);
  inherited BeforeFetch := ReplaceBeforeFetch;
end;

destructor TJvUniDacUniQuery.Destroy;
begin
  FreeAndNil(FThreadHandler);
  inherited Destroy;
end;

procedure TJvUniDacUniQuery.BreakExecution;
begin
  BreakExec;
end;

procedure TJvUniDacUniQuery.BringThreadDialogToFront;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BringDialogToFront;
end;

function TJvUniDacUniQuery.CurrentFetchDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentFetchDuration
  else
    Result := 0;
end;

function TJvUniDacUniQuery.CurrentOpenDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentOpenDuration
  else
    Result := 0;
end;

procedure TJvUniDacUniQuery.DoAfterOpen;
begin
  ThreadHandler.AfterOpen;
end;

procedure TJvUniDacUniQuery.DoAfterRefresh;
begin
  ThreadHandler.AfterRefresh;
end;

procedure TJvUniDacUniQuery.DoAfterScroll;
begin
  ThreadHandler.AfterScroll;
end;

procedure TJvUniDacUniQuery.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvUniDacUniQuery.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

function TJvUniDacUniQuery.DoGetInheritedNextRecord: Boolean;
begin
  Result := Inherited GetNextRecord;
end;

procedure TJvUniDacUniQuery.DoInheritedAfterOpen;
begin
  inherited DoAfterOpen;
end;

procedure TJvUniDacUniQuery.DoInheritedAfterRefresh;
begin
  inherited DoAfterRefresh;
end;

procedure TJvUniDacUniQuery.DoInheritedAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TJvUniDacUniQuery.DoInheritedBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

procedure TJvUniDacUniQuery.DoInheritedBeforeRefresh;
begin
  inherited DoBeforeRefresh;
end;

procedure TJvUniDacUniQuery.DoInheritedInternalLast;
begin
  inherited InternalLast;
end;

procedure TJvUniDacUniQuery.DoInheritedInternalRefresh;
begin
  inherited InternalRefresh;
end;

procedure TJvUniDacUniQuery.DoInheritedSetActive(Active: Boolean);
begin
  inherited SetActive(Active);
end;

procedure TJvUniDacUniQuery.DoInternalOpen;
begin
  InternalOpen;
end;

function TJvUniDacUniQuery.EofReached: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.EofReached
  else
    Result := False;
end;

function TJvUniDacUniQuery.ErrorException: Exception;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorException
  else
    Result := Nil;
end;

function TJvUniDacUniQuery.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvUniDacUniQuery.GetAfterOpenFetch: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvUniDacUniQuery.GetAfterOpenFetch1: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvUniDacUniQuery.GetAfterThreadExecution:
    TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterThreadExecution
  else
    Result := nil;
end;

function TJvUniDacUniQuery.GetBeforeThreadExecution:
    TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.BeforeThreadExecution
  else
    Result := nil;
end;

function TJvUniDacUniQuery.GetDatasetFetchAllRecords: Boolean;
begin
  Result := FetchAll;
end;

function TJvUniDacUniQuery.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.DialogOptions
  else
    Result := nil;
end;

function TJvUniDacUniQuery.GetEnhancedOptions:
    TJvUniDacThreadedDatasetEnhancedOptions;
begin
  if Assigned(ThreadHandler) then
    Result := TJvUniDacThreadedDatasetEnhancedOptions(ThreadHandler.EnhancedOptions)
  else
    Result := nil;
end;

function TJvUniDacUniQuery.GetNextRecord: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.GetNextRecord
  else
    Result := inherited GetNextRecord;
end;

function TJvUniDacUniQuery.GetOnThreadException:
    TJvThreadedDatasetThreadExceptionEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.OnThreadException
  else
    Result := nil;
end;

function TJvUniDacUniQuery.GetThreadOptions: TJvThreadedDatasetThreadOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadOptions
  else
    Result := nil;
end;

procedure TJvUniDacUniQuery.InternalLast;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalLast;
end;

procedure TJvUniDacUniQuery.InternalRefresh;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalRefresh;
end;

function TJvUniDacUniQuery.IsThreadAllowed: Boolean;
var ThreadedDatasetInterface : IJvThreadedDatasetInterface;
begin
  if Assigned(MasterSource) and Assigned(MasterSource.Dataset)
     and Supports(MasterSource.DataSet, IJvThreadedDatasetInterface, ThreadedDatasetInterface) then
    Result := not ThreadedDatasetInterface.ThreadIsActive
  else
    Result := True;
end;

procedure TJvUniDacUniQuery.ReplaceBeforeFetch(Dataset: TCustomDADataSet; var
    Cancel: Boolean);
begin
  if Assigned(ThreadHandler) then
    Cancel := ThreadHandler.CheckContinueRecordFetch <> tdccrContinue;
  if Assigned(BeforeFetch) and not Cancel then
    BeforeFetch(Dataset, Cancel);
end;

procedure TJvUniDacUniQuery.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;

procedure TJvUniDacUniQuery.SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvUniDacUniQuery.SetAfterOpenFetch1(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvUniDacUniQuery.SetAfterThreadExecution(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterThreadExecution := Value;
end;

procedure TJvUniDacUniQuery.SetBeforeThreadExecution(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvUniDacUniQuery.SetDatasetFetchAllRecords(const Value: Boolean);
begin
  FetchAll := Value;
end;

procedure TJvUniDacUniQuery.SetDialogOptions(Value:
    TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvUniDacUniQuery.SetEnhancedOptions(const Value:
    TJvUniDacThreadedDatasetEnhancedOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.EnhancedOptions.Assign(Value);
end;

procedure TJvUniDacUniQuery.SetOnThreadException(const Value:
    TJvThreadedDatasetThreadExceptionEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.OnThreadException := Value;
end;

procedure TJvUniDacUniQuery.SetThreadOptions(const Value:
    TJvThreadedDatasetThreadOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.ThreadOptions.Assign(Value);
end;

function TJvUniDacUniQuery.ThreadIsActive: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadIsActive
  else
    Result := False;
end;

//=== { TJvUniDacSmartQuery } ==================================================

constructor TJvUniDacUniTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadHandler := TJvUniDacDatasetThreadHandler.Create(Self, Self);
  inherited BeforeFetch := ReplaceBeforeFetch;
end;

destructor TJvUniDacUniTable.Destroy;
begin
  FreeAndNil(FThreadHandler);
  inherited Destroy;
end;

procedure TJvUniDacUniTable.BreakExecution;
begin
  BreakExec;
end;

procedure TJvUniDacUniTable.BringThreadDialogToFront;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BringDialogToFront;
end;

function TJvUniDacUniTable.CurrentFetchDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentFetchDuration
  else
    Result := 0;
end;

function TJvUniDacUniTable.CurrentOpenDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentOpenDuration
  else
    Result := 0;
end;

procedure TJvUniDacUniTable.DoAfterOpen;
begin
  ThreadHandler.AfterOpen;
end;

procedure TJvUniDacUniTable.DoAfterRefresh;
begin
  ThreadHandler.AfterRefresh;
end;

procedure TJvUniDacUniTable.DoAfterScroll;
begin
  ThreadHandler.AfterScroll;
end;

procedure TJvUniDacUniTable.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvUniDacUniTable.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

function TJvUniDacUniTable.DoGetInheritedNextRecord: Boolean;
begin
  Result := Inherited GetNextRecord;
end;

procedure TJvUniDacUniTable.DoInheritedAfterOpen;
begin
  inherited DoAfterOpen;
end;

procedure TJvUniDacUniTable.DoInheritedAfterRefresh;
begin
  inherited DoAfterRefresh;
end;

procedure TJvUniDacUniTable.DoInheritedAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TJvUniDacUniTable.DoInheritedBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

procedure TJvUniDacUniTable.DoInheritedBeforeRefresh;
begin
  inherited DoBeforeRefresh;
end;

procedure TJvUniDacUniTable.DoInheritedInternalLast;
begin
  inherited InternalLast;
end;

procedure TJvUniDacUniTable.DoInheritedInternalRefresh;
begin
  inherited InternalRefresh;
end;

procedure TJvUniDacUniTable.DoInheritedSetActive(Active: Boolean);
begin
  inherited SetActive(Active);
end;

procedure TJvUniDacUniTable.DoInternalOpen;
begin
  InternalOpen;
end;

function TJvUniDacUniTable.EofReached: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.EofReached
  else
    Result := False;
end;

function TJvUniDacUniTable.ErrorException: Exception;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorException
  else
    Result := Nil;
end;

function TJvUniDacUniTable.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvUniDacUniTable.GetAfterOpenFetch: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvUniDacUniTable.GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterThreadExecution
  else
    Result := nil;
end;

function TJvUniDacUniTable.GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.BeforeThreadExecution
  else
    Result := nil;
end;

function TJvUniDacUniTable.GetDatasetFetchAllRecords: Boolean;
begin
  Result := FetchAll;
end;

function TJvUniDacUniTable.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.DialogOptions
  else
    Result := nil;
end;

function TJvUniDacUniTable.GetEnhancedOptions:
    TJvUniDacThreadedDatasetEnhancedOptions;
begin
  if Assigned(ThreadHandler) then
    Result := TJvUniDacThreadedDatasetEnhancedOptions(ThreadHandler.EnhancedOptions)
  else
    Result := nil;
end;

function TJvUniDacUniTable.GetNextRecord: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.GetNextRecord
  else
    Result := inherited GetNextRecord;
end;

function TJvUniDacUniTable.GetOnThreadException: TJvThreadedDatasetThreadExceptionEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.OnThreadException
  else
    Result := nil;
end;

function TJvUniDacUniTable.GetThreadOptions: TJvThreadedDatasetThreadOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadOptions
  else
    Result := nil;
end;

procedure TJvUniDacUniTable.InternalLast;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalLast;
end;

procedure TJvUniDacUniTable.InternalRefresh;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalRefresh;
end;

function TJvUniDacUniTable.IsThreadAllowed: Boolean;
var ThreadedDatasetInterface : IJvThreadedDatasetInterface;
begin
  if Assigned(MasterSource) and Assigned(MasterSource.Dataset)
     and Supports(MasterSource.DataSet, IJvThreadedDatasetInterface, ThreadedDatasetInterface) then
    Result := not ThreadedDatasetInterface.ThreadIsActive
  else
    Result := True;
end;

procedure TJvUniDacUniTable.ReplaceBeforeFetch(Dataset: TCustomDADataSet; var
    Cancel: Boolean);
begin
  if Assigned(ThreadHandler) then
    Cancel := ThreadHandler.CheckContinueRecordFetch <> tdccrContinue;
  if Assigned(BeforeFetch) and not Cancel then
    BeforeFetch(Dataset, Cancel);
end;

procedure TJvUniDacUniTable.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;

procedure TJvUniDacUniTable.SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvUniDacUniTable.SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterThreadExecution := Value;
end;

procedure TJvUniDacUniTable.SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvUniDacUniTable.SetDatasetFetchAllRecords(const Value: Boolean);
begin
  FetchAll := Value;
end;

procedure TJvUniDacUniTable.SetDialogOptions(Value:
    TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvUniDacUniTable.SetEnhancedOptions(const Value:
    TJvUniDacThreadedDatasetEnhancedOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.EnhancedOptions.Assign(Value);
end;

procedure TJvUniDacUniTable.SetOnThreadException(const Value: TJvThreadedDatasetThreadExceptionEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.OnThreadException := Value;
end;

procedure TJvUniDacUniTable.SetThreadOptions(const Value:
    TJvThreadedDatasetThreadOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.ThreadOptions.Assign(Value);
end;

function TJvUniDacUniTable.ThreadIsActive: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadIsActive
  else
    Result := False;
end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
{$ENDIF USE_3RDPARTY_DEVART_UNIDAC}
end.

