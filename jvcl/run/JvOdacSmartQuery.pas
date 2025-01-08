{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOdacSmartQuery.PAS, released on 2002-05-26.

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

unit JvOdacSmartQuery;

{$I jvcl.inc}

interface

{$IFDEF USE_3RDPARTY_DEVART_ODAC}
uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Forms, Controls, DB,
  OraSmart, Ora, DBaccess,
  JvBaseDBThreadedDataset;
{$ENDIF USE_3RDPARTY_DEVART_ODAC}

{$IFDEF USE_3RDPARTY_DEVART_ODAC}
type
  TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions =
    class(TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions)
  public
    constructor Create; override;
  published
    property All;
  End;

  TJvOdacThreadedDatasetEnhancedOptions = Class(TJvBaseThreadedDatasetEnhancedOptions)
  private
    function GetAllowedContinueRecordFetchOptions: TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions;
    procedure SetAllowedContinueRecordFetchOptions(
      const Value: TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions);
  protected
    function CreateAllowedContinueRecordFetchOptions: TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
      override;
  published
    property AllowedContinueRecordFetchOptions: TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions read
      GetAllowedContinueRecordFetchOptions write SetAllowedContinueRecordFetchOptions;
  end;

  TJvOdacDatasetThreadHandler = class(TJvBaseDatasetThreadHandler)
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
  TJvOdacSmartQuery = class(TSmartQuery, IJvThreadedDatasetInterface)
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
  private
    FBeforeFetch: TBeforeFetchEvent;
    FThreadHandler: TJvBaseDatasetThreadHandler;
    function GetAfterOpenFetch: TDataSetNotifyEvent;
    function GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetEnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(const Value:
        TJvOdacThreadedDatasetEnhancedOptions);
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
    property AfterOpenFetch: TDataSetNotifyEvent read GetAfterOpenFetch write
        SetAfterOpenFetch;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read
        GetAfterThreadExecution write SetAfterThreadExecution;
    property BeforeFetch: TBeforeFetchEvent read FBeforeFetch write FBeforeFetch;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read
        GetBeforeThreadExecution write SetBeforeThreadExecution;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property EnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions read GetEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions write SetThreadOptions;
    property OnThreadException: TJvThreadedDatasetThreadExceptionEvent read
        GetOnThreadException write SetOnThreadException;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvOdacOraQuery = class(TOraQuery, IJvThreadedDatasetInterface)
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
    function GetEnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetAfterOpenFetch1(const Value: TDataSetNotifyEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(const Value:
        TJvOdacThreadedDatasetEnhancedOptions);
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
    property EnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions read GetEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions write SetThreadOptions;
    property OnThreadException: TJvThreadedDatasetThreadExceptionEvent read
        GetOnThreadException write SetOnThreadException;
  end;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvOdacOraTable = class(TOraTable, IJvThreadedDatasetInterface)
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
    function GetEnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDatasetFetchAllRecords(const Value: Boolean);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(const Value:
        TJvOdacThreadedDatasetEnhancedOptions);
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
    property AfterOpenFetch: TDataSetNotifyEvent read GetAfterOpenFetch write
        SetAfterOpenFetch;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read
        GetAfterThreadExecution write SetAfterThreadExecution;
    property BeforeFetch: TBeforeFetchEvent read FBeforeFetch write FBeforeFetch;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read
        GetBeforeThreadExecution write SetBeforeThreadExecution;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property EnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions read GetEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions write SetThreadOptions;
    property OnThreadException: TJvThreadedDatasetThreadExceptionEvent read
        GetOnThreadException write SetOnThreadException;
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
{$ENDIF USE_3RDPARTY_DEVART_ODAC}

implementation

{$IFDEF USE_3RDPARTY_DEVART_ODAC}
uses Variants, MemData;

//=== { TJvOdacSmartQuery } ==================================================

constructor TJvOdacSmartQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadHandler := TJvOdacDatasetThreadHandler.Create(Self, Self);
  inherited BeforeFetch := ReplaceBeforeFetch;
end;

destructor TJvOdacSmartQuery.Destroy;
begin
  FreeAndNil(FThreadHandler);
  inherited Destroy;
end;

procedure TJvOdacSmartQuery.BreakExecution;
begin
  BreakExec;
end;

procedure TJvOdacSmartQuery.BringThreadDialogToFront;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BringDialogToFront;
end;

function TJvOdacSmartQuery.CurrentFetchDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentFetchDuration
  else
    Result := 0;
end;

function TJvOdacSmartQuery.CurrentOpenDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentOpenDuration
  else
    Result := 0;
end;

procedure TJvOdacSmartQuery.DoAfterOpen;
begin
  ThreadHandler.AfterOpen;
end;

procedure TJvOdacSmartQuery.DoAfterRefresh;
begin
  ThreadHandler.AfterRefresh;
end;

procedure TJvOdacSmartQuery.DoAfterScroll;
begin
  ThreadHandler.AfterScroll;
end;

procedure TJvOdacSmartQuery.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvOdacSmartQuery.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

function TJvOdacSmartQuery.DoGetInheritedNextRecord: Boolean;
begin
  Result := Inherited GetNextRecord;
end;

procedure TJvOdacSmartQuery.DoInheritedAfterOpen;
begin
  inherited DoAfterOpen;
end;

procedure TJvOdacSmartQuery.DoInheritedAfterRefresh;
begin
  inherited DoAfterRefresh;
end;

procedure TJvOdacSmartQuery.DoInheritedAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TJvOdacSmartQuery.DoInheritedBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

procedure TJvOdacSmartQuery.DoInheritedBeforeRefresh;
begin
  inherited DoBeforeRefresh;
end;

procedure TJvOdacSmartQuery.DoInheritedInternalLast;
begin
  inherited InternalLast;
end;

procedure TJvOdacSmartQuery.DoInheritedInternalRefresh;
begin
  inherited InternalRefresh;
end;

procedure TJvOdacSmartQuery.DoInheritedSetActive(Active: Boolean);
begin
  inherited SetActive(Active);
end;

procedure TJvOdacSmartQuery.DoInternalOpen;
begin
  InternalOpen;
end;

function TJvOdacSmartQuery.EofReached: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.EofReached
  else
    Result := False;
end;

function TJvOdacSmartQuery.ErrorException: Exception;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorException
  else
    Result := Nil;
end;

function TJvOdacSmartQuery.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvOdacSmartQuery.GetAfterOpenFetch: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvOdacSmartQuery.GetAfterThreadExecution:
    TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterThreadExecution
  else
    Result := nil;
end;

function TJvOdacSmartQuery.GetBeforeThreadExecution:
    TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.BeforeThreadExecution
  else
    Result := nil;
end;

function TJvOdacSmartQuery.GetDatasetFetchAllRecords: Boolean;
begin
  Result := FetchAll;
end;

function TJvOdacSmartQuery.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.DialogOptions
  else
    Result := nil;
end;

function TJvOdacSmartQuery.GetEnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions;
begin
  if Assigned(ThreadHandler) then
    Result := TJvOdacThreadedDatasetEnhancedOptions(ThreadHandler.EnhancedOptions)
  else
    Result := nil;
end;

function TJvOdacSmartQuery.GetNextRecord: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.GetNextRecord
  else
    Result := inherited GetNextRecord;
end;

function TJvOdacSmartQuery.GetOnThreadException:
    TJvThreadedDatasetThreadExceptionEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.OnThreadException
  else
    Result := nil;
end;

function TJvOdacSmartQuery.GetThreadOptions: TJvThreadedDatasetThreadOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadOptions
  else
    Result := nil;
end;

procedure TJvOdacSmartQuery.InternalLast;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalLast;
end;

procedure TJvOdacSmartQuery.InternalRefresh;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalRefresh;
end;

function TJvOdacSmartQuery.IsThreadAllowed: Boolean;
var ThreadedDatasetInterface : IJvThreadedDatasetInterface;
begin
  if Assigned(MasterSource) and Assigned(MasterSource.Dataset)
     and Supports(MasterSource.DataSet, IJvThreadedDatasetInterface, ThreadedDatasetInterface) then
    Result := not ThreadedDatasetInterface.ThreadIsActive
  else
    Result := True;
end;

procedure TJvOdacSmartQuery.ReplaceBeforeFetch(Dataset: TCustomDADataSet; var Cancel: Boolean);
begin
  if Assigned(ThreadHandler) then
    Cancel := ThreadHandler.CheckContinueRecordFetch <> tdccrContinue;
  if Assigned(BeforeFetch) and not Cancel then
    BeforeFetch(Dataset, Cancel);
end;

procedure TJvOdacSmartQuery.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;

procedure TJvOdacSmartQuery.SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvOdacSmartQuery.SetAfterThreadExecution(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterThreadExecution := Value;
end;

procedure TJvOdacSmartQuery.SetBeforeThreadExecution(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvOdacSmartQuery.SetDatasetFetchAllRecords(const Value: Boolean);
begin
  FetchAll := Value;
end;

procedure TJvOdacSmartQuery.SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvOdacSmartQuery.SetEnhancedOptions(const Value: TJvOdacThreadedDatasetEnhancedOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.EnhancedOptions.Assign(Value);
end;

procedure TJvOdacSmartQuery.SetOnThreadException(const Value:
    TJvThreadedDatasetThreadExceptionEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.OnThreadException := Value;
end;

procedure TJvOdacSmartQuery.SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.ThreadOptions.Assign(Value);
end;

function TJvOdacSmartQuery.ThreadIsActive: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadIsActive
  else
    Result := False;
end;

constructor TJvOdacDatasetThreadHandler.Create(AOwner: TComponent; ADataset:
    TDataSet);
begin
  inherited Create(AOwner, ADataset);
end;

procedure TJvOdacDatasetThreadHandler.AfterRefresh;
begin
  inherited AfterRefresh;
  if EnhancedOptions.RefreshLastPosition then
    RestoreRefreshKeyFields;
end;

procedure TJvOdacDatasetThreadHandler.BeforeRefresh;
begin
  if EnhancedOptions.RefreshLastPosition then
    SaveRefreshKeyFields;
  inherited BeforeRefresh;
end;

//=== { TJvOdacDatasetThreadHandler } ========================================

function TJvOdacDatasetThreadHandler.CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions;
begin
  Result := TJvOdacThreadedDatasetEnhancedOptions.Create;
end;

procedure TJvOdacDatasetThreadHandler.RestoreRefreshKeyFields;
begin
  if Not (Dataset.Active and (Dataset is TOraDataset) and
    (TOraDataset(Dataset).KeyFields <> '') and (RefreshKeyFields <> '')) then
    Exit;
  TOraDataset(Dataset).LocateEx(RefreshKeyFields, RefreshKeyValues, [lxNearest])
end;

procedure TJvOdacDatasetThreadHandler.SaveRefreshKeyFields;
var KeyFields : String;
  Fields:TStringList;
  Key : string;
  p: Integer;
  Field: TField;
  i: Integer;
begin
  RefreshKeyFields := '';
  if Not (Dataset.Active and (Dataset is TOraDataset) and (TOraDataset(Dataset).KeyFields <> '')) then
    Exit;
  Fields := tStringList.create;
  try
    KeyFields := trim(TOraDataset(Dataset).KeyFields);
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

//=== { TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions } ============

constructor TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions.Create;
begin
  inherited Create;
  All := True;
end;

function
  TJvOdacThreadedDatasetEnhancedOptions.CreateAllowedContinueRecordFetchOptions:
    TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
begin
  Result := TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions.Create;
end;

function
  TJvOdacThreadedDatasetEnhancedOptions.GetAllowedContinueRecordFetchOptions:
    TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions;
begin
  Result := TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions(inherited AllowedContinueRecordFetchOptions);
end;

procedure
  TJvOdacThreadedDatasetEnhancedOptions.SetAllowedContinueRecordFetchOptions(
    const Value: TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions);
begin
  inherited AllowedContinueRecordFetchOptions := Value;
end;

//=== { TJvOdacSmartQuery } ==================================================

constructor TJvOdacOraQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadHandler := TJvOdacDatasetThreadHandler.Create(Self, Self);
  inherited BeforeFetch := ReplaceBeforeFetch;
end;

destructor TJvOdacOraQuery.Destroy;
begin
  FreeAndNil(FThreadHandler);
  inherited Destroy;
end;

procedure TJvOdacOraQuery.BreakExecution;
begin
  BreakExec;
end;

procedure TJvOdacOraQuery.BringThreadDialogToFront;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BringDialogToFront;
end;

function TJvOdacOraQuery.CurrentFetchDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentFetchDuration
  else
    Result := 0;
end;

function TJvOdacOraQuery.CurrentOpenDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentOpenDuration
  else
    Result := 0;
end;

procedure TJvOdacOraQuery.DoAfterOpen;
begin
  ThreadHandler.AfterOpen;
end;

procedure TJvOdacOraQuery.DoAfterRefresh;
begin
  ThreadHandler.AfterRefresh;
end;

procedure TJvOdacOraQuery.DoAfterScroll;
begin
  ThreadHandler.AfterScroll;
end;

procedure TJvOdacOraQuery.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvOdacOraQuery.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

function TJvOdacOraQuery.DoGetInheritedNextRecord: Boolean;
begin
  Result := Inherited GetNextRecord;
end;

procedure TJvOdacOraQuery.DoInheritedAfterOpen;
begin
  inherited DoAfterOpen;
end;

procedure TJvOdacOraQuery.DoInheritedAfterRefresh;
begin
  inherited DoAfterRefresh;
end;

procedure TJvOdacOraQuery.DoInheritedAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TJvOdacOraQuery.DoInheritedBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

procedure TJvOdacOraQuery.DoInheritedBeforeRefresh;
begin
  inherited DoBeforeRefresh;
end;

procedure TJvOdacOraQuery.DoInheritedInternalLast;
begin
  inherited InternalLast;
end;

procedure TJvOdacOraQuery.DoInheritedInternalRefresh;
begin
  inherited InternalRefresh;
end;

procedure TJvOdacOraQuery.DoInheritedSetActive(Active: Boolean);
begin
  inherited SetActive(Active);
end;

procedure TJvOdacOraQuery.DoInternalOpen;
begin
  InternalOpen;
end;

function TJvOdacOraQuery.EofReached: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.EofReached
  else
    Result := False;
end;

function TJvOdacOraQuery.ErrorException: Exception;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorException
  else
    Result := Nil;
end;

function TJvOdacOraQuery.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvOdacOraQuery.GetAfterOpenFetch: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvOdacOraQuery.GetAfterOpenFetch1: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvOdacOraQuery.GetAfterThreadExecution:
    TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterThreadExecution
  else
    Result := nil;
end;

function TJvOdacOraQuery.GetBeforeThreadExecution:
    TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.BeforeThreadExecution
  else
    Result := nil;
end;

function TJvOdacOraQuery.GetDatasetFetchAllRecords: Boolean;
begin
  Result := FetchAll;
end;

function TJvOdacOraQuery.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.DialogOptions
  else
    Result := nil;
end;

function TJvOdacOraQuery.GetEnhancedOptions:
    TJvOdacThreadedDatasetEnhancedOptions;
begin
  if Assigned(ThreadHandler) then
    Result := TJvOdacThreadedDatasetEnhancedOptions(ThreadHandler.EnhancedOptions)
  else
    Result := nil;
end;

function TJvOdacOraQuery.GetNextRecord: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.GetNextRecord
  else
    Result := inherited GetNextRecord;
end;

function TJvOdacOraQuery.GetOnThreadException:
    TJvThreadedDatasetThreadExceptionEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.OnThreadException
  else
    Result := nil;
end;

function TJvOdacOraQuery.GetThreadOptions: TJvThreadedDatasetThreadOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadOptions
  else
    Result := nil;
end;

procedure TJvOdacOraQuery.InternalLast;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalLast;
end;

procedure TJvOdacOraQuery.InternalRefresh;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalRefresh;
end;

function TJvOdacOraQuery.IsThreadAllowed: Boolean;
var ThreadedDatasetInterface : IJvThreadedDatasetInterface;
begin
  if Assigned(MasterSource) and Assigned(MasterSource.Dataset)
     and Supports(MasterSource.DataSet, IJvThreadedDatasetInterface, ThreadedDatasetInterface) then
    Result := not ThreadedDatasetInterface.ThreadIsActive
  else
    Result := True;
end;

procedure TJvOdacOraQuery.ReplaceBeforeFetch(Dataset: TCustomDADataSet; var
    Cancel: Boolean);
begin
  if Assigned(ThreadHandler) then
    Cancel := ThreadHandler.CheckContinueRecordFetch <> tdccrContinue;
  if Assigned(BeforeFetch) and not Cancel then
    BeforeFetch(Dataset, Cancel);
end;

procedure TJvOdacOraQuery.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;

procedure TJvOdacOraQuery.SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvOdacOraQuery.SetAfterOpenFetch1(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvOdacOraQuery.SetAfterThreadExecution(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterThreadExecution := Value;
end;

procedure TJvOdacOraQuery.SetBeforeThreadExecution(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvOdacOraQuery.SetDatasetFetchAllRecords(const Value: Boolean);
begin
  FetchAll := Value;
end;

procedure TJvOdacOraQuery.SetDialogOptions(Value:
    TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvOdacOraQuery.SetEnhancedOptions(const Value:
    TJvOdacThreadedDatasetEnhancedOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.EnhancedOptions.Assign(Value);
end;

procedure TJvOdacOraQuery.SetOnThreadException(const Value:
    TJvThreadedDatasetThreadExceptionEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.OnThreadException := Value;
end;

procedure TJvOdacOraQuery.SetThreadOptions(const Value:
    TJvThreadedDatasetThreadOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.ThreadOptions.Assign(Value);
end;

function TJvOdacOraQuery.ThreadIsActive: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadIsActive
  else
    Result := False;
end;

//=== { TJvOdacSmartQuery } ==================================================

constructor TJvOdacOraTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadHandler := TJvOdacDatasetThreadHandler.Create(Self, Self);
  inherited BeforeFetch := ReplaceBeforeFetch;
end;

destructor TJvOdacOraTable.Destroy;
begin
  FreeAndNil(FThreadHandler);
  inherited Destroy;
end;

procedure TJvOdacOraTable.BreakExecution;
begin
  BreakExec;
end;

procedure TJvOdacOraTable.BringThreadDialogToFront;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BringDialogToFront;
end;

function TJvOdacOraTable.CurrentFetchDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentFetchDuration
  else
    Result := 0;
end;

function TJvOdacOraTable.CurrentOpenDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentOpenDuration
  else
    Result := 0;
end;

procedure TJvOdacOraTable.DoAfterOpen;
begin
  ThreadHandler.AfterOpen;
end;

procedure TJvOdacOraTable.DoAfterRefresh;
begin
  ThreadHandler.AfterRefresh;
end;

procedure TJvOdacOraTable.DoAfterScroll;
begin
  ThreadHandler.AfterScroll;
end;

procedure TJvOdacOraTable.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvOdacOraTable.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

function TJvOdacOraTable.DoGetInheritedNextRecord: Boolean;
begin
  Result := Inherited GetNextRecord;
end;

procedure TJvOdacOraTable.DoInheritedAfterOpen;
begin
  inherited DoAfterOpen;
end;

procedure TJvOdacOraTable.DoInheritedAfterRefresh;
begin
  inherited DoAfterRefresh;
end;

procedure TJvOdacOraTable.DoInheritedAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TJvOdacOraTable.DoInheritedBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

procedure TJvOdacOraTable.DoInheritedBeforeRefresh;
begin
  inherited DoBeforeRefresh;
end;

procedure TJvOdacOraTable.DoInheritedInternalLast;
begin
  inherited InternalLast;
end;

procedure TJvOdacOraTable.DoInheritedInternalRefresh;
begin
  inherited InternalRefresh;
end;

procedure TJvOdacOraTable.DoInheritedSetActive(Active: Boolean);
begin
  inherited SetActive(Active);
end;

procedure TJvOdacOraTable.DoInternalOpen;
begin
  InternalOpen;
end;

function TJvOdacOraTable.EofReached: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.EofReached
  else
    Result := False;
end;

function TJvOdacOraTable.ErrorException: Exception;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorException
  else
    Result := Nil;
end;

function TJvOdacOraTable.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvOdacOraTable.GetAfterOpenFetch: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvOdacOraTable.GetAfterThreadExecution:
    TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterThreadExecution
  else
    Result := nil;
end;

function TJvOdacOraTable.GetBeforeThreadExecution:
    TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.BeforeThreadExecution
  else
    Result := nil;
end;

function TJvOdacOraTable.GetDatasetFetchAllRecords: Boolean;
begin
  Result := FetchAll;
end;

function TJvOdacOraTable.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.DialogOptions
  else
    Result := nil;
end;

function TJvOdacOraTable.GetEnhancedOptions:
    TJvOdacThreadedDatasetEnhancedOptions;
begin
  if Assigned(ThreadHandler) then
    Result := TJvOdacThreadedDatasetEnhancedOptions(ThreadHandler.EnhancedOptions)
  else
    Result := nil;
end;

function TJvOdacOraTable.GetNextRecord: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.GetNextRecord
  else
    Result := inherited GetNextRecord;
end;

function TJvOdacOraTable.GetOnThreadException:
    TJvThreadedDatasetThreadExceptionEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.OnThreadException
  else
    Result := nil;
end;

function TJvOdacOraTable.GetThreadOptions: TJvThreadedDatasetThreadOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadOptions
  else
    Result := nil;
end;

procedure TJvOdacOraTable.InternalLast;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalLast;
end;

procedure TJvOdacOraTable.InternalRefresh;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalRefresh;
end;

function TJvOdacOraTable.IsThreadAllowed: Boolean;
var ThreadedDatasetInterface : IJvThreadedDatasetInterface;
begin
  if Assigned(MasterSource) and Assigned(MasterSource.Dataset)
     and Supports(MasterSource.DataSet, IJvThreadedDatasetInterface, ThreadedDatasetInterface) then
    Result := not ThreadedDatasetInterface.ThreadIsActive
  else
    Result := True;
end;

procedure TJvOdacOraTable.ReplaceBeforeFetch(Dataset: TCustomDADataSet; var
    Cancel: Boolean);
begin
  if Assigned(ThreadHandler) then
    Cancel := ThreadHandler.CheckContinueRecordFetch <> tdccrContinue;
  if Assigned(BeforeFetch) and not Cancel then
    BeforeFetch(Dataset, Cancel);
end;

procedure TJvOdacOraTable.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;

procedure TJvOdacOraTable.SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvOdacOraTable.SetAfterThreadExecution(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterThreadExecution := Value;
end;

procedure TJvOdacOraTable.SetBeforeThreadExecution(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvOdacOraTable.SetDatasetFetchAllRecords(const Value: Boolean);
begin
  FetchAll := Value;
end;

procedure TJvOdacOraTable.SetDialogOptions(Value:
    TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvOdacOraTable.SetEnhancedOptions(const Value:
    TJvOdacThreadedDatasetEnhancedOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.EnhancedOptions.Assign(Value);
end;

procedure TJvOdacOraTable.SetOnThreadException(const Value:
    TJvThreadedDatasetThreadExceptionEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.OnThreadException := Value;
end;

procedure TJvOdacOraTable.SetThreadOptions(const Value:
    TJvThreadedDatasetThreadOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.ThreadOptions.Assign(Value);
end;

function TJvOdacOraTable.ThreadIsActive: Boolean;
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
{$ENDIF USE_3RDPARTY_DEVART_ODAC}
end.

