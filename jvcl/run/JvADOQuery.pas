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

unit JvADOQuery;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Forms, Controls,
  DB, ADODB,
  JvBaseDBThreadedDataset;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvADOQuery = class(TADOQuery, IJvThreadedDatasetInterface)
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
    FOnFetchProgress: TFetchProgressEvent;
    FThreadHandler: TJvBaseDatasetThreadHandler;
    function GetAfterOpenFetch: TDataSetNotifyEvent;
    function GetAfterOpenFetch1: TDataSetNotifyEvent;
    function GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetAfterOpenFetch1(const Value: TDataSetNotifyEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
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
    procedure ReplaceOnFetchProgress(DataSet: TCustomADODataSet; Progress, MaxProgress: Integer; var EventStatus:
        TEventStatus);
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
    property AfterOpenFetch: TDataSetNotifyEvent read GetAfterOpenFetch1 write SetAfterOpenFetch1;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read GetAfterThreadExecution write SetAfterThreadExecution;
    property OnFetchProgress: TFetchProgressEvent read FOnFetchProgress write FOnFetchProgress;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read GetBeforeThreadExecution write
        SetBeforeThreadExecution;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions write SetThreadOptions;
    property OnThreadException: TJvThreadedDatasetThreadExceptionEvent read GetOnThreadException write SetOnThreadException;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvADODataSet = class(TADODataSet, IJvThreadedDatasetInterface)
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
    FOnFetchProgress: TFetchProgressEvent;
    FThreadHandler: TJvBaseDatasetThreadHandler;
    function GetAfterOpenFetch: TDataSetNotifyEvent;
    function GetAfterOpenFetch1: TDataSetNotifyEvent;
    function GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetAfterOpenFetch1(const Value: TDataSetNotifyEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
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
    procedure ReplaceOnFetchProgress(DataSet: TCustomADODataSet; Progress, MaxProgress: Integer; var EventStatus:
        TEventStatus);
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
    property AfterOpenFetch: TDataSetNotifyEvent read GetAfterOpenFetch1 write SetAfterOpenFetch1;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read GetAfterThreadExecution write SetAfterThreadExecution;
    property OnFetchProgress: TFetchProgressEvent read FOnFetchProgress write FOnFetchProgress;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read GetBeforeThreadExecution write
        SetBeforeThreadExecution;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
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

implementation

uses
  Variants;

//=== { TJvADOSmartQuery } ==================================================

constructor TJvADOQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadHandler := TJvBaseDatasetThreadHandler.Create(Self, Self);
  inherited OnFetchProgress := ReplaceOnFetchProgress;
end;

destructor TJvADOQuery.Destroy;
begin
  FreeAndNil(FThreadHandler);
  inherited Destroy;
end;

procedure TJvADOQuery.BreakExecution;
begin
  //BreakExec;
end;

procedure TJvADOQuery.BringThreadDialogToFront;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BringDialogToFront;
end;

function TJvADOQuery.CurrentFetchDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentFetchDuration
  else
    Result := 0;
end;

function TJvADOQuery.CurrentOpenDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentOpenDuration
  else
    Result := 0;
end;

procedure TJvADOQuery.DoAfterOpen;
begin
  ThreadHandler.AfterOpen;
end;

procedure TJvADOQuery.DoAfterRefresh;
begin
  ThreadHandler.AfterRefresh;
end;

procedure TJvADOQuery.DoAfterScroll;
begin
  ThreadHandler.AfterScroll;
end;

procedure TJvADOQuery.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvADOQuery.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

function TJvADOQuery.DoGetInheritedNextRecord: Boolean;
begin
  Result := Inherited GetNextRecord;
end;

procedure TJvADOQuery.DoInheritedAfterOpen;
begin
  inherited DoAfterOpen;
end;

procedure TJvADOQuery.DoInheritedAfterRefresh;
begin
  inherited DoAfterRefresh;
end;

procedure TJvADOQuery.DoInheritedAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TJvADOQuery.DoInheritedBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

procedure TJvADOQuery.DoInheritedBeforeRefresh;
begin
  inherited DoBeforeRefresh;
end;

procedure TJvADOQuery.DoInheritedInternalLast;
begin
  inherited InternalLast;
end;

procedure TJvADOQuery.DoInheritedInternalRefresh;
begin
  inherited InternalRefresh;
end;

procedure TJvADOQuery.DoInheritedSetActive(Active: Boolean);
begin
  inherited SetActive(Active);
end;

procedure TJvADOQuery.DoInternalOpen;
begin
  InternalOpen;
end;

function TJvADOQuery.EofReached: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.EofReached
  else
    Result := False;
end;

function TJvADOQuery.ErrorException: Exception;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorException
  else
    Result := Nil;
end;

function TJvADOQuery.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvADOQuery.GetAfterOpenFetch: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvADOQuery.GetAfterOpenFetch1: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvADOQuery.GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterThreadExecution
  else
    Result := nil;
end;

function TJvADOQuery.GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.BeforeThreadExecution
  else
    Result := nil;
end;

function TJvADOQuery.GetDatasetFetchAllRecords: Boolean;
begin
  Result := False;
end;

function TJvADOQuery.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.DialogOptions
  else
    Result := nil;
end;

function TJvADOQuery.GetNextRecord: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.GetNextRecord
  else
    Result := inherited GetNextRecord;
end;

function TJvADOQuery.GetOnThreadException: TJvThreadedDatasetThreadExceptionEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.OnThreadException
  else
    Result := nil;
end;

function TJvADOQuery.GetThreadOptions: TJvThreadedDatasetThreadOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadOptions
  else
    Result := nil;
end;

procedure TJvADOQuery.InternalLast;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalLast;
end;

procedure TJvADOQuery.InternalRefresh;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalRefresh;
end;

function TJvADOQuery.IsThreadAllowed: Boolean;
begin
  Result := True;
end;

procedure TJvADOQuery.ReplaceOnFetchProgress(DataSet: TCustomADODataSet; Progress, MaxProgress: Integer; var
    EventStatus: TEventStatus);
begin
  if Assigned(ThreadHandler) then
    if ThreadHandler.CheckContinueRecordFetch <> tdccrContinue then
      EventStatus := esCancel
    else
      EventStatus := esOk;
  if Assigned(OnFetchProgress) and (EventStatus <> esCancel) then
    OnFetchProgress(Dataset, Progress, MaxProgress, EventStatus);
end;

procedure TJvADOQuery.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;

procedure TJvADOQuery.SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvADOQuery.SetAfterOpenFetch1(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvADOQuery.SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterThreadExecution := Value;
end;

procedure TJvADOQuery.SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvADOQuery.SetDatasetFetchAllRecords(const Value: Boolean);
begin
//  FetchAll := Value;
end;

procedure TJvADOQuery.SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvADOQuery.SetOnThreadException(const Value: TJvThreadedDatasetThreadExceptionEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.OnThreadException := Value;
end;

procedure TJvADOQuery.SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.ThreadOptions.Assign(Value);
end;

function TJvADOQuery.ThreadIsActive: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadIsActive
  else
    Result := False;
end;

//=== { TJvADOSmartQuery } ==================================================

constructor TJvADODataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadHandler := TJvBaseDatasetThreadHandler.Create(Self, Self);
  inherited OnFetchProgress := ReplaceOnFetchProgress;
end;

destructor TJvADODataSet.Destroy;
begin
  FreeAndNil(FThreadHandler);
  inherited Destroy;
end;

procedure TJvADODataSet.BreakExecution;
begin
  //BreakExec;
end;

procedure TJvADODataSet.BringThreadDialogToFront;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BringDialogToFront;
end;

function TJvADODataSet.CurrentFetchDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentFetchDuration
  else
    Result := 0;
end;

function TJvADODataSet.CurrentOpenDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentOpenDuration
  else
    Result := 0;
end;

procedure TJvADODataSet.DoAfterOpen;
begin
  ThreadHandler.AfterOpen;
end;

procedure TJvADODataSet.DoAfterRefresh;
begin
  ThreadHandler.AfterRefresh;
end;

procedure TJvADODataSet.DoAfterScroll;
begin
  ThreadHandler.AfterScroll;
end;

procedure TJvADODataSet.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvADODataSet.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

function TJvADODataSet.DoGetInheritedNextRecord: Boolean;
begin
  Result := Inherited GetNextRecord;
end;

procedure TJvADODataSet.DoInheritedAfterOpen;
begin
  inherited DoAfterOpen;
end;

procedure TJvADODataSet.DoInheritedAfterRefresh;
begin
  inherited DoAfterRefresh;
end;

procedure TJvADODataSet.DoInheritedAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TJvADODataSet.DoInheritedBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

procedure TJvADODataSet.DoInheritedBeforeRefresh;
begin
  inherited DoBeforeRefresh;
end;

procedure TJvADODataSet.DoInheritedInternalLast;
begin
  inherited InternalLast;
end;

procedure TJvADODataSet.DoInheritedInternalRefresh;
begin
  inherited InternalRefresh;
end;

procedure TJvADODataSet.DoInheritedSetActive(Active: Boolean);
begin
  inherited SetActive(Active);
end;

procedure TJvADODataSet.DoInternalOpen;
begin
  InternalOpen;
end;

function TJvADODataSet.EofReached: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.EofReached
  else
    Result := False;
end;

function TJvADODataSet.ErrorException: Exception;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorException
  else
    Result := Nil;
end;

function TJvADODataSet.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvADODataSet.GetAfterOpenFetch: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvADODataSet.GetAfterOpenFetch1: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvADODataSet.GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterThreadExecution
  else
    Result := nil;
end;

function TJvADODataSet.GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.BeforeThreadExecution
  else
    Result := nil;
end;

function TJvADODataSet.GetDatasetFetchAllRecords: Boolean;
begin
  Result := False;
end;

function TJvADODataSet.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.DialogOptions
  else
    Result := nil;
end;

function TJvADODataSet.GetNextRecord: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.GetNextRecord
  else
    Result := inherited GetNextRecord;
end;

function TJvADODataSet.GetOnThreadException: TJvThreadedDatasetThreadExceptionEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.OnThreadException
  else
    Result := nil;
end;

function TJvADODataSet.GetThreadOptions: TJvThreadedDatasetThreadOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadOptions
  else
    Result := nil;
end;

procedure TJvADODataSet.InternalLast;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalLast;
end;

procedure TJvADODataSet.InternalRefresh;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalRefresh;
end;

function TJvADODataSet.IsThreadAllowed: Boolean;
begin
  if Assigned(Datasource) and Assigned(Datasource.Dataset) and (Datasource.Dataset is TJvADODataSet) then
    Result := not TJvADODataSet(Datasource.Dataset).ThreadHandler.ThreadIsActive
  else
    Result := True;
end;

procedure TJvADODataSet.ReplaceOnFetchProgress(DataSet: TCustomADODataSet; Progress, MaxProgress: Integer; var
    EventStatus: TEventStatus);
begin
  if Assigned(ThreadHandler) then
    if ThreadHandler.CheckContinueRecordFetch <> tdccrContinue then
      EventStatus := esCancel
    else
      EventStatus := esOk;
  if Assigned(OnFetchProgress) and (EventStatus <> esCancel) then
    OnFetchProgress(Dataset, Progress, MaxProgress, EventStatus);
end;

procedure TJvADODataSet.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;

procedure TJvADODataSet.SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvADODataSet.SetAfterOpenFetch1(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvADODataSet.SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterThreadExecution := Value;
end;

procedure TJvADODataSet.SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvADODataSet.SetDatasetFetchAllRecords(const Value: Boolean);
begin
//  FetchAll := Value;
end;

procedure TJvADODataSet.SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvADODataSet.SetOnThreadException(const Value: TJvThreadedDatasetThreadExceptionEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.OnThreadException := Value;
end;

procedure TJvADODataSet.SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.ThreadOptions.Assign(Value);
end;

function TJvADODataSet.ThreadIsActive: Boolean;
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
end.

