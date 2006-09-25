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
located at http://jvcl.sourceforge.net

Description:
  Oracle Dataset with Threaded Functions

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvOdacSmartQuery;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, StdCtrls, ExtCtrls, Forms, Controls,
  DB,
  OraSmart, Ora,
  JvThread, JvThreadDialog, JvDynControlEngine, DBaccess,
  JvBaseDBThreadedDataset;

type

  TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions = Class (TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions)

  public
    constructor Create; override;
  published
    property All;
  End;

  TJvOdacThreadedDatasetEnhancedOptions = Class(TJvBaseThreadedDatasetEnhancedOptions)
  private
    function GetAllowedContinueRecordFetchOptions:
        TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions;
    procedure SetAllowedContinueRecordFetchOptions(const Value:
        TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions);
  protected
    function CreateAllowedContinueRecordFetchOptions:
        TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions; override;
  published
    property AllowedContinueRecordFetchOptions:
        TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions read
        GetAllowedContinueRecordFetchOptions write
        SetAllowedContinueRecordFetchOptions;
  End;

  TJvOdacDatasetThreadHandler = Class(TJvBaseDatasetThreadHandler)
  protected
    function CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions; override;
  End;

  TJvOdacSmartQuery = class(TSmartQuery, IJvThreadedDatasetInterface)
    procedure BreakExecution;
    procedure DoInheritedAfterOpen;
    procedure DoInheritedAfterRefresh;
    procedure DoInheritedBeforeOpen;
    procedure DoInheritedBeforeRefresh;
    procedure doInheritedInternalLast;
    procedure doInheritedInternalRefresh;
    procedure doInheritedSetActive(Active: Boolean);
    procedure doInternalOpen;
    function GetDatasetFetchAllRecords: Boolean;
    function IsThreadAllowed: Boolean;
    procedure SetDatasetFetchAllRecords(const Value: Boolean);
  private
    FBeforeFetch: TBeforeFetchEvent;
    FThreadHandler: TJvBaseDatasetThreadHandler;
    function GetBeforeThreadExecution1: TJvThreadedDatasetThreadEvent;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetEnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetBeforeThreadExecution1(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(const Value:
        TJvOdacThreadedDatasetEnhancedOptions);
    procedure SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
    property ThreadHandler: TJvBaseDatasetThreadHandler read FThreadHandler;
  protected
    procedure DoAfterOpen; override;
    procedure DoAfterRefresh; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforeRefresh; override;
    procedure InternalLast; override;
    procedure InternalRefresh; override;
    procedure ReplaceBeforeFetch(Dataset: TCustomDADataSet; var Cancel: boolean);
    procedure SetActive(Value: Boolean); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function CurrentFetchDuration: TDateTime;
    function CurrentOpenDuration: TDateTime;
    function ErrorMessage: string;
    function ThreadIsActive: Boolean;
  published
    property BeforeFetch: TBeforeFetchEvent read FBeforeFetch write FBeforeFetch;
    property BeforeThreadExecution1: TJvThreadedDatasetThreadEvent read
        GetBeforeThreadExecution1 write SetBeforeThreadExecution1;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions
        write SetDialogOptions;
    property EnhancedOptions: TJvOdacThreadedDatasetEnhancedOptions read
        GetEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions
        write SetThreadOptions;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

constructor TJvOdacSmartQuery.Create(AOwner : TComponent);
begin
  inherited Create (AOwner);
  FThreadHandler := TJvOdacDatasetThreadHandler.Create(Self, Self);
  Inherited BeforeFetch := ReplaceBeforeFetch;
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

procedure TJvOdacSmartQuery.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvOdacSmartQuery.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

procedure TJvOdacSmartQuery.DoInheritedAfterOpen;
begin
  Inherited DoAfterOpen;
end;

procedure TJvOdacSmartQuery.DoInheritedAfterRefresh;
begin
  Inherited DoAfterRefresh;
end;

procedure TJvOdacSmartQuery.DoInheritedBeforeOpen;
begin
  Inherited DoBeforeOpen;
end;

procedure TJvOdacSmartQuery.DoInheritedBeforeRefresh;
begin
  Inherited DoBeforeRefresh;
end;

procedure TJvOdacSmartQuery.doInheritedInternalLast;
begin
  Inherited InternalLast;
end;

procedure TJvOdacSmartQuery.doInheritedInternalRefresh;
begin
  Inherited InternalRefresh;
end;

procedure TJvOdacSmartQuery.doInheritedSetActive(Active: Boolean);
begin
  Inherited SetActive(Active);
end;

procedure TJvOdacSmartQuery.doInternalOpen;
begin
  InternalOpen;
end;

function TJvOdacSmartQuery.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvOdacSmartQuery.GetBeforeThreadExecution1:
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

function TJvOdacSmartQuery.GetEnhancedOptions:
    TJvOdacThreadedDatasetEnhancedOptions;
begin
  if Assigned(ThreadHandler) then
    Result := TJvOdacThreadedDatasetEnhancedOptions(ThreadHandler.EnhancedOptions)
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
begin
  if Assigned(MasterSource) and Assigned(MasterSource.Dataset) and (MasterSource.Dataset is TJvOdacSmartQuery) then
    Result := not TJvOdacSmartQuery(MasterSource.Dataset).ThreadHandler.ThreadIsActive
  else
    Result := True;
end;

procedure TJvOdacSmartQuery.ReplaceBeforeFetch(Dataset: TCustomDADataSet; var
    Cancel: boolean);
begin
  if Assigned(ThreadHandler) then
    Cancel := ThreadHandler.CheckContinueRecordFetch <> tdccrContinue;
  if Assigned(BeforeFetch) then
    BeforeFetch(Dataset, Cancel);
end;

procedure TJvOdacSmartQuery.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;

procedure TJvOdacSmartQuery.SetBeforeThreadExecution1(const Value:
    TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvOdacSmartQuery.SetDatasetFetchAllRecords(const Value: Boolean);
begin
  FetchAll := Value;
end;

procedure TJvOdacSmartQuery.SetDialogOptions(Value:
    TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvOdacSmartQuery.SetEnhancedOptions(const Value:
    TJvOdacThreadedDatasetEnhancedOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.EnhancedOptions.Assign(Value);
end;

procedure TJvOdacSmartQuery.SetThreadOptions(const Value:
    TJvThreadedDatasetThreadOptions);
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

function TJvOdacDatasetThreadHandler.CreateEnhancedOptions:
    TJvBaseThreadedDatasetEnhancedOptions;
begin
  Result := TJvOdacThreadedDatasetEnhancedOptions.Create;
end;

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
  Result := TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions(Inherited AllowedContinueRecordFetchOptions);
end;

procedure
    TJvOdacThreadedDatasetEnhancedOptions.SetAllowedContinueRecordFetchOptions(
    const Value: TJvOdacThreadedDatasetAllowedContinueRecordFetchOptions);
begin
  Inherited AllowedContinueRecordFetchOptions := Value;
end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
end.


