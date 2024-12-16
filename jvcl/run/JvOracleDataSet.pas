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

unit JvOracleDataSet;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, StdCtrls, ExtCtrls, Forms, Controls, DB,
  OracleData,
  JvThread, JvThreadDialog, JvDynControlEngine, JvBaseDBThreadedDataSet;

type
  TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions =
    class(TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions)
  public
    constructor Create; override;
  published
    property All;
    property Cancel;
    property Pause;
  end;

  TJvDoaThreadedDatasetEnhancedOptions = class(TJvBaseThreadedDatasetEnhancedOptions)
  private
    function GetAllowedContinueRecordFetchOptions: TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions;
    procedure SetAllowedContinueRecordFetchOptions(const Value: TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions);
  protected
    function CreateAllowedContinueRecordFetchOptions: TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions; override;
  published
    property AllowedContinueRecordFetchOptions: TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions read
      GetAllowedContinueRecordFetchOptions write SetAllowedContinueRecordFetchOptions;
  end;

  TJvOracleDatasetThreadHandler = class(TJvBaseDatasetThreadHandler)
  private
    function GetEnhancedOptions: TJvDoaThreadedDatasetEnhancedOptions;
    procedure SetEnhancedOptions(const Value: TJvDoaThreadedDatasetEnhancedOptions);
  protected
    function CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions; override;
  published
    property EnhancedOptions: TJvDoaThreadedDatasetEnhancedOptions read GetEnhancedOptions write SetEnhancedOptions;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvOracleDataset = class(TOracleDataset, IJvThreadedDatasetInterface)
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
    FAfterFetchRecord: TAfterFetchRecordEvent;
    FThreadHandler: TJvBaseDatasetThreadHandler;
    function GetAfterOpenFetch: TDataSetNotifyEvent;
    function GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetEnhancedOptions: TJvDoaThreadedDatasetEnhancedOptions;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(const Value: TJvDoaThreadedDatasetEnhancedOptions);
    procedure SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
    property ThreadHandler: TJvBaseDatasetThreadHandler read FThreadHandler;
  protected
    procedure DoAfterOpen; override;
    procedure DoAfterScroll; override;
    procedure DoAfterRefresh; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforeRefresh; override;
    function GetNextRecord: Boolean; override;
    function GetOnThreadException: TJvThreadedDatasetThreadExceptionEvent;
    procedure InternalLast; override;
    procedure InternalRefresh; override;
    procedure ReplaceAfterFetchRecord(Sender: TOracleDataSet; FilterAccept: Boolean;
      var Action: TAfterFetchRecordAction);
    procedure SetActive(Value: Boolean); override;
    procedure SetOnThreadException(const Value:
        TJvThreadedDatasetThreadExceptionEvent);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function CurrentFetchDuration: TDateTime;
    function CurrentOpenDuration: TDateTime;
    function EofReached: Boolean;
    function ErrorException: Exception;
    function ErrorMessage: string;
    function ThreadIsActive: Boolean;
  published
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read GetBeforeThreadExecution write
      SetBeforeThreadExecution;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property EnhancedOptions: TJvDoaThreadedDatasetEnhancedOptions read GetEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions write SetThreadOptions;
    property AfterFetchRecord: TAfterFetchRecordEvent read FAfterFetchRecord write FAfterFetchRecord;
    property AfterOpenFetch: TDataSetNotifyEvent read GetAfterOpenFetch write
        SetAfterOpenFetch;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read GetAfterThreadExecution write
      SetAfterThreadExecution;
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

implementation

constructor TJvOracleDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadHandler := TJvOracleDatasetThreadHandler.Create(Self, Self);
  inherited AfterFetchRecord := ReplaceAfterFetchRecord;
end;

destructor TJvOracleDataset.Destroy;
begin
  FreeAndNil(FThreadHandler);
  inherited Destroy;
end;

procedure TJvOracleDataset.BreakExecution;
begin
  if Assigned(Session) and Session.Connected then
    Session.BreakExecution;
end;

procedure TJvOracleDataset.BringThreadDialogToFront;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BringDialogToFront;
end;

function TJvOracleDataset.CurrentFetchDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentFetchDuration
  else
    Result := 0;
end;

function TJvOracleDataset.CurrentOpenDuration: TDateTime;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.CurrentOpenDuration
  else
    Result := 0;
end;

procedure TJvOracleDataset.InternalLast;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalLast;
end;

procedure TJvOracleDataset.InternalRefresh;
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.InternalRefresh;
end;

procedure TJvOracleDataset.SetActive(Value: Boolean);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.SetActive(Value);
end;


procedure TJvOracleDataset.DoAfterOpen;
begin
  ThreadHandler.AfterOpen;
end;

procedure TJvOracleDataset.DoAfterScroll;
begin
  ThreadHandler.AfterScroll;
end;

procedure TJvOracleDataset.DoAfterRefresh;
begin
  ThreadHandler.AfterRefresh;
end;

procedure TJvOracleDataset.DoBeforeOpen;
begin
  ThreadHandler.BeforeOpen;
end;

procedure TJvOracleDataset.DoBeforeRefresh;
begin
  ThreadHandler.BeforeRefresh;
end;

function TJvOracleDataset.DoGetInheritedNextRecord: Boolean;
begin
  Result := Inherited GetNextRecord;
end;

procedure TJvOracleDataset.DoInheritedAfterOpen;
begin
  inherited DoAfterOpen;
end;

procedure TJvOracleDataset.DoInheritedAfterRefresh;
begin
  inherited DoAfterRefresh;
end;

procedure TJvOracleDataset.DoInheritedAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TJvOracleDataset.DoInheritedBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

procedure TJvOracleDataset.DoInheritedBeforeRefresh;
begin
  inherited DoBeforeRefresh;
end;

procedure TJvOracleDataset.DoInheritedInternalLast;
begin
  inherited InternalLast;
end;

procedure TJvOracleDataset.DoInheritedInternalRefresh;
begin
  inherited InternalRefresh;
end;

procedure TJvOracleDataset.DoInheritedSetActive(Active: Boolean);
begin
  inherited SetActive(Active);
end;

procedure TJvOracleDataset.DoInternalOpen;
begin
  InternalOpen;
end;

function TJvOracleDataset.ErrorException: Exception;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorException
  else
    Result := Nil;
end;

function TJvOracleDataset.ErrorMessage: string;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ErrorMessage
  else
    Result := '';
end;

function TJvOracleDataset.GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.BeforeThreadExecution
  else
    Result := nil;
end;

function TJvOracleDataset.GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterThreadExecution
  else
    Result := nil;
end;

function TJvOracleDataset.GetDatasetFetchAllRecords: Boolean;
begin
  Result := QueryAllRecords;
end;

function TJvOracleDataset.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.DialogOptions
  else
    Result := nil;
end;

function TJvOracleDataset.GetEnhancedOptions: TJvDoaThreadedDatasetEnhancedOptions;
begin
  if Assigned(ThreadHandler) then
    Result := TJvDoaThreadedDatasetEnhancedOptions(ThreadHandler.EnhancedOptions)
  else
    Result := nil;
end;

function TJvOracleDataset.EofReached: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.EofReached
  else
    Result := False;
end;

function TJvOracleDataset.GetAfterOpenFetch: TDataSetNotifyEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.AfterOpenFetch
  else
    Result := nil;
end;

function TJvOracleDataset.GetNextRecord: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.GetNextRecord
  else
    Result := inherited GetNextRecord;
end;

function TJvOracleDataset.GetOnThreadException:
    TJvThreadedDatasetThreadExceptionEvent;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.OnThreadException
  else
    Result := nil;
end;

function TJvOracleDataset.GetThreadOptions: TJvThreadedDatasetThreadOptions;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadOptions
  else
    Result := nil;
end;

function TJvOracleDataset.IsThreadAllowed: Boolean;
begin
  if Assigned(Master) and (Master is TJvOracleDataset) then
    Result := not TJvOracleDataset(Master).ThreadHandler.ThreadIsActive
  else
    Result := True;
end;

procedure TJvOracleDataset.ReplaceAfterFetchRecord(Sender: TOracleDataSet;
  FilterAccept: Boolean; var Action: TAfterFetchRecordAction);
begin
  if Assigned(ThreadHandler) then
    case ThreadHandler.CheckContinueRecordFetch of
      tdccrContinue:
        Action := afContinue;
      tdccrAll:
        Action := afContinue;
      tdccrCancel:
        Action := afCancel;
      tdccrPause:
        Action := afPause;
      tdccrStop:
        Action := afStop;
    else
      Action := afStop;
    end;
  if Assigned(AfterFetchRecord) then
    AfterFetchRecord(Sender, FilterAccept, Action);
end;

procedure TJvOracleDataset.SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterOpenFetch := Value;
end;

procedure TJvOracleDataset.SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.BeforeThreadExecution := Value;
end;

procedure TJvOracleDataset.SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.AfterThreadExecution := Value;
end;

procedure TJvOracleDataset.SetDatasetFetchAllRecords(const Value: Boolean);
begin
  QueryAllRecords := Value;
end;

procedure TJvOracleDataset.SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.DialogOptions.Assign(Value);
end;

procedure TJvOracleDataset.SetEnhancedOptions(const Value: TJvDoaThreadedDatasetEnhancedOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.EnhancedOptions.Assign(Value);
end;

procedure TJvOracleDataset.SetOnThreadException(const Value:
    TJvThreadedDatasetThreadExceptionEvent);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.OnThreadException := Value;
end;

procedure TJvOracleDataset.SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
begin
  if Assigned(ThreadHandler) then
    ThreadHandler.ThreadOptions.Assign(Value);
end;

function TJvOracleDataset.ThreadIsActive: Boolean;
begin
  if Assigned(ThreadHandler) then
    Result := ThreadHandler.ThreadIsActive
  else
    Result := False;
end;

function TJvOracleDatasetThreadHandler.CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions;
begin
  Result := TJvDoaThreadedDatasetEnhancedOptions.Create;
end;

function TJvOracleDatasetThreadHandler.GetEnhancedOptions: TJvDoaThreadedDatasetEnhancedOptions;
begin
  Result := TJvDoaThreadedDatasetEnhancedOptions(inherited EnhancedOptions);
end;

procedure TJvOracleDatasetThreadHandler.SetEnhancedOptions(const Value: TJvDoaThreadedDatasetEnhancedOptions);
begin
  inherited EnhancedOptions := Value;
end;

constructor TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions.Create;
begin
  inherited Create;
  All := True;
end;

function
  TJvDoaThreadedDatasetEnhancedOptions.CreateAllowedContinueRecordFetchOptions:
    TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
begin
  Result := TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions.Create;
end;

function
  TJvDoaThreadedDatasetEnhancedOptions.GetAllowedContinueRecordFetchOptions:
    TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions;
begin
  Result := TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions(inherited AllowedContinueRecordFetchOptions);
end;

procedure
  TJvDoaThreadedDatasetEnhancedOptions.SetAllowedContinueRecordFetchOptions(
    const Value: TJvDoaThreadedDatasetAllowedContinueRecordFetchOptions);
begin
  inherited AllowedContinueRecordFetchOptions := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

