{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFManager.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvTFManager;

interface

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF VCL}
  Controls, Messages, Graphics, ImgList, ExtCtrls, Printers,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QGraphics, QImgList, QExtCtrls, QPrinters, Types, QWindows,
  {$ENDIF VisualCLX}
  {$IFDEF USEJVCL}
  JvComponent, JvTypes,
  {$ENDIF USEJVCL}
  JvTFUtils;

const
  CN_REQUESTREFRESH = $BD01;

type
  {$IFNDEF USEJVCL}
  THintString = string;
  {$ENDIF !USEJVCL}
  // Redeclaration of this type.  It is used in JvTFMonths.TJvTFDrawDWTitleEvent.
  // If not redeclared here, Delphi complains of 'unknown type' because it
  // will not automatically bring in 'JvTFUtils' into the uses clause when
  // a TJvTFDrawDWTitleEvent prototype is created.
  TTFDayOfWeek = JvTFUtils.TTFDayOfWeek;
  EJvTFScheduleManagerError = class(Exception);

  TJvTFTimeRange = record
    StartTime: TTime;
    EndTime: TTime;
  end;

  TJvTFServNotifyCode = (sncDestroyAppt,
    sncDestroySchedule,
    sncLoadAppt,
    sncSchedLoadAppt,
    sncSchedUnloadAppt,
    sncPostAppt,
    sncDeleteAppt,
    sncRequestSchedule,
    sncReleaseSchedule,
    sncConnectComponent,
    sncDisconnectComponent,
    sncConnectControl,
    sncDisconnectControl,
    sncConnectAppt,
    sncDisconnectAppt,
    sncRefresh);

  TJvTFScheduleManager = class;
  TJvTFSched = class;
  TJvTFAppt = class;
  TJvTFComponent = class;
  TJvTFControl = class;
  TJvTFPrinter = class;
  TJvTFHint = class;
  //  TJvTFNavigator = class;

  TJvTFSchedClass = class of TJvTFSched;
  TJvTFApptClass = class of TJvTFAppt;
  TJvTFHintClass = class of TJvTFHint;

  TCNRequestRefresh = record
    Msg: Cardinal;
    Schedule: TJvTFSched;
    Unused: Longint;
    Result: Longint;
  end;

  TJvTFDateList = class
  private
    FOnChange: TNotifyEvent;
  protected
    FList: TStringlist;
    function GetDate(Index: Integer): TDate;
    procedure Change; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(ADate: TDate): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Count: Integer;
    function IndexOf(ADate: TDate): Integer;
    property Dates[Index: Integer]: TDate read GetDate; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvTFNavEvent = procedure(Sender: TObject; aControl: TJvTFControl;
    SchedNames: TStringlist; Dates: TJvTFDateList) of object;
  TJvTFControlEvent = procedure(Sender: TObject; aControl: TJvTFControl) of object;
  TJvTFSchedEvent = procedure(Sender: TObject; Schedule: TJvTFSched) of object;
  TJvTFApptEvent = procedure(Sender: TObject; Appt: TJvTFAppt) of object;
  TJvTFVarApptEvent = procedure(Sender: TObject; var Appt: TJvTFAppt) of object;
  TJvTFFlushEvent = procedure(Sender, FlushObj: TObject; var FlushIt: Boolean) of object;

  // implicit post fix
  TJvTFPostApptQueryEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    var CanPost: Boolean) of object;

  TJvTFCustomImageMap = class(TPersistent)
  private
    FMap: TStringlist;
    function GetImage(MapIndex: Integer): Integer;
    procedure SetImage(MapIndex: Integer; Value: Integer);
    function GetImageName(MapIndex: Integer): string;
  protected
    FAppt: TJvTFAppt;
    procedure Change;
  public
    constructor Create(anAppt: TJvTFAppt);
    destructor Destroy; override;
    property Images[MapIndex: Integer]: Integer read GetImage write SetImage; default;
    property ImageNames[MapIndex: Integer]: string read GetImageName;
    function Count: Integer;
    procedure Add(const ImageName: string; ImageIndex: Integer);
    procedure Delete(MapIndex: Integer);
    procedure Move(SrcMapIndex, DestMapIndex: Integer);
    function FindMapIndex(const ImageName: string): Integer;
    function FindImageIndex(const ImageName: string): Integer;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
  end;

  TJvTFStatePic = (spAlarmEnabled, spAlarmDisabled, spShared, spRecurring,
    spModified);

  TJvTFStateImageMap = class(TPersistent)
  private
    FPics: array[Low(TJvTFStatePic)..High(TJvTFStatePic)] of Integer;

    procedure SetImage(StatePicID: TJvTFStatePic; Value: Integer);
    function GetImage(StatePicID: TJvTFStatePic): Integer;
    function GetAlarmDisabled: Integer;
    function GetAlarmEnabled: Integer;
    function GetModified: Integer;
    function GetRecurring: Integer;
    function GetShared: Integer;
    procedure SetAlarmDisabled(const Value: Integer);
    procedure SetAlarmEnabled(const Value: Integer);
    procedure SetModified(const Value: Integer);
    procedure SetRecurring(const Value: Integer);
    procedure SetShared(const Value: Integer);
  protected
    FScheduleManager: TJvTFScheduleManager;
    FUpdating: Boolean;
    procedure Change;
  public
    constructor Create(Serv: TJvTFScheduleManager);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    property Pics[Index: TJvTFStatePic]: Integer read GetImage write SetImage;
  published
    property AlarmEnabled: Integer {index spAlarmEnabled}
    read GetAlarmEnabled write SetAlarmEnabled;
    property AlarmDisabled: Integer {index spAlarmDisabled}
    read GetAlarmDisabled write SetAlarmDisabled;
    property Shared: Integer {index spShared}
    read GetShared write SetShared;
    property Recurring: Integer {index spRecurring}
    read GetRecurring write SetRecurring;
    //read GetImage write SetImage;
    property Modified: Integer {index spModified}
    read GetModified write SetModified;
  end;

  TDynTimeRangeArray = array of TJvTFTimeRange;

  TDynApptArray = array of TJvTFAppt;

  TDynSchedArray = array of TJvTFSched;

  TJvTFAppt = class(TPersistent)
  private
    FStartDate: TDate;
    FEndDate: TDate;
    FStartTime: TTime;
    FEndTime: TTime;
    FDescription: string;
    FAlarmEnabled: Boolean;
    FAlarmAdvance: Integer;
    FImageMap: TJvTFCustomImageMap;
    FData: Integer;
    FPersistent: Boolean;
    FColor: TColor;
    FBarColor: TColor;
    FRefreshed: Boolean;

    function GetDescription: string;
    procedure SetDescription(Value: string);
    procedure SetAlarmEnabled(Value: Boolean);
    procedure SetAlarmAdvance(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetBarColor(Value: TColor);
    function GetStartDateTime: TDateTime;
    function GetEndDateTime: TDateTime;
    function GetStartDate: TDate;
    function GetEndDate: TDate;
    function GetStartTime: TTime;
    function GetEndTime: TTime;
    procedure SetRefreshed(Value: Boolean);
  protected
    FID: string;
    FModified: Boolean;
    FScheduleManager: TJvTFScheduleManager;
    FConnections: TStringlist;
    FSchedules: TStringlist;
    FDeleting: Boolean;
    // implicit post fix
    FUpdating: Boolean;

    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode);
    procedure NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure NotifySchedule(Sched: TJvTFSched; Sender: TObject;
      Code: TJvTFServNotifyCode);

    function GetConnection(Index: Integer): TJvTFSched;
    function GetSchedule(Index: Integer): string;
    procedure CheckConnections;

    procedure Connect(Schedule: TJvTFSched);
    procedure Disconnect(Schedule: TJvTFSched);
    procedure Change;
    procedure InternalClearSchedules;
    procedure DeleteApptNotification;
    // implicit post fix
    procedure PostApptNotification;
    procedure RefreshNotification;
  public
    constructor Create(Serv: TJvTFScheduleManager; const ApptID: string); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure SetStartEnd(NewStartDate: TDate; NewStartTime: TTime;
      NewEndDate: TDate; NewEndTime: TTime);

    procedure SetModified;
    function Modified: Boolean; dynamic;
    property ScheduleManager: TJvTFScheduleManager read FScheduleManager;

    function ConnectionCount: Integer;
    property Connections[Index: Integer]: TJvTFSched read GetConnection;

    function ScheduleCount: Integer;
    property Schedules[Index: Integer]: string read GetSchedule;
    procedure AddSchedule(const SchedName: string);
    procedure RemoveSchedule(const SchedName: string);
    procedure AssignSchedules(List: TStrings);
    procedure ClearSchedules;
    function IndexOfSchedule(const SchedName: string): Integer;
    function Shared: Boolean;

    procedure Post;
    procedure Refresh;
    procedure Delete;

    // implicit post fix
    procedure BeginUpdate;
    procedure EndUpdate;
    property Updating: Boolean read FUpdating;

    property ImageMap: TJvTFCustomImageMap read FImageMap write FImageMap;
    procedure RefreshControls;
    property Refreshed: Boolean read FRefreshed write SetRefreshed;
  published
    property ID: string read FID;
    property StartDate: TDate read GetStartDate;
    property EndDate: TDate read GetEndDate;
    property StartTime: TTime read GetStartTime;
    property EndTime: TTime read GetEndTime;
    property StartDateTime: TDateTime read GetStartDateTime;
    property EndDateTime: TDateTime read GetEndDateTime;
    property Description: string read GetDescription write SetDescription;
    property AlarmEnabled: Boolean read FAlarmEnabled write SetAlarmEnabled;
    property AlarmAdvance: Integer read FAlarmAdvance write SetAlarmAdvance;
    property Data: Integer read FData write FData;
    property Persistent: Boolean read FPersistent write FPersistent;
    property Color: TColor read FColor write SetColor default clDefault;
    property BarColor: TColor read FBarColor write SetBarColor default clDefault;
  end;

  TJvTFSched = class(TObject)
  private
    FAppts: TStringlist;
    FConControls: TStringlist;
    FConComponents: TStringlist;
    FDestroying: Boolean;
    FData: Integer;
    FPersistent: Boolean;
    FSchedDisplayName: string;
    procedure SetSchedDisplayName(const Value: string);

    function GetAppt(Index: Integer): TJvTFAppt;
  protected
    FSchedName: string;
    FSchedDate: TDate;
    FScheduleManager: TJvTFScheduleManager;
    FCached: Boolean;
    FCachedTime: DWORD;
    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode);
    procedure NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure NotifyAppt(Appt: TJvTFAppt; Sender: TObject;
      Code: TJvTFServNotifyCode);
    function GetConControl(Index: Integer): TJvTFControl;
    function GetConComponent(Index: Integer): TJvTFComponent;
    procedure ConnectAppt(Appt: TJvTFAppt);
    procedure DisconnectAppt(Appt: TJvTFAppt);
    procedure ConnectionsOnChange(Sender: TObject);
    procedure CheckConnections;
    function GetFreeUsedTime(FreeTime: Boolean): TDynTimeRangeArray; dynamic;
  public
    constructor Create(Serv: TJvTFScheduleManager; const AName: string; ADate: TDate); virtual;
    destructor Destroy; override;

    function ApptCount: Integer;
    function ApptByID(const ID: string): TJvTFAppt;
    property Appts[Index: Integer]: TJvTFAppt read GetAppt;

    function ConControlCount: Integer;
    property ConControls[Index: Integer]: TJvTFControl read GetConControl;

    function ConComponentCount: Integer;
    property ConComponents[Index: Integer]: TJvTFComponent read GetConComponent;

    procedure AddAppt(Appt: TJvTFAppt);
    procedure RemoveAppt(Appt: TJvTFAppt);

    //procedure RefreshAppts;
    procedure Refresh;
    procedure PostAppts;

    // Conflict and free time methods
    function GetFreeTime: TDynTimeRangeArray; dynamic;
    function GetUsedTime: TDynTimeRangeArray; dynamic;
    function TimeIsFree(TimeRange: TJvTFTimeRange): Boolean; overload; dynamic;
    function TimeIsFree(RangeStart, RangeEnd: TTime): Boolean; overload; dynamic;
    // The ApptHasConflicts(anAppt : TJvTFAppt) method declared here checks
    //  ONLY THIS SCHEDULE!!
    function ApptHasConflicts(anAppt: TJvTFAppt): Boolean; dynamic;
    function EnumConflicts(TimeRange: TJvTFTimeRange): TDynApptArray;
      overload; dynamic;
    function EnumConflicts(RangeStart, RangeEnd: TTime): TDynApptArray;
      overload; dynamic;
    // The following EnumConflicts(anAppt : TJvTFAppt) checks
    //  ONLY THIS SCHEDULE!!
    function EnumConflicts(anAppt: TJvTFAppt): TDynApptArray;
      overload; dynamic;

    property Cached: Boolean read FCached;
    property CachedTime: DWORD read FCachedTime;
    property Destroying: Boolean read FDestroying;

    function GetFirstAppt: TJvTFAppt;
    function GetLastAppt: TJvTFAppt;
  published
    property SchedDisplayName: string read FSchedDisplayName
      write SetSchedDisplayName;
    property SchedName: string read FSchedName;
    property SchedDate: TDate read FSchedDate;
    property ScheduleManager: TJvTFScheduleManager read FScheduleManager;
    property Data: Integer read FData write FData;
    property Persistent: Boolean read FPersistent write FPersistent;
  end;

  TJvTFScheduleManagerCacheType = (ctNone, ctTimed, ctBuffer);
  TJvTFScheduleManagerCache = class(TPersistent)
  private
    FCacheType: TJvTFScheduleManagerCacheType;
    FTimedDelay: Integer;
    FBufferCount: Integer;
    FTimer: TTimer;
    procedure SetCacheType(Value: TJvTFScheduleManagerCacheType);
    procedure SetTimedDelay(Value: Integer);
    procedure SetBufferCount(Value: Integer);
  protected
    FScheduleManager: TJvTFScheduleManager;
    procedure FlushManager; virtual;
    procedure TimerOnTimer(Sender: TObject); virtual;
  public
    constructor Create(SchedManager: TJvTFScheduleManager);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CacheType: TJvTFScheduleManagerCacheType read FCacheType write SetCacheType
      default ctTimed;
    property TimedDelay: Integer read FTimedDelay write SetTimedDelay
      default 30000;
    property BufferCount: Integer read FBufferCount write SetBufferCount
      default 7;
  end;

  TJvTFSchedLoadMode = (slmOnDemand, slmBatch);
  TJvTFLoadBatchEvent = procedure(Sender: TObject; BatchName: string;
    BatchStartDate, BatchEndDate: TDate) of object;

  TJvTFGetApptDisplayTextEvent = procedure(Sender: TObject; Source: TComponent;
    Appt: TJvTFAppt; var DisplayText: string) of object;

  TJvTFApptDescEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    var Description: string) of object;

  TJvTFScheduleManager = class(TComponent)
  private
    FAlwaysPost: Boolean;
    FAppts: TStringlist;
    FSchedules: TStringlist;
    FConControls: TStringlist;
    FConComponents: TStringlist;
    FOnNeedAppts: TJvTFSchedEvent;
    FOnRefreshAppt: TJvTFApptEvent;
    FOnRefreshSched: TJvTFSchedEvent;
    FOnRefreshAll: TNotifyEvent;
    FOnDeleteAppt: TJvTFApptEvent;
    FOnPostAppt: TJvTFApptEvent;
    FOnFlush: TJvTFFlushEvent;
    FOnCreateAppt: TJvTFApptEvent;
    FOnCreateSchedule: TJvTFSchedEvent;
    FOnDestroyAppt: TJvTFApptEvent;
    FOnDestroySchedule: TJvTFSchedEvent;
    FOnGetApptDisplayText: TJvTFGetApptDisplayTextEvent;
    FOnGetApptDescription: TJvTFApptDescEvent;
    FOnSetApptDescription: TJvTFApptDescEvent;

    FSchedLoadMode: TJvTFSchedLoadMode;
    FOnLoadBatch: TJvTFLoadBatchEvent;
    FOnBatchesProcessed: TNotifyEvent;

    FRefreshAutoReconcile: Boolean;

    FStateImages: TCustomImageList;
    FCustomImages: TCustomImageList;
    FStateImageMap: TJvTFStateImageMap;
    FCache: TJvTFScheduleManagerCache;

    // implicit post fix
    FOnPostApptQuery: TJvTFPostApptQueryEvent;

    function GetAppt(Index: Integer): TJvTFAppt;
    function GetSchedule(Index: Integer): TJvTFSched;
    function GetConControl(Index: Integer): TJvTFControl;
    function GetConComponent(Index: Integer): TJvTFComponent;
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetCustomImages(Value: TCustomImageList);
    procedure SetCache(Value: TJvTFScheduleManagerCache);

    procedure SeTJvTFSchedLoadMode(Value: TJvTFSchedLoadMode);
    procedure SetRefreshAutoReconcile(Value: Boolean);
  protected
    FLoadingAppts: Boolean;
    FRefreshing: Boolean;
    FImageChangeLink: TChangeLink;
    FFlushing: Boolean;
    FDestroying: Boolean;

    FSchedBatch: TStringlist;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ConnectControl(ApptCtrl: TJvTFControl);
    procedure DisconnectControl(ApptCtrl: TJvTFControl);
    procedure ConnectComponent(Comp: TJvTFComponent);
    procedure DisconnectComponent(Comp: TJvTFComponent);

    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode); virtual;
    procedure NotifyAppt(Appt: TJvTFAppt; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure NotifySchedule(Sched: TJvTFSched; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure NotifyApptCtrl(ApptCtrl: TJvTFControl; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure NotifyComp(Comp: TJvTFComponent; Sender: TObject;
      Code: TJvTFServNotifyCode);

    procedure RetrieveSchedule(const SchedName: string; SchedDate: TDate;
      var Schedule: TJvTFSched; var LoadedNow: Boolean);

    procedure NeedAppts(Schedule: TJvTFSched); virtual;
    procedure AddAppt(Appt: TJvTFAppt);
    procedure RemoveAppt(Appt: TJvTFAppt);
    procedure RemoveSchedule(Sched: TJvTFSched);

    //procedure RefreshAppt(Appt : TJvTFAppt);
    procedure DeleteAppt(Appt: TJvTFAppt);
    procedure PostAppt(Appt: TJvTFAppt);

    // implicit post fix
    function QueryPostAppt(Appt: TJvTFAppt): Boolean;

    procedure AddToBatch(aSched: TJvTFSched);
    procedure LoadBatch(const BatchName: string; BatchStartDate,
      BatchEndDate: TDate); virtual;

    procedure RequestRefresh(ApptCtrl: TJvTFControl;
      Schedule: TJvTFSched); overload; dynamic;
    procedure RequestRefresh(Comp: TJvTFComponent;
      Schedule: TJvTFSched); overload; dynamic;

    procedure ImageListChange(Sender: TObject);
    procedure FlushAppts;
    function FlushObject(FlushObj: TObject): Boolean;

    procedure DoCreateApptEvent(anAppt: TJvTFAppt); dynamic;
    procedure DoCreateScheduleEvent(aSchedule: TJvTFSched); dynamic;
    procedure DoDestroyApptEvent(anAppt: TJvTFAppt); dynamic;
    procedure DoDestroyScheduleEvent(aSchedule: TJvTFSched); dynamic;

    procedure SetApptDescription(Appt: TJvTFAppt; var Value: string); virtual;
    procedure GetApptDescription(Appt: TJvTFAppt; var Value: string); virtual;
  public
    class function GetScheduleID(const SchedName: string; SchedDate: TDate): string;
    class function GenerateApptID: string; virtual;

    function GetSchedClass: TJvTFSchedClass; dynamic;
    function GetApptClass: TJvTFApptClass; dynamic;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ApptCount: Integer;
    property Appts[Index: Integer]: TJvTFAppt read GetAppt;
    function FindAppt(const ID: string): TJvTFAppt;

    function ScheduleCount: Integer;
    property Schedules[Index: Integer]: TJvTFSched read GetSchedule;
    function FindSchedule(const SchedName: string; SchedDate: TDate): TJvTFSched;

    function ConControlCount: Integer;
    property ConControls[Index: Integer]: TJvTFControl read GetConControl;
    function ConComponentCount: Integer;
    property ConComponents[Index: Integer]: TJvTFComponent read GetConComponent;

    function RequestSchedule(ApptCtrl: TJvTFControl; const SchedName: string;
      SchedDate: TDate): TJvTFSched; overload;
    function RequestSchedule(ApptCtrl: TJvTFControl; const SchedName: string;
      SchedDate: TDate; var LoadedNow: Boolean): TJvTFSched; overload;

    function RequestSchedule(Comp: TJvTFComponent; const SchedName: string;
      SchedDate: TDate): TJvTFSched; overload;
    function RequestSchedule(Comp: TJvTFComponent; const SchedName: string;
      SchedDate: TDate; var LoadedNow: Boolean): TJvTFSched; overload;

    procedure ReleaseSchedule(ApptCtrl: TJvTFControl; const SchedName: string;
      SchedDate: TDate); overload;
    procedure ReleaseSchedule(Comp: TJvTFComponent; const SchedName: string;
      SchedDate: TDate); overload;

    procedure ProcessBatches;

    procedure RequestAppt(const ID: string; var Appt: TJvTFAppt; var New: Boolean);

    property LoadingAppts: Boolean read FLoadingAppts;
    property Refreshing: Boolean read FRefreshing;

    procedure dbPostAppt(Appt: TJvTFAppt);
    procedure dbDeleteAppt(Appt: TJvTFAppt);
    procedure dbRefreshAppt(Appt: TJvTFAppt);
    procedure dbRefreshSched(Sched: TJvTFSched);
    procedure dbRefreshAll;
    procedure dbRefreshOrphans;
    function dbNewAppt(const ID: string): TJvTFAppt;

    procedure PostAppts;
    procedure RefreshAppts;
    procedure ReconcileRefresh(Scope: TObject);

    procedure RefreshConnections(Trigger: TObject); virtual;
    property Flushing: Boolean read FFlushing;
    procedure Flush(All: Boolean = False); virtual;

    function GetApptDisplayText(AComponent: TComponent;
      Appt: TJvTFAppt): string; virtual;
  published
    property AlwaysPost: Boolean read FAlwaysPost write FAlwaysPost default False;
    property OnNeedAppts: TJvTFSchedEvent read FOnNeedAppts write FOnNeedAppts;
    property OnRefreshAppt: TJvTFApptEvent read FOnRefreshAppt write FOnRefreshAppt;
    property OnRefreshSched: TJvTFSchedEvent read FOnRefreshSched
      write FOnRefreshSched;
    property OnRefreshAll: TNotifyEvent read FOnRefreshAll write FOnRefreshAll;
    property OnPostAppt: TJvTFApptEvent read FOnPostAppt write FOnPostAppt;
    property OnDeleteAppt: TJvTFApptEvent read FOnDeleteAppt write FOnDeleteAppt;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property CustomImages: TCustomImageList read FCustomImages write SetCustomImages;
    property StateImageMap: TJvTFStateImageMap read FStateImageMap write FStateImageMap;
    property Cache: TJvTFScheduleManagerCache read FCache write SetCache;
    // implicit post fix
    property OnPostApptQuery: TJvTFPostApptQueryEvent read FOnPostApptQuery
      write FOnPostApptQuery;
    property OnFlush: TJvTFFlushEvent read FOnFlush write FOnFlush;
    property OnCreateAppt: TJvTFApptEvent read FOnCreateAppt write FOnCreateAppt;
    property OnDestroyAppt: TJvTFApptEvent read FOnDestroyAppt write FOnDestroyAppt;
    property OnCreateSchedule: TJvTFSchedEvent read FOnCreateSchedule
      write FOnCreateSchedule;
    property OnDestroySchedule: TJvTFSchedEvent read FOnDestroySchedule
      write FOnDestroySchedule;
    property OnLoadBatch: TJvTFLoadBatchEvent read FOnLoadBatch write FOnLoadBatch;
    property OnBatchesProcessed: TNotifyEvent read FOnBatchesProcessed
      write FOnBatchesProcessed;
    property OnGetApptDisplayText: TJvTFGetApptDisplayTextEvent
      read FOnGetApptDisplayText write FOnGetApptDisplayText;
    property OnGetApptDescription: TJvTFApptDescEvent read FOnGetApptDescription
      write FOnGetApptDescription;
    property OnSetApptDescription: TJvTFApptDescEvent read FOnSetApptDescription
      write FOnSetApptDescription;

    property SchedLoadMode: TJvTFSchedLoadMode read FSchedLoadMode
      write SeTJvTFSchedLoadMode default slmOnDemand;
    property RefreshAutoReconcile: Boolean read FRefreshAutoReconcile
      write SetRefreshAutoReconcile default False;
  end;

  TJvTFHintProps = class(TPersistent)
  private
    FHintColor: TColor;
    FHintHidePause: Integer;
    FHintPause: Integer;
    procedure SetHintColor(Value: TColor);
    procedure SetHintHidePause(Value: Integer);
    procedure SetHintPause(Value: Integer);
  protected
    FControl: TJvTFControl;
    procedure Change; virtual;
  public
    constructor Create(AOwner: TJvTFControl);
    procedure Assign(Source: TPersistent); override;
  published
    property HintColor: TColor read FHintColor write SetHintColor
      default clDefault;
    property HintHidePause: Integer read FHintHidePause write SetHintHidePause
      default -1;
    property HintPause: Integer read FHintPause write SetHintPause
      default -1;
  end;

  TJvTFHintType = (shtAppt, shtStartEnd, shtCell, shtObj);

  TJvTFShowHintEvent = procedure(Sender: TObject; HintType: TJvTFHintType;
    Ref: TObject; var HintRect: TRect; var HintText: string) of object;

  // NOTE:
  // The Pause property has the same meaning as the Application.HintPause
  // property.  The ShortPause property has the same meaning as the
  // Application.HintHidePause property.
  TJvTFHint = class(THintWindow)
  private
    FTimer: TTimer;
    FPause: Integer;
    FShortPause: Integer;
    FOnShowHint: TJvTFShowHintEvent;
    FRefProps: TJvTFHintProps;
    procedure SetPause(Value: Integer);
    procedure SetShortPause(Value: Integer);
  protected
    FApptCtrl: TJvTFControl;
    FOldAppt: TJvTFAppt;
    FOldObj: TObject;
    FShortTimer: Boolean;
    FHintRect: TRect;
    FHintText: string;
    FHintCell: TPoint;
    FHintType: TJvTFHintType;
    procedure TimerOnTimer(Sender: TObject); virtual;
    procedure PrepTimer(Short: Boolean);
    procedure SetHintText(StartDate, EndDate: TDate; StartTime, EndTime: TTime;
      const Desc: string; ShowDatesTimes, ShowDesc: Boolean);
    procedure DoHint(Sustained: Boolean);
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    procedure PropertyCheck; dynamic;
  public
    constructor Create(anApptCtrl: TJvTFControl); reintroduce;
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: THintString); override;
    procedure ApptHint(Appt: TJvTFAppt; X, Y: Integer;
      ShowDatesTimes, ShowDesc, FormattedDesc: Boolean); virtual;
    procedure StartEndHint(StartDate, EndDate: TDate; StartTime, EndTime: TTime;
      X, Y: Integer; ShowDates: Boolean);
    procedure CellHint(Row, Col: Integer; const HintText: string; CellRect: TRect);

    procedure MultiLineObjHint(Obj: TObject; X, Y: Integer; Hints: TStrings);

    procedure ReleaseHandle; virtual;
    // See above note on Pause and ShortPause properties
    property Pause: Integer read FPause write SetPause default 3000;
    property ShortPause: Integer read FShortPause write SetShortPause default 1500;
    property OnShowHint: TJvTFShowHintEvent read FOnShowHint write FOnShowHint;
    property HintType: TJvTFHintType read FHintType;
    property RefProps: TJvTFHintProps read FRefProps write FRefProps;
  end;

  TJvTFDragInfo = class
  private
    FApptCtrl: TJvTFControl;
    FSchedule: TJvTFSched;
    FAppt: TJvTFAppt;
    FShift: TShiftState;
  public
    property ApptCtrl: TJvTFControl read FApptCtrl write FApptCtrl;
    property Schedule: TJvTFSched read FSchedule write FSchedule;
    property Appt: TJvTFAppt read FAppt write FAppt;
    property Shift: TShiftState read FShift write FShift;
  end;

  {$IFDEF USEJVCL}
  TJvTFComponent = class(TJvComponent)
    {$ELSE}
  TJvTFComponent = class(TComponent)
    {$ENDIF USEJVCL}
  private
    FScheduleManager: TJvTFScheduleManager;
    FSchedules: TStringlist;
    procedure SetManager(Value: TJvTFScheduleManager);
    function GetSchedule(Index: Integer): TJvTFSched;
  protected
    FDateFormat: string;
    FTimeFormat: string;

    procedure UpdateDesigner;

    procedure SetDateFormat(const Value: string); virtual;
    procedure SetTimeFormat(const Value: string); virtual;
    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode); virtual;
    procedure ReqSchedNotification(Schedule: TJvTFSched); virtual;
    procedure RelSchedNotification(Schedule: TJvTFSched); virtual;
    procedure NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure RefreshComponent; dynamic;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    procedure DestroyApptNotification(anAppt: TJvTFAppt); virtual;
    procedure DestroySchedNotification(aSched: TJvTFSched); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ScheduleCount: Integer;
    property Schedules[Index: Integer]: TJvTFSched read GetSchedule;
    function FindSchedule(const SchedName: string; SchedDate: TDate): TJvTFSched;
    function RetrieveSchedule(const SchedName: string; SchedDate: TDate): TJvTFSched;
    procedure ReleaseSchedule(const SchedName: string; SchedDate: TDate); virtual;
    procedure ReleaseSchedules;
    procedure ProcessBatches;
  published
    property ScheduleManager: TJvTFScheduleManager read FScheduleManager write SetManager;
  end;

  {$IFDEF USEJVCL}
  TJvTFControl = class(TJvCustomControl)
    {$ELSE}
  TJvTFControl = class(TCustomControl)
    {$ENDIF USEJVCL}
  private
    FScheduleManager: TJvTFScheduleManager;
    FSchedules: TStringlist;
    //    FNavigator : TJvTFNavigator;
    //    FOnNavigate : TJvTFNavEvent;
    procedure SetManager(Value: TJvTFScheduleManager);
    function GetSchedule(Index: Integer): TJvTFSched;
    //    procedure SetNavigator(Value: TJvTFNavigator);
  protected
    FDateFormat: string;
    FTimeFormat: string;
    FDragInfo: TJvTFDragInfo;
    FShift: TShiftState;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDateFormat(const Value: string); virtual;
    procedure SetTimeFormat(const Value: string); virtual;
    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode); virtual;
    procedure ReqSchedNotification(Schedule: TJvTFSched); virtual;
    procedure RelSchedNotification(Schedule: TJvTFSched); virtual;
    procedure NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure CNRequestRefresh(var Msg: TCNRequestRefresh); message CN_REQUESTREFRESH;
    procedure RefreshControl; dynamic;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    procedure DestroyApptNotification(anAppt: TJvTFAppt); virtual;
    procedure DestroySchedNotification(aSched: TJvTFSched); virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringlist;
      Dates: TJvTFDateList); virtual;
    //    property Navigator : TJvTFNavigator read FNavigator write SetNavigator;
    //    property OnNavigate : TJvTFNavEvent read FOnNavigate write FOnNavigate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ScheduleCount: Integer;
    property Schedules[Index: Integer]: TJvTFSched read GetSchedule;
    function FindSchedule(const SchedName: string; SchedDate: TDate): TJvTFSched;
    function RetrieveSchedule(const SchedName: string; SchedDate: TDate): TJvTFSched;
    procedure ReleaseSchedule(const SchedName: string; SchedDate: TDate); virtual;
    procedure ReleaseSchedules;
    property DragInfo: TJvTFDragInfo read FDragInfo;
    procedure ProcessBatches;
  published
    property ScheduleManager: TJvTFScheduleManager read FScheduleManager write SetManager;
  end;

  EJvTFPrinterError = class(Exception);
  TJvTFMargins = TRect;
  TJvTFPrinterMeasure = (pmPixels, pmInches, pmMM);
  TJvTFPrinterState = (spsNoDoc, spsCreating, spsAssembling, spsFinished);
  TJvTFPrinterDrawEvent = procedure(Sender: TObject; aCanvas: TCanvas;
    ARect: TRect; PageNum: Integer) of object;

  TJvTFProgressEvent = procedure(Sender: TObject; Current, Total: Integer)
    of object;

  TJvTFPrinterPageLayout = class(TPersistent)
  private
    FFooterHeight: Integer;
    FHeaderHeight: Integer;
    FMargins: TJvTFMargins;
    FPrinter: TJvTFPrinter;
    procedure SetFooterHeight(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
    function GetMargin(Index: Integer): Integer;
    procedure SetMargin(Index: Integer; Value: Integer);
  protected
    procedure Change; virtual;
    property Printer: TJvTFPrinter read FPrinter;
    procedure SetPropertyCheck;
  public
    constructor Create(aPrinter: TJvTFPrinter); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property FooterHeight: Integer read FFooterHeight write SetFooterHeight;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight;
    property MarginLeft: Integer index 1 read GetMargin write SetMargin;
    property MarginTop: Integer index 2 read GetMargin write SetMargin;
    property MarginRight: Integer index 3 read GetMargin write SetMargin;
    property MarginBottom: Integer index 4 read GetMargin write SetMargin;
  end;

  TJvTFPrinter = class(TJvTFComponent)
  private
    FPages: TStringlist;
    FBodies: TStringlist;
    FMarginOffsets: TJvTFMargins; // always in pixels
    FMeasure: TJvTFPrinterMeasure;
    FOnDrawBody: TJvTFPrinterDrawEvent;
    FOnDrawHeader: TJvTFPrinterDrawEvent;
    FOnDrawFooter: TJvTFPrinterDrawEvent;
    FOnPrintProgress: TJvTFProgressEvent;
    FOnAssembleProgress: TJvTFProgressEvent;
    FOnMarginError: TNotifyEvent;
    FTitle: string;
    FDirectPrint: Boolean;
    {$IFDEF VCL}
    function GetPage(Index: Integer): TMetafile;
    {$ENDIF VCL}
    function GetBodyHeight: Integer; // always in pixels
    function GetBodyWidth: Integer; // always in pixels
    function GetBodyLeft: Integer; // always in pixels
    function GetBodyTop: Integer; // always in pixels
    function GetDocDateTime: TDateTime;
    procedure SetPageLayout(Value: TJvTFPrinterPageLayout);
    procedure SetDirectPrint(Value: Boolean);
  protected
    FPageLayout: TJvTFPrinterPageLayout;
    FState: TJvTFPrinterState;
    FDocDateTime: TDateTime;
    FPageCount: Integer; // NOTE: SEE GetPageCount !!
    FConvertingProps: Boolean;
    FAborted: Boolean;

    procedure SetMarginOffset(Index: Integer; Value: Integer); // always in pixels
    function GetMarginOffset(Index: Integer): Integer; // always in pixels
    function GetUnprintable: TJvTFMargins; // always in pixels
    procedure MarginError; dynamic;
    procedure InitializeMargins;
    property BodyHeight: Integer read GetBodyHeight; // always in pixels
    property BodyWidth: Integer read GetBodyWidth; // always in pixels
    property BodyLeft: Integer read GetBodyLeft; // always in pixels
    property BodyTop: Integer read GetBodyTop; // always in pixels
    procedure DrawBody(aCanvas: TCanvas; ARect: TRect; PageNum: Integer); virtual;
    procedure DrawHeader(aCanvas: TCanvas; ARect: TRect; PageNum: Integer); virtual;
    procedure DrawFooter(aCanvas: TCanvas; ARect: TRect; PageNum: Integer); virtual;
    procedure SetTitle(const Value: string); virtual;
    function GetPageCount: Integer;
    procedure SetMeasure(Value: TJvTFPrinterMeasure); virtual;
    procedure CreateLayout; virtual;
    procedure SetPropertyCheck; dynamic;

    procedure GetHeaderFooterRects(var HeaderRect, FooterRect: TRect);

    // document management methods
    procedure CreateDoc; dynamic;
    procedure NewPage; dynamic;
    procedure FinishDoc; dynamic;
    procedure NewDoc; dynamic;
    property DirectPrint: Boolean read FDirectPrint write SetDirectPrint
      default False;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PageCount: Integer read GetPageCount;
    {$IFDEF VCL}
    property Pages[Index: Integer]: TMetafile read GetPage;
    {$ENDIF VCL}
    function ConvertMeasure(Value: Integer; FromMeasure,
      ToMeasure: TJvTFPrinterMeasure; Horizontal: Boolean): Integer;
    function ScreenToPrinter(Value: Integer; Horizontal: Boolean): Integer;
    function PrinterToScreen(Value: Integer; Horizontal: Boolean): Integer;

    property State: TJvTFPrinterState read FState;
    procedure FreeDoc; dynamic;
    procedure Print; dynamic;
    procedure AbortPrint;
    property DocDateTime: TDateTime read GetDocDateTime;
    property ConvertingProps: Boolean read FConvertingProps;
    procedure SaveDocToFiles(BaseFileName: TFileName);
    property Aborted: Boolean read FAborted;
  published
    property PageLayout: TJvTFPrinterPageLayout read FPageLayout
      write SetPageLayout;
    property Measure: TJvTFPrinterMeasure read FMeasure write SetMeasure
      default pmInches;
    property OnDrawBody: TJvTFPrinterDrawEvent read FOnDrawBody
      write FOnDrawBody;
    property OnDrawHeader: TJvTFPrinterDrawEvent read FOnDrawHeader
      write FOnDrawHeader;
    property OnDrawFooter: TJvTFPrinterDrawEvent read FOnDrawFooter
      write FOnDrawFooter;
    property OnPrintProgress: TJvTFProgressEvent read FOnPrintProgress
      write FOnPrintProgress;
    property OnAssembleProgress: TJvTFProgressEvent read FOnAssembleProgress
      write FOnAssembleProgress;
    property OnMarginError: TNotifyEvent read FOnMarginError
      write FOnMarginError;
    property Title: string read FTitle write SetTitle;
  end;

  TJvTFUniversalPrinter = class(TJvTFPrinter)
  public
    procedure NewDoc; override;
    procedure CreateDoc; override;
    procedure NewPage; override;
    procedure FinishDoc; override;
  published
    property DirectPrint;
  end;

  TJvTFDWNameSource = (dwnsSysLong, dwnsSysShort, dwnsCustom);

  TJvTFDrawDWTitleEvent = procedure(Sender: TObject; aCanvas: TCanvas;
    ARect: TRect; DOW: TTFDayOfWeek; DWName: string) of object;

  TJvTFDWNames = class(TPersistent)
  private
    FSource: TJvTFDWNameSource;
    FDWN_Sunday: string;
    FDWN_Monday: string;
    FDWN_Tuesday: string;
    FDWN_Wednesday: string;
    FDWN_Thursday: string;
    FDWN_Friday: string;
    FDWN_Saturday: string;

    FOnChange: TNotifyEvent;

    procedure SetDWN(Index: Integer; const Value: string);
    function GetDWN(Index: Integer): string;
    procedure SetSource(Value: TJvTFDWNameSource);
  protected
    procedure Change; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function GetDWName(DWIndex: Integer): string;
  published
    property Source: TJvTFDWNameSource read FSource write SetSource
      default dwnsSysShort;
    property DWN_Sunday: string index 1 read GetDWN write SetDWN;
    property DWN_Monday: string index 2 read GetDWN write SetDWN;
    property DWN_Tuesday: string index 3 read GetDWN write SetDWN;
    property DWN_Wednesday: string index 4 read GetDWN write SetDWN;
    property DWN_Thursday: string index 5 read GetDWN write SetDWN;
    property DWN_Friday: string index 6 read GetDWN write SetDWN;
    property DWN_Saturday: string index 7 read GetDWN write SetDWN;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  //  TJvTFNavigator = class(TComponent)
  //  private
  //    FBeforeNavigate : TJvTFNavEvent;
  //    FAfterNavigate : TJvTFNavEvent;
  //    FControls : TStringList;
  //    function GetControl(Index: Integer): TJvTFControl;
  //  protected
  //    FNavigating : Boolean;
  //    procedure RegisterControl(aControl: TJvTFControl);
  //    procedure UnregisterControl(aControl: TJvTFControl);
  //  public
  //    constructor Create(AOwner: TComponent); override;
  //    destructor Destroy; override;
  //
  //    function ControlCount : Integer;
  //    property Controls[Index: Integer] : TJvTFControl read GetControl;
  //
  //    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringList;
  //      Dates: TJvTFDateList); virtual;
  //    property Navigating : Boolean read FNavigating;
  //  published
  //    property BeforeNavigate : TJvTFNavEvent read FBeforeNavigate
  //      write FBeforeNavigate;
  //    property AfterNavigate : TJvTFNavEvent read FAfterNavigate
  //      write FAfterNavigate;
  //  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvConsts, JvResources,
  {$ENDIF USEJVCL}
  {$IFDEF VisualCLX}
  QDialogs, QForms;
{$ENDIF VisualCLX}
{$IFDEF VCL}
Dialogs, Forms;
{$ENDIF VCL}

{$IFNDEF USEJVCL}
resourcestring
  RsECouldNotCreateCustomImageMap = 'Could not create CustomImageMap.  ' +
    'Appointment not assigned';
  RsECouldNotCreateAppointmentObject = 'Could not create Appointment object.  ' +
    'ScheduleManager not assigned';
  RsEScheduleManagerNotificationFailedSc = 'ScheduleManager notification failed.  ScheduleManager not assigned';
  RsEScheduleNotificationFailed = 'Schedule notification failed.  ' +
    'Schedule not assigned';
  RsEInvalidStartAndEndTimes = 'Invalid start and end times';
  RsEInvalidStartAndEndDates = 'Invalid start and end dates';
  RsEAppointmentNotificationFailed = 'Appointment notification failed.  ' +
    'Appointment not assigned';
  RsECouldNotCreateNewAppointment = 'Could not create new appointment. ' +
    'Appointment with given ID already exists';
  RsEInvalidTriggerForRefreshControls = 'Invalid Trigger for RefreshControls';
  RsEInvalidScopeInReconcileRefresh = 'Invalid Scope in ReconcileRefresh';
  RsECouldNotRetrieveSchedule = 'Could not retrieve schedule.  ' +
    'ScheduleManager not assigned';
  RsECouldNotReleaseSchedule = 'Could not release schedule.  ' +
    'ScheduleManager not assigned';
  RsECouldNotCreateADocumentBecauseA = 'Could not create a document because a ' +
    'document already exists';
  RsECouldNotFinishDocumentBecauseNo = 'Could not finish document because no ' +
    'document has been created';
  RsEDocumentDoesNotExist = 'Document does not exist';
  RsEDocumentPagesCannotBeAccessedIf = 'Document pages cannot be accessed if ' +
    'printing directly to the printer';
  RsEDocumentPagesAreInaccessibleUntil = 'Document pages are inaccessible until ' +
    'the document has been finished';
  RsECouldNotRetrievePageCount = 'Could not retrieve page count ' +
    'because document does not exist';
  RsEOnlyAFinishedDocumentCanBePrinted = 'Only a finished document can be printed';
  RsEThereAreNoPagesToPrint = 'There are no pages to print';
  RsEDocumentMustBeFinishedToSaveToFile = 'Document must be Finished to save to file';
  RsEThisPropertyCannotBeChangedIfA = 'This property cannot be changed if a ' +
    'document exists';
  RsECouldNotCreateTJvTFPrinterPageLayou = 'Could not create TJvTFPrinterPageLayout ' +
    'because aPrinter must be assigned';
  RsEInvalidFooterHeightd = 'Invalid Footer Height (%d)';
  RsEInvalidHeaderHeightd = 'Invalid Header Height (%d)';
  {$ENDIF USEJVCL}

function AdjustEndTime(ATime: TTime): TTime;
begin
  Result := Frac(Frac(ATime) - Frac(EncodeTime(0, 0, 1, 0)));
end;

function CenterRect(Rect1, Rect2: TRect): TRect;
var
  Rect1Width,
    Rect1Height,
    Rect2Width,
    Rect2Height: Integer;
begin
  Rect1Width := Rect1.Right - Rect1.Left - 1;
  Rect1Height := Rect1.Bottom - Rect1.Top - 1;
  Rect2Width := Rect2.Right - Rect2.Left - 1;
  Rect2Height := Rect2.Bottom - Rect2.Top - 1;

  Result.Left := Rect1.Left + ((Rect1Width - Rect2Width) div 2) - 1;
  Result.Top := Rect1.Top + ((Rect1Height - Rect2Height) div 2) - 1;
  Result.Right := Result.Left + Rect2Width;
  Result.Bottom := Result.Top + Rect2Height;
end;

function MoveRect(ARect: TRect; NewLeft, NewTop: Integer): TRect;
var
  XOffset,
    YOffset: Integer;
begin
  XOffset := NewLeft - ARect.Left;
  YOffset := NewTop - ARect.Top;
  with Result do
  begin
    Left := ARect.Left + XOffset;
    Right := ARect.Right + XOffset;
    Top := ARect.Top + YOffset;
    Bottom := ARect.Bottom + YOffset;
  end;
end;

function StripCRLF(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if (S[I] <> #13) and (S[I] <> #10) then
      Result := Result + S[I];
end;

{ TJvTFCustomImageMap }

constructor TJvTFCustomImageMap.Create(anAppt: TJvTFAppt);
begin
  if not Assigned(anAppt) then
    raise EJvTFScheduleManagerError.CreateRes(@RsECouldNotCreateCustomImageMap);

  inherited Create;
  FAppt := anAppt;
  FMap := TStringlist.Create;
end;

destructor TJvTFCustomImageMap.Destroy;
begin
  FMap.Free;
  inherited;
end;

function TJvTFCustomImageMap.GetImage(MapIndex: Integer): Integer;
begin
  Result := Integer(FMap.Objects[MapIndex]);
end;

procedure TJvTFCustomImageMap.SetImage(MapIndex, Value: Integer);
begin
  FMap.Objects[MapIndex] := TObject(Value);
end;

function TJvTFCustomImageMap.GetImageName(MapIndex: Integer): string;
begin
  Result := FMap[MapIndex];
end;

procedure TJvTFCustomImageMap.Change;
begin
  if Assigned(FAppt.ScheduleManager) then
  begin
    FAppt.ScheduleManager.RefreshConnections(FAppt);
    // implicit post fix
    FAppt.Change;
  end;
end;

function TJvTFCustomImageMap.Count: Integer;
begin
  Result := FMap.Count;
end;

procedure TJvTFCustomImageMap.Add(const ImageName: string; ImageIndex: Integer);
begin
  if FMap.IndexOf(ImageName) = -1 then
  begin
    FMap.AddObject(ImageName, TObject(ImageIndex));
    Change;
  end;
end;

procedure TJvTFCustomImageMap.Delete(MapIndex: Integer);
begin
  FMap.Delete(MapIndex);
  Change;
end;

procedure TJvTFCustomImageMap.Move(SrcMapIndex, DestMapIndex: Integer);
begin
  FMap.Move(SrcMapIndex, DestMapIndex);
end;

function TJvTFCustomImageMap.FindMapIndex(const ImageName: string): Integer;
begin
  Result := FMap.IndexOf(ImageName);
end;

function TJvTFCustomImageMap.FindImageIndex(const ImageName: string): Integer;
begin
  Result := FindMapIndex(ImageName);
  if Result > -1 then
    Result := GetImage(Result);
end;

procedure TJvTFCustomImageMap.Clear;
begin
  while FMap.Count > 0 do
    FMap.Delete(0);
  Change;
end;

procedure TJvTFCustomImageMap.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvTFCustomImageMap then
  begin
    while FMap.Count > 0 do
      FMap.Delete(0);

    for I := 0 to TJvTFCustomImageMap(Source).Count - 1 do
      Add(TJvTFCustomImageMap(Source).ImageNames[I],
        TJvTFCustomImageMap(Source).Images[I]);
    Change;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TJvTFStateImageMap }

constructor TJvTFStateImageMap.Create(Serv: TJvTFScheduleManager);
var
  I: TJvTFStatePic;
begin
  inherited Create;

  for I := Low(TJvTFStatePic) to High(TJvTFStatePic) do
    FPics[I] := -1;

  FUpdating := False;
end;

procedure TJvTFStateImageMap.SetImage(StatePicID: TJvTFStatePic; Value: Integer);
begin
  if Value < -1 then
    Value := -1;
  if FPics[StatePicID] <> Value then
  begin
    FPics[StatePicID] := Value;
    Change;
  end;
end;

function TJvTFStateImageMap.GetImage(StatePicID: TJvTFStatePic): Integer;
begin
  Result := FPics[StatePicID];
end;

function TJvTFStateImageMap.GetAlarmDisabled: Integer;
begin
  Result := GetImage(spAlarmDisabled);
end;

function TJvTFStateImageMap.GetAlarmEnabled: Integer;
begin
  Result := GetImage(spAlarmEnabled);
end;

function TJvTFStateImageMap.GetModified: Integer;
begin
  Result := GetImage(spModified);
end;

function TJvTFStateImageMap.GetRecurring: Integer;
begin
  Result := GetImage(spRecurring);
end;

function TJvTFStateImageMap.GetShared: Integer;
begin
  Result := GetImage(spShared);
end;

procedure TJvTFStateImageMap.SetAlarmDisabled(const Value: Integer);
begin
  SetImage(spAlarmDisabled, Value);
end;

procedure TJvTFStateImageMap.SetAlarmEnabled(const Value: Integer);
begin
  SetImage(spAlarmEnabled, Value);
end;

procedure TJvTFStateImageMap.SetModified(const Value: Integer);
begin
  SetImage(spModified, Value);
end;

procedure TJvTFStateImageMap.SetRecurring(const Value: Integer);
begin
  SetImage(spRecurring, Value);
end;

procedure TJvTFStateImageMap.SetShared(const Value: Integer);
begin
  SetImage(spShared, Value);
end;

procedure TJvTFStateImageMap.Change;
begin
  if Assigned(FScheduleManager) and not (csLoading in FScheduleManager.ComponentState) and
    not (csDesigning in FScheduleManager.ComponentState) and not FUpdating then
    FScheduleManager.RefreshConnections(nil);
end;

procedure TJvTFStateImageMap.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TJvTFStateImageMap.EndUpdate;
begin
  if FUpdating then
  begin
    FUpdating := False;
    Change;
  end;
end;

procedure TJvTFStateImageMap.Clear;
var
  I: TJvTFStatePic;
begin
  for I := Low(TJvTFStatePic) to High(TJvTFStatePic) do
    FPics[I] := -1;
  Change;
end;

procedure TJvTFStateImageMap.Assign(Source: TPersistent);
var
  Pic: TJvTFStatePic;
begin
  if Source is TJvTFStateImageMap then
  begin
    for Pic := Low(TJvTFStatePic) to High(TJvTFStatePic) do
      FPics[Pic] := TJvTFStateImageMap(Source).Pics[Pic];
    Change;
  end
  else
    inherited Assign(Source);
end;

{ TJvTFAppt }

constructor TJvTFAppt.Create(Serv: TJvTFScheduleManager; const ApptID: string);
begin
  if not Assigned(Serv) then
    raise EJvTFScheduleManagerError.CreateRes(@RsECouldNotCreateAppointmentObject);

  inherited Create;

  FSchedules := TStringlist.Create;
  FConnections := TStringlist.Create;

  FStartDate := Date;
  FStartTime := Time;
  FEndDate := Date;
  FEndTime := FStartTime + EncodeTime(0, 1, 0, 0);
  FScheduleManager := Serv;

  if ApptID <> '' then
    FID := ApptID
  else
    FID := FScheduleManager.GenerateApptID;

  FModified := False;
  FColor := clDefault;
  FBarColor := clDefault;

  FImageMap := TJvTFCustomImageMap.Create(Self);

  ScheduleManager.Notify(Self, sncLoadAppt);

  Serv.DoCreateApptEvent(Self);
end;

destructor TJvTFAppt.Destroy;
begin
  if Assigned(ScheduleManager) then
    ScheduleManager.DoDestroyApptEvent(Self);

  ScheduleManager.Notify(Self, sncDestroyAppt);

  FSchedules.Free;
  FConnections.Free;
  FImageMap.Free;

  inherited;
end;

function TJvTFAppt.GetDescription: string;
begin
  Result := FDescription;
  ScheduleManager.GetApptDescription(Self, Result);
end;

procedure TJvTFAppt.SetDescription(Value: string);
begin
  ScheduleManager.SetApptDescription(Self, Value);
  if Value <> FDescription then
  begin
    FDescription := Value;
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := True;
      Change;
    end;
  end;
end;

procedure TJvTFAppt.SetAlarmEnabled(Value: Boolean);
begin
  if Value <> FAlarmEnabled then
  begin
    FAlarmEnabled := Value;
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := True;
      Change;
    end;
  end;
end;

procedure TJvTFAppt.SetAlarmAdvance(Value: Integer);
begin
  if Value < 0 then
    Value := 0;

  if Value <> FAlarmAdvance then
  begin
    FAlarmAdvance := Value;
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := True;
      Change;
    end;
  end;
end;

procedure TJvTFAppt.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := True;
      Change;
    end;
  end;
end;

procedure TJvTFAppt.SetBarColor(Value: TColor);
begin
  if Value <> FBarColor then
  begin
    FBarColor := Value;
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := True;
      Change;
    end;
  end;
end;

procedure TJvTFAppt.Notify(Sender: TObject; Code: TJvTFServNotifyCode);
begin
  case Code of
    sncConnectAppt: Connect(TJvTFSched(Sender));
    sncDisconnectAppt: Disconnect(TJvTFSched(Sender));
    // implicit post fix
    //sncPostAppt        : FModified := False;
    sncPostAppt: PostApptNotification;
    sncDeleteAppt: InternalClearSchedules;
    sncRefresh: FModified := False;
  end;
end;

procedure TJvTFAppt.NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Serv) then
    Serv.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEScheduleManagerNotificationFailedSc);
end;

procedure TJvTFAppt.NotifySchedule(Sched: TJvTFSched; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Sched) then
    Sched.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEScheduleNotificationFailed);
end;

function TJvTFAppt.GetConnection(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FConnections.Objects[Index]);
end;

function TJvTFAppt.GetSchedule(Index: Integer): string;
begin
  Result := FSchedules[Index];
end;

procedure TJvTFAppt.CheckConnections;
var
  Schedule: TJvTFSched;
  I: Integer;
  ADate: TDate;
  Temp: TStringlist;
begin
  // Schedules --> Connections
  for I := 0 to ScheduleCount - 1 do
  begin
    ADate := StartDate;
    while Trunc(ADate) <= Trunc(EndDate) do
    begin
      Schedule := ScheduleManager.FindSchedule(Schedules[I], ADate);
      if Assigned(Schedule) and (FConnections.IndexOfObject(Schedule) = -1) then
        Connect(Schedule);

      ADate := ADate + 1;
    end;
  end;

  // Connections --> Schedules
  Temp := TStringlist.Create;
  try
    Temp.Assign(FConnections);
    for I := 0 to Temp.Count - 1 do
    begin
      Schedule := TJvTFSched(Temp.Objects[I]);
      if (FSchedules.IndexOf(Schedule.SchedName) = -1) or
        ((Trunc(Schedule.SchedDate) < Trunc(StartDate)) or
        (Trunc(Schedule.SchedDate) > Trunc(EndDate))) then
        Disconnect(Schedule);
    end;
  finally
    Temp.Free;
  end;

  { implicit post fix
  If not FDeleting and not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
    // To avoid display anomolies we need to post the appt here.
    Post;
  }
end;

procedure TJvTFAppt.Connect(Schedule: TJvTFSched);
var
  SchedID: string;
  I: Integer;
begin
  if Assigned(Schedule) then
  begin
    Schedule.Notify(Self, sncConnectAppt);

    SchedID := ScheduleManager.GetScheduleID(Schedule.SchedName, Schedule.SchedDate);
    I := FConnections.IndexOf(SchedID);
    if I = -1 then
    begin
      FConnections.AddObject(SchedID, Schedule);
      ScheduleManager.RefreshConnections(Schedule);
    end;
  end;
end;

procedure TJvTFAppt.Disconnect(Schedule: TJvTFSched);
var
  I: Integer;
begin
  if Assigned(Schedule) then
  begin
    Schedule.Notify(Self, sncDisconnectAppt);

    I := FConnections.IndexOfObject(Schedule);
    if I > -1 then
    begin
      FConnections.Delete(I);
      ScheduleManager.RefreshConnections(Schedule);
    end;
  end;
end;

procedure TJvTFAppt.Change;
begin
  // implicit post fix
  if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing and not Updating then
    Post;
  ScheduleManager.RefreshConnections(Self);
end;

procedure TJvTFAppt.InternalClearSchedules;
begin
  FSchedules.Clear;
  CheckConnections;
end;

procedure TJvTFAppt.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvTFAppt then
  begin
    for I := 0 to TJvTFAppt(Source).ScheduleCount - 1 do
      AddSchedule(TJvTFAppt(Source).Schedules[I]);
    ImageMap.Assign(TJvTFAppt(Source).ImageMap);
    SetStartEnd(TJvTFAppt(Source).StartDate, TJvTFAppt(Source).StartTime,
      TJvTFAppt(Source).EndDate, TJvTFAppt(Source).EndTime);
    Description := TJvTFAppt(Source).Description;
    AlarmEnabled := TJvTFAppt(Source).AlarmEnabled;
    AlarmAdvance := TJvTFAppt(Source).AlarmAdvance;
    Data := TJvTFAppt(Source).Data;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TJvTFAppt.SetStartEnd(NewStartDate: TDate; NewStartTime: TTime;
  NewEndDate: TDate; NewEndTime: TTime);
begin
  // The following avoids time overflow into next day when it is not
  //  intended.  (Add appt to last row of days would cause invalid
  //  start/end exception.)
  if Frac(NewEndTime) <= EncodeTime(0, 0, 0, 999) then
    NewEndTime := EncodeTime(23, 59, 59, 0);

  if Trunc(NewStartDate) <= Trunc(NewEndDate) then
  begin
    if Trunc(NewStartDate) = Trunc(NewEndDate) then
      if Frac(NewStartTime) >= Frac(NewEndTime) then
        raise EJvTFScheduleManagerError.CreateRes(@RsEInvalidStartAndEndTimes);

    FStartDate := NewStartDate;
    FEndDate := NewEndDate;
    FStartTime := NewStartTime;
    FEndTime := NewEndTime;

    CheckConnections;

    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := True;
      Change;
    end
  end
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEInvalidStartAndEndDates);
end;

procedure TJvTFAppt.SetModified;
begin
  FModified := True;
  // implicit post fix
  Change;
end;

function TJvTFAppt.Modified: Boolean;
begin
  Result := FModified;
end;

function TJvTFAppt.ConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

function TJvTFAppt.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

procedure TJvTFAppt.AddSchedule(const SchedName: string);
var
  ADate: TDate;
  Schedule: TJvTFSched;
begin
  if SchedName = '' then
    Exit;

  // Add it to the schedules list
  if FSchedules.IndexOf(SchedName) = -1 then
  begin
    FSchedules.Add(SchedName);
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := True;
      // implicit post fix
      Change;
    end;
  end;

  // Check for needed connections
  //  (Only connects to currently loaded schedules.  Will not load a schedule.)
  ADate := StartDate;
  while Trunc(ADate) <= Trunc(EndDate) do
  begin
    Schedule := ScheduleManager.FindSchedule(SchedName, ADate);
    if Assigned(Schedule) then
      Connect(Schedule);
    ADate := ADate + 1;
  end;

  { implicit post fix
  // To avoid display anomolies we need to post the appt here.
  If not FDeleting and not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
    Post;
  }
end;

procedure TJvTFAppt.RemoveSchedule(const SchedName: string);
var
  I: Integer;
  ADate: TDate;
  Schedule: TJvTFSched;
begin
  if SchedName = '' then
    Exit;

  // Remove it from the schedule list
  I := FSchedules.IndexOf(SchedName);
  if I > -1 then
  begin
    FSchedules.Delete(I);
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := True;
      // implicit post fix
      Change;
    end;
  end;

  // Check for invalid connections and disconnect
  ADate := StartDate;
  while Trunc(ADate) <= Trunc(EndDate) do
  begin
    Schedule := ScheduleManager.FindSchedule(SchedName, ADate);
    if Assigned(Schedule) then
      Disconnect(Schedule);

    ADate := ADate + 1;
  end;

  { implicit post fix
  // To avoid display anomolies we need to post the appt here.
  If not FDeleting and not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing Then
    Post;
  }
end;

procedure TJvTFAppt.AssignSchedules(List: TStrings);
begin
  FSchedules.Assign(List);
  if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
  begin
    FModified := True;
    // implicit post fix
    Change;
  end;

  CheckConnections;
end;

procedure TJvTFAppt.ClearSchedules;
begin
  FSchedules.Clear;

  if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
  begin
    FModified := True;
    // implicit post fix
    Change;
  end;

  CheckConnections;
end;

function TJvTFAppt.IndexOfSchedule(const SchedName: string): Integer;
begin
  Result := FSchedules.IndexOf(SchedName);
end;

function TJvTFAppt.Shared: Boolean;
begin
  Result := ScheduleCount > 1;
end;

procedure TJvTFAppt.Post;
begin
  ScheduleManager.dbPostAppt(Self);
end;

procedure TJvTFAppt.Refresh;
begin
  ScheduleManager.dbRefreshAppt(Self);
end;

procedure TJvTFAppt.Delete;
begin
  ScheduleManager.dbDeleteAppt(Self);
end;

procedure TJvTFAppt.RefreshControls;
begin
  ScheduleManager.RefreshConnections(Self);
end;

function TJvTFAppt.GetEndDateTime: TDateTime;
begin
  Result := Trunc(EndDate) + Frac(EndTime);
end;

function TJvTFAppt.GetStartDateTime: TDateTime;
begin
  Result := Trunc(StartDate) + Frac(StartTime);
end;

function TJvTFAppt.GetEndDate: TDate;
begin
  Result := Int(FEndDate);
end;

function TJvTFAppt.GetEndTime: TTime;
begin
  Result := Frac(FEndTime);
end;

function TJvTFAppt.GetStartDate: TDate;
begin
  Result := Int(FStartDate);
end;

function TJvTFAppt.GetStartTime: TTime;
begin
  Result := Frac(FStartTime);
end;

procedure TJvTFAppt.DeleteApptNotification;
begin
  FDeleting := True;
  try
    InternalClearSchedules;
  finally
    FDeleting := False;
  end;
end;

procedure TJvTFAppt.PostApptNotification;
begin
  FModified := False;
  FUpdating := False;
end;

procedure TJvTFAppt.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TJvTFAppt.EndUpdate;
begin
  if FUpdating then
  begin
    FUpdating := False;
    Change;
  end;
end;

procedure TJvTFAppt.SetRefreshed(Value: Boolean);
begin
  FRefreshed := Value;
end;

procedure TJvTFAppt.RefreshNotification;
begin
  FModified := False;
  Refreshed := False;
end;

{ TJvTFSched }

constructor TJvTFSched.Create(Serv: TJvTFScheduleManager; const AName: string;
  ADate: TDate);
begin
  inherited Create;

  FScheduleManager := Serv;
  FSchedName := AName;
  FSchedDate := ADate;

  FAppts := TStringlist.Create;
  FConControls := TStringlist.Create;
  FConControls.OnChange := ConnectionsOnChange;
  FConComponents := TStringlist.Create;
  FConComponents.OnChange := ConnectionsOnChange;

  if Assigned(Serv) then
    Serv.DoCreateScheduleEvent(Self);
end;

destructor TJvTFSched.Destroy;
var
  Ctrl: TJvTFControl;
  Comp: TJvTFComponent;
  Appt: TJvTFAppt;
begin
  FDestroying := True;

  if Assigned(ScheduleManager) then
    ScheduleManager.DoDestroyScheduleEvent(Self);

  while ConControlCount > 0 do
  begin
    Ctrl := TJvTFControl(FConControls.Objects[0]);
    ScheduleManager.ReleaseSchedule(Ctrl, SchedName, SchedDate);
  end;

  while ConComponentCount > 0 do
  begin
    Comp := TJvTFComponent(FConComponents.Objects[0]);
    ScheduleManager.ReleaseSchedule(Comp, SchedName, SchedDate);
  end;

  while ApptCount > 0 do
  begin
    Appt := Appts[0];
    Appt.Notify(Self, sncDisconnectAppt);
  end;

  ScheduleManager.Notify(Self, sncDestroySchedule);

  FAppts.Free;
  FConControls.Free;
  FConComponents.Free;

  inherited;
end;

function TJvTFSched.GetAppt(Index: Integer): TJvTFAppt;
begin
  Result := TJvTFAppt(FAppts.Objects[Index]);
end;

procedure TJvTFSched.Notify(Sender: TObject; Code: TJvTFServNotifyCode);
var
  I: Integer;
  ConList: TStringlist;
begin
  if Sender is TJvTFControl then
    ConList := FConControls
  else if Sender is TJvTFComponent then
    ConList := FConComponents
  else
    ConList := nil;

  case Code of
    sncRequestSchedule:
      if ConList.IndexOfObject(Sender) = -1 then
        ConList.AddObject('', Sender);
    sncReleaseSchedule:
      begin
        I := ConList.IndexOfObject(Sender);
        if I > -1 then
          ConList.Delete(I);
      end;

    sncConnectAppt:
      ConnectAppt(TJvTFAppt(Sender));

    sncDisconnectAppt:
      DisconnectAppt(TJvTFAppt(Sender));
  end;
end;

procedure TJvTFSched.NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Serv) then
    Serv.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEScheduleManagerNotificationFailedSc);
end;

procedure TJvTFSched.NotifyAppt(Appt: TJvTFAppt; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Appt) then
    Appt.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEAppointmentNotificationFailed);
end;

function TJvTFSched.GetConControl(Index: Integer): TJvTFControl;
begin
  Result := TJvTFControl(FConControls.Objects[Index]);
end;

function TJvTFSched.GetConComponent(Index: Integer): TJvTFComponent;
begin
  Result := TJvTFComponent(FConComponents.Objects[Index]);
end;

procedure TJvTFSched.ConnectAppt(Appt: TJvTFAppt);
begin
  if FAppts.IndexOf(Appt.ID) = -1 then
    FAppts.AddObject(Appt.ID, Appt);
end;

procedure TJvTFSched.DisconnectAppt(Appt: TJvTFAppt);
var
  I: Integer;
begin
  I := FAppts.IndexOf(Appt.ID);
  if I > -1 then
    FAppts.Delete(I);
end;

procedure TJvTFSched.ConnectionsOnChange(Sender: TObject);
begin
  if (FConControls.Count = 0) and (FConComponents.Count = 0) then
  begin
    FCached := True;
    {$IFDEF VCL}
    FCachedTime := Windows.GetTickCount;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FCachedTime := GetTickCount;
    {$ENDIF VisualCLX}
  end
  else
    FCached := False;
end;

procedure TJvTFSched.CheckConnections;
var
  I: Integer;
  Appt: TJvTFAppt;
  DateHit,
    NameMatch,
    NotConnected: Boolean;
begin
  // Check each appt in the ScheduleManager to see if that appt should be connected
  //  to this schedule.  If so, then connect it.
  for I := 0 to ScheduleManager.ApptCount - 1 do
  begin
    Appt := ScheduleManager.Appts[I];
    DateHit := (Trunc(SchedDate) >= Trunc(Appt.StartDate)) and
      (Trunc(SchedDate) <= Trunc(Appt.EndDate));
    NameMatch := Appt.IndexOfSchedule(SchedName) > -1;
    NotConnected := ApptByID(Appt.ID) = nil;
    if DateHit and NameMatch and NotConnected then
      Appt.Notify(Self, sncConnectAppt);
  end;
end;

function TJvTFSched.GetFreeUsedTime(FreeTime: Boolean): TDynTimeRangeArray;
var
  // 60 mins X 24 hrs = 1440 ==> minutes in a day
  DayArray: array[0..1439] of Boolean; // I'm a poet and don't know it.
  I,
    J,
    MinStart,
    MinEnd: Integer;
  anAppt: TJvTFAppt;
  StartTime,
    EndTime: TTime;
  Switch,
    MinIsFree,
    InRange: Boolean;

  ////////////////////////////////
  // SUBORDINATE ROUTINES
  ////////////////////////////////

  function TimeToMinNum(ATime: TTime): Integer;
  var
    H, M, S, MS: Word;
  begin
    DecodeTime(ATime, H, M, S, MS);
    Result := H * 60 + M;
  end;

  function MinNumToTime(MinNum: Integer): TTime;
  begin
    Result := EncodeTime(MinNum div 60, MinNum mod 60, 0, 0);
  end;

  procedure StartRange;
  begin
    StartTime := MinNumToTime(I);
    InRange := True;
  end;

  procedure EndRange;
  begin
    EndTime := MinNumToTime(I);

    // add range to resultant array
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].StartTime := StartTime;
    Result[High(Result)].EndTime := EndTime;

    InRange := False;
  end;

  ////////////////////
  // MAIN ROUTINE
  ////////////////////
begin
  // Initialize resultant array
  SetLength(Result, 1);
  Result[0].StartTime := 0.0;
  Result[0].EndTime := EncodeTime(23, 59, 59, 0);

  // EXIT if nothing to do
  if ApptCount = 0 then
  begin
    if not FreeTime then
      SetLength(Result, 0);
    Exit;
  end;

  // Initialize working array
  //  True ==> free minute
  //  False ==> used minute
  for I := 0 to 1439 do
    DayArray[I] := True;

  // Go through the appts and mark used minutes in the working array
  for I := 0 to ApptCount - 1 do
  begin
    anAppt := Appts[I];
    MinStart := TimeToMinNum(anAppt.StartTime);
    MinEnd := TimeToMinNum(AdjustEndTime(anAppt.EndTime));

    for J := MinStart to MinEnd do
      DayArray[J] := False;
  end;

  // Now convert working array to resultant array
  SetLength(Result, 0);
  MinIsFree := not FreeTime;
  for I := 0 to 1439 do
  begin
    Switch := DayArray[I] xor MinIsFree;
    MinIsFree := DayArray[I];
    if Switch then
      if MinIsFree then
        if FreeTime then
          StartRange
        else
          EndRange
      else if FreeTime then
        EndRange
      else
        StartRange
  end;

  // close and add the last range if needed
  if InRange then
  begin
    I := 1439; // set I to last min of day
    EndRange;
  end;
end;

function TJvTFSched.ApptCount: Integer;
begin
  Result := FAppts.Count;
end;

function TJvTFSched.ApptByID(const ID: string): TJvTFAppt;
var
  I: Integer;
begin
  Result := nil;
  I := FAppts.IndexOf(ID);
  if I > -1 then
    Result := TJvTFAppt(FAppts.Objects[I]);
end;

function TJvTFSched.ConControlCount: Integer;
begin
  Result := FConControls.Count;
end;

function TJvTFSched.ConComponentCount: Integer;
begin
  Result := FConComponents.Count;
end;

procedure TJvTFSched.AddAppt(Appt: TJvTFAppt);
begin
  if Assigned(Appt) then
    Appt.AddSchedule(SchedName);
end;

procedure TJvTFSched.RemoveAppt(Appt: TJvTFAppt);
begin
  if Assigned(Appt) then
    Appt.RemoveSchedule(SchedName);
end;
{
procedure TJvTFSched.RefreshAppts;
Var
  I,
  J,
  K : Integer;
  ApptIDList,
  RefList : TStringList;
  Appt : TJvTFAppt;
  Sched : TJvTFSched;
  RefID : String;
begin
  // In a multi-user environment, appt objects may be deleted as a result
  // of calling dbRefreshAppt.  (Component user may call Appt.Free.)
  // To account for this we need to build a list of appt ID's instead of
  // working directly from the ScheduleManager's appointment list.
  // We also need to build a list of connections (Components and
  // TJvTFControls) that need to be refreshed.

  ApptIDList := TStringList.Create;
  RefList := TStringList.Create;
  RefList.Duplicates := dupIgnore;
  Try
    For I := 0 to ApptCount - 1 do
      Begin
        Appt := Appts[I];
        ApptIDList.Add(Appt.ID);
        For J := 0 to Appt.ConnectionCount - 1 do
          Begin
            Sched := Appt.Connections[J];
            For K := 0 to Sched.ConComponentCount - 1 do
              Begin
                RefID := IntToStr(Integer(Sched.ConComponents[K]));
                RefList.AddObject(RefID, Sched.ConComponents[K]);
              End;
            For K := 0 to Sched.ConControlCount - 1 do
              Begin
                RefID := IntToStr(Integer(Sched.ConControls[K]));
                RefList.AddObject(RefID, Sched.ConControls[K]);
              End;
          End;
      End;

    For I := 0 to ApptIDList.Count - 1 do
      Begin
        Appt := ScheduleManager.FindAppt(ApptIDList[I]);
        If Assigned(Appt) Then
          ScheduleManager.dbRefreshAppt(Appt);
      End;

    For I := 0 to RefList.Count - 1 do
      ScheduleManager.RefreshConnections(RefList.Objects[I]);
  Finally
    ApptIDList.Free;
    RefList.Free;
  End;
end;
}

procedure TJvTFSched.PostAppts;
var
  I: Integer;
begin
  for I := 0 to ApptCount - 1 do
    ScheduleManager.dbPostAppt(Appts[I]);
end;

function TJvTFSched.GetFreeTime: TDynTimeRangeArray;
begin
  Result := GetFreeUsedTime(True);
end;

function TJvTFSched.GetUsedTime: TDynTimeRangeArray;
begin
  Result := GetFreeUsedTime(False);
end;

function TJvTFSched.TimeIsFree(TimeRange: TJvTFTimeRange): Boolean;
var
  Appt: TJvTFAppt;
  I: Integer;
begin
  Result := True;
  I := 0;

  while (I < ApptCount) and Result do
  begin
    Appt := Appts[I];
    if (Frac(Appt.StartTime) <= Frac(AdjustEndTime(TimeRange.EndTime))) and
      (Frac(AdjustEndTime(Appt.EndTime)) >= Frac(TimeRange.StartTime)) then
      Result := False
    else
      Inc(I);
  end;
end;

function TJvTFSched.TimeIsFree(RangeStart, RangeEnd: TTime): Boolean;
var
  TimeRange: TJvTFTimeRange;
begin
  TimeRange.StartTime := RangeStart;
  TimeRange.EndTime := RangeEnd;
  Result := TimeIsFree(TimeRange);
end;

function TJvTFSched.ApptHasConflicts(anAppt: TJvTFAppt): Boolean;
var
  Appt: TJvTFAppt;
  I: Integer;
begin
  Result := False;
  I := 0;

  while (I < ApptCount) and not Result do
  begin
    Appt := Appts[I];
    if (Appt <> anAppt) and // Don't flag for the given appt
    (Frac(Appt.StartTime) <= Frac(AdjustEndTime(anAppt.EndTime))) and
      (Frac(AdjustEndTime(Appt.EndTime)) >= Frac(anAppt.StartTime)) then
      Result := True
    else
      Inc(I);
  end;
end;

function TJvTFSched.EnumConflicts(TimeRange: TJvTFTimeRange): TDynApptArray;
var
  Appt: TJvTFAppt;
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to ApptCount - 1 do
  begin
    Appt := Appts[I];
    if (Frac(Appt.StartTime) <= Frac(AdjustEndTime(TimeRange.EndTime))) and
      (Frac(AdjustEndTime(Appt.EndTime)) >= Frac(TimeRange.StartTime)) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Appt;
    end;
  end;
end;

function TJvTFSched.EnumConflicts(RangeStart, RangeEnd: TTime): TDynApptArray;
var
  TimeRange: TJvTFTimeRange;
begin
  TimeRange.StartTime := RangeStart;
  TimeRange.EndTime := RangeEnd;
  Result := EnumConflicts(TimeRange);
end;

function TJvTFSched.EnumConflicts(anAppt: TJvTFAppt): TDynApptArray;
var
  Appt: TJvTFAppt;
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to ApptCount - 1 do
  begin
    Appt := Appts[I];
    if (Appt <> anAppt) and // don't add the given appt
    (Frac(Appt.StartTime) <= Frac(AdjustEndTime(anAppt.EndTime))) and
      (Frac(AdjustEndTime(Appt.EndTime)) >= Frac(anAppt.StartTime)) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Appt;
    end;
  end;
end;

function TJvTFSched.GetFirstAppt: TJvTFAppt;
var
  I: Integer;
  anAppt: TJvTFAppt;
begin
  Result := nil;
  I := 0;
  while (I < ApptCount) do
  begin
    anAppt := Appts[I];
    if Trunc(anAppt.StartDate) < Trunc(SchedDate) then
    begin
      Result := anAppt;
      Break; // APPOINTMENT STARTS AT 0:00 (12:00am) SO LEAVE LOOP
    end
    else if not Assigned(Result) then
      Result := anAppt
    else if Frac(anAppt.StartTime) < Frac(Result.StartTime) then
      Result := anAppt;
    Inc(I);
  end;
end;

function TJvTFSched.GetLastAppt: TJvTFAppt;
var
  I: Integer;
  anAppt: TJvTFAppt;
begin
  Result := nil;
  I := 0;
  while (I < ApptCount) do
  begin
    anAppt := Appts[I];
    if Trunc(anAppt.EndDate) > Trunc(SchedDate) then
    begin
      Result := anAppt;
      Break; // APPOINTMENT ENDS AT 23:59 (11:59pm) SO LEAVE LOOP
    end
    else if not Assigned(Result) then
      Result := anAppt
    else if Frac(anAppt.EndTime) > Frac(Result.EndTime) then
      Result := anAppt;
    Inc(I);
  end;
end;

procedure TJvTFSched.Refresh;
begin
  ScheduleManager.dbRefreshSched(Self);
end;

procedure TJvTFSched.SetSchedDisplayName(const Value: string);
begin
  if FSchedDisplayName <> Value then
  begin
    FSchedDisplayName := Value;
    ScheduleManager.RefreshConnections(Self);
  end;
end;

{ TJvTFScheduleManagerCache }

constructor TJvTFScheduleManagerCache.Create(SchedManager: TJvTFScheduleManager);
begin
  inherited Create;
  FScheduleManager := SchedManager;

  FCacheType := ctTimed;
  FTimedDelay := 30000;
  FBufferCount := 7;

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := TimerOnTimer;
  FTimer.Interval := FTimedDelay;
  FTimer.Enabled := FCacheType = ctTimed;
end;

destructor TJvTFScheduleManagerCache.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TJvTFScheduleManagerCache.SetCacheType(Value: TJvTFScheduleManagerCacheType);
begin
  if Value <> FCacheType then
  begin
    FCacheType := Value;
    FTimer.Enabled := Value = ctTimed;
    FlushManager;
  end;
end;

procedure TJvTFScheduleManagerCache.SetTimedDelay(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FTimedDelay then
  begin
    FTimedDelay := Value;
    FTimer.Enabled := False;
    FTimer.Interval := Value;
    if CacheType = ctTimed then
    begin
      FTimer.Enabled := True;
      FlushManager;
    end;
  end;
end;

procedure TJvTFScheduleManagerCache.SetBufferCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FBufferCount then
  begin
    FBufferCount := Value;
    if CacheType = ctBuffer then
      FlushManager;
  end;
end;

procedure TJvTFScheduleManagerCache.FlushManager;
begin
  if Assigned(FScheduleManager) then
    FScheduleManager.Flush(False);
end;

procedure TJvTFScheduleManagerCache.TimerOnTimer(Sender: TObject);
begin
  FlushManager;
end;

procedure TJvTFScheduleManagerCache.Assign(Source: TPersistent);
begin
  if Source is TJvTFScheduleManagerCache then
  begin
    FCacheType := TJvTFScheduleManagerCache(Source).CacheType;
    FTimedDelay := TJvTFScheduleManagerCache(Source).TimedDelay;
    FBufferCount := TJvTFScheduleManagerCache(Source).BufferCount;
    if FTimer.Enabled then
    begin
      FTimer.Enabled := False;
      FTimer.Interval := FTimedDelay;
      FTimer.Enabled := FCacheType = ctTimed;
    end;
    FlushManager;
  end
  else
    inherited Assign(Source);
end;

{ TJvTFScheduleManager }

class function TJvTFScheduleManager.GetScheduleID(const SchedName: string;
  SchedDate: TDate): string;
begin
  Result := SchedName + IntToStr(Trunc(SchedDate));
end;

class function TJvTFScheduleManager.GenerateApptID: string;
var
  I: Integer;
begin
  Result := FloatToStr(Now);
  Randomize;
  for I := 1 to 5 do
    Result := Result + Chr(Random(25) + 65);
end;

constructor TJvTFScheduleManager.Create(AOwner: TComponent);
begin
  inherited;

  FSchedLoadMode := slmOnDemand;

  FAppts := TStringlist.Create;
  FSchedules := TStringlist.Create;

  FSchedBatch := TStringlist.Create;
  FSchedBatch.Sorted := True;
  FSchedBatch.Duplicates := dupIgnore;

  FConControls := TStringlist.Create;
  FConComponents := TStringlist.Create;

  FStateImageMap := TJvTFStateImageMap.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FCache := TJvTFScheduleManagerCache.Create(Self);
end;

destructor TJvTFScheduleManager.Destroy;
begin
  FDestroying := True;

  while ConControlCount > 0 do
    ConControls[0].ScheduleManager := nil;

  while ConComponentCount > 0 do
    ConComponents[0].ScheduleManager := nil;

  while ScheduleCount > 0 do
  begin
    Schedules[0].Free;
  end;

  while ApptCount > 0 do
    Appts[0].Free;

  FAppts.Free;
  FSchedBatch.Free;
  FSchedules.Free;
  FConControls.Free;
  FConComponents.Free;
  FStateImageMap.Free;

  StateImages := nil;
  CustomImages := nil;
  FImageChangeLink.Free;

  FCache.Free;

  inherited;
end;

function TJvTFScheduleManager.GetAppt(Index: Integer): TJvTFAppt;
begin
  Result := TJvTFAppt(FAppts.Objects[Index]);
end;

function TJvTFScheduleManager.GetSchedule(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

function TJvTFScheduleManager.GetConControl(Index: Integer): TJvTFControl;
begin
  Result := TJvTFControl(FConControls.Objects[Index]);
end;

function TJvTFScheduleManager.GetConComponent(Index: Integer): TJvTFComponent;
begin
  Result := TJvTFComponent(FConComponents.Objects[Index]);
end;

procedure TJvTFScheduleManager.SetStateImages(Value: TCustomImageList);
begin
  if Assigned(FStateImages) then
    FStateImages.UnRegisterChanges(FImageChangeLink);

  FStateImages := Value;

  if Assigned(FStateImages) then
  begin
    FStateImages.RegisterChanges(FImageChangeLink);
    FStateImages.FreeNotification(Self);
  end;
end;

procedure TJvTFScheduleManager.SetCustomImages(Value: TCustomImageList);
begin
  if Assigned(FCustomImages) then
    FCustomImages.UnRegisterChanges(FImageChangeLink);

  FCustomImages := Value;

  if Assigned(FCustomImages) then
  begin
    FCustomImages.RegisterChanges(FImageChangeLink);
    FCustomImages.FreeNotification(Self);
  end;
end;

procedure TJvTFScheduleManager.SetCache(Value: TJvTFScheduleManagerCache);
begin
  FCache.Assign(Value);
end;

procedure TJvTFScheduleManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = StateImages then
    begin
      StateImages := nil;
      RefreshConnections(nil);
    end
    else if AComponent = CustomImages then
    begin
      CustomImages := nil;
      RefreshConnections(nil);
    end;
end;

procedure TJvTFScheduleManager.ConnectControl(ApptCtrl: TJvTFControl);
var
  I: Integer;
begin
  if not Assigned(ApptCtrl) then
    Exit;

  I := FConControls.IndexOfObject(ApptCtrl);
  if I = -1 then
    FConControls.AddObject('', ApptCtrl);
end;

procedure TJvTFScheduleManager.DisconnectControl(ApptCtrl: TJvTFControl);
var
  I: Integer;
begin
  if not Assigned(ApptCtrl) then
    Exit;

  I := FConControls.IndexOfObject(ApptCtrl);
  if I > -1 then
  begin
    ApptCtrl.ReleaseSchedules;
    FConControls.Delete(I);
  end;
end;

procedure TJvTFScheduleManager.ConnectComponent(Comp: TJvTFComponent);
var
  I: Integer;
begin
  if not Assigned(Comp) then
    Exit;

  I := FConComponents.IndexOfObject(Comp);
  if I = -1 then
    FConComponents.AddObject('', Comp);
end;

procedure TJvTFScheduleManager.DisconnectComponent(Comp: TJvTFComponent);
var
  I: Integer;
begin
  if not Assigned(Comp) then
    Exit;

  I := FConComponents.IndexOfObject(Comp);
  if I > -1 then
  begin
    Comp.ReleaseSchedules;
    FConComponents.Delete(I);
  end;
end;

procedure TJvTFScheduleManager.Notify(Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  case Code of
    sncConnectComponent: ConnectComponent(TJvTFComponent(Sender));
    sncDisconnectComponent: DisconnectComponent(TJvTFComponent(Sender));
    sncConnectControl: ConnectControl(TJvTFControl(Sender));
    sncDisconnectControl: DisconnectControl(TJvTFControl(Sender));
    sncLoadAppt: AddAppt(TJvTFAppt(Sender));
    sncDestroyAppt: RemoveAppt(TJvTFAppt(Sender));
    sncDestroySchedule: RemoveSchedule(TJvTFSched(Sender));
  end;
end;

procedure TJvTFScheduleManager.NotifyAppt(Appt: TJvTFAppt; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Appt) then
    Appt.Notify(Sender, Code);
end;

procedure TJvTFScheduleManager.NotifySchedule(Sched: TJvTFSched; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Sched) then
    Sched.Notify(Sender, Code);
end;

procedure TJvTFScheduleManager.NotifyApptCtrl(ApptCtrl: TJvTFControl;
  Sender: TObject; Code: TJvTFServNotifyCode);
begin
  if Assigned(ApptCtrl) then
    ApptCtrl.Notify(Sender, Code);
end;

procedure TJvTFScheduleManager.NotifyComp(Comp: TJvTFComponent;
  Sender: TObject; Code: TJvTFServNotifyCode);
begin
  if Assigned(Comp) then
    Comp.Notify(Sender, Code);
end;

procedure TJvTFScheduleManager.RetrieveSchedule(const SchedName: string; SchedDate: TDate;
  var Schedule: TJvTFSched; var LoadedNow: Boolean);
var
  SchedID: string;
  I: Integer;
begin
  SchedID := GetScheduleID(SchedName, SchedDate);
  I := FSchedules.IndexOf(SchedID);

  if I > -1 then
  begin
    Schedule := TJvTFSched(FSchedules.Objects[I]);
    LoadedNow := False;
  end
  else
  begin
    //Schedule := TJvTFSched.Create(Self, SchedName, SchedDate);
    Schedule := GetSchedClass.Create(Self, SchedName, SchedDate);
    FSchedules.AddObject(SchedID, Schedule);
    LoadedNow := True;
    if Cache.CacheType = ctBuffer then
      Flush(False);
    Schedule.CheckConnections;
  end;
end;

procedure TJvTFScheduleManager.NeedAppts(Schedule: TJvTFSched);
begin
  FLoadingAppts := True;
  try
    if Assigned(FOnNeedAppts) then
      FOnNeedAppts(Self, Schedule);
  finally
    FLoadingAppts := False;
    RefreshConnections(Schedule);
  end;
end;

procedure TJvTFScheduleManager.AddAppt(Appt: TJvTFAppt);
begin
  if FAppts.IndexOfObject(Appt) = -1 then
    FAppts.AddObject(Appt.ID, Appt);
end;

procedure TJvTFScheduleManager.RemoveAppt(Appt: TJvTFAppt);
var
  I: Integer;
begin
  for I := 0 to ConControlCount - 1 do
    NotifyApptCtrl(ConControls[I], Appt, sncDestroyAppt);

  for I := 0 to ConComponentCount - 1 do
    NotifyComp(ConComponents[I], Appt, sncDestroyAppt);

  while Appt.ConnectionCount > 0 do
    Appt.Notify(Appt.Connections[0], sncDisconnectAppt);

  FAppts.Delete(FAppts.IndexOfObject(Appt));
end;

procedure TJvTFScheduleManager.RemoveSchedule(Sched: TJvTFSched);
var
  I: Integer;
begin
  for I := 0 to ConControlCount - 1 do
    NotifyApptCtrl(ConControls[I], Sched, sncDestroySchedule);

  for I := 0 to ConComponentCount - 1 do
    NotifyComp(ConComponents[I], Sched, sncDestroySchedule);

  FSchedules.Delete(FSchedules.IndexOfObject(Sched));
  Flush(False);
end;

{
procedure TJvTFScheduleManager.RefreshAppt(Appt: TJvTFAppt);
begin
  FLoadingAppts := True;
  Try
    NotifyAppt(Appt, Self, sncRefresh);
    If Assigned(FOnRefreshAppt) Then
      FOnRefreshAppt(Self, Appt);
  Finally
    FLoadingAppts := False;
  End;
end;
}

procedure TJvTFScheduleManager.DeleteAppt(Appt: TJvTFAppt);
begin
  if Assigned(FOnDeleteAppt) then
    FOnDeleteAppt(Self, Appt);
end;

procedure TJvTFScheduleManager.PostAppt(Appt: TJvTFAppt);
begin
  if Assigned(FOnPostAppt) then
    FOnPostAppt(Self, Appt);
end;

procedure TJvTFScheduleManager.RequestRefresh(ApptCtrl: TJvTFControl;
  Schedule: TJvTFSched);
begin
  NotifyApptCtrl(ApptCtrl, Self, sncRefresh);
  {
    If Assigned(ApptCtrl) Then
      Windows.PostMessage(ApptCtrl.Handle, CN_REQUESTREFRESH, Integer(Schedule), 0)
    Else
      Raise EJvTFScheduleManagerError.Create('Could not send refresh request.  ' +
                                        'ApptCtrl not assigned');
  }
end;

procedure TJvTFScheduleManager.RequestRefresh(Comp: TJvTFComponent;
  Schedule: TJvTFSched);
begin
  NotifyComp(Comp, Self, sncRefresh);
end;

procedure TJvTFScheduleManager.ImageListChange(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    RefreshConnections(nil);
end;

procedure TJvTFScheduleManager.FlushAppts;
var
  I: Integer;
begin
  I := 0;
  while I < ApptCount do
    if (Appts[I].ConnectionCount = 0) and not Appts[I].Persistent then
    begin
      if not FlushObject(Appts[I]) then
        Inc(I);
    end
    else
      Inc(I);
end;

function TJvTFScheduleManager.FlushObject(FlushObj: TObject): Boolean;
var
  FlushIt: Boolean;
begin
  Result := False;
  if Assigned(FlushObj) then
  begin
    FlushIt := True;
    if Assigned(FOnFlush) then
      FOnFlush(Self, FlushObj, FlushIt);
    if FlushIt then
      FlushObj.Free;
    Result := FlushIt;
  end;
end;

procedure TJvTFScheduleManager.DoCreateApptEvent(anAppt: TJvTFAppt);
begin
  if Assigned(FOnCreateAppt) then
    FOnCreateAppt(Self, anAppt);
end;

procedure TJvTFScheduleManager.DoCreateScheduleEvent(aSchedule: TJvTFSched);
begin
  if Assigned(FOnCreateSchedule) then
    FOnCreateSchedule(Self, aSchedule);
end;

procedure TJvTFScheduleManager.DoDestroyApptEvent(anAppt: TJvTFAppt);
begin
  if Assigned(FOnDestroyAppt) then
    FOnDestroyAppt(Self, anAppt);
end;

procedure TJvTFScheduleManager.DoDestroyScheduleEvent(aSchedule: TJvTFSched);
begin
  if Assigned(FOnDestroySchedule) then
    FOnDestroySchedule(Self, aSchedule);
end;

function TJvTFScheduleManager.ApptCount: Integer;
begin
  Result := FAppts.Count;
end;

function TJvTFScheduleManager.FindAppt(const ID: string): TJvTFAppt;
var
  I: Integer;
begin
  Result := nil;
  I := FAppts.IndexOf(ID);
  if I > -1 then
    Result := TJvTFAppt(FAppts.Objects[I]);
end;

function TJvTFScheduleManager.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

function TJvTFScheduleManager.FindSchedule(const SchedName: string;
  SchedDate: TDate): TJvTFSched;
var
  I: Integer;
begin
  Result := nil;
  I := FSchedules.IndexOf(GetScheduleID(SchedName, SchedDate));
  if I > -1 then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFScheduleManager.ConControlCount: Integer;
begin
  Result := FConControls.Count;
end;

function TJvTFScheduleManager.ConComponentCount: Integer;
begin
  Result := FConComponents.Count;
end;

function TJvTFScheduleManager.RequestSchedule(ApptCtrl: TJvTFControl;
  const SchedName: string; SchedDate: TDate): TJvTFSched;
var
  ApptsNeeded: Boolean;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, ApptsNeeded);

  if Assigned(ApptCtrl) then
  begin
    Result.Notify(ApptCtrl, sncRequestSchedule);
    ApptCtrl.Notify(Result, sncRequestSchedule);
  end;

  if ApptsNeeded then
    if SchedLoadMode = slmOnDemand then
      NeedAppts(Result)
    else
    begin
      AddToBatch(Result);
    end;
end;

function TJvTFScheduleManager.RequestSchedule(ApptCtrl: TJvTFControl;
  const SchedName: string; SchedDate: TDate; var LoadedNow: Boolean): TJvTFSched;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, LoadedNow);

  if Assigned(ApptCtrl) then
  begin
    Result.Notify(ApptCtrl, sncRequestSchedule);
    ApptCtrl.Notify(Result, sncRequestSchedule);
  end;

  if LoadedNow then
    NeedAppts(Result);
end;

function TJvTFScheduleManager.RequestSchedule(Comp: TJvTFComponent;
  const SchedName: string; SchedDate: TDate): TJvTFSched;
var
  ApptsNeeded: Boolean;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, ApptsNeeded);

  if Assigned(Comp) then
  begin
    Result.Notify(Comp, sncRequestSchedule);
    Comp.Notify(Result, sncRequestSchedule);
  end;

  if ApptsNeeded then
    NeedAppts(Result);
end;

function TJvTFScheduleManager.RequestSchedule(Comp: TJvTFComponent;
  const SchedName: string; SchedDate: TDate; var LoadedNow: Boolean): TJvTFSched;
begin
  RetrieveSchedule(SchedName, SchedDate, Result, LoadedNow);

  if Assigned(Comp) then
  begin
    Result.Notify(Comp, sncRequestSchedule);
    Comp.Notify(Result, sncRequestSchedule);
  end;

  if LoadedNow then
    NeedAppts(Result);
end;

procedure TJvTFScheduleManager.ReleaseSchedule(ApptCtrl: TJvTFControl;
  const SchedName: string; SchedDate: TDate);
var
  SchedID: string;
  I: Integer;
  Schedule: TJvTFSched;
begin
  SchedID := GetScheduleID(SchedName, SchedDate);
  I := FSchedules.IndexOf(SchedID);

  if I > -1 then
  begin
    Schedule := TJvTFSched(FSchedules.Objects[I]);

    if Assigned(ApptCtrl) then
    begin
      Schedule.Notify(ApptCtrl, sncReleaseSchedule);
      ApptCtrl.Notify(Schedule, sncReleaseSchedule);
    end;

    if (Cache.CacheType = ctBuffer) then
      Flush(False);
  end;
end;

procedure TJvTFScheduleManager.ReleaseSchedule(Comp: TJvTFComponent;
  const SchedName: string; SchedDate: TDate);
var
  SchedID: string;
  I: Integer;
  Schedule: TJvTFSched;
begin
  SchedID := GetScheduleID(SchedName, SchedDate);
  I := FSchedules.IndexOf(SchedID);

  if I > -1 then
  begin
    Schedule := TJvTFSched(FSchedules.Objects[I]);

    if Assigned(Comp) then
    begin
      Schedule.Notify(Comp, sncReleaseSchedule);
      Comp.Notify(Schedule, sncReleaseSchedule);
    end;

    if Cache.CacheType = ctBuffer then
      Flush(False);
  end;
end;

procedure TJvTFScheduleManager.RequestAppt(const ID: string; var Appt: TJvTFAppt;
  var New: Boolean);
var
  I: Integer;
begin
  I := -1;
  if ID <> '' then
    I := FAppts.IndexOf(ID);

  if I > -1 then
  begin
    Appt := TJvTFAppt(FAppts.Objects[I]);
    New := False;
  end
  else
  begin
    //Appt := TJvTFAppt.Create(Self, ID);
    Appt := GetApptClass.Create(Self, ID);
    New := True;
  end;
end;

procedure TJvTFScheduleManager.dbPostAppt(Appt: TJvTFAppt);
begin
  { implicit post fix
    If Assigned(Appt) Then
      If (AlwaysPost or Appt.Modified) Then
        Begin
          PostAppt(Appt);
          Appt.Notify(Self, sncPostAppt);
        End;
  }

    // implicit post fix
  if Assigned(Appt) and
    (AlwaysPost or Appt.Modified) and
    QueryPostAppt(Appt) then
  begin
    PostAppt(Appt);
    Appt.Notify(Self, sncPostAppt);
  end;
end;

procedure TJvTFScheduleManager.dbDeleteAppt(Appt: TJvTFAppt);
begin
  if Assigned(Appt) then
  begin
    DeleteAppt(Appt);
    Appt.Notify(Self, sncDeleteAppt);
  end;
end;

procedure TJvTFScheduleManager.dbRefreshAppt(Appt: TJvTFAppt);
begin
  if Assigned(Appt) then
  begin
    FRefreshing := True;
    try
      Appt.Notify(Self, sncRefresh);
      if Assigned(FOnRefreshAppt) then
        FOnRefreshAppt(Self, Appt);
      if RefreshAutoReconcile then
        ReconcileRefresh(Appt);
    finally
      FRefreshing := False;

      // BUG - IT'S A LITTLE LATE TO BE USING THE APPT AS A REFRESH TRIGGER!!!
      //RefreshConnections(Appt);
      // Use nil as trigger to refresh everything
      RefreshConnections(nil);
    end;
  end;
  {
    If Assigned(Appt) Then
      RefreshAppt(Appt);
  }
end;

function TJvTFScheduleManager.dbNewAppt(const ID: string): TJvTFAppt;
var
  New: Boolean;
begin
  Result := nil;
  RequestAppt(ID, Result, New);
  if not New then
    raise EJvTFScheduleManagerError.CreateRes(@RsECouldNotCreateNewAppointment);
end;

procedure TJvTFScheduleManager.PostAppts;
var
  I: Integer;
begin
  for I := 0 to ApptCount - 1 do
    dbPostAppt(Appts[I]);
end;

procedure TJvTFScheduleManager.RefreshAppts;
var
  I: Integer;
  ApptIDList: TStringlist;
  Appt: TJvTFAppt;
begin
  // In a multi-user environment, appt objects may be deleted as a result
  // of calling dbRefreshAppt.  (Component user may call Appt.Free.)
  // To account for this we need to build a list of appt ID's instead of
  // working directly from the ScheduleManager's appointment list.

  ApptIDList := TStringlist.Create;
  try
    for I := 0 to ApptCount - 1 do
    begin
      Appt := Appts[I];
      ApptIDList.Add(Appt.ID);
    end;

    for I := 0 to ApptIDList.Count - 1 do
    begin
      Appt := FindAppt(ApptIDList[I]);
      if Assigned(Appt) then
        dbRefreshAppt(Appt);
    end;

    RefreshConnections(nil);
  finally
    ApptIDList.Free;
  end;
end;

procedure TJvTFScheduleManager.RefreshConnections(Trigger: TObject);
var
  Sched: TJvTFSched;
  Appt: TJvTFAppt;
  I: Integer;
begin
  // Do not refresh if we're loading or refreshing appts
  if FLoadingAppts or Refreshing then
    Exit;

  if Trigger = nil then
  begin
    // refresh all schedules for all controls connected to ScheduleManager
    for I := 0 to ConControlCount - 1 do
      RequestRefresh(ConControls[I], nil);
    // refresh all schedules for all components connected to the ScheduleManager
    for I := 0 to ConComponentCount - 1 do
      RequestRefresh(ConComponents[I], nil);
  end
  else if Trigger is TJvTFComponent then
  begin
    // refresh all schedules for given component
    RequestRefresh(TJvTFComponent(Trigger), nil);
  end
  else if Trigger is TJvTFControl then
  begin
    // refresh all schedules for given control
    RequestRefresh(TJvTFControl(Trigger), nil);
  end
  else if Trigger is TJvTFSched then
  begin
    // refresh all appt controls connected to schedule
    Sched := TJvTFSched(Trigger);
    for I := 0 to Sched.ConControlCount - 1 do
      RequestRefresh(Sched.ConControls[I], Sched);
    // refresh all utf components connected to schedule
    for I := 0 to Sched.ConComponentCount - 1 do
      RequestRefresh(Sched.ConComponents[I], Sched);
  end
  else if Trigger is TJvTFAppt then
  begin
    // refresh all appt controls for all schedules connected to this appt
    Appt := TJvTFAppt(Trigger);
    for I := 0 to Appt.ConnectionCount - 1 do
      RefreshConnections(Appt.Connections[I]);
  end
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEInvalidTriggerForRefreshControls)
end;

procedure TJvTFScheduleManager.Flush(All: Boolean); //param All defaults to False
var
  I: Integer;
  Sched: TJvTFSched;
  MRUList: TStringlist;
  CacheTimeUp: Boolean;
begin
  if FFlushing or FDestroying then
    Exit;

  FFlushing := True;
  try
    if All then
    begin
      I := 0;
      while I < ScheduleCount do
      begin
        Sched := Schedules[I];
        if Sched.Cached and not Sched.Persistent then
        begin
          if not FlushObject(Sched) then
            Inc(I);
        end
        else
          Inc(I);
      end;
      FlushAppts;
    end
    else if Cache.CacheType = ctTimed then
    begin
      I := 0;
      while I < ScheduleCount do
      begin
        Sched := Schedules[I];
        {$IFDEF VCL}
        CacheTimeUp := Windows.GetTickCount - Sched.CachedTime >=
          UINT(Cache.TimedDelay);
        {$ENDIF VCL}
        {$IFDEF VisualCLX}
        CacheTimeUp := GetTickCount - Sched.CachedTime >=
          UINT(Cache.TimedDelay);
        {$ENDIF VisualCLX}
        if Sched.Cached and CacheTimeUp then
        begin
          if not FlushObject(Sched) then
            Inc(I);
        end
        else
          Inc(I);
      end;
      FlushAppts;
    end
    else if Cache.CacheType = ctBuffer then
    begin
      MRUList := TStringlist.Create;
      try
        MRUList.Sorted := True;
        MRUList.Duplicates := dupAccept;
        for I := 0 to ScheduleCount - 1 do
        begin
          Sched := Schedules[I];
          if Sched.Cached then
            MRUList.AddObject(IntToHex(Sched.CachedTime, 8), Sched);
        end;
        for I := 0 to MRUList.Count - 1 - Cache.BufferCount do
          FlushObject(MRUList.Objects[I]);
        FlushAppts;
      finally
        MRUList.Free;
      end;
    end;

  finally
    FFlushing := False;
  end;
end;

procedure TJvTFScheduleManager.dbRefreshAll;
var
  I: Integer;
begin
  FRefreshing := True;
  try
    for I := 0 to ApptCount - 1 do
      NotifyAppt(Appts[I], Self, sncRefresh);
    if Assigned(FOnRefreshAll) then
      FOnRefreshAll(Self);
    if RefreshAutoReconcile then
      ReconcileRefresh(Self);
  finally
    FRefreshing := False;
    RefreshConnections(nil);
  end;
end;

procedure TJvTFScheduleManager.dbRefreshOrphans;
var
  I: Integer;
begin
  for I := 0 to ApptCount - 1 do
    if Appts[I].ConnectionCount = 0 then
      dbRefreshAppt(Appts[I]);
end;

procedure TJvTFScheduleManager.dbRefreshSched(Sched: TJvTFSched);
var
  I: Integer;
begin
  if Assigned(Sched) then
  begin
    FRefreshing := True;
    try
      for I := 0 to Sched.ApptCount - 1 do
        NotifyAppt(Sched.Appts[I], Self, sncRefresh);
      if Assigned(FOnRefreshSched) then
        FOnRefreshSched(Self, Sched);
      if RefreshAutoReconcile then
        ReconcileRefresh(Sched);
    finally
      FRefreshing := False;
      RefreshConnections(Sched);
    end;
  end;
end;

procedure TJvTFScheduleManager.SeTJvTFSchedLoadMode(Value: TJvTFSchedLoadMode);
begin
  if (Value <> FSchedLoadMode) and (Value = slmOnDemand) then
    // make sure we process any queued batches before changing mode
    ProcessBatches;

  FSchedLoadMode := Value;
end;

procedure TJvTFScheduleManager.AddToBatch(aSched: TJvTFSched);
var
  SchedID: string;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(aSched.SchedName, aSched.SchedDate);
  FSchedBatch.AddObject(SchedID, aSched);
end;

procedure TJvTFScheduleManager.ProcessBatches;
var
  I: Integer;
  aSched: TJvTFSched;
  CompName: string;
  CompDate: TDate;
  BatchName: string;
  BatchStartDate: TDate;
  BatchEndDate: TDate;

  /////////////////////////////////////////////
  //        SUBORDINATE ROUTINES
  /////////////////////////////////////////////

  procedure UpdateCompares(_aSched: TJvTFSched);
  begin
    CompName := _aSched.SchedName;
    CompDate := _aSched.SchedDate;
  end;

  procedure NewBatch(_aSched: TJvTFSched);
  begin
    BatchName := _aSched.SchedName;
    BatchStartDate := _aSched.SchedDate;
    BatchEndDate := _aSched.SchedDate;
  end;

  ////////////////
  // MAIN ROUTINE
  ////////////////
begin
  if FSchedBatch.Count = 0 then
    Exit;

  // added by Mike 1/14/01
  FLoadingAppts := True;
  try
    // Prime the process (reminds me of COBOL - yuck!)
    aSched := TJvTFSched(FSchedBatch.Objects[0]);
    UpdateCompares(aSched);
    NewBatch(aSched);

    for I := 1 to FSchedBatch.Count - 1 do
    begin
      aSched := TJvTFSched(FSchedBatch.Objects[I]);

      if (aSched.SchedName <> CompName) or
        (Trunc(aSched.SchedDate) - 1 <> Trunc(CompDate)) then
      begin
        // Hit new batch.  Load the current batch and then
        // set batch info to new batch.
        LoadBatch(BatchName, BatchStartDate, BatchEndDate);
        NewBatch(aSched);
      end
      else
        // Still in current batch.  Update the batch end date.
        BatchEndDate := aSched.SchedDate;

      UpdateCompares(aSched);
    end;

    // Load the last batch
    LoadBatch(BatchName, BatchStartDate, BatchEndDate);

    FSchedBatch.Clear;

    // ADD OnBatchesProcessed EVENT HERE !!
    if Assigned(FOnBatchesProcessed) then
      FOnBatchesProcessed(Self);
  finally
    // added by Mike 1/14/01
    FLoadingAppts := False;
    // added by Mike 1/14/01
    RefreshConnections(nil);
  end;
end;

procedure TJvTFScheduleManager.LoadBatch(const BatchName: string; BatchStartDate,
  BatchEndDate: TDate);
begin
  if Assigned(FOnLoadBatch) then
    FOnLoadBatch(Self, BatchName, BatchStartDate, BatchEndDate);
end;

function TJvTFScheduleManager.QueryPostAppt(Appt: TJvTFAppt): Boolean;
begin
  Result := True;
  if Assigned(FOnPostApptQuery) then
    FOnPostApptQuery(Self, Appt, Result);
end;

function TJvTFScheduleManager.GetApptDisplayText(AComponent: TComponent;
  Appt: TJvTFAppt): string;
begin
  if Assigned(Appt) then
    Result := Appt.Description
  else
    Result := '';

  if Assigned(FOnGetApptDisplayText) then
    FOnGetApptDisplayText(Self, AComponent, Appt, Result);
end;

procedure TJvTFScheduleManager.SetApptDescription(Appt: TJvTFAppt;
  var Value: string);
begin
  if Assigned(FOnSetApptDescription) then
    FOnSetApptDescription(Self, Appt, Value);
end;

procedure TJvTFScheduleManager.GetApptDescription(Appt: TJvTFAppt;
  var Value: string);
begin
  if Assigned(FOnGetApptDescription) then
    FOnGetApptDescription(Self, Appt, Value);
end;

function TJvTFScheduleManager.GetApptClass: TJvTFApptClass;
begin
  Result := TJvTFAppt;
end;

function TJvTFScheduleManager.GetSchedClass: TJvTFSchedClass;
begin
  Result := TJvTFSched;
end;

procedure TJvTFScheduleManager.ReconcileRefresh(Scope: TObject);
var
  Appt: TJvTFAppt;
  Sched: TJvTFSched;
  I: Integer;
begin
  if Scope is TJvTFAppt then
  begin
    Appt := TJvTFAppt(Scope);
    if not Appt.Refreshed then
      Appt.ClearSchedules;
  end
  else if Scope is TJvTFSched then
  begin
    Sched := TJvTFSched(Scope);
    I := 0;
    while I < Sched.ApptCount do
    begin
      Appt := Sched.Appts[I];
      if not Appt.Refreshed then
        Appt.ClearSchedules
      else
        Inc(I);
    end;
  end
  else if Scope is TJvTFScheduleManager then
    for I := 0 to ApptCount - 1 do
      ReconcileRefresh(Appts[I])
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEInvalidScopeInReconcileRefresh);
end;

procedure TJvTFScheduleManager.SetRefreshAutoReconcile(Value: Boolean);
begin
  FRefreshAutoReconcile := Value;
end;

{ TJvTFHint }

constructor TJvTFHint.Create(anApptCtrl: TJvTFControl);
begin
  inherited Create(anApptCtrl);
  FApptCtrl := anApptCtrl;
  FTimer := TTimer.Create(Self);
  FShortPause := 1000;
  FPause := 3000;
  FTimer.OnTimer := TimerOnTimer;
  PrepTimer(True);
end;

destructor TJvTFHint.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TJvTFHint.SetPause(Value: Integer);
begin
  FPause := Value;
end;

procedure TJvTFHint.SetShortPause(Value: Integer);
begin
  FShortPause := Value;
end;

procedure TJvTFHint.TimerOnTimer(Sender: TObject);
begin
  FTimer.Enabled := False;

  if FShortTimer then
    DoHint(False)
  else
  begin
    ReleaseHandle;
    PrepTimer(True);
  end;
end;

procedure TJvTFHint.PrepTimer(Short: Boolean);
begin
  ReleaseHandle;
  FShortTimer := Short;
  if Short then
    FTimer.Interval := FShortPause
  else
    FTimer.Interval := FPause;
end;

procedure TJvTFHint.SetHintText(StartDate, EndDate: TDate; StartTime,
  EndTime: TTime; const Desc: string; ShowDatesTimes, ShowDesc: Boolean);
var
  ShowDates: Boolean;
  HintText,
    DFormat,
    TFormat: string;
begin
  HintText := '';
  if ShowDatesTimes then
  begin
    DFormat := FApptCtrl.DateFormat;
    TFormat := FApptCtrl.TimeFormat;
    ShowDates := Trunc(StartDate) <> Trunc(EndDate);

    if ShowDates then
      HintText := FormatDateTime(DFormat, StartDate) + ' ';
    HintText := HintText + FormatDateTime(TFormat, StartTime) + ' - ';
    if ShowDates then
      HintText := HintText + FormatDateTime(DFormat, EndDate) + ' ';
    HintText := HintText + FormatDateTime(TFormat, EndTime);
  end;

  if ShowDesc then
  begin
    if HintText <> '' then
      HintText := HintText + #13#10;
    HintText := HintText + Desc;
  end;
  FHintText := HintText;
end;

procedure TJvTFHint.DoHint(Sustained: Boolean);
var
  Ref: TObject;
begin
  PropertyCheck;
  {
  If Assigned(FOnShowHint) Then
    FOnShowHint(Self, HintType, FHintRect, FHintText);
  }

  if Assigned(FOnShowHint) then
  begin
    if HintType = shtAppt then
      Ref := FOldAppt
    else if HintType = shtObj then
      Ref := FOldObj
    else
      Ref := nil;

    FOnShowHint(Self, HintType, Ref, FHintRect, FHintText);
  end;
  {$IFDEF VCL}
  if not Windows.IsRectEmpty(FHintRect) and (FHintText <> '') then
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    if not IsRectEmpty(FHintRect) and (FHintText <> '') then
      {$ENDIF VisualCLX}
      if Sustained then
      begin
        inherited ActivateHint(FHintRect, FHintText);
      end
      else
        ActivateHint(FHintRect, FHintText);
end;

{$IFDEF VCL}

procedure TJvTFHint.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not CS_SAVEBITS;
  end;
end;
{$ENDIF VCL}

procedure TJvTFHint.ActivateHint(Rect: TRect; const AHint: THintString);
begin
  PrepTimer(False);
  inherited;
  // Reset the timer so we get the full interval
  FTimer.Enabled := False;
  FTimer.Enabled := True;
end;

procedure TJvTFHint.ApptHint(Appt: TJvTFAppt; X, Y: Integer; ShowDatesTimes,
  ShowDesc, FormattedDesc: Boolean);
var
  HintTopLeft: TPoint;
  Immediate: Boolean;
  ApptDesc: string;
begin
  if Appt <> FOldAppt then
  begin
    FHintType := shtAppt;
    Immediate := not FShortTimer;
    FHintCell := Point(-100, -100);
    FOldAppt := Appt;
    if Assigned(Appt) then
    begin
      ApptDesc := Appt.Description;
      if not FormattedDesc then
        ApptDesc := StripCRLF(ApptDesc);
      SetHintText(Appt.StartDate, Appt.EndDate, Appt.StartTime, Appt.EndTime,
        ApptDesc, ShowDatesTimes, ShowDesc);
      FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
      HintTopLeft := FApptCtrl.ClientToScreen(Point(X, Y));
      FHintRect := MoveRect(FHintRect, HintTopLeft.X, HintTopLeft.Y);
      if Immediate then
        DoHint(False)
      else
      begin
        PrepTimer(True);
        FTimer.Enabled := True;
      end;
    end
    else
    begin
      ReleaseHandle;
      PrepTimer(True);
    end;
  end;
end;

procedure TJvTFHint.StartEndHint(StartDate, EndDate: TDate; StartTime,
  EndTime: TTime; X, Y: Integer; ShowDates: Boolean);
var
  HintTopLeft: TPoint;
begin
  FHintType := shtStartEnd;
  SetHintText(StartDate, EndDate, StartTime, EndTime, '', True, False);
  FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
  HintTopLeft := FApptCtrl.ClientToScreen(Point(X, Y));
  FHintRect := MoveRect(FHintRect, HintTopLeft.X, HintTopLeft.Y);
  if HandleAllocated and Showing then
    BoundsRect := FHintRect
  else
    DoHint(True);
end;

procedure TJvTFHint.CellHint(Row, Col: Integer; const HintText: string; CellRect: TRect);
var
  Immediate: Boolean;
  DiffCell: Boolean;
begin
  DiffCell := (Row <> FHintCell.Y) or (Col <> FHintCell.X);
  if DiffCell or not FTimer.Enabled then
  begin
    FHintType := shtCell;
    FOldAppt := nil;
    ReleaseHandle;
    FHintCell.X := Col;
    FHintCell.Y := Row;
    Immediate := not FShortTimer;
    FHintText := HintText;
    //If (FHintText <> '') and DiffCell Then
    if FHintText <> '' then
    begin
      CellRect.TopLeft := FApptCtrl.ClientToScreen(CellRect.TopLeft);
      CellRect.BottomRight := FApptCtrl.ClientToScreen(CellRect.BottomRight);
      FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
      FHintRect := CenterRect(CellRect, FHintRect);
      if Immediate then
        DoHint(False)
      else
      begin
        PrepTimer(True);
        FTimer.Enabled := True;
      end;
    end
    else
    begin
      ReleaseHandle;
      PrepTimer(True);
    end;
  end;
end;

procedure TJvTFHint.ReleaseHandle;
begin
  FTimer.Enabled := False;
  DestroyHandle;
end;

procedure TJvTFHint.PropertyCheck;
begin
  if Assigned(RefProps) then
  begin
    if RefProps.HintColor = clDefault then
      Color := Application.HintColor
    else
      Color := RefProps.HintColor;

    if RefProps.HintHidePause = -1 then
      Pause := Application.HintHidePause
    else
      Pause := RefProps.HintHidePause;

    if RefProps.HintPause = -1 then
      ShortPause := Application.HintPause
    else
      ShortPause := RefProps.HintPause;
  end;
end;

procedure TJvTFHint.MultiLineObjHint(Obj: TObject; X, Y: Integer;
  Hints: TStrings);
var
  Immediate: Boolean;
  HintTopLeft: TPoint;
begin
  if Obj <> FOldObj then
  begin
    FOldAppt := nil;
    FHintType := shtObj;
    Immediate := not FShortTimer;
    FHintCell := Point(-100, -100);
    FOldObj := Obj;
    if Assigned(Obj) and (Hints.Count > 0) then
    begin
      FHintText := Hints.Text;
      FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
      HintTopLeft := FApptCtrl.ClientToScreen(Point(X + 8, Y + 16));
      FHintRect := MoveRect(FHintRect, HintTopLeft.X, HintTopLeft.Y);

      if Immediate then
        DoHint(False)
      else
      begin
        PrepTimer(True);
        FTimer.Enabled := True;
      end;
    end
    else
    begin
      ReleaseHandle;
      PrepTimer(True);
    end;
  end;
end;

{ TJvTFControl }

constructor TJvTFControl.Create(AOwner: TComponent);
begin
  inherited;

  FSchedules := TStringlist.Create;
  FTimeFormat := 't'; // global short time format
  FDateFormat := 'ddddd'; // global short date format
end;

destructor TJvTFControl.Destroy;
begin
  ScheduleManager := nil;
  FSchedules.Free;

  inherited;
end;

procedure TJvTFControl.SetManager(Value: TJvTFScheduleManager);
begin
  if Value <> FScheduleManager then
  begin
    if Assigned(FScheduleManager) then
      FScheduleManager.Notify(Self, sncDisconnectControl);
    FScheduleManager := nil;

    if Assigned(Value) then
      Value.Notify(Self, sncConnectControl);
    FScheduleManager := Value;
  end;
end;

function TJvTFControl.GetSchedule(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

procedure TJvTFControl.SetDateFormat(const Value: string);
begin
  if FDateFormat <> Value then
  begin
    FDateFormat := Value;
    Invalidate;
  end;
end;

procedure TJvTFControl.SetTimeFormat(const Value: string);
begin
  if FTimeFormat <> Value then
  begin
    FTimeFormat := Value;
    Invalidate;
  end;
end;

procedure TJvTFControl.Notify(Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  case Code of
    sncRequestSchedule: ReqSchedNotification(TJvTFSched(Sender));
    sncReleaseSchedule: RelSchedNotification(TJvTFSched(Sender));
    sncRefresh: RefreshControl;
    sncDestroyAppt: DestroyApptNotification(TJvTFAppt(Sender));
    sncDestroySchedule: DestroySchedNotification(TJvTFSched(Sender));
  end;
end;

procedure TJvTFControl.ReqSchedNotification(Schedule: TJvTFSched);
var
  SchedID: string;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(Schedule.SchedName, Schedule.SchedDate);
  if FSchedules.IndexOf(SchedID) = -1 then
    FSchedules.AddObject(SchedID, Schedule);
end;

procedure TJvTFControl.RelSchedNotification(Schedule: TJvTFSched);
var
  I: Integer;
begin
  I := FSchedules.IndexOfObject(Schedule);
  if I > -1 then
    FSchedules.Delete(I);
end;

procedure TJvTFControl.NotifyManager(Serv: TJvTFScheduleManager;
  Sender: TObject; Code: TJvTFServNotifyCode);
begin
  if Assigned(Serv) then
    Serv.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEScheduleManagerNotificationFailedSc);
end;

procedure TJvTFControl.CNRequestRefresh(var Msg: TCNRequestRefresh);
begin
  Invalidate;
end;

procedure TJvTFControl.RefreshControl;
begin
  Invalidate;
end;

procedure TJvTFControl.DestroyApptNotification(anAppt: TJvTFAppt);
begin
  // do nothing, leave implementation to successors
end;

procedure TJvTFControl.DestroySchedNotification(aSched: TJvTFSched);
begin
  // do nothing, leave implementation to successors
end;

procedure TJvTFControl.DoStartDrag(var DragObject: TDragObject);
begin
  inherited;

  FDragInfo := TJvTFDragInfo.Create;
  with FDragInfo do
  begin
    ApptCtrl := Self;
    Shift := Self.FShift;
  end;

  {
  Originally, a specific drag object was created and given to the DragObject
  param.  This worked fine.  Because of differences in the VCL DragObject
  hierarachy between D3 and D4, the decision was made to move away from
  using a drag object.

    FDragAppt := TDragAppt.Create(Self);
    With FDragAppt do
      Begin
        ApptCtrl := Self;
        Schedule := SelSchedule;
        Appt := SelAppt;
        Shift := FDragShift;
      End;
    DragObject := FDragAppt;
  }
end;

procedure TJvTFControl.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;

  FDragInfo.Free;
  FDragInfo := nil;
end;

procedure TJvTFControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FShift := Shift;
end;

function TJvTFControl.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

function TJvTFControl.FindSchedule(const SchedName: string;
  SchedDate: TDate): TJvTFSched;
var
  I: Integer;
begin
  Result := nil;

  I := FSchedules.IndexOf(TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate));
  if I > -1 then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFControl.RetrieveSchedule(const SchedName: string;
  SchedDate: TDate): TJvTFSched;
begin
  Result := FindSchedule(SchedName, SchedDate);

  if not Assigned(Result) then
    if Assigned(ScheduleManager) then
      Result := ScheduleManager.RequestSchedule(Self, SchedName, SchedDate)
    else
      raise EJvTFScheduleManagerError.CreateRes(@RsECouldNotRetrieveSchedule);
end;

procedure TJvTFControl.ReleaseSchedule(const SchedName: string;
  SchedDate: TDate);
var
  SchedID: string;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate);
  if FSchedules.IndexOf(SchedID) > -1 then
    if Assigned(ScheduleManager) then
      ScheduleManager.ReleaseSchedule(Self, SchedName, SchedDate)
    else
      raise EJvTFScheduleManagerError.CreateRes(@RsECouldNotReleaseSchedule);

end;

procedure TJvTFControl.ReleaseSchedules;
begin
  while ScheduleCount > 0 do
    ReleaseSchedule(Schedules[0].SchedName, Schedules[0].SchedDate);
end;

procedure TJvTFControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  //  If (AComponent = Navigator) and (Operation = opRemove) Then
  //    Navigator := nil;
end;

//procedure TJvTFControl.SetNavigator(Value: TJvTFNavigator);
//begin
//  If Value <> FNavigator Then
//    Begin
//      If Assigned(FNavigator) Then
//        FNavigator.UnregisterControl(Self);
//      FNavigator := nil;
//
//      If Assigned(Value) Then
//        Value.RegisterControl(Self);
//      FNavigator := Value;
//    End;
//end;

procedure TJvTFControl.Navigate(aControl: TJvTFControl;
  SchedNames: TStringlist; Dates: TJvTFDateList);
begin
  //  If Assigned(FOnNavigate) Then
  //    FOnNavigate(Self, aControl, SchedNames, Dates);
end;

procedure TJvTFControl.ProcessBatches;
begin
  if Assigned(ScheduleManager) and (ScheduleManager.SchedLoadMode = slmBatch) then
    ScheduleManager.ProcessBatches;
end;

{ TJvTFComponent }

constructor TJvTFComponent.Create(AOwner: TComponent);
begin
  inherited;

  FSchedules := TStringlist.Create;
  FTimeFormat := 't'; // global short time format
  FDateFormat := 'ddddd'; // global short date format
end;

destructor TJvTFComponent.Destroy;
begin
  ScheduleManager := nil;
  FSchedules.Free;

  inherited;
end;

procedure TJvTFComponent.DestroyApptNotification(anAppt: TJvTFAppt);
begin
  // do nothing, leave implementation to descendants
end;

procedure TJvTFComponent.DestroySchedNotification(aSched: TJvTFSched);
begin
  // do nothing, leave implementation to descendants
end;

function TJvTFComponent.FindSchedule(const SchedName: string;
  SchedDate: TDate): TJvTFSched;
var
  I: Integer;
begin
  Result := nil;

  I := FSchedules.IndexOf(TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate));
  if I > -1 then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFComponent.GetSchedule(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

procedure TJvTFComponent.Notify(Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  case Code of
    sncRequestSchedule: ReqSchedNotification(TJvTFSched(Sender));
    sncReleaseSchedule: RelSchedNotification(TJvTFSched(Sender));
    sncRefresh: RefreshComponent;
    sncDestroyAppt: DestroyApptNotification(TJvTFAppt(Sender));
    sncDestroySchedule: DestroySchedNotification(TJvTFSched(Sender));
  end;
end;

procedure TJvTFComponent.NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Serv) then
    Serv.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.CreateRes(@RsEScheduleManagerNotificationFailedSc);
end;

procedure TJvTFComponent.ProcessBatches;
begin
  if Assigned(ScheduleManager) and (ScheduleManager.SchedLoadMode = slmBatch) then
    ScheduleManager.ProcessBatches;
end;

procedure TJvTFComponent.RefreshComponent;
begin
  // do nothing, leave implementation to descendants
end;

procedure TJvTFComponent.ReleaseSchedule(const SchedName: string;
  SchedDate: TDate);
var
  SchedID: string;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate);
  if FSchedules.IndexOf(SchedID) > -1 then
    if Assigned(ScheduleManager) then
      ScheduleManager.ReleaseSchedule(Self, SchedName, SchedDate)
    else
      raise EJvTFScheduleManagerError.CreateRes(@RsECouldNotReleaseSchedule);
end;

procedure TJvTFComponent.ReleaseSchedules;
begin
  while ScheduleCount > 0 do
    ReleaseSchedule(Schedules[0].SchedName, Schedules[0].SchedDate);
end;

procedure TJvTFComponent.RelSchedNotification(Schedule: TJvTFSched);
var
  I: Integer;
begin
  I := FSchedules.IndexOfObject(Schedule);
  if I > -1 then
    FSchedules.Delete(I);
end;

procedure TJvTFComponent.ReqSchedNotification(Schedule: TJvTFSched);
var
  SchedID: string;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(Schedule.SchedName, Schedule.SchedDate);
  if FSchedules.IndexOf(SchedID) = -1 then
    FSchedules.AddObject(SchedID, Schedule);
end;

function TJvTFComponent.RetrieveSchedule(const SchedName: string;
  SchedDate: TDate): TJvTFSched;
begin
  Result := FindSchedule(SchedName, SchedDate);

  if not Assigned(Result) then
    if Assigned(ScheduleManager) then
      Result := ScheduleManager.RequestSchedule(Self, SchedName, SchedDate)
    else
      raise EJvTFScheduleManagerError.CreateRes(@RsECouldNotRetrieveSchedule);
end;

function TJvTFComponent.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

procedure TJvTFComponent.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
end;

procedure TJvTFComponent.SetManager(Value: TJvTFScheduleManager);
begin
  if Value <> FScheduleManager then
  begin
    if Assigned(FScheduleManager) then
      FScheduleManager.Notify(Self, sncDisconnectComponent);
    FScheduleManager := nil;

    if Assigned(Value) then
      Value.Notify(Self, sncConnectComponent);
    FScheduleManager := Value;
  end;
end;

procedure TJvTFComponent.SetTimeFormat(const Value: string);
begin
  FTimeFormat := Value;
end;

procedure TJvTFComponent.UpdateDesigner;
var
  ParentForm: TCustomForm;
begin
  if (csDesigning in ComponentState) and not (csUpdating in ComponentState) then
  begin
    try
      ParentForm := TCustomForm(Owner);
      if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
        ParentForm.Designer.Modified;
    except
      // handle the exception by doing nothing
    end;
  end;
end;

{ TJvTFPrinter }

procedure TJvTFPrinter.AbortPrint;
begin
  if Printer.Printing then
    Printer.Abort
  else
    FAborted := True;
end;

function TJvTFPrinter.ConvertMeasure(Value: Integer; FromMeasure,
  ToMeasure: TJvTFPrinterMeasure; Horizontal: Boolean): Integer;
const
  MMFactor = 2.54;
var
  PPI: Integer;
begin
  {$IFDEF VCL}
  if Horizontal then
    PPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  else
    PPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Horizontal then
    PPI := GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  else
    PPI := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ENDIF VisualCLX}
  if (FromMeasure = pmPixels) and (ToMeasure = pmInches) then
    Result := round(Value / PPI * 100)
  else if (FromMeasure = pmPixels) and (ToMeasure = pmMM) then
    Result := round(Value / PPI * 100 * MMFactor)
  else if (FromMeasure = pmInches) and (ToMeasure = pmPixels) then
    Result := round(Value / 100 * PPI)
  else if (FromMeasure = pmInches) and (ToMeasure = pmMM) then
    Result := round(Value * MMFactor)
  else if (FromMeasure = pmMM) and (ToMeasure = pmPixels) then
    Result := round(Value / MMFactor / 100 * PPI)
  else if (FromMeasure = pmMM) and (ToMeasure = pmInches) then
    Result := round(Value / MMFactor)
  else
    Result := Value;
end;

constructor TJvTFPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateLayout;
  FMeasure := pmInches;
  FPages := TStringlist.Create;
  FBodies := TStringlist.Create;
  InitializeMargins;
end;

destructor TJvTFPrinter.Destroy;
begin
  FreeDoc;
  FBodies.Free;
  FPages.Free;

  FPageLayout.Free;
  inherited;
end;

procedure TJvTFPrinter.CreateDoc;
begin
  if State = spsNoDoc then
  begin
    FState := spsCreating;
    FAborted := False;

    FDocDateTime := Now;
    if DirectPrint then
      Printer.BeginDoc;
  end
  else
    raise EJvTFPrinterError.CreateRes(@RsECouldNotCreateADocumentBecauseA);
end;

procedure TJvTFPrinter.CreateLayout;
begin
  FPageLayout := TJvTFPrinterPageLayout.Create(Self);
end;

procedure TJvTFPrinter.DrawBody(aCanvas: TCanvas; ARect: TRect;
  PageNum: Integer);
begin
  if Assigned(FOnDrawBody) then
    FOnDrawBody(Self, aCanvas, ARect, PageNum);
end;

procedure TJvTFPrinter.DrawFooter(aCanvas: TCanvas; ARect: TRect;
  PageNum: Integer);
begin
  if Assigned(FOnDrawFooter) then
    FOnDrawFooter(Self, aCanvas, ARect, PageNum);
end;

procedure TJvTFPrinter.DrawHeader(aCanvas: TCanvas; ARect: TRect;
  PageNum: Integer);
begin
  if Assigned(FOnDrawHeader) then
    FOnDrawHeader(Self, aCanvas, ARect, PageNum);
end;

procedure TJvTFPrinter.FinishDoc;
var
  I: Integer;
  aCanvas: TMetafileCanvas;
  HeaderRect,
    FooterRect: TRect;
begin
  if Aborted then
    Exit;

  if State <> spsCreating then
    raise EJvTFPrinterError.CreateRes(@RsECouldNotFinishDocumentBecauseNo);

  FPageCount := FBodies.Count;
  FState := spsAssembling;
  try
    if Assigned(FOnAssembleProgress) then
      FOnAssembleProgress(Self, 0, FBodies.Count);

    if DirectPrint then
      Printer.EndDoc
    else
    begin
      GetHeaderFooterRects(HeaderRect, FooterRect);
      I := 0;
      while (I < FBodies.Count) and not Aborted do
      begin
        aCanvas := TMetafileCanvas(FBodies.Objects[I]);

        try
          DrawHeader(aCanvas, HeaderRect, I + 1);
          DrawFooter(aCanvas, FooterRect, I + 1);
        finally
          aCanvas.Free;
          FBodies.Objects[I] := nil;
        end;

        if Assigned(FOnAssembleProgress) then
          FOnAssembleProgress(Self, I + 1, FBodies.Count);

        Inc(I);
        Application.ProcessMessages;
      end;
    end;

    FBodies.Clear;
  finally
    FState := spsFinished;
  end;
end;

procedure TJvTFPrinter.FreeDoc;
begin
  while FBodies.Count > 0 do
  begin
    FBodies.Objects[0].Free;
    FBodies.Delete(0);
  end;

  while FPages.Count > 0 do
  begin
    FPages.Objects[0].Free;
    FPages.Delete(0);
  end;

  FState := spsNoDoc;
end;

function TJvTFPrinter.GetBodyHeight: Integer; // always in pixels
var
  PhysHeight,
    TopMarginPels,
    BottomMarginPels,
    HeaderPels,
    FooterPels: Integer;
begin
  {$IFDEF VCL}
  PhysHeight := Windows.GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  PhysHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  {$ENDIF VisualCLX}
  TopMarginPels := ConvertMeasure(PageLayout.MarginTop, Measure, pmPixels,
    False);
  BottomMarginPels := ConvertMeasure(PageLayout.MarginBottom, Measure, pmPixels,
    False);
  HeaderPels := ConvertMeasure(PageLayout.HeaderHeight, Measure, pmPixels,
    False);
  FooterPels := ConvertMeasure(PageLayout.FooterHeight, Measure, pmPixels,
    False);

  Result := PhysHeight - TopMarginPels - BottomMarginPels -
    HeaderPels - FooterPels;
end;

function TJvTFPrinter.GetBodyLeft: Integer; // always in pixels
begin
  Result := GetMarginOffset(1);
end;

function TJvTFPrinter.GetBodyTop: Integer; // always in pixels
begin
  Result := GetMarginOffset(2) +
    ConvertMeasure(PageLayout.HeaderHeight, Measure, pmPixels, False) + 1;
end;

function TJvTFPrinter.GetBodyWidth: Integer; // always in pixels
var
  PhysWidth,
    LeftMarginPels,
    RightMarginPels: Integer;
begin
  {$IFDEF VCL}
  PhysWidth := Windows.GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  PhysWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  {$ENDIF VisualCLX}
  LeftMarginPels := ConvertMeasure(PageLayout.MarginLeft, Measure, pmPixels, True);
  RightMarginPels := ConvertMeasure(PageLayout.MarginRight, Measure, pmPixels, True);

  Result := PhysWidth - LeftMarginPels - RightMarginPels;
end;

function TJvTFPrinter.GetDocDateTime: TDateTime;
begin
  if State = spsNoDoc then
    raise EJvTFPrinterError.CreateRes(@RsEDocumentDoesNotExist);

  Result := FDocDateTime;
end;

procedure TJvTFPrinter.GetHeaderFooterRects(var HeaderRect,
  FooterRect: TRect);
begin
  HeaderRect.Left := FMarginOffsets.Left;
  HeaderRect.Top := FMarginOffsets.Top;
  HeaderRect.Right := HeaderRect.Left + BodyWidth;
  HeaderRect.Bottom := HeaderRect.Top + ConvertMeasure(PageLayout.HeaderHeight,
    Measure, pmPixels, False);

  FooterRect.Left := HeaderRect.Left;
  FooterRect.Right := HeaderRect.Right;
  FooterRect.Top := BodyTop + BodyHeight;
  FooterRect.Bottom := FooterRect.Top + ConvertMeasure(PageLayout.FooterHeight,
    Measure, pmPixels, False);
end;

function TJvTFPrinter.GetMarginOffset(Index: Integer): Integer;
begin
  case Index of
    1: Result := FMarginOffsets.Left;
    2: Result := FMarginOffsets.Top;
    3: Result := FMarginOffsets.Right;
  else
    Result := FMarginOffsets.Bottom;
  end;
end;

{$IFDEF VCL}

function TJvTFPrinter.GetPage(Index: Integer): TMetafile;
begin
  if DirectPrint then
    raise EJvTFPrinterError.CreateRes(@RsEDocumentPagesCannotBeAccessedIf);

  if State <> spsFinished then
    raise EJvTFPrinterError.CreateRes(@RsEDocumentPagesAreInaccessibleUntil);
  Result := TMetafile(FPages.Objects[Index]);
end;
{$ENDIF VCL}

function TJvTFPrinter.GetPageCount: Integer;
begin
  case State of
    spsNoDoc: raise EJvTFPrinterError.CreateRes(@RsECouldNotRetrievePageCount);
    spsCreating: Result := FBodies.Count;
    spsAssembling: Result := FPageCount;
    spsFinished: Result := FPages.Count;
  else
    Result := -1;
  end;
end;

function TJvTFPrinter.GetUnprintable: TJvTFMargins;
var
  LeftMarg,
    TopMarg,
    WidthPaper,
    HeightPaper,
    WidthPrintable,
    HeightPrintable: Integer;
begin
  {$IFDEF VCL}
  LeftMarg := Windows.GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  TopMarg := Windows.GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  WidthPaper := Windows.GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  HeightPaper := Windows.GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  LeftMarg := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  TopMarg := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  WidthPaper := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  HeightPaper := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  {$ENDIF VisualCLX}
  WidthPrintable := Printer.PageWidth;
  HeightPrintable := Printer.PageHeight;

  with Result do
  begin
    Left := LeftMarg;
    Top := TopMarg;
    Right := WidthPaper - WidthPrintable - LeftMarg;
    Bottom := HeightPaper - HeightPrintable - TopMarg;
  end;
end;

procedure TJvTFPrinter.InitializeMargins;
var
  I,
    Unprintable,
    NewMargin: Integer;
  Horz: Boolean;
begin
  for I := 1 to 4 do
  begin
    SetMarginOffset(I, 0);

    case I of
      1: Unprintable := GetUnprintable.Left;
      2: Unprintable := GetUnprintable.Top;
      3: Unprintable := GetUnprintable.Right;
    else
      Unprintable := GetUnprintable.Bottom;
    end;

    Horz := (I = 1) or (I = 3);
    NewMargin := ConvertMeasure(Unprintable, pmPixels, Measure, Horz);

    case I of
      1: PageLayout.FMargins.Left := NewMargin;
      2: PageLayout.FMargins.Top := NewMargin;
      3: PageLayout.FMargins.Right := NewMargin;
    else
      PageLayout.FMargins.Bottom := NewMargin;
    end;
  end;
end;

procedure TJvTFPrinter.MarginError;
begin
  if Assigned(FOnMarginError) then
    FOnMarginError(Self);
end;

procedure TJvTFPrinter.NewDoc;
begin
  FreeDoc;
  CreateDoc;
end;

procedure TJvTFPrinter.NewPage;
var
  aMetafile: TMetafile;
  aCanvas: TCanvas;
  HeaderRect,
    FooterRect: TRect;
begin
  if Aborted then
    Exit;

  if DirectPrint then
  begin
    if PageCount > 0 then
      Printer.NewPage;
    aCanvas := Printer.Canvas;
    FPages.Add('');
  end
  else
  begin
    // Create a TMetafile for the page
    aMetafile := TMetafile.Create;
    FPages.AddObject('', aMetafile);
    // Create a TMetafileCanvas as a canvas for the page.
    // Store the canvas in FBodies so we can retrieve it later to draw
    // the header and footer.
    aCanvas := TMetafileCanvas.Create(aMetafile, Printer.Handle);
  end;
  FBodies.AddObject('', aCanvas);
  {$IFDEF VCL}
  aCanvas.Font.PixelsPerInch := Windows.GetDeviceCaps(Printer.Handle,
    LOGPIXELSX);

  Windows.SetViewPortOrgEx(aCanvas.Handle, BodyLeft, BodyTop, nil);
  DrawBody(aCanvas, Rect(BodyLeft, BodyTop, BodyWidth - BodyLeft,
    BodyHeight - BodyTop), FPages.Count);
  Windows.SetViewPortOrgEx(aCanvas.Handle, 0, 0, nil);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  aCanvas.Font.PixelsPerInch := GetDeviceCaps(Printer.Handle,
    LOGPIXELSX);

  SetViewPortOrgEx(aCanvas.Handle, BodyLeft, BodyTop, nil);
  DrawBody(aCanvas, Rect(BodyLeft, BodyTop, BodyWidth - BodyLeft,
    BodyHeight - BodyTop), FPages.Count);
  SetViewPortOrgEx(aCanvas.Handle, 0, 0, nil);
  {$ENDIF VisualCLX}
  if DirectPrint then
  begin
    GetHeaderFooterRects(HeaderRect, FooterRect);
    DrawHeader(aCanvas, HeaderRect, PageCount);
    DrawFooter(aCanvas, FooterRect, PageCount);
  end;
end;

procedure TJvTFPrinter.Print;
var
  I: Integer;
begin
  if Aborted or DirectPrint then
    Exit;

  if State <> spsFinished then
    raise EJvTFPrinterError.CreateRes(@RsEOnlyAFinishedDocumentCanBePrinted);
  if PageCount = 0 then
    raise EJvTFPrinterError.CreateRes(@RsEThereAreNoPagesToPrint);

  if Assigned(FOnPrintProgress) then
    FOnPrintProgress(Self, 0, PageCount);
  Application.ProcessMessages;

  Printer.Title := Title;
  Printer.BeginDoc;
  if not Printer.Aborted then
    Printer.Canvas.Draw(0, 0, Pages[0]);

  if Assigned(FOnPrintProgress) then
    FOnPrintProgress(Self, 1, PageCount);
  Application.ProcessMessages;

  I := 1;
  while (I < PageCount) and not Printer.Aborted do
  begin
    if not Printer.Aborted then
      Printer.NewPage;
    if not Printer.Aborted then
      Printer.Canvas.Draw(0, 0, Pages[I]);
    Inc(I);
    if Assigned(FOnPrintProgress) then
      FOnPrintProgress(Self, I, PageCount);
    Application.ProcessMessages;
  end;

  if not Printer.Aborted then
    Printer.EndDoc;
end;

function TJvTFPrinter.PrinterToScreen(Value: Integer;
  Horizontal: Boolean): Integer;
var
  ScreenPPI,
    PrinterPPI: Integer;
begin
  ScreenPPI := Screen.PixelsPerInch;
  {$IFDEF VCL}
  if Horizontal then
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  else
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Horizontal then
    PrinterPPI := GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  else
    PrinterPPI := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ENDIF VisualCLX}
  Result := Trunc(ScreenPPI / PrinterPPI * Value);
end;

procedure TJvTFPrinter.SaveDocToFiles(BaseFileName: TFileName);
var
  I: Integer;
begin
  if State <> spsFinished then
    raise EJvTFPrinterError.CreateRes(@RsEDocumentMustBeFinishedToSaveToFile);

  for I := 0 to PageCount - 1 do
    Pages[I].SaveToFile(BaseFileName + '_' + IntToStr(I + 1) + '.emf');
end;

function TJvTFPrinter.ScreenToPrinter(Value: Integer;
  Horizontal: Boolean): Integer;
var
  ScreenPPI,
    PrinterPPI: Integer;
begin
  ScreenPPI := Screen.PixelsPerInch;
  {$IFDEF VCL}
  if Horizontal then
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  else
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Horizontal then
    PrinterPPI := GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  else
    PrinterPPI := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ENDIF VisualCLX}
  Result := Trunc(PrinterPPI / ScreenPPI * Value);
end;

procedure TJvTFPrinter.SetDirectPrint(Value: Boolean);
begin
  SetPropertyCheck;
  FDirectPrint := Value;
end;

procedure TJvTFPrinter.SetMarginOffset(Index, Value: Integer);
begin
  // Allow negative value...
  // SetMargin will catch that case and throw exception
  case Index of
    1:
      FMarginOffsets.Left := Value;
    2:
      FMarginOffsets.Top := Value;
    3:
      FMarginOffsets.Right := Value;
  else
    FMarginOffsets.Bottom := Value;
  end;
end;

procedure TJvTFPrinter.SetMeasure(Value: TJvTFPrinterMeasure);
begin
  try
    FConvertingProps := True;
    if Value <> FMeasure then
    begin
      PageLayout.FHeaderHeight := ConvertMeasure(PageLayout.FHeaderHeight,
        FMeasure, Value, False);
      PageLayout.FFooterHeight := ConvertMeasure(PageLayout.FFooterHeight,
        FMeasure, Value, False);

      PageLayout.FMargins.Left := ConvertMeasure(PageLayout.FMargins.Left,
        FMeasure, Value, True);
      PageLayout.FMargins.Right := ConvertMeasure(PageLayout.FMargins.Right,
        FMeasure, Value, True);
      PageLayout.FMargins.Top := ConvertMeasure(PageLayout.FMargins.Top,
        FMeasure, Value, False);
      PageLayout.FMargins.Bottom := ConvertMeasure(PageLayout.FMargins.Bottom,
        FMeasure, Value, False);
      FMeasure := Value;
    end;
  finally
    FConvertingProps := False;
  end;
end;

procedure TJvTFPrinter.SetPageLayout(Value: TJvTFPrinterPageLayout);
begin
  FPageLayout.Assign(Value);
end;

procedure TJvTFPrinter.SetPropertyCheck;
begin
  if (State <> spsNoDoc) and not ConvertingProps then
    raise EJvTFPrinterError.CreateRes(@RsEThisPropertyCannotBeChangedIfA);
end;

procedure TJvTFPrinter.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

{ TJvTFPrinterPageLayout }

procedure TJvTFPrinterPageLayout.Assign(Source: TPersistent);
var
  SourceMeas,
    DestMeas: TJvTFPrinterMeasure;
  WorkVal: Integer;
  SourceLayout: TJvTFPrinterPageLayout;
begin
  if (Source is TJvTFPrinterPageLayout) and Assigned(Printer) and
    Assigned(TJvTFPrinterPageLayout(Source).Printer) then
  begin
    SourceLayout := TJvTFPrinterPageLayout(Source);
    SourceMeas := SourceLayout.Printer.Measure;
    DestMeas := Printer.Measure;

    WorkVal := SourceLayout.MarginLeft;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, True);
    SetMargin(1, WorkVal);

    WorkVal := SourceLayout.MarginTop;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, False);
    SetMargin(2, WorkVal);

    WorkVal := SourceLayout.MarginRight;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, True);
    SetMargin(3, WorkVal);

    WorkVal := SourceLayout.MarginBottom;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, False);
    SetMargin(4, WorkVal);

    WorkVal := SourceLayout.HeaderHeight;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, False);
    SetHeaderHeight(WorkVal);

    WorkVal := SourceLayout.FooterHeight;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, False);
    SetFooterHeight(WorkVal);
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFPrinterPageLayout.Change;
begin
  // do nothing, leave to descendants
end;

constructor TJvTFPrinterPageLayout.Create(aPrinter: TJvTFPrinter);
begin
  inherited Create;
  if not Assigned(aPrinter) then
    raise EJvTFPrinterError.CreateRes(@RsECouldNotCreateTJvTFPrinterPageLayou);

  FPrinter := aPrinter;
end;

function TJvTFPrinterPageLayout.GetMargin(Index: Integer): Integer;
begin
  case Index of
    1: Result := FMargins.Left;
    2: Result := FMargins.Top;
    3: Result := FMargins.Right;
  else
    Result := FMargins.Bottom;
  end;
end;

procedure TJvTFPrinterPageLayout.SetFooterHeight(Value: Integer);
var
  Check: Integer;
begin
  SetPropertyCheck;

  if Value < 0 then
    Value := 0;

  if Value <> FFooterHeight then
  begin
    Check := FFooterHeight;
    FFooterHeight := Value;
    if Printer.BodyHeight < 1 then
    begin
      FFooterHeight := Check;
      raise EJvTFPrinterError.CreateResFmt(@RsEInvalidFooterHeightd, [Value]);
    end
    else
      Change;
  end;
end;

procedure TJvTFPrinterPageLayout.SetHeaderHeight(Value: Integer);
var
  Check: Integer;
begin
  SetPropertyCheck;

  if Value < 0 then
    Value := 0;
  if Value <> FHeaderHeight then
  begin
    Check := FHeaderHeight;
    FHeaderHeight := Value;
    if Printer.BodyHeight < 1 then
    begin
      FHeaderHeight := Check;
      raise EJvTFPrinterError.CreateResFmt(@RsEInvalidHeaderHeightd, [Value]);
    end
    else
      Change;
  end;
end;

procedure TJvTFPrinterPageLayout.SetMargin(Index, Value: Integer);
var
  Unprintable,
    UserMarginPels,
    CurrMargin,
    NewMargin: Integer;
  Horz,
    Err: Boolean;
begin
  SetPropertyCheck;

  CurrMargin := GetMargin(Index);
  if Value <> CurrMargin then
  begin
    Horz := (Index = 1) or (Index = 3);
    case Index of
      1: Unprintable := Printer.GetUnprintable.Left;
      2: Unprintable := Printer.GetUnprintable.Top;
      3: Unprintable := Printer.GetUnprintable.Right;
    else
      Unprintable := Printer.GetUnprintable.Bottom;
    end;

    UserMarginPels := Printer.ConvertMeasure(Value, Printer.Measure,
      pmPixels, Horz);
    Printer.SetMarginOffset(Index, UserMarginPels - Unprintable);

    if Printer.GetMarginOffset(Index) >= 0 then
    begin
      Err := False;
      NewMargin := Value;
    end
    else
    begin
      Err := True;
      Printer.SetMarginOffset(Index, 0);
      NewMargin := Printer.ConvertMeasure(Unprintable, pmPixels,
        Printer.Measure, Horz);
    end;

    if not Err then
      case Index of
        1: FMargins.Left := NewMargin;
        2: FMargins.Top := NewMargin;
        3: FMargins.Right := NewMargin;
      else
        FMargins.Bottom := NewMargin;
      end
    else
      //SetMargin(Index, NewMargin);
      case Index of
        1: MarginLeft := NewMargin;
        2: MarginTop := NewMargin;
        3: MarginRight := NewMargin;
      else
        MarginBottom := NewMargin;
      end;

    if Err and Assigned(Printer) then
    begin
      Printer.UpdateDesigner;
      Printer.MarginError;
    end;

    Change;
  end;
end;

procedure TJvTFPrinterPageLayout.SetPropertyCheck;
begin
  Printer.SetPropertyCheck;
end;

{ TJvTFUniversalPrinter }

procedure TJvTFUniversalPrinter.CreateDoc;
begin
  inherited;
end;

procedure TJvTFUniversalPrinter.FinishDoc;
begin
  inherited;
end;

procedure TJvTFUniversalPrinter.NewDoc;
begin
  inherited;
end;

procedure TJvTFUniversalPrinter.NewPage;
begin
  inherited;
end;

{ TJvTFHintProps }

procedure TJvTFHintProps.Assign(Source: TPersistent);
begin
  if Source is TJvTFHint then
  begin
    FHintColor := TJvTFHintProps(Source).HintColor;
    FHintHidePause := TJvTFHintProps(Source).HintHidePause;
    FHintPause := TJvTFHintProps(Source).HintPause;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFHintProps.Change;
begin
  // do nothing
end;

constructor TJvTFHintProps.Create(AOwner: TJvTFControl);
begin
  inherited Create;
  FControl := AOwner;

  FHintColor := clDefault;
  FHintHidePause := -1;
  FHintPause := -1;
end;

procedure TJvTFHintProps.SetHintColor(Value: TColor);
begin
  if Value <> FHintColor then
  begin
    FHintColor := Value;
    Change;
  end;
end;

procedure TJvTFHintProps.SetHintHidePause(Value: Integer);
begin
  if Value < -1 then
    Value := -1;

  if Value <> FHintHidePause then
  begin
    FHintHidePause := Value;
    Change;
  end;
end;

procedure TJvTFHintProps.SetHintPause(Value: Integer);
begin
  if Value < -1 then
    Value := -1;

  if Value <> HintPause then
  begin
    FHintPause := Value;
    Change;
  end;
end;

{ TJvTFDWNames }

procedure TJvTFDWNames.Assign(Source: TPersistent);
begin
  if Source is TJvTFDWNames then
  begin
    FDWN_Sunday := TJvTFDWNames(Source).DWN_Sunday;
    FDWN_Monday := TJvTFDWNames(Source).DWN_Monday;
    FDWN_Tuesday := TJvTFDWNames(Source).DWN_Tuesday;
    FDWN_Wednesday := TJvTFDWNames(Source).DWN_Wednesday;
    FDWN_Thursday := TJvTFDWNames(Source).DWN_Thursday;
    FDWN_Friday := TJvTFDWNames(Source).DWN_Friday;
    FDWN_Saturday := TJvTFDWNames(Source).DWN_Saturday;
    FSource := TJvTFDWNames(Source).Source;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFDWNames.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TJvTFDWNames.Create;
begin
  inherited;
  FSource := dwnsSysShort;
  FDWN_Sunday := 'S';
  FDWN_Monday := 'M';
  FDWN_Tuesday := 'T';
  FDWN_Wednesday := 'W';
  FDWN_Thursday := 'T';
  FDWN_Friday := 'F';
  FDWN_Saturday := 'S';
end;

function TJvTFDWNames.GetDWN(Index: Integer): string;
begin
  case Index of
    1: Result := FDWN_Sunday;
    2: Result := FDWN_Monday;
    3: Result := FDWN_Tuesday;
    4: Result := FDWN_Wednesday;
    5: Result := FDWN_Thursday;
    6: Result := FDWN_Friday;
    7: Result := FDWN_Saturday;
  else
    Result := '';
  end;
end;

function TJvTFDWNames.GetDWName(DWIndex: Integer): string;
begin
  case Source of
    dwnsSysLong: Result := SysUtils.LongDayNames[DWIndex];
    dwnsSysShort: Result := SysUtils.ShortDayNames[DWIndex];
  else // dwnsCustom
    Result := GetDWN(DWIndex);
  end;
end;

procedure TJvTFDWNames.SetDWN(Index: Integer; const Value: string);
begin
  case Index of
    1: FDWN_Sunday := Value;
    2: FDWN_Monday := Value;
    3: FDWN_Tuesday := Value;
    4: FDWN_Wednesday := Value;
    5: FDWN_Thursday := Value;
    6: FDWN_Friday := Value;
    7: FDWN_Saturday := Value;
  end;

  if Source = dwnsCustom then
    Change;
end;

procedure TJvTFDWNames.SetSource(Value: TJvTFDWNameSource);
begin
  if Value <> FSource then
  begin
    FSource := Value;
    Change;
  end;
end;

{ TJvTFDateList }

function TJvTFDateList.Add(ADate: TDate): Integer;
begin
  Result := FList.Add(IntToStr(Trunc(ADate)));
  Change;
end;

procedure TJvTFDateList.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvTFDateList.Clear;
begin
  FList.Clear;
  Change;
end;

function TJvTFDateList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TJvTFDateList.Create;
begin
  inherited Create;
  FList := TStringlist.Create;
  FList.Sorted := True;
  FList.Duplicates := dupIgnore;
end;

procedure TJvTFDateList.Delete(Index: Integer);
begin
  FList.Delete(Index);
  Change;
end;

destructor TJvTFDateList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TJvTFDateList.GetDate(Index: Integer): TDate;
begin
  Result := StrToInt(FList[Index]);
end;

function TJvTFDateList.IndexOf(ADate: TDate): Integer;
begin
  Result := FList.IndexOf(IntToStr(Trunc(ADate)));
end;

{ TJvTFNavigator }

//function TJvTFNavigator.ControlCount: Integer;
//begin
//  Result := FControls.Count;
//end;
//
//constructor TJvTFNavigator.Create(AOwner: TComponent);
//begin
//  inherited;
//  FControls := TStringList.Create;
//end;
//
//destructor TJvTFNavigator.Destroy;
//begin
//  While ControlCount > 0 do
//    UnregisterControl(Controls[0]);
//  FControls.Free;
//
//  inherited;
//end;
//
//function TJvTFNavigator.GetControl(Index: Integer): TJvTFControl;
//begin
//  Result := TJvTFControl(FControls.Objects[Index]);
//end;
//
//procedure TJvTFNavigator.Navigate(aControl: TJvTFControl;
//  SchedNames: TStringList; Dates: TJvTFDateList);
//var
//  I : Integer;
//  Control : TJvTFControl;
//begin
//  If Navigating or not Assigned(aControl) Then
//    Exit;
//
//  If Assigned(FBeforeNavigate) Then
//    FBeforeNavigate(Self, aControl, SchedNames, Dates);
//
//  FNavigating := True;
//  Try
//    For I := 0 to ControlCount - 1 do
//      Begin
//        Control := Controls[I];
//        If Control <> aControl Then
//          //Controls[I].Notify(aControl, sncNavigate);
//          Control.Navigate(aControl, SchedNames, Dates);
//      End;
//  Finally
//    FNavigating := False;
//  End;
//
//  If Assigned(FAfterNavigate) Then
//    FAfterNavigate(Self, aControl, SchedNames, Dates);
//end;
//
//procedure TJvTFNavigator.RegisterControl(aControl: TJvTFControl);
//var
//  I : Integer;
//begin
//  I := FControls.IndexOfObject(aControl);
//  If I = -1 Then
//    FControls.AddObject('', aControl);
//end;
//
//procedure TJvTFNavigator.UnregisterControl(aControl: TJvTFControl);
//var
//  I : Integer;
//begin
//  I := FControls.IndexOfObject(aControl);
//  If I > -1 Then
//    FControls.Delete(I);
//end;

end.

