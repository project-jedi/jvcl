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

Last Modified: 2003-08-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTFManager;

interface

uses Windows, Classes, Controls, SysUtils, Messages, Graphics, ImgList,
  ExtCtrls, Printers, JvTFUtils
{$IFDEF USEJVCL}, JvComponent{$ENDIF};

const
  CN_REQUESTREFRESH = $BD01;

{$HPPEMIT '#define TDate Controls::TDate'}
{$HPPEMIT '#define TTime Controls::TTime'}
type
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
    function GetDate(Index: integer): TDate;
    procedure Change; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(ADate: TDate): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    function Count: integer;
    function IndexOf(ADate: TDate): integer;
    property Dates[Index: integer]: TDate read GetDate; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvTFNavEvent = procedure(Sender: TObject; aControl: TJvTFControl;
    SchedNames: TStringlist; Dates: TJvTFDateList) of object;
  TJvTFControlEvent = procedure(Sender: TObject; aControl: TJvTFControl) of object;
  TJvTFSchedEvent = procedure(Sender: TObject; Schedule: TJvTFSched) of object;
  TJvTFApptEvent = procedure(Sender: TObject; Appt: TJvTFAppt) of object;
  TJvTFVarApptEvent = procedure(Sender: TObject; var Appt: TJvTFAppt) of object;
  TJvTFFlushEvent = procedure(Sender, FlushObj: TObject; var FlushIt: boolean) of object;

  // implicit post fix
  TJvTFPostApptQueryEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    var CanPost: boolean) of object;

  TJvTFCustomImageMap = class(TObject)
  private
    FMap: TStringlist;
    function GetImage(MapIndex: integer): integer;
    procedure SetImage(MapIndex: integer; Value: integer);
    function GetImageName(MapIndex: integer): string;
  protected
    FAppt: TJvTFAppt;
    procedure Change;
  public
    constructor Create(anAppt: TJvTFAppt);
    destructor Destroy; override;
    property Images[MapIndex: integer]: integer read GetImage write SetImage; default;
    property ImageNames[MapIndex: integer]: string read GetImageName;
    function Count: integer;
    procedure Add(ImageName: string; ImageIndex: integer);
    procedure Delete(MapIndex: integer);
    procedure Move(SrcMapIndex, DestMapIndex: integer);
    function FindMapIndex(ImageName: string): integer;
    function FindImageIndex(ImageName: string): integer;
    procedure Clear;
    procedure Assign(Source: TJvTFCustomImageMap); dynamic;
  end;

  TJvTFStatePic = (spAlarmEnabled, spAlarmDisabled, spShared, spRecurring,
    spModified);

  TJvTFStateImageMap = class(TPersistent)
  private
    FPics: array[Low(TJvTFStatePic)..High(TJvTFStatePic)] of integer;

    procedure SetImage(StatePicID: TJvTFStatePic; Value: integer);
    function GetImage(StatePicID: TJvTFStatePic): integer;
    function GetAlarmDisabled: integer;
    function GetAlarmEnabled: integer;
    function GetModified: integer;
    function GetRecurring: integer;
    function GetShared: integer;
    procedure SetAlarmDisabled(const Value: integer);
    procedure SetAlarmEnabled(const Value: integer);
    procedure SetModified(const Value: integer);
    procedure SetRecurring(const Value: integer);
    procedure SetShared(const Value: integer);
  protected
    FScheduleManager: TJvTFScheduleManager;
    FUpdating: boolean;
    procedure Change;
  public
    constructor Create(Serv: TJvTFScheduleManager);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    property Pics[Index: TJvTFStatePic]: integer read GetImage write SetImage;
  published
    property AlarmEnabled: integer {index spAlarmEnabled}
          read GetAlarmEnabled write SetAlarmEnabled;
    property AlarmDisabled: integer {index spAlarmDisabled}
          read GetAlarmDisabled write SetAlarmDisabled;
    property Shared: integer {index spShared}
          read GetShared write SetShared;
    property Recurring: integer {index spRecurring}
          read GetRecurring write SetRecurring;
          //read GetImage write SetImage;
    property Modified: integer {index spModified}
          read GetModified write SetModified;
  end;

  TDynTimeRangeArray = array of TJvTFTimeRange;

  TDynApptArray = array of TJvTFAppt;

  TDynSchedArray = array of TJvTFSched;

  TJvTFAppt = class(TObject)
  private
    FStartDate: TDate;
    FEndDate: TDate;
    FStartTime: TTime;
    FEndTime: TTime;
    FDescription: string;
    FAlarmEnabled: boolean;
    FAlarmAdvance: integer;
    FImageMap: TJvTFCustomImageMap;
    FData: integer;
    FPersistent: boolean;
    FColor: TColor;
    FBarColor: TColor;
    FRefreshed: boolean;

    function GetDescription: string;
    procedure SetDescription(Value: string);
    procedure SetAlarmEnabled(Value: boolean);
    procedure SetAlarmAdvance(Value: integer);
    procedure SetColor(Value: TColor);
    procedure SetBarColor(Value: TColor);
    function GetStartDateTime: TDateTime;
    function GetEndDateTime: TDateTime;
    function GetStartDate: TDate;
    function GetEndDate: TDate;
    function GetStartTime: TTime;
    function GetEndTime: TTime;
    procedure SetRefreshed(Value: boolean);
  protected
    FID: string;
    FModified: boolean;
    FScheduleManager: TJvTFScheduleManager;
    FConnections: TStringlist;
    FSchedules: TStringlist;
    FDeleting: boolean;
    // implicit post fix
    FUpdating: boolean;

    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode);
    procedure NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure NotifySchedule(Sched: TJvTFSched; Sender: TObject;
      Code: TJvTFServNotifyCode);

    function GetConnection(Index: integer): TJvTFSched;
    function GetSchedule(Index: integer): string;
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
    constructor Create(Serv: TJvTFScheduleManager; ApptID: string); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TJvTFAppt); dynamic;
    procedure SetStartEnd(NewStartDate: TDate; NewStartTime: TTime;
      NewEndDate: TDate; NewEndTime: TTime);

    procedure SetModified;
    function Modified: boolean; dynamic;
    property ScheduleManager: TJvTFScheduleManager read FScheduleManager;

    function ConnectionCount: integer;
    property Connections[Index: integer]: TJvTFSched read GetConnection;

    function ScheduleCount: integer;
    property Schedules[Index: integer]: string read GetSchedule;
    procedure AddSchedule(SchedName: string);
    procedure RemoveSchedule(SchedName: string);
    procedure AssignSchedules(List: TStrings);
    procedure ClearSchedules;
    function IndexOfSchedule(SchedName: string): integer;
    function Shared: boolean;

    procedure Post;
    procedure Refresh;
    procedure Delete;

    // implicit post fix
    procedure BeginUpdate;
    procedure EndUpdate;
    property Updating: boolean read FUpdating;

    property ImageMap: TJvTFCustomImageMap read FImageMap write FImageMap;
    procedure RefreshControls;
    property Refreshed: boolean read FRefreshed write SetRefreshed;
  published
    property ID: string read FID;
    property StartDate: TDate read GetStartDate;
    property EndDate: TDate read GetEndDate;
    property StartTime: TTime read GetStartTime;
    property EndTime: TTime read GetEndTime;
    property StartDateTime: TDateTime read GetStartDateTime;
    property EndDateTime: TDateTime read GetEndDateTime;
    property Description: string read GetDescription write SetDescription;
    property AlarmEnabled: boolean read FAlarmEnabled write SetAlarmEnabled;
    property AlarmAdvance: integer read FAlarmAdvance write SetAlarmAdvance;
    property Data: integer read FData write FData;
    property Persistent: boolean read FPersistent write FPersistent;
    property Color: TColor read FColor write SetColor default clDefault;
    property BarColor: TColor read FBarColor write SetBarColor default clDefault;
  end;

  TJvTFSched = class(TObject)
  private
    FAppts: TStringlist;
    FConControls: TStringlist;
    FConComponents: TStringlist;
    FDestroying: boolean;
    FData: integer;
    FPersistent: boolean;
    FSchedDisplayName: string;
    procedure SetSchedDisplayName(Value: string);

    function GetAppt(Index: integer): TJvTFAppt;
  protected
    FSchedName: string;
    FSchedDate: TDate;
    FScheduleManager: TJvTFScheduleManager;
    FCached: boolean;
    FCachedTime: DWORD;
    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode);
    procedure NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
      Code: TJvTFServNotifyCode);
    procedure NotifyAppt(Appt: TJvTFAppt; Sender: TObject;
      Code: TJvTFServNotifyCode);
    function GetConControl(Index: integer): TJvTFControl;
    function GetConComponent(Index: integer): TJvTFComponent;
    procedure ConnectAppt(Appt: TJvTFAppt);
    procedure DisconnectAppt(Appt: TJvTFAppt);
    procedure ConnectionsOnChange(Sender: TObject);
    procedure CheckConnections;
    function GetFreeUsedTime(FreeTime: boolean): TDynTimeRangeArray; dynamic;
  public
    constructor Create(Serv: TJvTFScheduleManager; AName: string; ADate: TDate); virtual;
    destructor Destroy; override;

    function ApptCount: integer;
    function ApptByID(ID: string): TJvTFAppt;
    property Appts[Index: integer]: TJvTFAppt read GetAppt;

    function ConControlCount: integer;
    property ConControls[Index: integer]: TJvTFControl read GetConControl;

    function ConComponentCount: integer;
    property ConComponents[Index: integer]: TJvTFComponent read GetConComponent;

    procedure AddAppt(Appt: TJvTFAppt);
    procedure RemoveAppt(Appt: TJvTFAppt);

    //procedure RefreshAppts;
    procedure Refresh;
    procedure PostAppts;

    // Conflict and free time methods
    function GetFreeTime: TDynTimeRangeArray; dynamic;
    function GetUsedTime: TDynTimeRangeArray; dynamic;
    function TimeIsFree(TimeRange: TJvTFTimeRange): boolean; overload; dynamic;
    function TimeIsFree(RangeStart, RangeEnd: TTime): boolean; overload; dynamic;
    // The ApptHasConflicts(anAppt : TJvTFAppt) method declared here checks
    //  ONLY THIS SCHEDULE!!
    function ApptHasConflicts(anAppt: TJvTFAppt): boolean; dynamic;
    function EnumConflicts(TimeRange: TJvTFTimeRange): TDynApptArray;
      overload; dynamic;
    function EnumConflicts(RangeStart, RangeEnd: TTime): TDynApptArray;
      overload; dynamic;
    // The following EnumConflicts(anAppt : TJvTFAppt) checks
    //  ONLY THIS SCHEDULE!!
    function EnumConflicts(anAppt: TJvTFAppt): TDynApptArray;
      overload; dynamic;

    property Cached: boolean read FCached;
    property CachedTime: DWORD read FCachedTime;
    property Destroying: boolean read FDestroying;

    function GetFirstAppt: TJvTFAppt;
    function GetLastAppt: TJvTFAppt;
  published
    property SchedDisplayName: string read FSchedDisplayName
      write SetSchedDisplayName;
    property SchedName: string read FSchedName;
    property SchedDate: TDate read FSchedDate;
    property ScheduleManager: TJvTFScheduleManager read FScheduleManager;
    property Data: integer read FData write FData;
    property Persistent: boolean read FPersistent write FPersistent;
  end;

  TJvTFScheduleManagerCacheType = (ctNone, ctTimed, ctBuffer);
  TJvTFScheduleManagerCache = class(TPersistent)
  private
    FCacheType: TJvTFScheduleManagerCacheType;
    FTimedDelay: integer;
    FBufferCount: integer;
    FTimer: TTimer;
    procedure SetCacheType(Value: TJvTFScheduleManagerCacheType);
    procedure SetTimedDelay(Value: integer);
    procedure SetBufferCount(Value: integer);
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
    property TimedDelay: integer read FTimedDelay write SetTimedDelay
      default 30000;
    property BufferCount: integer read FBufferCount write SetBufferCount
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
    FAlwaysPost: boolean;
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

    FRefreshAutoReconcile: boolean;

    FStateImages: TCustomImageList;
    FCustomImages: TCustomImageList;
    FStateImageMap: TJvTFStateImageMap;
    FCache: TJvTFScheduleManagerCache;

    // implicit post fix
    FOnPostApptQuery: TJvTFPostApptQueryEvent;

    function GetAppt(Index: integer): TJvTFAppt;
    function GetSchedule(Index: integer): TJvTFSched;
    function GetConControl(Index: integer): TJvTFControl;
    function GetConComponent(Index: integer): TJvTFComponent;
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetCustomImages(Value: TCustomImageList);
    procedure SetCache(Value: TJvTFScheduleManagerCache);

    procedure SeTJvTFSchedLoadMode(Value: TJvTFSchedLoadMode);
    procedure SetRefreshAutoReconcile(Value: boolean);
  protected
    FLoadingAppts: boolean;
    FRefreshing: boolean;
    FImageChangeLink: TChangeLink;
    FFlushing: boolean;
    FDestroying: boolean;

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

    procedure RetrieveSchedule(SchedName: string; SchedDate: TDate;
      var Schedule: TJvTFSched; var LoadedNow: boolean);

    procedure NeedAppts(Schedule: TJvTFSched); virtual;
    procedure AddAppt(Appt: TJvTFAppt);
    procedure RemoveAppt(Appt: TJvTFAppt);
    procedure RemoveSchedule(Sched: TJvTFSched);

    //procedure RefreshAppt(Appt : TJvTFAppt);
    procedure DeleteAppt(Appt: TJvTFAppt);
    procedure PostAppt(Appt: TJvTFAppt);

    // implicit post fix
    function QueryPostAppt(Appt: TJvTFAppt): boolean;

    procedure AddToBatch(aSched: TJvTFSched);
    procedure LoadBatch(BatchName: string; BatchStartDate,
      BatchEndDate: TDate); virtual;

    procedure RequestRefresh(ApptCtrl: TJvTFControl;
      Schedule: TJvTFSched); overload; dynamic;
    procedure RequestRefresh(Comp: TJvTFComponent;
      Schedule: TJvTFSched); overload; dynamic;

    procedure ImageListChange(Sender: TObject);
    procedure FlushAppts;
    function FlushObject(FlushObj: TObject): boolean;

    procedure DoCreateApptEvent(anAppt: TJvTFAppt); dynamic;
    procedure DoCreateScheduleEvent(aSchedule: TJvTFSched); dynamic;
    procedure DoDestroyApptEvent(anAppt: TJvTFAppt); dynamic;
    procedure DoDestroyScheduleEvent(aSchedule: TJvTFSched); dynamic;

    procedure SetApptDescription(Appt: TJvTFAppt; var Value: string); virtual;
    procedure GetApptDescription(Appt: TJvTFAppt; var Value: string); virtual;
  public
    class function GetScheduleID(SchedName: string; SchedDate: TDate): string;
    class function GenerateApptID: string; virtual;

    function GetSchedClass: TJvTFSchedClass; dynamic;
    function GetApptClass: TJvTFApptClass; dynamic;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ApptCount: integer;
    property Appts[Index: integer]: TJvTFAppt read GetAppt;
    function FindAppt(ID: string): TJvTFAppt;

    function ScheduleCount: integer;
    property Schedules[Index: integer]: TJvTFSched read GetSchedule;
    function FindSchedule(SchedName: string; SchedDate: TDate): TJvTFSched;

    function ConControlCount: integer;
    property ConControls[Index: integer]: TJvTFControl read GetConControl;
    function ConComponentCount: integer;
    property ConComponents[Index: integer]: TJvTFComponent read GetConComponent;

    function RequestSchedule(ApptCtrl: TJvTFControl; SchedName: string;
      SchedDate: TDate): TJvTFSched; overload;
    function RequestSchedule(ApptCtrl: TJvTFControl; SchedName: string;
      SchedDate: TDate; var LoadedNow: boolean): TJvTFSched; overload;

    function RequestSchedule(Comp: TJvTFComponent; SchedName: string;
      SchedDate: TDate): TJvTFSched; overload;
    function RequestSchedule(Comp: TJvTFComponent; SchedName: string;
      SchedDate: TDate; var LoadedNow: boolean): TJvTFSched; overload;

    procedure ReleaseSchedule(ApptCtrl: TJvTFControl; SchedName: string;
      SchedDate: TDate); overload;
    procedure ReleaseSchedule(Comp: TJvTFComponent; SchedName: string;
      SchedDate: TDate); overload;

    procedure ProcessBatches;

    procedure RequestAppt(ID: string; var Appt: TJvTFAppt; var New: boolean);

    property LoadingAppts: boolean read FLoadingAppts;
    property Refreshing: boolean read FRefreshing;

    procedure dbPostAppt(Appt: TJvTFAppt);
    procedure dbDeleteAppt(Appt: TJvTFAppt);
    procedure dbRefreshAppt(Appt: TJvTFAppt);
    procedure dbRefreshSched(Sched: TJvTFSched);
    procedure dbRefreshAll;
    procedure dbRefreshOrphans;
    function dbNewAppt(ID: string): TJvTFAppt;

    procedure PostAppts;
    procedure RefreshAppts;
    procedure ReconcileRefresh(Scope: TObject);

    procedure RefreshConnections(Trigger: TObject); virtual;
    property Flushing: boolean read FFlushing;
    procedure Flush(All: boolean = false); virtual;

    function GetApptDisplayText(AComponent: TComponent;
      Appt: TJvTFAppt): string; virtual;
  published
    property AlwaysPost: boolean read FAlwaysPost write FAlwaysPost default false;
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
    property RefreshAutoReconcile: boolean read FRefreshAutoReconcile
      write SetRefreshAutoReconcile default false;
  end;

  TJvTFHintProps = class(TPersistent)
  private
    FHintColor: TColor;
    FHintHidePause: integer;
    FHintPause: integer;
    procedure SetHintColor(Value: TColor);
    procedure SetHintHidePause(Value: integer);
    procedure SetHintPause(Value: integer);
  protected
    FControl: TJvTFControl;
    procedure Change; virtual;
  public
    constructor Create(AOwner: TJvTFControl);
    procedure Assign(Source: TPersistent); override;
  published
    property HintColor: TColor read FHintColor write SetHintColor
      default clDefault;
    property HintHidePause: integer read FHintHidePause write SetHintHidePause
      default -1;
    property HintPause: integer read FHintPause write SetHintPause
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
    FPause: integer;
    FShortPause: integer;
    FOnShowHint: TJvTFShowHintEvent;
    FRefProps: TJvTFHintProps;
    procedure SetPause(Value: integer);
    procedure SetShortPause(Value: integer);
  protected
    FApptCtrl: TJvTFControl;
    FOldAppt: TJvTFAppt;
    FOldObj: TObject;
    FShortTimer: boolean;
    FHintRect: TRect;
    FHintText: string;
    FHintCell: TPoint;
    FHintType: TJvTFHintType;
    procedure TimerOnTimer(Sender: TObject); virtual;
    procedure PrepTimer(Short: boolean);
    procedure SetHintText(StartDate, EndDate: TDate; StartTime, EndTime: TTime;
      Desc: string; ShowDatesTimes, ShowDesc: boolean);
    procedure DoHint(Sustained: boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PropertyCheck; dynamic;
  public
    constructor Create(anApptCtrl: TJvTFControl); reintroduce;
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ApptHint(Appt: TJvTFAppt; X, Y: integer;
      ShowDatesTimes, ShowDesc, FormattedDesc: boolean); virtual;
    procedure StartEndHint(StartDate, EndDate: TDate; StartTime, EndTime: TTime;
      X, Y: integer; ShowDates: boolean);
    procedure CellHint(Row, Col: integer; HintText: string; CellRect: TRect);

    procedure MultiLineObjHint(Obj: TObject; X, Y: integer; Hints: TStrings);

    procedure ReleaseHandle; virtual;
    // See above note on Pause and ShortPause properties
    property Pause: integer read FPause write SetPause default 3000;
    property ShortPause: integer read FShortPause write SetShortPause default 1500;
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
  TJvTFComponent = class(TJVComponent)
{$ELSE}
  TJvTFComponent = class(TComponent)
{$ENDIF}
  private
    FScheduleManager: TJvTFScheduleManager;
    FSchedules: TStringlist;
    procedure SetManager(Value: TJvTFScheduleManager);
    function GetSchedule(Index: integer): TJvTFSched;
  protected
    FDateFormat: string;
    FTimeFormat: string;

    procedure UpdateDesigner;

    procedure SetDateFormat(Value: string); virtual;
    procedure SetTimeFormat(Value: string); virtual;
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

    function ScheduleCount: integer;
    property Schedules[Index: integer]: TJvTFSched read GetSchedule;
    function FindSchedule(SchedName: string; SchedDate: TDate): TJvTFSched;
    function RetrieveSchedule(SchedName: string; SchedDate: TDate): TJvTFSched;
    procedure ReleaseSchedule(SchedName: string; SchedDate: TDate); virtual;
    procedure ReleaseSchedules;
    procedure ProcessBatches;
  published
    property ScheduleManager: TJvTFScheduleManager read FScheduleManager write SetManager;
  end;

{$IFDEF USEJVCL}
  TJvTFControl = class(TJvCustomControl)
{$ELSE}
  TJvTFControl = class(TCustomControl)
{$ENDIF}
  private
    FScheduleManager: TJvTFScheduleManager;
    FSchedules: TStringlist;
//    FNavigator : TJvTFNavigator;
//    FOnNavigate : TJvTFNavEvent;
    procedure SetManager(Value: TJvTFScheduleManager);
    function GetSchedule(Index: integer): TJvTFSched;
//    procedure SetNavigator(Value: TJvTFNavigator);
  protected
    FDateFormat: string;
    FTimeFormat: string;
    FDragInfo: TJvTFDragInfo;
    FShift: TShiftState;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDateFormat(Value: string); virtual;
    procedure SetTimeFormat(Value: string); virtual;
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
    procedure DoEndDrag(Target: TObject; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringlist;
      Dates: TJvTFDateList); virtual;
//    property Navigator : TJvTFNavigator read FNavigator write SetNavigator;
//    property OnNavigate : TJvTFNavEvent read FOnNavigate write FOnNavigate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ScheduleCount: integer;
    property Schedules[Index: integer]: TJvTFSched read GetSchedule;
    function FindSchedule(SchedName: string; SchedDate: TDate): TJvTFSched;
    function RetrieveSchedule(SchedName: string; SchedDate: TDate): TJvTFSched;
    procedure ReleaseSchedule(SchedName: string; SchedDate: TDate); virtual;
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
    ARect: TRect; PageNum: integer) of object;

  TJvTFProgressEvent = procedure(Sender: TObject; Current, Total: integer)
    of object;

  TJvTFPrinterPageLayout = class(TPersistent)
  private
    FFooterHeight: integer;
    FHeaderHeight: integer;
    FMargins: TJvTFMargins;
    FPrinter: TJvTFPrinter;
    procedure SetFooterHeight(Value: integer);
    procedure SetHeaderHeight(Value: integer);
    function GetMargin(Index: integer): integer;
    procedure SetMargin(Index: integer; Value: integer);
  protected
    procedure Change; virtual;
    property Printer: TJvTFPrinter read FPrinter;
    procedure SetPropertyCheck;
  public
    constructor Create(aPrinter: TJvTFPrinter); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property FooterHeight: integer read FFooterHeight write SetFooterHeight;
    property HeaderHeight: integer read FHeaderHeight write SetHeaderHeight;
    property MarginLeft: integer index 1 read GetMargin write SetMargin;
    property MarginTop: integer index 2 read GetMargin write SetMargin;
    property MarginRight: integer index 3 read GetMargin write SetMargin;
    property MarginBottom: integer index 4 read GetMargin write SetMargin;
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
    FDirectPrint: boolean;

    function GetPage(Index: integer): TMetafile;
    function GetBodyHeight: integer; // always in pixels
    function GetBodyWidth: integer; // always in pixels
    function GetBodyLeft: integer; // always in pixels
    function GetBodyTop: integer; // always in pixels
    function GetDocDateTime: TDateTime;
    procedure SetPageLayout(Value: TJvTFPrinterPageLayout);
    procedure SetDirectPrint(Value: boolean);
  protected
    FPageLayout: TJvTFPrinterPageLayout;
    FState: TJvTFPrinterState;
    FDocDateTime: TDateTime;
    FPageCount: integer; // NOTE: SEE GetPageCount !!
    FConvertingProps: boolean;
    FAborted: boolean;

    procedure SetMarginOffset(Index: integer; Value: integer); // always in pixels
    function GetMarginOffset(Index: integer): integer; // always in pixels
    function GetUnprintable: TJvTFMargins; // always in pixels
    procedure MarginError; dynamic;
    procedure InitializeMargins;
    property BodyHeight: integer read GetBodyHeight; // always in pixels
    property BodyWidth: integer read GetBodyWidth; // always in pixels
    property BodyLeft: integer read GetBodyLeft; // always in pixels
    property BodyTop: integer read GetBodyTop; // always in pixels
    procedure DrawBody(aCanvas: TCanvas; ARect: TRect; PageNum: integer); virtual;
    procedure DrawHeader(aCanvas: TCanvas; ARect: TRect; PageNum: integer); virtual;
    procedure DrawFooter(aCanvas: TCanvas; ARect: TRect; PageNum: integer); virtual;
    procedure SetTitle(Value: string); virtual;
    function GetPageCount: integer;
    procedure SetMeasure(Value: TJvTFPrinterMeasure); virtual;
    procedure CreateLayout; virtual;
    procedure SetPropertyCheck; dynamic;

    procedure GetHeaderFooterRects(var HeaderRect, FooterRect: TRect);

    // document management methods
    procedure CreateDoc; dynamic;
    procedure NewPage; dynamic;
    procedure FinishDoc; dynamic;
    procedure NewDoc; dynamic;
    property DirectPrint: boolean read FDirectPrint write SetDirectPrint
      default false;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PageCount: integer read GetPageCount;
    property Pages[Index: integer]: TMetafile read GetPage;

    function ConvertMeasure(Value: integer; FromMeasure,
      ToMeasure: TJvTFPrinterMeasure; Horizontal: boolean): integer;
    function ScreenToPrinter(Value: integer; Horizontal: boolean): integer;
    function PrinterToScreen(Value: integer; Horizontal: boolean): integer;

    property State: TJvTFPrinterState read FState;
    procedure FreeDoc; dynamic;
    procedure Print; dynamic;
    procedure AbortPrint;
    property DocDateTime: TDateTime read GetDocDateTime;
    property ConvertingProps: boolean read FConvertingProps;
    procedure SaveDocToFiles(BaseFileName: TFileName);
    property Aborted: boolean read FAborted;
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

    procedure SetDWN(Index: integer; Value: string);
    function GetDWN(Index: integer): string;
    procedure SetSource(Value: TJvTFDWNameSource);
  protected
    procedure Change; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function GetDWName(DWIndex: integer): string;
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

{$HPPEMIT '#undef TDate'}
{$HPPEMIT '#undef TTime'}


resourcestring
  sCouldNotCreateCustomImageMap = 'Could not create CustomImageMap.  ' +
      'Appointment not assigned';
  sCouldNotCreateAppointmentObject = 'Could not create Appointment object.  ' +
      'ScheduleManager not assigned';
  sScheduleManagerNotificationFailedSc = 'ScheduleManager notification failed.  ScheduleManager not assigned';
  sScheduleNotificationFailed = 'Schedule notification failed.  ' +
      'Schedule not assigned';
  sInvalidStartAndEndTimes = 'Invalid start and end times';
  sInvalidStartAndEndDates = 'Invalid start and end dates';
  sAppointmentNotificationFailed = 'Appointment notification failed.  ' +
      'Appointment not assigned';
  sCouldNotCreateNewAppointment = 'Could not create new appointment. ' +
      'Appointment with given ID already exists';
  sInvalidTriggerForRefreshControls = 'Invalid Trigger for RefreshControls';
  sInvalidScopeInReconcileRefresh = 'Invalid Scope in ReconcileRefresh';
  sCouldNotRetrieveSchedule = 'Could not retrieve schedule.  ' +
        'ScheduleManager not assigned';
  sCouldNotReleaseSchedule = 'Could not release schedule.  ' +
        'ScheduleManager not assigned';
  sCouldNotCreateADocumentBecauseA = 'Could not create a document because a ' +
      'document already exists';
  sCouldNotFinishDocumentBecauseNo = 'Could not finish document because no ' +
      'document has been created';
  sDocumentDoesNotExist = 'Document does not exist';
  sDocumentPagesCannotBeAccessedIf = 'Document pages cannot be accessed if ' +
      'printing directly to the printer';
  sDocumentPagesAreInaccessibleUntil = 'Document pages are inaccessible until ' +
      'the document has been finished';
  sCouldNotRetrievePageCount = 'Could not retrieve page count ' +
        'because document does not exist';
  sOnlyAFinishedDocumentCanBePrinted = 'Only a finished document can be printed';
  sThereAreNoPagesToPrint = 'There are no pages to print';
  sDocumentMustBeFinishedToSaveToFile = 'Document must be Finished to save to file';
  sThisPropertyCannotBeChangedIfA = 'This property cannot be changed if a ' +
      'document exists';
  sCouldNotCreateTJvTFPrinterPageLayou = 'Could not create TJvTFPrinterPageLayout ' +
      'because aPrinter must be assigned';
  sInvalidFooterHeightd = 'Invalid Footer Height (%d)';
  sInvalidHeaderHeightd = 'Invalid Header Height (%d)';

implementation

uses
  Dialogs, Forms,
  JvConsts;

{ Common }

function AdjustEndTime(ATime: TTime): TTime;
begin
  Result := Frac(Frac(ATime) - Frac(EncodeTime(0, 0, 1, 0)));
end;

function CenterRect(Rect1, Rect2: TRect): TRect;
var
  Rect1Width,
    Rect1Height,
    Rect2Width,
    Rect2Height: integer;
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

function MoveRect(ARect: TRect; NewLeft, NewTop: integer): TRect;
var
  XOffset,
    YOffset: integer;
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

function StripCRLF(S: string): string;
var
  I: integer;
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
    raise EJvTFScheduleManagerError.Create(sCouldNotCreateCustomImageMap);

  inherited Create;
  FAppt := anAppt;
  FMap := TStringlist.Create;
end;

destructor TJvTFCustomImageMap.Destroy;
begin
  FMap.Free;
  inherited;
end;

function TJvTFCustomImageMap.GetImage(MapIndex: integer): integer;
begin
  Result := integer(FMap.Objects[MapIndex]);
end;

procedure TJvTFCustomImageMap.SetImage(MapIndex, Value: integer);
begin
  FMap.Objects[MapIndex] := TObject(Value);
end;

function TJvTFCustomImageMap.GetImageName(MapIndex: integer): string;
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

function TJvTFCustomImageMap.Count: integer;
begin
  Result := FMap.Count;
end;

procedure TJvTFCustomImageMap.Add(ImageName: string; ImageIndex: integer);
begin
  if FMap.IndexOf(ImageName) = -1 then
  begin
    FMap.AddObject(ImageName, TObject(ImageIndex));
    Change;
  end;
end;

procedure TJvTFCustomImageMap.Delete(MapIndex: integer);
begin
  FMap.Delete(MapIndex);
  Change;
end;

procedure TJvTFCustomImageMap.Move(SrcMapIndex, DestMapIndex: integer);
begin
  FMap.Move(SrcMapIndex, DestMapIndex);
end;

function TJvTFCustomImageMap.FindMapIndex(ImageName: string): integer;
begin
  Result := FMap.IndexOf(ImageName);
end;

function TJvTFCustomImageMap.FindImageIndex(ImageName: string): integer;
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

procedure TJvTFCustomImageMap.Assign(Source: TJvTFCustomImageMap);
var
  I: integer;
begin
  while FMap.Count > 0 do
    FMap.Delete(0);

  for I := 0 to Source.Count - 1 do
    Add(Source.ImageNames[I], Source.Images[I]);

  Change;
end;

{ TJvTFStateImageMap }

constructor TJvTFStateImageMap.Create(Serv: TJvTFScheduleManager);
var
  I: TJvTFStatePic;
begin
  inherited Create;

  for I := Low(TJvTFStatePic) to High(TJvTFStatePic) do
    FPics[I] := -1;

  FUpdating := false;
end;

procedure TJvTFStateImageMap.SetImage(StatePicID: TJvTFStatePic; Value: integer);
begin
  if Value < -1 then
    Value := -1;
  if FPics[StatePicID] <> Value then
  begin
    FPics[StatePicID] := Value;
    Change;
  end;
end;

function TJvTFStateImageMap.GetImage(StatePicID: TJvTFStatePic): integer;
begin
  Result := FPics[StatePicID];
end;

function TJvTFStateImageMap.GetAlarmDisabled: integer;
begin
  Result := GetImage(spAlarmDisabled);
end;

function TJvTFStateImageMap.GetAlarmEnabled: integer;
begin
  Result := GetImage(spAlarmEnabled);
end;

function TJvTFStateImageMap.GetModified: integer;
begin
  Result := GetImage(spModified);
end;

function TJvTFStateImageMap.GetRecurring: integer;
begin
  Result := GetImage(spRecurring);
end;

function TJvTFStateImageMap.GetShared: integer;
begin
  Result := GetImage(spShared);
end;

procedure TJvTFStateImageMap.SetAlarmDisabled(const Value: integer);
begin
  SetImage(spAlarmDisabled, Value);
end;

procedure TJvTFStateImageMap.SetAlarmEnabled(const Value: integer);
begin
  SetImage(spAlarmEnabled, Value);
end;

procedure TJvTFStateImageMap.SetModified(const Value: integer);
begin
  SetImage(spModified, Value);
end;

procedure TJvTFStateImageMap.SetRecurring(const Value: integer);
begin
  SetImage(spRecurring, Value);
end;

procedure TJvTFStateImageMap.SetShared(const Value: integer);
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
  FUpdating := true;
end;

procedure TJvTFStateImageMap.EndUpdate;
begin
  if FUpdating then
  begin
    FUpdating := false;
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

constructor TJvTFAppt.Create(Serv: TJvTFScheduleManager; ApptID: string);
begin
  if not Assigned(Serv) then
    raise EJvTFScheduleManagerError.Create(sCouldNotCreateAppointmentObject);

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

  FModified := false;
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
      FModified := true;
      Change;
    end;
  end;
end;

procedure TJvTFAppt.SetAlarmEnabled(Value: boolean);
begin
  if Value <> FAlarmEnabled then
  begin
    FAlarmEnabled := Value;
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := true;
      Change;
    end;
  end;
end;

procedure TJvTFAppt.SetAlarmAdvance(Value: integer);
begin
  if Value < 0 then
    Value := 0;

  if Value <> FAlarmAdvance then
  begin
    FAlarmAdvance := Value;
    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := true;
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
      FModified := true;
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
      FModified := true;
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
    sncRefresh: FModified := false;
  end;
end;

procedure TJvTFAppt.NotifyManager(Serv: TJvTFScheduleManager; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Serv) then
    Serv.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.Create(sScheduleManagerNotificationFailedSc);
end;

procedure TJvTFAppt.NotifySchedule(Sched: TJvTFSched; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Sched) then
    Sched.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.Create(sScheduleNotificationFailed);
end;

function TJvTFAppt.GetConnection(Index: integer): TJvTFSched;
begin
  Result := TJvTFSched(FConnections.Objects[Index]);
end;

function TJvTFAppt.GetSchedule(Index: integer): string;
begin
  Result := FSchedules[Index];
end;

procedure TJvTFAppt.CheckConnections;
var
  Schedule: TJvTFSched;
  I: integer;
  ADate: TDate;
  Temp: TStringlist;
begin
  // Schedules --> Connections
  for I := 0 to ScheduleCount - 1 do
  begin
    ADate := StartDate;
    while trunc(ADate) <= trunc(EndDate) do
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
        ((trunc(Schedule.SchedDate) < trunc(StartDate)) or
        (trunc(Schedule.SchedDate) > trunc(EndDate))) then
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
  I: integer;
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
  I: integer;
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

procedure TJvTFAppt.Assign(Source: TJvTFAppt);
var
  I: integer;
begin
  for I := 0 to Source.ScheduleCount - 1 do
    AddSchedule(Source.Schedules[I]);
  ImageMap.Assign(Source.ImageMap);
  SetStartEnd(Source.StartDate, Source.StartTime, Source.EndDate, Source.EndTime);
  Description := Source.Description;
  AlarmEnabled := Source.AlarmEnabled;
  AlarmAdvance := Source.AlarmAdvance;
  Data := Source.Data;
end;

procedure TJvTFAppt.SetStartEnd(NewStartDate: TDate; NewStartTime: TTime;
  NewEndDate: TDate; NewEndTime: TTime);
begin
  // The following avoids time overflow into next day when it is not
  //  intended.  (Add appt to last row of days would cause invalid
  //  start/end exception.)
  if Frac(NewEndTime) <= EncodeTime(0, 0, 0, 999) then
    NewEndTime := EncodeTime(23, 59, 59, 0);

  if trunc(NewStartDate) <= trunc(NewEndDate) then
  begin
    if trunc(NewStartDate) = trunc(NewEndDate) then
      if Frac(NewStartTime) >= Frac(NewEndTime) then
        raise EJvTFScheduleManagerError.Create(sInvalidStartAndEndTimes);

    FStartDate := NewStartDate;
    FEndDate := NewEndDate;
    FStartTime := NewStartTime;
    FEndTime := NewEndTime;

    CheckConnections;

    if not ScheduleManager.LoadingAppts and not ScheduleManager.Refreshing then
    begin
      FModified := true;
      Change;
    end
  end
  else
    raise EJvTFScheduleManagerError.Create(sInvalidStartAndEndDates);
end;

procedure TJvTFAppt.SetModified;
begin
  FModified := true;
  // implicit post fix
  Change;
end;

function TJvTFAppt.Modified: boolean;
begin
  Result := FModified;
end;

function TJvTFAppt.ConnectionCount: integer;
begin
  Result := FConnections.Count;
end;

function TJvTFAppt.ScheduleCount: integer;
begin
  Result := FSchedules.Count;
end;

procedure TJvTFAppt.AddSchedule(SchedName: string);
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
      FModified := true;
          // implicit post fix
      Change;
    end;
  end;

  // Check for needed connections
  //  (Only connects to currently loaded schedules.  Will not load a schedule.)
  ADate := StartDate;
  while trunc(ADate) <= trunc(EndDate) do
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

procedure TJvTFAppt.RemoveSchedule(SchedName: string);
var
  I: integer;
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
      FModified := true;
          // implicit post fix
      Change;
    end;
  end;

  // Check for invalid connections and disconnect
  ADate := StartDate;
  while trunc(ADate) <= trunc(EndDate) do
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
    FModified := true;
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
    FModified := true;
      // implicit post fix
    Change;
  end;

  CheckConnections;
end;

function TJvTFAppt.IndexOfSchedule(SchedName: string): integer;
begin
  Result := FSchedules.IndexOf(SchedName);
end;

function TJvTFAppt.Shared: boolean;
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
  Result := trunc(EndDate) + Frac(EndTime);
end;

function TJvTFAppt.GetStartDateTime: TDateTime;
begin
  Result := trunc(StartDate) + Frac(StartTime);
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
  FDeleting := true;
  try
    InternalClearSchedules;
  finally
    FDeleting := false;
  end;
end;

procedure TJvTFAppt.PostApptNotification;
begin
  FModified := false;
  FUpdating := false;
end;

procedure TJvTFAppt.BeginUpdate;
begin
  FUpdating := true;
end;

procedure TJvTFAppt.EndUpdate;
begin
  if FUpdating then
  begin
    FUpdating := false;
    Change;
  end;
end;

procedure TJvTFAppt.SetRefreshed(Value: boolean);
begin
  FRefreshed := Value;
end;

procedure TJvTFAppt.RefreshNotification;
begin
  FModified := false;
  Refreshed := false;
end;

{ TJvTFSched }

constructor TJvTFSched.Create(Serv: TJvTFScheduleManager; AName: string;
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
  FDestroying := true;

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

function TJvTFSched.GetAppt(Index: integer): TJvTFAppt;
begin
  Result := TJvTFAppt(FAppts.Objects[Index]);
end;

procedure TJvTFSched.Notify(Sender: TObject; Code: TJvTFServNotifyCode);
var
  I: integer;
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
    raise EJvTFScheduleManagerError.Create(sScheduleManagerNotificationFailedSc);
end;

procedure TJvTFSched.NotifyAppt(Appt: TJvTFAppt; Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  if Assigned(Appt) then
    Appt.Notify(Sender, Code)
  else
    raise EJvTFScheduleManagerError.Create(sAppointmentNotificationFailed);
end;

function TJvTFSched.GetConControl(Index: integer): TJvTFControl;
begin
  Result := TJvTFControl(FConControls.Objects[Index]);
end;

function TJvTFSched.GetConComponent(Index: integer): TJvTFComponent;
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
  I: integer;
begin
  I := FAppts.IndexOf(Appt.ID);
  if I > -1 then
    FAppts.Delete(I);
end;

procedure TJvTFSched.ConnectionsOnChange(Sender: TObject);
begin
  if (FConControls.Count = 0) and (FConComponents.Count = 0) then
  begin
    FCached := true;
    FCachedTime := Windows.GetTickCount;
  end
  else
    FCached := false;
end;

procedure TJvTFSched.CheckConnections;
var
  I: integer;
  Appt: TJvTFAppt;
  DateHit,
    NameMatch,
    NotConnected: boolean;
begin
  // Check each appt in the ScheduleManager to see if that appt should be connected
  //  to this schedule.  If so, then connect it.
  for I := 0 to ScheduleManager.ApptCount - 1 do
  begin
    Appt := ScheduleManager.Appts[I];
    DateHit := (trunc(SchedDate) >= trunc(Appt.StartDate)) and
      (trunc(SchedDate) <= trunc(Appt.EndDate));
    NameMatch := Appt.IndexOfSchedule(SchedName) > -1;
    NotConnected := ApptByID(Appt.ID) = nil;
    if DateHit and NameMatch and NotConnected then
      Appt.Notify(Self, sncConnectAppt);
  end;
end;

function TJvTFSched.GetFreeUsedTime(FreeTime: boolean): TDynTimeRangeArray;
var
  // 60 mins X 24 hrs = 1440 ==> minutes in a day
  DayArray: array[0..1439] of boolean; // I'm a poet and don't know it.
  I,
    J,
    MinStart,
    MinEnd: integer;
  anAppt: TJvTFAppt;
  StartTime,
    EndTime: TTime;
  Switch,
    MinIsFree,
    InRange: boolean;

               ////////////////////////////////
               // SUBORDINATE ROUTINES
               ////////////////////////////////

  function TimeToMinNum(ATime: TTime): integer;
  var
    H, M, S, MS: Word;
  begin
    DecodeTime(ATime, H, M, S, MS);
    Result := H * 60 + M;
  end;

  function MinNumToTime(MinNum: integer): TTime;
  begin
    Result := EncodeTime(MinNum div 60, MinNum mod 60, 0, 0);
  end;

  procedure StartRange;
  begin
    StartTime := MinNumToTime(I);
    InRange := true;
  end;

  procedure EndRange;
  begin
    EndTime := MinNumToTime(I);

                // add range to resultant array
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].StartTime := StartTime;
    Result[High(Result)].EndTime := EndTime;

    InRange := false;
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
    DayArray[I] := true;

  // Go through the appts and mark used minutes in the working array
  for I := 0 to ApptCount - 1 do
  begin
    anAppt := Appts[I];
    MinStart := TimeToMinNum(anAppt.StartTime);
    MinEnd := TimeToMinNum(AdjustEndTime(anAppt.EndTime));

    for J := MinStart to MinEnd do
      DayArray[J] := false;
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

function TJvTFSched.ApptCount: integer;
begin
  Result := FAppts.Count;
end;

function TJvTFSched.ApptByID(ID: string): TJvTFAppt;
var
  I: integer;
begin
  Result := nil;
  I := FAppts.IndexOf(ID);
  if I > -1 then
    Result := TJvTFAppt(FAppts.Objects[I]);
end;

function TJvTFSched.ConControlCount: integer;
begin
  Result := FConControls.Count;
end;

function TJvTFSched.ConComponentCount: integer;
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
  I: integer;
begin
  for I := 0 to ApptCount - 1 do
    ScheduleManager.dbPostAppt(Appts[I]);
end;


function TJvTFSched.GetFreeTime: TDynTimeRangeArray;
begin
  Result := GetFreeUsedTime(true);
end;

function TJvTFSched.GetUsedTime: TDynTimeRangeArray;
begin
  Result := GetFreeUsedTime(false);
end;

function TJvTFSched.TimeIsFree(TimeRange: TJvTFTimeRange): boolean;
var
  Appt: TJvTFAppt;
  I: integer;
begin
  Result := true;
  I := 0;

  while (I < ApptCount) and Result do
  begin
    Appt := Appts[I];
    if (Frac(Appt.StartTime) <= Frac(AdjustEndTime(TimeRange.EndTime))) and
      (Frac(AdjustEndTime(Appt.EndTime)) >= Frac(TimeRange.StartTime)) then
      Result := false
    else
      Inc(I);
  end;
end;

function TJvTFSched.TimeIsFree(RangeStart, RangeEnd: TTime): boolean;
var
  TimeRange: TJvTFTimeRange;
begin
  TimeRange.StartTime := RangeStart;
  TimeRange.EndTime := RangeEnd;
  Result := TimeIsFree(TimeRange);
end;

function TJvTFSched.ApptHasConflicts(anAppt: TJvTFAppt): boolean;
var
  Appt: TJvTFAppt;
  I: integer;
begin
  Result := false;
  I := 0;

  while (I < ApptCount) and not Result do
  begin
    Appt := Appts[I];
    if (Appt <> anAppt) and // Don't flag for the given appt
      (Frac(Appt.StartTime) <= Frac(AdjustEndTime(anAppt.EndTime))) and
      (Frac(AdjustEndTime(Appt.EndTime)) >= Frac(anAppt.StartTime)) then
      Result := true
    else
      Inc(I);
  end;
end;

function TJvTFSched.EnumConflicts(TimeRange: TJvTFTimeRange): TDynApptArray;
var
  Appt: TJvTFAppt;
  I: integer;
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
  I: integer;
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
  I: integer;
  anAppt: TJvTFAppt;
begin
  Result := nil;
  I := 0;
  while (I < ApptCount) do
  begin
    anAppt := Appts[I];
    if trunc(anAppt.StartDate) < trunc(SchedDate) then
    begin
      Result := anAppt;
      break; // APPOINTMENT STARTS AT 0:00 (12:00am) SO LEAVE LOOP
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
  I: integer;
  anAppt: TJvTFAppt;
begin
  Result := nil;
  I := 0;
  while (I < ApptCount) do
  begin
    anAppt := Appts[I];
    if trunc(anAppt.EndDate) > trunc(SchedDate) then
    begin
      Result := anAppt;
      break; // APPOINTMENT ENDS AT 23:59 (11:59pm) SO LEAVE LOOP
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

procedure TJvTFSched.SetSchedDisplayName(Value: string);
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

procedure TJvTFScheduleManagerCache.SetTimedDelay(Value: integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FTimedDelay then
  begin
    FTimedDelay := Value;
    FTimer.Enabled := false;
    FTimer.Interval := Value;
    if CacheType = ctTimed then
    begin
      FTimer.Enabled := true;
      FlushManager;
    end;
  end;
end;

procedure TJvTFScheduleManagerCache.SetBufferCount(Value: integer);
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
    FScheduleManager.Flush(false);
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
      FTimer.Enabled := false;
      FTimer.Interval := FTimedDelay;
      FTimer.Enabled := FCacheType = ctTimed;
    end;
    FlushManager;
  end
  else
    inherited Assign(Source);
end;

{ TJvTFScheduleManager }

class function TJvTFScheduleManager.GetScheduleID(SchedName: string;
  SchedDate: TDate): string;
begin
  Result := SchedName + IntToStr(trunc(SchedDate));
end;

class function TJvTFScheduleManager.GenerateApptID: string;
var
  I: integer;
begin
  Result := FloatToStr(Now);
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
  FSchedBatch.Sorted := true;
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
  FDestroying := true;

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

function TJvTFScheduleManager.GetAppt(Index: integer): TJvTFAppt;
begin
  Result := TJvTFAppt(FAppts.Objects[Index]);
end;

function TJvTFScheduleManager.GetSchedule(Index: integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

function TJvTFScheduleManager.GetConControl(Index: integer): TJvTFControl;
begin
  Result := TJvTFControl(FConControls.Objects[Index]);
end;

function TJvTFScheduleManager.GetConComponent(Index: integer): TJvTFComponent;
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
  I: integer;
begin
  if not Assigned(ApptCtrl) then
    Exit;

  I := FConControls.IndexOfObject(ApptCtrl);
  if I = -1 then
    FConControls.AddObject('', ApptCtrl);
end;

procedure TJvTFScheduleManager.DisconnectControl(ApptCtrl: TJvTFControl);
var
  I: integer;
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
  I: integer;
begin
  if not Assigned(Comp) then
    Exit;

  I := FConComponents.IndexOfObject(Comp);
  if I = -1 then
    FConComponents.AddObject('', Comp);
end;

procedure TJvTFScheduleManager.DisconnectComponent(Comp: TJvTFComponent);
var
  I: integer;
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

procedure TJvTFScheduleManager.RetrieveSchedule(SchedName: string; SchedDate: TDate;
  var Schedule: TJvTFSched; var LoadedNow: boolean);
var
  SchedID: string;
  I: integer;
begin
  SchedID := GetScheduleID(SchedName, SchedDate);
  I := FSchedules.IndexOf(SchedID);

  if I > -1 then
  begin
    Schedule := TJvTFSched(FSchedules.Objects[I]);
    LoadedNow := false;
  end
  else
  begin
      //Schedule := TJvTFSched.Create(Self, SchedName, SchedDate);
    Schedule := GetSchedClass.Create(Self, SchedName, SchedDate);
    FSchedules.AddObject(SchedID, Schedule);
    LoadedNow := true;
    if Cache.CacheType = ctBuffer then
      Flush(false);
    Schedule.CheckConnections;
  end;
end;

procedure TJvTFScheduleManager.NeedAppts(Schedule: TJvTFSched);
begin
  FLoadingAppts := true;
  try
    if Assigned(FOnNeedAppts) then
      FOnNeedAppts(Self, Schedule);
  finally
    FLoadingAppts := false;
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
  I: integer;
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
  I: integer;
begin
  for I := 0 to ConControlCount - 1 do
    NotifyApptCtrl(ConControls[I], Sched, sncDestroySchedule);

  for I := 0 to ConComponentCount - 1 do
    NotifyComp(ConComponents[I], Sched, sncDestroySchedule);

  FSchedules.Delete(FSchedules.IndexOfObject(Sched));
  Flush(false);
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
  I: integer;
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

function TJvTFScheduleManager.FlushObject(FlushObj: TObject): boolean;
var
  FlushIt: boolean;
begin
  Result := false;
  if Assigned(FlushObj) then
  begin
    FlushIt := true;
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

function TJvTFScheduleManager.ApptCount: integer;
begin
  Result := FAppts.Count;
end;

function TJvTFScheduleManager.FindAppt(ID: string): TJvTFAppt;
var
  I: integer;
begin
  Result := nil;
  I := FAppts.IndexOf(ID);
  if I > -1 then
    Result := TJvTFAppt(FAppts.Objects[I]);
end;

function TJvTFScheduleManager.ScheduleCount: integer;
begin
  Result := FSchedules.Count;
end;

function TJvTFScheduleManager.FindSchedule(SchedName: string;
  SchedDate: TDate): TJvTFSched;
var
  I: integer;
begin
  Result := nil;
  I := FSchedules.IndexOf(GetScheduleID(SchedName, SchedDate));
  if I > -1 then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFScheduleManager.ConControlCount: integer;
begin
  Result := FConControls.Count;
end;

function TJvTFScheduleManager.ConComponentCount: integer;
begin
  Result := FConComponents.Count;
end;

function TJvTFScheduleManager.RequestSchedule(ApptCtrl: TJvTFControl;
  SchedName: string; SchedDate: TDate): TJvTFSched;
var
  ApptsNeeded: boolean;
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
  SchedName: string; SchedDate: TDate; var LoadedNow: boolean): TJvTFSched;
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
  SchedName: string; SchedDate: TDate): TJvTFSched;
var
  ApptsNeeded: boolean;
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
  SchedName: string; SchedDate: TDate; var LoadedNow: boolean): TJvTFSched;
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
  SchedName: string; SchedDate: TDate);
var
  SchedID: string;
  I: integer;
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
      Flush(false);
  end;
end;


procedure TJvTFScheduleManager.ReleaseSchedule(Comp: TJvTFComponent;
  SchedName: string; SchedDate: TDate);
var
  SchedID: string;
  I: integer;
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
      Flush(false);
  end;
end;

procedure TJvTFScheduleManager.RequestAppt(ID: string; var Appt: TJvTFAppt;
  var New: boolean);
var
  I: integer;
begin
  I := -1;
  if ID <> '' then
    I := FAppts.IndexOf(ID);

  if I > -1 then
  begin
    Appt := TJvTFAppt(FAppts.Objects[I]);
    New := false;
  end
  else
  begin
      //Appt := TJvTFAppt.Create(Self, ID);
    Appt := GetApptClass.Create(Self, ID);
    New := true;
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
    FRefreshing := true;
    try
      Appt.Notify(Self, sncRefresh);
      if Assigned(FOnRefreshAppt) then
        FOnRefreshAppt(Self, Appt);
      if RefreshAutoReconcile then
        ReconcileRefresh(Appt);
    finally
      FRefreshing := false;

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

function TJvTFScheduleManager.dbNewAppt(ID: string): TJvTFAppt;
var
  New: boolean;
begin
  Result := nil;
  RequestAppt(ID, Result, New);
  if not New then
    raise EJvTFScheduleManagerError.Create(sCouldNotCreateNewAppointment);
end;

procedure TJvTFScheduleManager.PostAppts;
var
  I: integer;
begin
  for I := 0 to ApptCount - 1 do
    dbPostAppt(Appts[I]);
end;

procedure TJvTFScheduleManager.RefreshAppts;
var
  I: integer;
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
  I: integer;
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
    raise EJvTFScheduleManagerError.Create(sInvalidTriggerForRefreshControls)
end;

procedure TJvTFScheduleManager.Flush(All: boolean); //param All defaults to False
var
  I: integer;
  Sched: TJvTFSched;
  MRUList: TStringlist;
  CacheTimeUp: boolean;
begin
  if FFlushing or FDestroying then
    Exit;

  FFlushing := true;
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
        CacheTimeUp := Windows.GetTickCount - Sched.CachedTime >=
          UINT(Cache.TimedDelay);
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
        MRUList.Sorted := true;
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
    FFlushing := false;
  end;
end;

procedure TJvTFScheduleManager.dbRefreshAll;
var
  I: integer;
begin
  FRefreshing := true;
  try
    for I := 0 to ApptCount - 1 do
      NotifyAppt(Appts[I], Self, sncRefresh);
    if Assigned(FOnRefreshAll) then
      FOnRefreshAll(Self);
    if RefreshAutoReconcile then
      ReconcileRefresh(Self);
  finally
    FRefreshing := false;
    RefreshConnections(nil);
  end;
end;

procedure TJvTFScheduleManager.dbRefreshOrphans;
var
  I: integer;
begin
  for I := 0 to ApptCount - 1 do
    if Appts[I].ConnectionCount = 0 then
      dbRefreshAppt(Appts[I]);
end;

procedure TJvTFScheduleManager.dbRefreshSched(Sched: TJvTFSched);
var
  I: integer;
begin
  if Assigned(Sched) then
  begin
    FRefreshing := true;
    try
      for I := 0 to Sched.ApptCount - 1 do
        NotifyAppt(Sched.Appts[I], Self, sncRefresh);
      if Assigned(FOnRefreshSched) then
        FOnRefreshSched(Self, Sched);
      if RefreshAutoReconcile then
        ReconcileRefresh(Sched);
    finally
      FRefreshing := false;
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
  I: integer;
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
  FLoadingAppts := true;
  try
    // Prime the process (reminds me of COBOL - yuck!)
    aSched := TJvTFSched(FSchedBatch.Objects[0]);
    UpdateCompares(aSched);
    NewBatch(aSched);

    for I := 1 to FSchedBatch.Count - 1 do
    begin
      aSched := TJvTFSched(FSchedBatch.Objects[I]);

      if (aSched.SchedName <> CompName) or
        (trunc(aSched.SchedDate) - 1 <> trunc(CompDate)) then
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
    FLoadingAppts := false;
    // added by Mike 1/14/01
    RefreshConnections(nil);
  end;
end;

procedure TJvTFScheduleManager.LoadBatch(BatchName: string; BatchStartDate,
  BatchEndDate: TDate);
begin
  if Assigned(FOnLoadBatch) then
    FOnLoadBatch(Self, BatchName, BatchStartDate, BatchEndDate);
end;

function TJvTFScheduleManager.QueryPostAppt(Appt: TJvTFAppt): boolean;
begin
  Result := true;
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
  I: integer;
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
    raise EJvTFScheduleManagerError.Create(sInvalidScopeInReconcileRefresh);
end;

procedure TJvTFScheduleManager.SetRefreshAutoReconcile(Value: boolean);
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
  PrepTimer(true);
end;

destructor TJvTFHint.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TJvTFHint.SetPause(Value: integer);
begin
  FPause := Value;
end;

procedure TJvTFHint.SetShortPause(Value: integer);
begin
  FShortPause := Value;
end;

procedure TJvTFHint.TimerOnTimer(Sender: TObject);
begin
  FTimer.Enabled := false;

  if FShortTimer then
    DoHint(false)
  else
  begin
    ReleaseHandle;
    PrepTimer(true);
  end;
end;

procedure TJvTFHint.PrepTimer(Short: boolean);
begin
  ReleaseHandle;
  FShortTimer := Short;
  if Short then
    FTimer.Interval := FShortPause
  else
    FTimer.Interval := FPause;
end;

procedure TJvTFHint.SetHintText(StartDate, EndDate: TDate; StartTime,
  EndTime: TTime; Desc: string; ShowDatesTimes, ShowDesc: boolean);
var
  ShowDates: boolean;
  HintText,
    DFormat,
    TFormat: string;
begin
  HintText := '';
  if ShowDatesTimes then
  begin
    DFormat := FApptCtrl.DateFormat;
    TFormat := FApptCtrl.TimeFormat;
    ShowDates := trunc(StartDate) <> trunc(EndDate);

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

procedure TJvTFHint.DoHint(Sustained: boolean);
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

  if not Windows.IsRectEmpty(FHintRect) and (FHintText <> '') then
    if Sustained then
    begin
      inherited ActivateHint(FHintRect, FHintText);
    end
    else
      ActivateHint(FHintRect, FHintText);
end;

procedure TJvTFHint.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not CS_SAVEBITS;
  end;
end;

procedure TJvTFHint.ActivateHint(Rect: TRect; const AHint: string);
begin
  PrepTimer(false);
  inherited;
  // Reset the timer so we get the full interval
  FTimer.Enabled := false;
  FTimer.Enabled := true;
end;

procedure TJvTFHint.ApptHint(Appt: TJvTFAppt; X, Y: integer; ShowDatesTimes,
  ShowDesc, FormattedDesc: boolean);
var
  HintTopLeft: TPoint;
  Immediate: boolean;
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
        DoHint(false)
      else
      begin
        PrepTimer(true);
        FTimer.Enabled := true;
      end;
    end
    else
    begin
      ReleaseHandle;
      PrepTimer(true);
    end;
  end;
end;

procedure TJvTFHint.StartEndHint(StartDate, EndDate: TDate; StartTime,
  EndTime: TTime; X, Y: integer; ShowDates: boolean);
var
  HintTopLeft: TPoint;
begin
  FHintType := shtStartEnd;
  SetHintText(StartDate, EndDate, StartTime, EndTime, '', true, false);
  FHintRect := CalcHintRect(FApptCtrl.Width, FHintText, nil);
  HintTopLeft := FApptCtrl.ClientToScreen(Point(X, Y));
  FHintRect := MoveRect(FHintRect, HintTopLeft.X, HintTopLeft.Y);
  if HandleAllocated and Showing then
    BoundsRect := FHintRect
  else
    DoHint(true);
end;

procedure TJvTFHint.CellHint(Row, Col: integer; HintText: string; CellRect: TRect);
var
  Immediate: boolean;
  DiffCell: boolean;
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
        DoHint(false)
      else
      begin
        PrepTimer(true);
        FTimer.Enabled := true;
      end;
    end
    else
    begin
      ReleaseHandle;
      PrepTimer(true);
    end;
  end;
end;

procedure TJvTFHint.ReleaseHandle;
begin
  FTimer.Enabled := false;
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

procedure TJvTFHint.MultiLineObjHint(Obj: TObject; X, Y: integer;
  Hints: TStrings);
var
  Immediate: boolean;
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
        DoHint(false)
      else
      begin
        PrepTimer(true);
        FTimer.Enabled := true;
      end;
    end
    else
    begin
      ReleaseHandle;
      PrepTimer(true);
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

function TJvTFControl.GetSchedule(Index: integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

procedure TJvTFControl.SetDateFormat(Value: string);
begin
  if FDateFormat <> Value then
  begin
    FDateFormat := Value;
    Invalidate;
  end;
end;

procedure TJvTFControl.SetTimeFormat(Value: string);
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
  I: integer;
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
    raise EJvTFScheduleManagerError.Create(sScheduleManagerNotificationFailedSc);
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

procedure TJvTFControl.DoEndDrag(Target: TObject; X, Y: integer);
begin
  inherited;

  FDragInfo.Free;
  FDragInfo := nil;
end;

procedure TJvTFControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  FShift := Shift;
end;

function TJvTFControl.ScheduleCount: integer;
begin
  Result := FSchedules.Count;
end;

function TJvTFControl.FindSchedule(SchedName: string;
  SchedDate: TDate): TJvTFSched;
var
  I: integer;
begin
  Result := nil;

  I := FSchedules.IndexOf(TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate));
  if I > -1 then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFControl.RetrieveSchedule(SchedName: string;
  SchedDate: TDate): TJvTFSched;
begin
  Result := FindSchedule(SchedName, SchedDate);

  if not Assigned(Result) then
    if Assigned(ScheduleManager) then
      Result := ScheduleManager.RequestSchedule(Self, SchedName, SchedDate)
    else
      raise EJvTFScheduleManagerError.Create(sCouldNotRetrieveSchedule);
end;

procedure TJvTFControl.ReleaseSchedule(SchedName: string;
  SchedDate: TDate);
var
  SchedID: string;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate);
  if FSchedules.IndexOf(SchedID) > -1 then
    if Assigned(ScheduleManager) then
      ScheduleManager.ReleaseSchedule(Self, SchedName, SchedDate)
    else
      raise EJvTFScheduleManagerError.Create(sCouldNotReleaseSchedule);

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

function TJvTFComponent.FindSchedule(SchedName: string;
  SchedDate: TDate): TJvTFSched;
var
  I: integer;
begin
  Result := nil;

  I := FSchedules.IndexOf(TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate));
  if I > -1 then
    Result := TJvTFSched(FSchedules.Objects[I]);
end;

function TJvTFComponent.GetSchedule(Index: integer): TJvTFSched;
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
    raise EJvTFScheduleManagerError.Create(sScheduleManagerNotificationFailedSc);
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

procedure TJvTFComponent.ReleaseSchedule(SchedName: string;
  SchedDate: TDate);
var
  SchedID: string;
begin
  SchedID := TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate);
  if FSchedules.IndexOf(SchedID) > -1 then
    if Assigned(ScheduleManager) then
      ScheduleManager.ReleaseSchedule(Self, SchedName, SchedDate)
    else
      raise EJvTFScheduleManagerError.Create(sCouldNotReleaseSchedule);
end;

procedure TJvTFComponent.ReleaseSchedules;
begin
  while ScheduleCount > 0 do
    ReleaseSchedule(Schedules[0].SchedName, Schedules[0].SchedDate);
end;

procedure TJvTFComponent.RelSchedNotification(Schedule: TJvTFSched);
var
  I: integer;
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

function TJvTFComponent.RetrieveSchedule(SchedName: string;
  SchedDate: TDate): TJvTFSched;
begin
  Result := FindSchedule(SchedName, SchedDate);

  if not Assigned(Result) then
    if Assigned(ScheduleManager) then
      Result := ScheduleManager.RequestSchedule(Self, SchedName, SchedDate)
    else
      raise EJvTFScheduleManagerError.Create(sCouldNotRetrieveSchedule);
end;

function TJvTFComponent.ScheduleCount: integer;
begin
  Result := FSchedules.Count;
end;

procedure TJvTFComponent.SetDateFormat(Value: string);
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

procedure TJvTFComponent.SetTimeFormat(Value: string);
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
    FAborted := true;
end;

function TJvTFPrinter.ConvertMeasure(Value: integer; FromMeasure,
  ToMeasure: TJvTFPrinterMeasure; Horizontal: boolean): integer;
const
  MMFactor = 2.54;
var
  PPI: integer;
begin
  if Horizontal then
    PPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  else
    PPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);

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
  inherited;

  CreateLayout;
  FMeasure := pmInches;
  FPages := TStringlist.Create;
  FBodies := TStringlist.Create;
  InitializeMargins;
end;

procedure TJvTFPrinter.CreateDoc;
begin
  if State = spsNoDoc then
  begin
    FState := spsCreating;
    FAborted := false;

    FDocDateTime := Now;
    if DirectPrint then
      Printer.BeginDoc;
  end
  else
    raise EJvTFPrinterError.Create(sCouldNotCreateADocumentBecauseA);
end;

procedure TJvTFPrinter.CreateLayout;
begin
  FPageLayout := TJvTFPrinterPageLayout.Create(Self);
end;

destructor TJvTFPrinter.Destroy;
begin
  FreeDoc;
  FBodies.Free;
  FPages.Free;

  FPageLayout.Free;
  inherited;
end;

procedure TJvTFPrinter.DrawBody(aCanvas: TCanvas; ARect: TRect;
  PageNum: integer);
begin
  if Assigned(FOnDrawBody) then
    FOnDrawBody(Self, aCanvas, ARect, PageNum);
end;

procedure TJvTFPrinter.DrawFooter(aCanvas: TCanvas; ARect: TRect;
  PageNum: integer);
begin
  if Assigned(FOnDrawFooter) then
    FOnDrawFooter(Self, aCanvas, ARect, PageNum);
end;

procedure TJvTFPrinter.DrawHeader(aCanvas: TCanvas; ARect: TRect;
  PageNum: integer);
begin
  if Assigned(FOnDrawHeader) then
    FOnDrawHeader(Self, aCanvas, ARect, PageNum);
end;

procedure TJvTFPrinter.FinishDoc;
var
  I: integer;
  aCanvas: TMetafileCanvas;
  HeaderRect,
    FooterRect: TRect;
begin
  if Aborted then
    Exit;

  if State <> spsCreating then
    raise EJvTFPrinterError.Create(sCouldNotFinishDocumentBecauseNo);

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

function TJvTFPrinter.GetBodyHeight: integer; // always in pixels
var
  PhysHeight,
    TopMarginPels,
    BottomMarginPels,
    HeaderPels,
    FooterPels: integer;
begin
  PhysHeight := Windows.GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  TopMarginPels := ConvertMeasure(PageLayout.MarginTop, Measure, pmPixels,
    false);
  BottomMarginPels := ConvertMeasure(PageLayout.MarginBottom, Measure, pmPixels,
    false);
  HeaderPels := ConvertMeasure(PageLayout.HeaderHeight, Measure, pmPixels,
    false);
  FooterPels := ConvertMeasure(PageLayout.FooterHeight, Measure, pmPixels,
    false);

  Result := PhysHeight - TopMarginPels - BottomMarginPels -
    HeaderPels - FooterPels;
end;

function TJvTFPrinter.GetBodyLeft: integer; // always in pixels
begin
  Result := GetMarginOffset(1);
end;

function TJvTFPrinter.GetBodyTop: integer; // always in pixels
begin
  Result := GetMarginOffset(2) +
    ConvertMeasure(PageLayout.HeaderHeight, Measure, pmPixels, false) + 1;
end;

function TJvTFPrinter.GetBodyWidth: integer; // always in pixels
var
  PhysWidth,
    LeftMarginPels,
    RightMarginPels: integer;
begin
  PhysWidth := Windows.GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  LeftMarginPels := ConvertMeasure(PageLayout.MarginLeft, Measure, pmPixels, true);
  RightMarginPels := ConvertMeasure(PageLayout.MarginRight, Measure, pmPixels, true);

  Result := PhysWidth - LeftMarginPels - RightMarginPels;
end;

function TJvTFPrinter.GetDocDateTime: TDateTime;
begin
  if State = spsNoDoc then
    raise EJvTFPrinterError.Create(sDocumentDoesNotExist);

  Result := FDocDateTime;
end;

procedure TJvTFPrinter.GetHeaderFooterRects(var HeaderRect,
  FooterRect: TRect);
begin
  HeaderRect.Left := FMarginOffsets.Left;
  HeaderRect.Top := FMarginOffsets.Top;
  HeaderRect.Right := HeaderRect.Left + BodyWidth;
  HeaderRect.Bottom := HeaderRect.Top + ConvertMeasure(PageLayout.HeaderHeight,
    Measure, pmPixels, false);

  FooterRect.Left := HeaderRect.Left;
  FooterRect.Right := HeaderRect.Right;
  FooterRect.Top := BodyTop + BodyHeight;
  FooterRect.Bottom := FooterRect.Top + ConvertMeasure(PageLayout.FooterHeight,
    Measure, pmPixels, false);
end;

function TJvTFPrinter.GetMarginOffset(Index: integer): integer;
begin
  case Index of
    1: Result := FMarginOffsets.Left;
    2: Result := FMarginOffsets.Top;
    3: Result := FMarginOffsets.Right;
  else
    Result := FMarginOffsets.Bottom;
  end;
end;

function TJvTFPrinter.GetPage(Index: integer): TMetafile;
begin
  if DirectPrint then
    raise EJvTFPrinterError.Create(sDocumentPagesCannotBeAccessedIf);

  if State <> spsFinished then
    raise EJvTFPrinterError.Create(sDocumentPagesAreInaccessibleUntil);
  Result := TMetafile(FPages.Objects[Index]);
end;

function TJvTFPrinter.GetPageCount: integer;
begin
  case State of
    spsNoDoc: raise EJvTFPrinterError.Create(sCouldNotRetrievePageCount);
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
    HeightPrintable: integer;
begin
  LeftMarg := Windows.GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  TopMarg := Windows.GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  WidthPaper := Windows.GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  HeightPaper := Windows.GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
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
    NewMargin: integer;
  Horz: boolean;
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
  aCanvas.Font.PixelsPerInch := Windows.GetDeviceCaps(Printer.Handle,
    LOGPIXELSX);

  Windows.SetViewPortOrgEx(aCanvas.Handle, BodyLeft, BodyTop, nil);
  DrawBody(aCanvas, Rect(BodyLeft, BodyTop, BodyWidth - BodyLeft,
    BodyHeight - BodyTop), FPages.Count);
  Windows.SetViewPortOrgEx(aCanvas.Handle, 0, 0, nil);

  if DirectPrint then
  begin
    GetHeaderFooterRects(HeaderRect, FooterRect);
    DrawHeader(aCanvas, HeaderRect, PageCount);
    DrawFooter(aCanvas, FooterRect, PageCount);
  end;
end;

procedure TJvTFPrinter.Print;
var
  I: integer;
begin
  if Aborted or DirectPrint then
    Exit;

  if State <> spsFinished then
    raise EJvTFPrinterError.Create(sOnlyAFinishedDocumentCanBePrinted);
  if PageCount = 0 then
    raise EJvTFPrinterError.Create(sThereAreNoPagesToPrint);

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

function TJvTFPrinter.PrinterToScreen(Value: integer;
  Horizontal: boolean): integer;
var
  ScreenPPI,
    PrinterPPI: integer;
begin
  ScreenPPI := Screen.PixelsPerInch;
  if Horizontal then
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX)
  else
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);

  Result := trunc(ScreenPPI / PrinterPPI * Value);
end;

procedure TJvTFPrinter.SaveDocToFiles(BaseFileName: TFileName);
var
  I: integer;
begin
  if State <> spsFinished then
    raise EJvTFPrinterError.Create(sDocumentMustBeFinishedToSaveToFile);

  for I := 0 to PageCount - 1 do
    Pages[I].SaveToFile(BaseFileName + '_' + IntToStr(I + 1) + '.emf');
end;

function TJvTFPrinter.ScreenToPrinter(Value: integer;
  Horizontal: boolean): integer;
var
  ScreenPPI,
    PrinterPPI: integer;
begin
  ScreenPPI := Screen.PixelsPerInch;
  if Horizontal then
  begin
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  end
  else
  begin
    PrinterPPI := Windows.GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  end;

  Result := trunc(PrinterPPI / ScreenPPI * Value);
end;

procedure TJvTFPrinter.SetDirectPrint(Value: boolean);
begin
  SetPropertyCheck;
  FDirectPrint := Value;
end;

procedure TJvTFPrinter.SetMarginOffset(Index, Value: integer);
begin
  // Allow negative value...
  // SetMargin will catch that case and throw exception
  case Index of
    1: FMarginOffsets.Left := Value;
    2: FMarginOffsets.Top := Value;
    3: FMarginOffsets.Right := Value;
  else
    FMarginOffsets.Bottom := Value;
  end;
end;

procedure TJvTFPrinter.SetMeasure(Value: TJvTFPrinterMeasure);
begin
  try
    FConvertingProps := true;
    if Value <> FMeasure then
    begin
      PageLayout.FHeaderHeight := ConvertMeasure(PageLayout.FHeaderHeight,
        FMeasure, Value, false);
      PageLayout.FFooterHeight := ConvertMeasure(PageLayout.FFooterHeight,
        FMeasure, Value, false);

      PageLayout.FMargins.Left := ConvertMeasure(PageLayout.FMargins.Left,
        FMeasure, Value, true);
      PageLayout.FMargins.Right := ConvertMeasure(PageLayout.FMargins.Right,
        FMeasure, Value, true);
      PageLayout.FMargins.Top := ConvertMeasure(PageLayout.FMargins.Top,
        FMeasure, Value, false);
      PageLayout.FMargins.Bottom := ConvertMeasure(PageLayout.FMargins.Bottom,
        FMeasure, Value, false);
      FMeasure := Value;
    end;
  finally
    FConvertingProps := false;
  end;
end;

procedure TJvTFPrinter.SetPageLayout(Value: TJvTFPrinterPageLayout);
begin
  FPageLayout.Assign(Value);
end;

procedure TJvTFPrinter.SetPropertyCheck;
begin
  if (State <> spsNoDoc) and not ConvertingProps then
    raise EJvTFPrinterError.Create(sThisPropertyCannotBeChangedIfA);
end;

procedure TJvTFPrinter.SetTitle(Value: string);
begin
  FTitle := Value;
end;

{ TJvTFPrinterPageLayout }

procedure TJvTFPrinterPageLayout.Assign(Source: TPersistent);
var
  SourceMeas,
    DestMeas: TJvTFPrinterMeasure;
  WorkVal: integer;
  SourceLayout: TJvTFPrinterPageLayout;
begin
  if (Source is TJvTFPrinterPageLayout) and Assigned(Printer) and
    Assigned(TJvTFPrinterPageLayout(Source).Printer) then
  begin
    SourceLayout := TJvTFPrinterPageLayout(Source);
    SourceMeas := SourceLayout.Printer.Measure;
    DestMeas := Printer.Measure;

    WorkVal := SourceLayout.MarginLeft;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, true);
    SetMargin(1, WorkVal);

    WorkVal := SourceLayout.MarginTop;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, false);
    SetMargin(2, WorkVal);

    WorkVal := SourceLayout.MarginRight;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, true);
    SetMargin(3, WorkVal);

    WorkVal := SourceLayout.MarginBottom;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, false);
    SetMargin(4, WorkVal);

    WorkVal := SourceLayout.HeaderHeight;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, false);
    SetHeaderHeight(WorkVal);

    WorkVal := SourceLayout.FooterHeight;
    WorkVal := Printer.ConvertMeasure(WorkVal, SourceMeas, DestMeas, false);
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
    raise EJvTFPrinterError.Create(sCouldNotCreateTJvTFPrinterPageLayou);

  FPrinter := aPrinter;
end;

function TJvTFPrinterPageLayout.GetMargin(Index: integer): integer;
begin
  case Index of
    1: Result := FMargins.Left;
    2: Result := FMargins.Top;
    3: Result := FMargins.Right;
  else
    Result := FMargins.Bottom;
  end;
end;

procedure TJvTFPrinterPageLayout.SetFooterHeight(Value: integer);
var
  Check: integer;
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
      raise EJvTFPrinterError.CreateFmt(sInvalidFooterHeightd, [Value]);
    end
    else
      Change;
  end;
end;

procedure TJvTFPrinterPageLayout.SetHeaderHeight(Value: integer);
var
  Check: integer;
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
      raise EJvTFPrinterError.CreateFmt(sInvalidHeaderHeightd, [Value]);
    end
    else
      Change;
  end;
end;

procedure TJvTFPrinterPageLayout.SetMargin(Index, Value: integer);
var
  Unprintable,
    UserMarginPels,
    CurrMargin,
    NewMargin: integer;
  Horz,
    Err: boolean;
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
      Err := false;
      NewMargin := Value;
    end
    else
    begin
      Err := true;
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

procedure TJvTFHintProps.SetHintHidePause(Value: integer);
begin
  if Value < -1 then
    Value := -1;

  if Value <> FHintHidePause then
  begin
    FHintHidePause := Value;
    Change;
  end;
end;

procedure TJvTFHintProps.SetHintPause(Value: integer);
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

function TJvTFDWNames.GetDWN(Index: integer): string;
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

function TJvTFDWNames.GetDWName(DWIndex: integer): string;
begin
  case Source of
    dwnsSysLong: Result := SysUtils.LongDayNames[DWIndex];
    dwnsSysShort: Result := SysUtils.ShortDayNames[DWIndex];
  else // dwnsCustom
    Result := GetDWN(DWIndex);
  end;
end;

procedure TJvTFDWNames.SetDWN(Index: integer; Value: string);
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

function TJvTFDateList.Add(ADate: TDate): integer;
begin
  Result := FList.Add(IntToStr(trunc(ADate)));
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

function TJvTFDateList.Count: integer;
begin
  Result := FList.Count;
end;

constructor TJvTFDateList.Create;
begin
  inherited Create;
  FList := TStringlist.Create;
  FList.Sorted := true;
  FList.Duplicates := dupIgnore;
end;

procedure TJvTFDateList.Delete(Index: integer);
begin
  FList.Delete(Index);
  Change;
end;

destructor TJvTFDateList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TJvTFDateList.GetDate(Index: integer): TDate;
begin
  Result := StrToInt(FList[Index]);
end;

function TJvTFDateList.IndexOf(ADate: TDate): integer;
begin
  Result := FList.IndexOf(IntToStr(trunc(ADate)));
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

initialization
  Randomize;

end.

